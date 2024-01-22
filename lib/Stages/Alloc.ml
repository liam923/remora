open! Base

type host = Acorn.host [@@deriving sexp_of]
type device = Acorn.device [@@deriving sexp_of]

let getUsesInDimension Acorn.Index.{ const = _; refs; lens } =
  Set.union
    (refs |> Map.keys |> Set.of_list (module Identifier))
    (lens |> Map.keys |> Set.of_list (module Identifier))
;;

let getUsesInShapeElement = function
  | Acorn.Index.Add dim -> getUsesInDimension dim
  | Acorn.Index.ShapeRef ref -> Set.singleton (module Identifier) ref
;;

let getUsesInNeShape shape =
  shape
  |> NeList.to_list
  |> List.map ~f:getUsesInShapeElement
  |> Set.union_list (module Identifier)
;;

let rec getUsesInType =
  let open Acorn.Type in
  function
  | Atom (Literal _) -> Set.empty (module Identifier)
  | Array { element; shape } ->
    Set.union (getUsesInType (Atom element)) (getUsesInNeShape shape)
  | Atom (Sigma { parameters; body }) ->
    let params =
      parameters |> List.map ~f:(fun p -> p.binding) |> Set.of_list (module Identifier)
    in
    Set.diff (getUsesInType body) params
  | Tuple elements ->
    elements |> List.map ~f:getUsesInType |> Set.union_list (module Identifier)
;;

let convertDimension Corn.Index.{ const; refs } =
  Acorn.Index.{ const; refs; lens = Map.empty (module Identifier) }
;;

let convertShapeElement = function
  | Corn.Index.Add dim -> Acorn.Index.Add (convertDimension dim)
  | Corn.Index.ShapeRef ref -> Acorn.Index.ShapeRef ref
;;

let convertShape = List.map ~f:convertShapeElement

let getShapeLen shape =
  let open Acorn.Index in
  shape
  |> List.map ~f:(function
    | Add dim -> dim
    | ShapeRef ref -> dimensionLen ref)
  |> List.fold ~init:(dimensionConstant 0) ~f:addDimensions
;;

let rec convertParallelism (parallelism : Corn.Expr.parallelism) =
  match parallelism with
  | KnownParallelism n -> Acorn.Expr.KnownParallelism n
  | Parallelism { shape; rest } ->
    Acorn.Expr.Parallelism
      { shape = convertShapeElement shape; rest = convertParallelism rest }
  | MaxParallelism pars -> Acorn.Expr.MaxParallelism (List.map pars ~f:convertParallelism)
;;

let rec canonicalizeType (type' : Corn.Type.t) : Acorn.Type.t =
  match type' with
  | Tuple elements -> Tuple (canonicalizeTupleType elements)
  | Array { element; size } ->
    Acorn.Type.array ~element:(canonicalizeType element) ~size:(convertShapeElement size)
  | Sigma sigma -> Atom (Sigma (canonicalizeSigmaType sigma))
  | Literal literal -> Atom (Literal literal)

and canonicalizeSigmaType Corn.Type.{ parameters; body } =
  Acorn.Type.{ parameters; body = canonicalizeType body }

and canonicalizeTupleType elements = List.map elements ~f:canonicalizeType

let convertProduction ({ productionId; type' } : Corn.Expr.production)
  : Acorn.Expr.production
  =
  { productionId; type' = canonicalizeType type' }
;;

let rec convertProductionTuple : Corn.Expr.productionTuple -> Acorn.Expr.productionTuple
  = function
  | ProductionTuple { elements; type' } ->
    ProductionTuple
      { elements = List.map elements ~f:convertProductionTuple
      ; type' = canonicalizeType type'
      }
  | ProductionTupleAtom production -> ProductionTupleAtom (convertProduction production)
;;

(* Chop the head element of the shape of the array. If a tuple, do this recursively until
   an array is hit *)
let rec guillotineType (type' : Acorn.Type.t) ~expectedSize : Acorn.Type.t =
  match type' with
  | Array { element; shape = head :: rest } ->
    assert (Acorn.Index.equal_shapeElement head expectedSize);
    NeList.of_list rest
    |> Option.map ~f:(fun shape -> Acorn.Type.Array { element; shape })
    |> Option.value ~default:(Atom element)
  | Tuple elements ->
    Tuple
      (List.map elements ~f:(fun elementType -> guillotineType ~expectedSize elementType))
  | Atom _ -> raise @@ Unreachable.Error "Expected array type or tuple of arrays"
;;

let typeAsTuple (type' : Acorn.Type.t) : Acorn.Type.tuple =
  match type' with
  | Tuple elements -> elements
  | _ -> raise @@ Unimplemented.Error "expected tuple type"
;;

module MemSourceState = struct
  include State

  type 'a u = (Set.M(Identifier).t Map.M(Identifier).t, 'a, unit) State.t

  let withEnvExtensions extensions prog =
    let open Let_syntax in
    let%bind () =
      modify ~f:(fun init ->
        List.fold extensions ~init ~f:(fun env (key, data) -> Map.set env ~key ~data))
    in
    let%bind res = prog in
    let%bind () =
      modify ~f:(fun init ->
        List.fold extensions ~init ~f:(fun env (key, _) -> Map.remove env key))
    in
    return res
  ;;

  let writeToMem ~valueSources ~memSources =
    modify ~f:(fun env ->
      (* For every mem source appearing as a source in the env, all value sources must
         be added to that entry *)
      let partiallyExtendedEnv =
        Map.map env ~f:(fun sourceEntries ->
          if Set.is_empty (Set.inter sourceEntries memSources)
          then sourceEntries
          else Set.union sourceEntries valueSources)
      in
      (* For each mem source, its sources must be appended with the value sources *)
      Set.fold memSources ~init:partiallyExtendedEnv ~f:(fun env addrSource ->
        Map.update env addrSource ~f:(fun addrSourceEntries ->
          Set.union
            (Option.value addrSourceEntries ~default:(Set.empty (module Identifier)))
            valueSources)))
  ;;
end

let rec getPossibleMemSourcesInMem (mem : Acorn.Mem.t)
  : Set.M(Identifier).t MemSourceState.u
  =
  let open State.Let_syntax in
  match mem with
  | Ref { id; type' = _ } ->
    let%bind env = State.get () in
    return
    @@ Set.union
         (Map.find env id |> Option.value ~default:(Set.empty (module Identifier)))
         (Set.singleton (module Identifier) id)
  | TupleDeref { tuple; index = _; type' = _ } -> getPossibleMemSourcesInMem tuple
  | Values { elements; type' = _ } ->
    elements
    |> List.map ~f:getPossibleMemSourcesInMem
    |> State.all
    >>| Set.union_list (module Identifier)
  | Index { mem; offset = _; type' = _ } -> getPossibleMemSourcesInMem mem
;;

let rec getPossibleMemSources
  : type l. (l, 'd) Acorn.Expr.t -> Set.M(Identifier).t MemSourceState.u
  =
  let open State.Let_syntax in
  fun (expr : (_, _) Acorn.Expr.t) ->
    let getPossibleMemSourcesInLoopBlock
      : type m lInner p.
        (l, lInner, p, _, m) Acorn.Expr.loopBlock -> Set.M(Identifier).t MemSourceState.u
      =
      fun Acorn.Expr.
            { frameShape = _
            ; mapArgs
            ; mapMemArgs
            ; mapIotas = _
            ; mapBody
            ; mapBodyMatcher = _
            ; mapResults = _
            ; mapResultMemInterim
            ; mapResultMemFinal
            ; consumer
            ; type' = _
            } ->
      let mapArgExtensions =
        List.map mapArgs ~f:(fun { binding; ref } ->
          binding, Set.singleton (module Identifier) ref.id)
      in
      let%bind mapMemArgExtensions =
        mapMemArgs
        |> List.map ~f:(fun { memBinding; mem } ->
          let%bind sources = getPossibleMemSourcesInMem mem in
          return (memBinding, sources))
        |> MemSourceState.all
      in
      let mapEnvExtensions = mapArgExtensions @ mapMemArgExtensions in
      let%bind mapSources =
        MemSourceState.withEnvExtensions mapEnvExtensions @@ getPossibleMemSources mapBody
      in
      let%bind consumerSources =
        let iterUntilStable inputs ~f =
          (* Repeated run f, feeding its input into itself, until the input doesn't change.
             The env will not change from the runs besides the final one *)
          MemSourceState.make ~f:(fun env ->
            let rec iter inputs =
              let outEnv, outputs = MemSourceState.run (f inputs) env in
              if [%equal: Set.M(Identifier).t] inputs outputs
              then outEnv, outputs
              else iter outputs
            in
            iter inputs)
        in
        let getPossibleMemSourcesInReduce
          Acorn.Expr.{ arg; zero; body; d = _; character = _; type' = _ }
          =
          let%bind zeroSources =
            Option.value_map
              zero
              ~f:getPossibleMemSources
              ~default:(return @@ Set.empty (module Identifier))
          in
          let initialSources = Set.union zeroSources mapSources in
          iterUntilStable initialSources ~f:(fun sources ->
            let envExtensions =
              [ arg.firstBinding, sources; arg.secondBinding, sources ]
            in
            MemSourceState.withEnvExtensions envExtensions @@ getPossibleMemSources body)
        in
        match consumer with
        | Nothing -> return @@ Set.empty (module Identifier)
        | Just
            (ReducePar
              { reduce; interimResultMemInterim; interimResultMemFinal; outerBody }) ->
          let%bind innerSources = getPossibleMemSourcesInReduce reduce in
          let%bind interimResultMemInterimSources =
            getPossibleMemSourcesInMem interimResultMemInterim
          in
          let%bind () =
            MemSourceState.writeToMem
              ~valueSources:innerSources
              ~memSources:interimResultMemInterimSources
          in
          let%bind interimResultMemInterimSources =
            getPossibleMemSourcesInMem interimResultMemInterim
          in
          let%bind outerSources =
            iterUntilStable interimResultMemInterimSources ~f:(fun sources ->
              let envExtensions =
                [ reduce.arg.firstBinding, sources; reduce.arg.secondBinding, sources ]
              in
              MemSourceState.withEnvExtensions envExtensions
              @@ getPossibleMemSources outerBody)
          in
          let%bind interimResultMemInterimSources =
            getPossibleMemSourcesInMem interimResultMemInterim
          in
          let%bind () =
            MemSourceState.writeToMem
              ~valueSources:outerSources
              ~memSources:interimResultMemInterimSources
          in
          let%bind interimResultMemInterimSources =
            getPossibleMemSourcesInMem interimResultMemInterim
          in
          let%bind interimResultMemFinalSources =
            getPossibleMemSourcesInMem interimResultMemFinal
          in
          let%bind () =
            MemSourceState.writeToMem
              ~valueSources:interimResultMemInterimSources
              ~memSources:interimResultMemFinalSources
          in
          getPossibleMemSourcesInMem interimResultMemFinal
        | Just (ReduceSeq reduce) -> getPossibleMemSourcesInReduce reduce
        | Just
            (Fold
              { zeroArg; arrayArgs; mappedMemArgs; body; d = _; character = _; type' = _ })
          ->
          let%bind zeroSources = getPossibleMemSources zeroArg.zeroValue in
          let arrayArgsExtensions =
            List.map arrayArgs ~f:(fun { binding; production = _ } -> binding, mapSources)
          in
          let%bind mappedMemArgsExtensions =
            mappedMemArgs
            |> List.map ~f:(fun { memBinding; mem } ->
              let%bind sources = getPossibleMemSourcesInMem mem in
              return (memBinding, sources))
            |> MemSourceState.all
          in
          let nonZeroExtensions = arrayArgsExtensions @ mappedMemArgsExtensions in
          iterUntilStable zeroSources ~f:(fun zeroSources ->
            let envExtensions = (zeroArg.zeroBinding, zeroSources) :: nonZeroExtensions in
            MemSourceState.withEnvExtensions envExtensions @@ getPossibleMemSources body)
        | Just
            (Scatter
              { valuesArg = _
              ; indicesArg = _
              ; dIn = _
              ; dOut = _
              ; memInterim
              ; memFinal
              ; type' = _
              }) ->
          let%bind memInterimSources = getPossibleMemSourcesInMem memInterim in
          let%bind () =
            MemSourceState.writeToMem
              ~memSources:memInterimSources
              ~valueSources:mapSources
          in
          let%bind memInterimSources = getPossibleMemSourcesInMem memInterim in
          let%bind memFinalSources = getPossibleMemSourcesInMem memFinal in
          let%bind () =
            MemSourceState.writeToMem
              ~memSources:memFinalSources
              ~valueSources:memInterimSources
          in
          getPossibleMemSourcesInMem memFinal
      in
      let%bind mapResultMemInterimSources =
        getPossibleMemSourcesInMem mapResultMemInterim
      in
      let%bind () =
        MemSourceState.writeToMem
          ~memSources:mapResultMemInterimSources
          ~valueSources:mapSources
      in
      let%bind mapResultMemInterimSources =
        getPossibleMemSourcesInMem mapResultMemInterim
      in
      let%bind mapResultMemFinalSources = getPossibleMemSourcesInMem mapResultMemFinal in
      let%bind () =
        MemSourceState.writeToMem
          ~memSources:mapResultMemFinalSources
          ~valueSources:mapResultMemInterimSources
      in
      let%bind mapResultMemFinalSources = getPossibleMemSourcesInMem mapResultMemFinal in
      return @@ Set.union mapResultMemFinalSources consumerSources
    in
    match expr with
    | Ref { id; type' = _ } ->
      let%bind env = State.get () in
      return
      @@ Set.union
           (Map.find env id |> Option.value ~default:(Set.empty (module Identifier)))
           (Set.singleton (module Identifier) id)
    | Box { indices = _; body; type' = _ } -> getPossibleMemSources body
    | Literal (IntLiteral _ | FloatLiteral _ | CharacterLiteral _ | BooleanLiteral _) ->
      return @@ Set.empty (module Identifier)
    | ScalarPrimitive { op = _; args; type' = _ } ->
      args
      |> List.map ~f:getPossibleMemSources
      |> State.all
      >>| Set.union_list (module Identifier)
    | TupleDeref { tuple; index = _; type' = _ } -> getPossibleMemSources tuple
    | Values { elements; type' = _ } ->
      elements
      |> List.map ~f:getPossibleMemSources
      |> State.all
      >>| Set.union_list (module Identifier)
    | BoxValue { box; type' = _ } -> getPossibleMemSources box
    | IndexLet { indexArgs = _; body; type' = _ } -> getPossibleMemSources body
    | Let { args; body } ->
      let%bind envExtensions =
        args
        |> List.map ~f:(fun { binding; value } ->
          let%bind valueSources = getPossibleMemSources value in
          return (binding, valueSources))
        |> MemSourceState.all
      in
      MemSourceState.withEnvExtensions envExtensions @@ getPossibleMemSources body
    | MallocLet { memArgs; body } ->
      let envExtensions =
        List.map memArgs ~f:(fun { memBinding; memType = _; memLoc = _ } ->
          memBinding, Set.singleton (module Identifier) memBinding)
      in
      MemSourceState.withEnvExtensions envExtensions @@ getPossibleMemSources body
    | ReifyDimensionIndex { dim = _ } -> return @@ Set.empty (module Identifier)
    | LoopBlock loopBlock -> getPossibleMemSourcesInLoopBlock loopBlock
    | LoopKernel { kernel; captures = _; blocks = _; threads = _ } ->
      getPossibleMemSourcesInLoopBlock kernel
    | ContiguousSubArray
        { arrayArg; indexArg = _; originalShape = _; resultShape = _; type' = _ } ->
      getPossibleMemSources arrayArg
    | IfParallelismHitsCutoff { cutoff = _; then'; else'; parallelism = _; type' = _ } ->
      let%bind thenSources = getPossibleMemSources then' in
      let%bind elseSources = getPossibleMemSources else' in
      return @@ Set.union thenSources elseSources
    | Eseq { statement; expr; type' = _ } ->
      let%bind () = updateMemSourceEnvFromStatement statement in
      getPossibleMemSources expr
    | Getmem { addr; type' = _ } -> getPossibleMemSourcesInMem addr

and updateMemSourceEnvFromStatement
  : type l. (l, 'd) Acorn.Expr.statement -> unit MemSourceState.u
  =
  let open MemSourceState.Let_syntax in
  fun (stmnt : (_, _) Acorn.Expr.statement) ->
    match stmnt with
    | MapKernel
        { kernel = { map; mapResultMemInterim; mapResultMemFinal }
        ; captures = _
        ; blocks = _
        ; threads = _
        } ->
      let%bind () = updateMemSourceEnvFromMapInKernel map in
      let%bind mapResultMemInterimSources =
        getPossibleMemSourcesInMem mapResultMemInterim
      in
      let%bind mapResultMemFinalSources = getPossibleMemSourcesInMem mapResultMemFinal in
      MemSourceState.writeToMem
        ~valueSources:mapResultMemInterimSources
        ~memSources:mapResultMemFinalSources
    | Putmem { expr; addr; type' = _ } ->
      let%bind exprSources = getPossibleMemSources expr in
      let%bind addrSources = getPossibleMemSourcesInMem addr in
      MemSourceState.writeToMem ~memSources:addrSources ~valueSources:exprSources
    | ComputeForSideEffects expr -> getPossibleMemSources expr >>| fun _ -> ()
    | Statements statements ->
      statements |> List.map ~f:updateMemSourceEnvFromStatement |> MemSourceState.all_unit
    | SLet { args; body } ->
      let%bind envExtensions =
        args
        |> List.map ~f:(fun { binding; value } ->
          let%bind valueSources = getPossibleMemSources value in
          return (binding, valueSources))
        |> MemSourceState.all
      in
      MemSourceState.withEnvExtensions envExtensions
      @@ updateMemSourceEnvFromStatement body
    | SMallocLet { memArgs; body } ->
      let envExtensions =
        List.map memArgs ~f:(fun { memBinding; memType = _; memLoc = _ } ->
          memBinding, Set.singleton (module Identifier) memBinding)
      in
      MemSourceState.withEnvExtensions envExtensions
      @@ updateMemSourceEnvFromStatement body
    | ReifyShapeIndex { shape = _; mem = _ } -> return ()

and updateMemSourceEnvFromMapInKernel
  Acorn.Expr.{ frameShape = _; mapArgs; mapMemArgs; mapIotas = _; mapBody; type' = _ }
  =
  let open MemSourceState.Let_syntax in
  let mapArgExtensions =
    List.map mapArgs ~f:(fun { binding; ref } ->
      binding, Set.singleton (module Identifier) ref.id)
  in
  let%bind mapMemArgExtensions =
    mapMemArgs
    |> List.map ~f:(fun { memBinding; mem } ->
      let%bind sources = getPossibleMemSourcesInMem mem in
      return (memBinding, sources))
    |> MemSourceState.all
  in
  let mapEnvExtensions = mapArgExtensions @ mapMemArgExtensions in
  MemSourceState.withEnvExtensions mapEnvExtensions
  @@ updateMemSourceEnvFromMapBody mapBody

and updateMemSourceEnvFromMapBody = function
  | Acorn.Expr.MapBodyStatement statement -> updateMemSourceEnvFromStatement statement
  | Acorn.Expr.MapBodySubMaps maps ->
    maps |> List.map ~f:updateMemSourceEnvFromMapInKernel |> MemSourceState.all_unit
;;

module AllocAcc = struct
  type allocation =
    { binding : Identifier.t
    ; mallocLoc : Acorn.Expr.mallocLoc
    ; uses : Set.M(Identifier).t
    ; type' : Acorn.Type.t
    }

  type ('a, 'e) t = ('a * allocation list, 'e) CompilerState.u

  include Monad.Make2 (struct
      type nonrec ('a, 'e) t = ('a, 'e) t

      let return v = CompilerState.return (v, [])

      let bind v ~f =
        let open CompilerState.Let_syntax in
        let%bind a, aAcc = v in
        let%map b, bAcc = f a in
        b, bAcc @ aAcc
      ;;

      let map =
        `Custom
          (fun v ~f ->
            let open CompilerState.Let_syntax in
            let%map a, acc = v in
            f a, acc)
      ;;
    end)

  let createId name =
    let open CompilerState.Let_syntax in
    let%map id = CompilerState.createId name in
    id, []
  ;;

  let malloc ~mallocLoc type' name =
    let open CompilerState.Let_syntax in
    let%map binding, _ = createId name in
    let acc = { binding; mallocLoc; uses = getUsesInType type'; type' } in
    let ref = Acorn.Mem.Ref { id = binding; type' } in
    ref, [ acc ]
  ;;

  let all_opt prog =
    let open CompilerState in
    let open Let_syntax in
    match prog with
    | Some prog ->
      let%map result, acc = prog in
      Some result, acc
    | None -> return (None, [])
  ;;

  (* Multiply each allocation by the multiplier if it appears in the list of used allocations *)
  let multiplyUsedAllocations
    :  multiplier:Acorn.Index.shapeElement -> used:('a -> Set.M(Identifier).t)
    -> ('a, 'e) t -> ('a * Acorn.Expr.memArg list, 'e) t
    =
    fun ~multiplier ~used:getUsed prog ->
    let open CompilerState in
    let open Let_syntax in
    let%bind result, rawAllocs = prog in
    let used = getUsed result in
    let rawUsedAllocs, unusedAllocs =
      List.partition_tf rawAllocs ~f:(fun alloc -> Set.mem used alloc.binding)
    in
    let%map usedAllocs, memArgs =
      rawUsedAllocs
      |> List.map ~f:(fun alloc ->
        let%map binding = createId (Identifier.name alloc.binding) in
        ( { binding
          ; mallocLoc = alloc.mallocLoc
          ; uses = Set.union (getUsesInShapeElement multiplier) alloc.uses
          ; type' = Acorn.Type.array ~element:alloc.type' ~size:multiplier
          }
        , Acorn.Expr.
            { memBinding = alloc.binding
            ; mem =
                Ref
                  { id = binding
                  ; type' = Acorn.Type.array ~element:alloc.type' ~size:multiplier
                  }
            } ))
      |> all
      >>| List.unzip
    in
    (result, memArgs), usedAllocs @ unusedAllocs
  ;;

  let multiplyUsedAllocationsInExpr
    : type l.
      multiplier:Acorn.Index.shapeElement
      -> ((l, _) Acorn.Expr.t, 'e) t
      -> ((l, _) Acorn.Expr.t * Acorn.Expr.memArg list, 'e) t
    =
    fun ~multiplier prog ->
    multiplyUsedAllocations
      ~multiplier
      ~used:(fun expr ->
        MemSourceState.runA (getPossibleMemSources expr) (Map.empty (module Identifier)))
      prog
  ;;

  let allocationToMallocMemArg { binding; mallocLoc; uses = _; type' }
    : Acorn.Expr.memMallocArg
    =
    { memBinding = binding; memType = type'; memLoc = mallocLoc }
  ;;
end

let avoidCapturesGen ~wrap ~capturesToAvoid prog =
  let open CompilerState in
  let open Let_syntax in
  let%bind result, allocs = prog in
  let allocsToDeclare, allocsToPropogate =
    List.partition_map allocs ~f:(fun (alloc : AllocAcc.allocation) ->
      if Set.is_empty (Set.inter capturesToAvoid alloc.uses)
      then Second alloc
      else First (AllocAcc.allocationToMallocMemArg alloc))
  in
  let result =
    match allocsToDeclare with
    | [] -> result
    | _ :: _ -> wrap ~allocsToDeclare result
  in
  return (result, allocsToPropogate)
;;

let avoidCaptures ~capturesToAvoid prog =
  avoidCapturesGen
    ~wrap:(fun ~allocsToDeclare body ->
      Acorn.Expr.MallocLet { memArgs = allocsToDeclare; body })
    ~capturesToAvoid
    prog
;;

let avoidCapturesInStatement ~capturesToAvoid prog =
  avoidCapturesGen
    ~wrap:(fun ~allocsToDeclare body ->
      Acorn.Expr.SMallocLet { memArgs = allocsToDeclare; body })
    ~capturesToAvoid
    prog
;;

let declareAllAllocs prog =
  let open CompilerState in
  let open Let_syntax in
  let%bind result, allocs = prog in
  let result =
    match allocs with
    | [] -> result
    | _ :: _ ->
      Acorn.Expr.MallocLet
        { memArgs = List.map allocs ~f:AllocAcc.allocationToMallocMemArg; body = result }
  in
  return result
;;

let declareAllUsedAllocs prog =
  let open CompilerState in
  let open Let_syntax in
  let%bind expr, allAllocs = prog in
  let used =
    MemSourceState.runA (getPossibleMemSources expr) (Map.empty (module Identifier))
  in
  let usedAllocs, unusedAllocs =
    List.partition_tf allAllocs ~f:(fun alloc -> Set.mem used AllocAcc.(alloc.binding))
  in
  let expr =
    match usedAllocs with
    | [] -> expr
    | _ :: _ ->
      Acorn.Expr.MallocLet
        { memArgs = List.map usedAllocs ~f:AllocAcc.allocationToMallocMemArg
        ; body = expr
        }
  in
  return (expr, unusedAllocs)
;;

type targetAddr =
  | TargetValue of Acorn.Mem.t
  | TargetValues of targetAddr option list
[@@deriving sexp_of]

type 'l allocResult =
  { expr : 'l Acorn.Expr.sansCaptures
  ; statement : 'l Acorn.Expr.statementSansCaptures
  }

let getExpr { expr; statement = _ } = expr
let getStatement { expr = _; statement } = statement

let rec allocRequest
  : type l.
    mallocLoc:Acorn.Expr.mallocLoc
    -> writeToAddr:targetAddr option
    -> l Corn.Expr.t
    -> (l allocResult, 'e) AllocAcc.t
  =
  fun ~mallocLoc ~writeToAddr:targetAddr expr ->
  let open AllocAcc in
  let open Let_syntax in
  let open Acorn in
  let alloc = allocRequest ~mallocLoc in
  let allocStatement ~writeToAddr expr = alloc ~writeToAddr expr >>| getStatement in
  let allocExpr ~writeToAddr expr = alloc ~writeToAddr expr >>| getExpr in
  let rec partialUnwrittenExprToAllocResult
    : type l'.
      targetAddr:targetAddr option
      -> exprToWriteExtractor:(l' Expr.sansCaptures -> (l', unit) Expr.t)
      -> l' Expr.sansCaptures
      -> (l' allocResult, 'e) AllocAcc.t
    =
    fun ~targetAddr ~exprToWriteExtractor expr ->
    match targetAddr with
    | None ->
      return { expr; statement = ComputeForSideEffects (exprToWriteExtractor expr) }
    | Some (TargetValue mem) ->
      let%map binding = createId "expr-result" in
      let ref = Expr.{ id = binding; type' = Expr.type' expr } in
      { expr =
          Expr.Let
            { args = [ { binding; value = expr } ]
            ; body =
                Expr.eseq
                  ~statements:
                    [ Expr.putmem ~expr:(exprToWriteExtractor (Ref ref)) ~addr:mem ]
                  ~expr:(Ref ref)
            }
      ; statement = Expr.putmem ~expr ~addr:mem
      }
    | Some (TargetValues targetAddrs) ->
      let%bind binding = createId "expr-result" in
      let ref = Expr.{ id = binding; type' = Expr.type' expr } in
      let%map elements =
        targetAddrs
        |> List.mapi ~f:(fun index targetAddr ->
          partialUnwrittenExprToAllocResult
            ~targetAddr
            ~exprToWriteExtractor
            (Expr.tupleDeref ~tuple:(Ref ref) ~index))
        |> all
      in
      { expr =
          Let
            { args = [ { binding; value = expr } ]
            ; body =
                Expr.Values
                  { elements = List.map elements ~f:(fun e -> e.expr)
                  ; type' = typeAsTuple @@ Expr.type' expr
                  }
            }
      ; statement =
          SLet
            { args = [ { binding; value = expr } ]
            ; body = Statements (List.map elements ~f:(fun e -> e.statement))
            }
      }
  in
  let unwrittenExprToAllocResult expr =
    partialUnwrittenExprToAllocResult ~targetAddr ~exprToWriteExtractor:(fun e -> e) expr
  in
  let writtenExprToAllocResult expr = { expr; statement = ComputeForSideEffects expr } in
  let statementToAllocResult ~mem statement =
    { expr = Expr.eseq ~statements:[ statement ] ~expr:(Expr.getmem mem); statement }
  in
  let rec getMemForResult ?(mallocLoc = mallocLoc) ?(targetAddr = targetAddr) type' name
    : (Mem.t, _) AllocAcc.t
    =
    match targetAddr with
    | None -> malloc ~mallocLoc type' name
    | Some (TargetValue memVal) -> return memVal
    | Some (TargetValues targetAddrs) ->
      let type' = typeAsTuple type' in
      List.zip_exn type' targetAddrs
      |> List.mapi ~f:(fun i (type', targetAddr) ->
        getMemForResult ~targetAddr type' [%string "%{name}.%{i#Int}"])
      |> all
      >>| Mem.values
  in
  let allocLoopBlock
    : type lInner seqOrPar exists.
      wrapLoopBlock:
        ((l, lInner, seqOrPar, unit, exists) Expr.loopBlock -> l Expr.sansCaptures)
      -> blocks:int
      -> innerMallocLoc:Acorn.Expr.mallocLoc
      -> createMapTargetAddr:(targetAddr option -> targetAddr option)
      -> createMapResultMemFinal:(Acorn.Mem.t -> (Acorn.Mem.t, 'e) AllocAcc.t)
      -> createConsumerTargetAddr:(targetAddr option -> targetAddr option)
      -> createConsumerResultMemFinal:(Acorn.Mem.t -> (Acorn.Mem.t, 'e) AllocAcc.t)
      -> (l, lInner, seqOrPar, exists) Corn.Expr.loopBlock
      -> (l allocResult, 'e) AllocAcc.t
    =
    fun ~wrapLoopBlock
        ~blocks
        ~innerMallocLoc
        ~createMapTargetAddr
        ~createMapResultMemFinal
        ~createConsumerTargetAddr
        ~createConsumerResultMemFinal
        { frameShape
        ; mapArgs
        ; mapIotas
        ; mapBody
        ; mapBodyMatcher
        ; mapResults
        ; consumer
        ; type'
        } ->
    let type' = canonicalizeTupleType type' in
    let mapTargetAddr, consumerTargetAddr =
      match targetAddr with
      | None -> None, None
      | Some (TargetValue targetAddr) ->
        ( Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index:0))
        , Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index:1)) )
      | Some (TargetValues [ mapTarget; consumerTarget ]) -> mapTarget, consumerTarget
      | Some (TargetValues _) -> raise @@ Unreachable.Error "Expected 2 element tuple"
    in
    let mapArgs =
      List.map mapArgs ~f:(fun { binding; ref } ->
        Expr.{ binding; ref = { id = ref.id; type' = canonicalizeType ref.type' } })
    in
    let bindingsForMapBody =
      List.map mapArgs ~f:(fun arg -> arg.binding)
      @ List.map mapIotas ~f:(fun iota -> iota.iota)
      |> Set.of_list (module Identifier)
    in
    let rec createTargetAddrMapArgs type' = function
      | None ->
        let%bind mem = malloc ~mallocLoc:innerMallocLoc type' "map-mem" in
        createTargetAddrMapArgs type' (Some (TargetValue mem))
      | Some (TargetValue targetValue) ->
        let elementType =
          guillotineType ~expectedSize:(convertShapeElement frameShape)
          @@ Mem.type' targetValue
        in
        let%map binding =
          createId
            (match targetValue with
             | Ref ref -> Identifier.name ref.id
             | _ -> "map-mem")
        in
        ( Some (TargetValue (Ref { id = binding; type' = elementType }))
        , [ Expr.{ memBinding = binding; mem = targetValue } ]
        , targetValue )
      | Some (TargetValues targetValues) ->
        let elementTypes = typeAsTuple type' in
        let%map targets, memArgs, mapMems =
          List.zip_exn elementTypes targetValues
          |> List.map ~f:(fun (t, e) -> createTargetAddrMapArgs t e)
          |> all
          >>| List.unzip3
        in
        ( Some (TargetValues targets)
        , List.concat memArgs
        , Mem.Values { elements = mapMems; type' } )
    in
    let%bind mapResultsAddr, mapTargetAddrMemArgs, mapResultMemInterim =
      createTargetAddrMapArgs (List.nth_exn type' 0) (createMapTargetAddr mapTargetAddr)
    in
    let%bind mapResultMemFinal = createMapResultMemFinal mapResultMemInterim in
    let mapResultTargets =
      match mapResultsAddr with
      | None ->
        mapResults
        |> List.map ~f:(fun id -> id, None)
        |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
      | Some (TargetValue targetAddr) ->
        mapResults
        |> List.mapi ~f:(fun index id ->
          id, Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index)))
        |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
      | Some (TargetValues targetAddrs) ->
        List.zip_exn mapResults targetAddrs
        |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
    in
    let rec createTargetAddr (mapBodyMatcher : Expr.tupleMatch) =
      match mapBodyMatcher with
      | Binding binding -> Map.find mapResultTargets binding |> Option.value ~default:None
      | Unpack elements -> Some (TargetValues (List.map elements ~f:createTargetAddr))
    in
    let mapBodyTargetAddr = createTargetAddr mapBodyMatcher in
    let%bind mapBody, mapBodyMemArgs =
      allocRequest ~mallocLoc:innerMallocLoc ~writeToAddr:mapBodyTargetAddr mapBody
      >>| getExpr
      |> avoidCaptures ~capturesToAvoid:bindingsForMapBody
      |> multiplyUsedAllocationsInExpr ~multiplier:(convertShapeElement frameShape)
    in
    let mapMemArgs =
      List.map mapTargetAddrMemArgs ~f:(fun { memBinding; mem } ->
        Acorn.Expr.{ memBinding; mem })
      @ mapBodyMemArgs
    in
    let processReduce Corn.Expr.{ arg; zero; body; d; character; type' = reduceType } =
      let d = convertDimension d in
      let%bind zero =
        zero
        |> Option.map ~f:(fun zero ->
          allocRequest ~mallocLoc ~writeToAddr:None zero >>| getExpr)
        |> all_opt
      and body =
        allocRequest ~mallocLoc:innerMallocLoc ~writeToAddr:None body
        >>| getExpr
        |> declareAllUsedAllocs
      in
      let%map character =
        match character with
        | Reduce -> return Expr.Reduce
        | Scan ->
          let resultType = Type.array ~element:(Expr.type' body) ~size:(Add d) in
          let%map mem = malloc ~mallocLoc resultType "scan-result" in
          Expr.Scan mem
        | OpenScan ->
          let resultType =
            Type.array
              ~element:(Expr.type' body)
              ~size:(Add { d with const = d.const + 1 })
          in
          let%map mem = malloc ~mallocLoc resultType "open-scan-result" in
          Expr.OpenScan mem
      in
      Expr.
        { arg =
            { firstBinding = arg.firstBinding
            ; secondBinding = arg.secondBinding
            ; production = convertProductionTuple arg.production
            }
        ; zero
        ; body
        ; d
        ; character
        ; type' = canonicalizeType reduceType
        }
    in
    let processConsumer
      : type t.
        ((l, lInner, t) Corn.Expr.consumerOp, exists) Maybe.t
        -> (((l, lInner, t, unit) Expr.consumerOp, exists) Maybe.t * bool, _) AllocAcc.t
      = function
      | Nothing -> return (Maybe.Nothing, false)
      | Just (ReduceSeq reduce) ->
        let%map reduce = processReduce reduce in
        Maybe.Just (Expr.ReduceSeq reduce), true
      | Just (ReducePar { reduce; outerBody }) ->
        let%bind reduce = processReduce reduce in
        let%bind outerBody =
          allocRequest ~mallocLoc ~writeToAddr:None outerBody
          >>| getExpr
          |> declareAllUsedAllocs
        in
        let interimResultMemType =
          Type.array
            ~element:(Expr.type' reduce.body)
            ~size:(Add (Index.dimensionConstant blocks))
        in
        let%bind interimResultMemInterim =
          malloc ~mallocLoc:innerMallocLoc interimResultMemType "reduce-interim-result"
        in
        let%bind interimResultMemFinal =
          malloc ~mallocLoc interimResultMemType "reduce-interim-result"
        in
        return
        @@ ( Maybe.Just
               (Expr.ReducePar
                  { reduce; interimResultMemInterim; interimResultMemFinal; outerBody })
           , true )
      | Just (Fold { zeroArg; arrayArgs; body; d; character; type' }) ->
        let d = convertDimension d in
        let%bind zeroValue =
          allocRequest ~mallocLoc ~writeToAddr:None zeroArg.zeroValue >>| getExpr
        and body, bodyMemArgs =
          allocRequest ~mallocLoc:innerMallocLoc ~writeToAddr:None body
          >>| getExpr
          |> avoidCaptures
               ~capturesToAvoid:
                 (Set.of_list
                    (module Identifier)
                    ([ zeroArg.zeroBinding ]
                     @ List.map arrayArgs ~f:(fun arg -> arg.binding)))
          |> multiplyUsedAllocationsInExpr ~multiplier:(convertShapeElement frameShape)
        in
        let%map character =
          match character with
          | Fold -> return (Expr.Fold : l Expr.foldCharacter)
          | Trace ->
            let resultType = Type.array ~element:(Expr.type' body) ~size:(Add d) in
            let%map mem = malloc ~mallocLoc resultType "trace-result" in
            Expr.Trace mem
          | OpenTrace ->
            let resultType =
              Type.array
                ~element:(Expr.type' body)
                ~size:(Add { d with const = d.const + 1 })
            in
            let%map mem = malloc ~mallocLoc resultType "open-trace-result" in
            Expr.OpenTrace mem
        in
        ( Maybe.Just
            (Expr.Fold
               { zeroArg = { zeroBinding = zeroArg.zeroBinding; zeroValue }
               ; arrayArgs =
                   List.map arrayArgs ~f:(fun { binding; production } ->
                     Expr.{ binding; production = convertProduction production })
               ; mappedMemArgs = bodyMemArgs
               ; body
               ; d
               ; character
               ; type' = canonicalizeType type'
               })
        , true )
      | Just (Scatter { valuesArg; indicesArg; dIn; dOut; type' }) ->
        let type' = canonicalizeType type' in
        let%bind memInterim =
          getMemForResult
            ~mallocLoc:innerMallocLoc
            ~targetAddr:(createConsumerTargetAddr consumerTargetAddr)
            type'
            "scatter-array"
        in
        let%bind memFinal = createConsumerResultMemFinal memInterim in
        return
        @@ ( Maybe.Just
               (Expr.Scatter
                  { valuesArg = convertProduction valuesArg
                  ; indicesArg = convertProduction indicesArg
                  ; dIn = convertDimension dIn
                  ; dOut = convertDimension dOut
                  ; memInterim
                  ; memFinal
                  ; type'
                  })
           , false )
    in
    let%bind consumer, consumerCopyRequired = processConsumer consumer in
    let loopBlock : (l, lInner, seqOrPar, unit, exists) Expr.loopBlock =
      { frameShape = convertShapeElement frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapMemArgs
      ; mapBodyMatcher
      ; mapResults
      ; mapResultMemInterim
      ; mapResultMemFinal
      ; consumer
      ; type'
      }
    in
    if consumerCopyRequired
    then
      partialUnwrittenExprToAllocResult
        ~targetAddr:consumerTargetAddr
        ~exprToWriteExtractor:(fun loopBlock -> Expr.tupleDeref ~tuple:loopBlock ~index:1)
        (wrapLoopBlock loopBlock)
    else return @@ writtenExprToAllocResult (wrapLoopBlock loopBlock)
  in
  match expr with
  | Ref { id; type' } ->
    (* TODO: make let write immediately to targetAddr so no copy is necessary *)
    unwrittenExprToAllocResult (Ref { id; type' = canonicalizeType type' })
  | Frame { elements; dimension = _; type' } ->
    let type' = canonicalizeType type' in
    let%bind mem = getMemForResult type' "frame-array" in
    let%map statements =
      elements
      |> List.mapi ~f:(fun offset element ->
        let elementType = Corn.Expr.type' element in
        let elementTarget =
          Some
            (TargetValue
               (Mem.Index
                  { mem
                  ; offset = Index.dimensionConstant offset
                  ; type' = canonicalizeType elementType
                  }))
        in
        allocStatement ~writeToAddr:elementTarget element)
      |> all
    in
    statementToAllocResult ~mem (Statements statements)
  | BoxValue { box; type' } ->
    let%bind box = allocExpr ~writeToAddr:None box in
    unwrittenExprToAllocResult @@ BoxValue { box; type' = canonicalizeType type' }
  | IndexLet { indexArgs; body; type' } ->
    let argBindings =
      indexArgs
      |> List.map ~f:(fun arg -> arg.indexBinding)
      |> Set.of_list (module Identifier)
    in
    let%map indexArgs =
      indexArgs
      |> List.map ~f:(fun { indexBinding; indexValue; sort } ->
        let%map indexValue =
          match indexValue with
          | Runtime value ->
            let%map value = allocExpr ~writeToAddr:None value in
            Expr.Runtime value
          | FromBox { box; i } ->
            let%map box = allocExpr ~writeToAddr:None box in
            Expr.FromBox { box; i }
        in
        Expr.{ indexBinding; indexValue; sort })
      |> all
    and body =
      avoidCaptures ~capturesToAvoid:argBindings @@ allocExpr ~writeToAddr:targetAddr body
    in
    writtenExprToAllocResult
    @@ IndexLet { indexArgs; body; type' = canonicalizeType type' }
  | ReifyIndex { index = Dimension dim; type' = _ } ->
    unwrittenExprToAllocResult @@ ReifyDimensionIndex { dim = convertDimension dim }
  | ReifyIndex { index = Shape shape; type' } ->
    let type' = canonicalizeType type' in
    let shape = convertShape shape in
    (match type' with
     | Atom (Sigma sigma) ->
       let shapeLen = getShapeLen shape in
       let%bind mem =
         malloc
           ~mallocLoc
           (Array { element = Literal IntLiteral; shape = [ Add shapeLen ] })
           "reify-index-array"
       in
       unwrittenExprToAllocResult
       @@ Box
            { indices =
                [ { expr = ReifyDimensionIndex { dim = shapeLen }
                  ; index = Dimension shapeLen
                  }
                ]
            ; body =
                Expr.eseq
                  ~statements:[ ReifyShapeIndex { shape; mem } ]
                  ~expr:(Getmem { addr = mem; type' = sigma.body })
            ; type' = sigma
            }
     | _ ->
       let%map mem = getMemForResult type' "reify-index-array" in
       statementToAllocResult ~mem (ReifyShapeIndex { shape; mem }))
  | Let { args; body; type' = _ } ->
    let argBindings =
      args |> List.map ~f:(fun arg -> arg.binding) |> Set.of_list (module Identifier)
    in
    let%map args =
      args
      |> List.map ~f:(fun { binding; value } ->
        let%map value = allocExpr ~writeToAddr:None value in
        Expr.{ binding; value })
      |> all
    and body =
      avoidCaptures ~capturesToAvoid:argBindings @@ allocExpr ~writeToAddr:targetAddr body
    in
    writtenExprToAllocResult @@ Let { args; body }
  | LoopBlock loopBlock ->
    allocLoopBlock
      ~wrapLoopBlock:(fun loopBlock -> Expr.LoopBlock loopBlock)
      ~blocks:0
      ~innerMallocLoc:mallocLoc
      ~createMapTargetAddr:(fun targetAddr -> targetAddr)
      ~createMapResultMemFinal:(fun mem -> return mem)
      ~createConsumerTargetAddr:(fun targetAddr -> targetAddr)
      ~createConsumerResultMemFinal:(fun mem -> return mem)
      loopBlock
  | LoopKernel { kernel = loopBlock; blocks; threads } ->
    let type' = canonicalizeType @@ Tuple loopBlock.type' in
    let%bind loopBlockResultMem = getMemForResult type' "loop-block-mem-result" in
    allocLoopBlock
      ~wrapLoopBlock:(fun loopBlock ->
        Expr.LoopKernel { kernel = loopBlock; captures = (); blocks; threads })
      ~blocks
      ~innerMallocLoc:MallocDevice
      ~createMapTargetAddr:(fun _ -> None)
      ~createMapResultMemFinal:(fun _ ->
        return @@ Mem.tupleDeref ~tuple:loopBlockResultMem ~index:0)
      ~createConsumerTargetAddr:(fun _ -> None)
      ~createConsumerResultMemFinal:(fun _ ->
        return @@ Mem.tupleDeref ~tuple:loopBlockResultMem ~index:1)
      loopBlock
  | MapKernel { kernel = mapKernel; blocks; threads } ->
    let rec allocMapKernel
      ~outerBindingsForMapBody
      ~writeToAddr:targetAddr
      Corn.Expr.
        { frameShape; mapArgs; mapIotas; mapBody; mapBodyMatcher; mapResults; type' }
      : (unit Expr.mapInKernel * Mem.t, _) AllocAcc.t
      =
      let type' = canonicalizeType type' in
      let mapArgs =
        List.map mapArgs ~f:(fun { binding; ref } ->
          Expr.{ binding; ref = { id = ref.id; type' = canonicalizeType ref.type' } })
      in
      let innerBindingsForMapBody =
        List.map mapArgs ~f:(fun arg -> arg.binding)
        @ List.map mapIotas ~f:(fun iota -> iota.iota)
        |> Set.of_list (module Identifier)
      in
      let bindingsForMapBody =
        Set.union outerBindingsForMapBody innerBindingsForMapBody
      in
      let rec createTargetAddrMapArgs type' = function
        | None ->
          let%bind mem = malloc ~mallocLoc:MallocDevice type' "map-mem" in
          createTargetAddrMapArgs type' (Some (TargetValue mem))
        | Some (TargetValue targetValue) ->
          let elementType =
            guillotineType ~expectedSize:(convertShapeElement frameShape)
            @@ Mem.type' targetValue
          in
          let%map binding =
            createId
              (match targetValue with
               | Ref ref -> Identifier.name ref.id
               | _ -> "map-mem")
          in
          ( Some (TargetValue (Ref { id = binding; type' = elementType }))
          , [ Expr.{ memBinding = binding; mem = targetValue } ]
          , targetValue )
        | Some (TargetValues targetValues) ->
          let elementTypes = typeAsTuple type' in
          let%map targets, memArgs, mapMems =
            List.zip_exn elementTypes targetValues
            |> List.map ~f:(fun (t, e) -> createTargetAddrMapArgs t e)
            |> all
            >>| List.unzip3
          in
          ( Some (TargetValues targets)
          , List.concat memArgs
          , Mem.Values { elements = mapMems; type' } )
      in
      let%bind mapResultTargetAddr, mapTargetAddrMemArgs, mapResultMemInterim =
        createTargetAddrMapArgs type' targetAddr
      in
      let mapResultTargets =
        match mapResultTargetAddr with
        | None ->
          mapResults
          |> List.map ~f:(fun id -> id, None)
          |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
        | Some (TargetValue targetAddr) ->
          mapResults
          |> List.mapi ~f:(fun index id ->
            id, Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index)))
          |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
        | Some (TargetValues targetAddrs) ->
          List.zip_exn mapResults targetAddrs
          |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
      in
      let rec createTargetAddr (mapBodyMatcher : Expr.tupleMatch) =
        match mapBodyMatcher with
        | Binding binding ->
          Map.find mapResultTargets binding |> Option.value ~default:None
        | Unpack elements -> Some (TargetValues (List.map elements ~f:createTargetAddr))
      in
      let mapBodyTargetAddr = createTargetAddr mapBodyMatcher in
      let rec allocMapBodySubMap
        ~writeToAddr:targetAddr
        (mapBodySubMap : Corn.Expr.mapBodySubMap)
        =
        match mapBodySubMap with
        | MapBodyMap mapKernel ->
          let%bind mapKernel =
            allocMapKernel
              ~outerBindingsForMapBody:bindingsForMapBody
              ~writeToAddr:targetAddr
              mapKernel
          in
          return [ mapKernel ]
        | MapBodyValues elements ->
          let elementsAndTargetAddrs =
            match targetAddr with
            | None -> List.map elements ~f:(fun element -> element, None)
            | Some (TargetValue targetAddr) ->
              List.mapi elements ~f:(fun index element ->
                element, Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index)))
            | Some (TargetValues targetAddrs) -> List.zip_exn elements targetAddrs
          in
          let%bind subMaps =
            elementsAndTargetAddrs
            |> List.map ~f:(fun (element, targetAddr) ->
              allocMapBodySubMap ~writeToAddr:targetAddr element)
            |> all
          in
          return @@ List.concat subMaps
        | MapBodyDeref { tuple; index } ->
          let rec mapBodySubMapType : Corn.Expr.mapBodySubMap -> Type.t = function
            | MapBodyMap mapKernel -> canonicalizeType mapKernel.type'
            | MapBodyValues elements -> Tuple (List.map elements ~f:mapBodySubMapType)
            | MapBodyDeref { tuple; index } ->
              let tupleType = typeAsTuple @@ mapBodySubMapType tuple in
              List.nth_exn tupleType index
          in
          let tupleSize = List.length @@ typeAsTuple (mapBodySubMapType tuple) in
          let tupleTargetAddrs =
            List.init tupleSize ~f:(fun i -> if i = index then targetAddr else None)
          in
          allocMapBodySubMap ~writeToAddr:(Some (TargetValues tupleTargetAddrs)) @@ tuple
      in
      let allocMapBody ~writeToAddr:targetAddr (mapBody : Corn.Expr.mapBody) =
        match mapBody with
        | MapBodyExpr expr ->
          let%bind expr =
            allocDevice ~writeToAddr:targetAddr expr
            >>| getStatement
            |> avoidCapturesInStatement ~capturesToAvoid:bindingsForMapBody
          in
          return @@ Expr.MapBodyStatement expr
        | MapBodySubMap subMap ->
          let%bind subMaps, _ =
            allocMapBodySubMap ~writeToAddr:targetAddr subMap >>| List.unzip
          in
          return @@ Expr.MapBodySubMaps subMaps
      in
      let%map mapBody, mapBodyMemArgs =
        allocMapBody ~writeToAddr:mapBodyTargetAddr mapBody
        |> multiplyUsedAllocations
             ~multiplier:(convertShapeElement frameShape)
             ~used:(fun mapBody ->
               let open MemSourceState.Let_syntax in
               let getUsesInBody =
                 let%bind () = updateMemSourceEnvFromMapBody mapBody in
                 getPossibleMemSourcesInMem mapResultMemInterim
               in
               MemSourceState.runA getUsesInBody (Map.empty (module Identifier)))
      in
      let mapMemArgs =
        List.map mapTargetAddrMemArgs ~f:(fun { memBinding; mem } ->
          Acorn.Expr.{ memBinding; mem })
        @ mapBodyMemArgs
      in
      ( ({ frameShape = convertShapeElement frameShape
         ; mapArgs
         ; mapIotas
         ; mapBody
         ; mapMemArgs
         ; type'
         }
          : unit Expr.mapInKernel)
      , mapResultMemInterim )
    in
    let type' = canonicalizeType mapKernel.type' in
    let%bind mapResultMemFinal = getMemForResult type' "map-mem-result" in
    let%map mapInKernel, mapResultMemInterim =
      allocMapKernel
        ~outerBindingsForMapBody:(Set.empty (module Identifier))
        ~writeToAddr:None
        mapKernel
    in
    let mapKernel =
      Expr.MapKernel
        { kernel = { map = mapInKernel; mapResultMemInterim; mapResultMemFinal }
        ; captures = ()
        ; blocks
        ; threads
        }
    in
    { expr = Expr.eseq ~statements:[ mapKernel ] ~expr:(Expr.getmem mapResultMemFinal)
    ; statement = mapKernel
    }
  | Literal literal -> unwrittenExprToAllocResult @@ Literal literal
  | Box { indices; body; bodyType = _; type' } ->
    let%bind indices =
      indices
      |> List.map ~f:(fun index ->
        match index with
        | Dimension dim ->
          let dim = convertDimension dim in
          return
          @@ Expr.{ expr = Expr.ReifyDimensionIndex { dim }; index = Dimension dim }
        | Shape shape ->
          let shape = convertShape shape in
          let reifiedShapeType =
            Type.Array
              { element = Literal IntLiteral; shape = [ Add (getShapeLen shape) ] }
          in
          let%map mem = malloc ~mallocLoc reifiedShapeType "reify-index-array" in
          Expr.
            { expr =
                eseq
                  ~statements:[ ReifyShapeIndex { shape; mem } ]
                  ~expr:(Getmem { addr = mem; type' = reifiedShapeType })
            ; index = Shape shape
            })
      |> all
    in
    let sigmaParamBindings =
      type'.parameters
      |> List.map ~f:(fun p -> p.binding)
      |> Set.of_list (module Identifier)
    in
    let%bind body =
      allocExpr ~writeToAddr:None body
      |> avoidCaptures ~capturesToAvoid:sigmaParamBindings
    in
    unwrittenExprToAllocResult
    @@ Box { indices; body; type' = canonicalizeSigmaType type' }
  | Values { elements; type' } ->
    let elementsAndTargetAddrs =
      match targetAddr with
      | None -> List.map elements ~f:(fun element -> element, None)
      | Some (TargetValue targetAddr) ->
        List.mapi elements ~f:(fun index element ->
          element, Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index)))
      | Some (TargetValues targetAddrs) -> List.zip_exn elements targetAddrs
    in
    let%map elements =
      elementsAndTargetAddrs
      |> List.map ~f:(fun (element, targetAddr) -> alloc ~writeToAddr:targetAddr element)
      |> all
    in
    (* Write is ensured because the elements were directly written to the target addrs *)
    (* EnsureWrite.mark @@ Values { elements; type' } *)
    { expr =
        Values
          { elements = List.map elements ~f:(fun r -> r.expr)
          ; type' = canonicalizeTupleType type'
          }
    ; statement = Statements (List.map elements ~f:(fun r -> r.statement))
    }
  | ScalarPrimitive { op; args; type' } ->
    let%bind args = args |> List.map ~f:(allocExpr ~writeToAddr:None) |> all in
    unwrittenExprToAllocResult
    @@ ScalarPrimitive { op; args; type' = canonicalizeType type' }
  | TupleDeref { tuple; index; type' } ->
    let tupleSize =
      match Corn.Expr.type' tuple with
      | Tuple elements -> List.length elements
      | _ -> raise @@ Unreachable.Error "expected tuple type"
    in
    let tupleTargetAddrs =
      List.init tupleSize ~f:(fun i -> if i = index then targetAddr else None)
    in
    let%map tuple = alloc ~writeToAddr:(Some (TargetValues tupleTargetAddrs)) tuple in
    { expr = TupleDeref { tuple = tuple.expr; index; type' = canonicalizeType type' }
    ; statement = tuple.statement
    }
  | ContiguousSubArray { arrayArg; indexArg; originalShape; resultShape; type' } ->
    let%bind arrayArg = allocExpr ~writeToAddr:None arrayArg
    and indexArg = allocExpr ~writeToAddr:None indexArg in
    unwrittenExprToAllocResult
    @@ ContiguousSubArray
         { arrayArg
         ; indexArg
         ; originalShape = convertShape originalShape
         ; resultShape = convertShape resultShape
         ; type' = canonicalizeType type'
         }
  | Append { args; type' } ->
    let canonType = canonicalizeType type' in
    let%bind mem = getMemForResult canonType "append-array" in
    let%map statements =
      args
      |> List.folding_map ~init:(Index.dimensionConstant 0) ~f:(fun offset arg ->
        let argType =
          match Corn.Expr.type' arg with
          | Array array -> array
          | _ -> raise @@ Unreachable.Error "expected array type"
        in
        let argCount =
          match argType.size with
          | Add dim -> convertDimension dim
          | ShapeRef _ -> raise @@ Unreachable.Error "expected dimension index"
        in
        let newOffset = Index.addDimensions offset argCount in
        let argTarget =
          Some
            (TargetValue
               (Mem.Index { mem; offset; type' = canonicalizeType @@ Array argType }))
        in
        newOffset, allocStatement ~writeToAddr:argTarget arg)
      |> all
    in
    statementToAllocResult ~mem (Statements statements)
  | Zip { zipArg; nestCount = _; type' = _ } -> alloc ~writeToAddr:targetAddr zipArg
  | Unzip { unzipArg; type' = _ } -> alloc ~writeToAddr:targetAddr unzipArg
  | IfParallelismHitsCutoff { parallelism; cutoff; then'; else'; type' } ->
    let%bind then' = allocExpr ~writeToAddr:targetAddr then'
    and else' = allocExpr ~writeToAddr:targetAddr else' in
    unwrittenExprToAllocResult
    @@ IfParallelismHitsCutoff
         { parallelism = convertParallelism parallelism
         ; cutoff
         ; then'
         ; else'
         ; type' = canonicalizeType type'
         }

and allocHost ~writeToAddr expr = allocRequest ~mallocLoc:MallocHost ~writeToAddr expr

and allocDevice ~writeToAddr expr : (device allocResult, _) AllocAcc.t =
  allocRequest ~mallocLoc:MallocDevice ~writeToAddr expr
;;

let alloc (expr : Corn.t)
  : (Acorn.sansCaptures, ('s option, string) Source.annotate) CompilerState.u
  =
  let open AllocAcc in
  allocHost ~writeToAddr:None expr >>| getExpr |> declareAllAllocs
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Corn.t
  type output = Acorn.sansCaptures
  type error = (SB.source option, string) Source.annotate

  let name = "Alloc"

  let run input =
    CompilerPipeline.S.makeF ~f:(fun state -> CompilerState.run (alloc input) state)
  ;;
end
