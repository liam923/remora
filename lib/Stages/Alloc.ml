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

let rec arrayElementType (type' : Acorn.Type.t) ~expectedSize : Acorn.Type.t =
  match type' with
  | Array array ->
    assert (Acorn.Index.equal_shapeElement (NeList.hd array.shape) expectedSize);
    Atom array.element
  | Tuple elements ->
    Tuple
      (List.map elements ~f:(fun elementType ->
         arrayElementType ~expectedSize elementType))
  | Atom _ -> raise @@ Unreachable.Error "Expected array type or tuple of arrays"
;;

let typeAsTuple (type' : Acorn.Type.t) : Acorn.Type.tuple =
  match type' with
  | Tuple elements -> elements
  | _ -> raise @@ Unimplemented.Error "expected tuple type"
;;

module AllocAcc = struct
  type allocation =
    { binding : Identifier.t
    ; hostOrDevice : Acorn.Mem.mallocHostOrDevice
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

  let malloc ~hostOrDevice type' name =
    let open CompilerState.Let_syntax in
    let%map binding, _ = createId name in
    let acc = { binding; hostOrDevice; uses = getUsesInType type'; type' } in
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

  let multiplyAllocations
    : type l.
      multiplier:Acorn.Index.shapeElement
      -> ('a, 'e) t
      -> ('a * l Acorn.Expr.memArg list, 'e) t
    =
    fun ~multiplier prog ->
    let open CompilerState in
    let open Let_syntax in
    let%bind result, acc = prog in
    let%map allocs, memArgs =
      acc
      |> List.map ~f:(fun alloc ->
        let%map binding = createId (Identifier.name alloc.binding) in
        ( { binding
          ; hostOrDevice = alloc.hostOrDevice
          ; uses = Set.union (getUsesInShapeElement multiplier) alloc.uses
          ; type' = Acorn.Type.array ~element:alloc.type' ~size:multiplier
          }
        , Acorn.Expr.
            { memBinding = alloc.binding
            ; mem = Ref { id = binding; type' = alloc.type' }
            } ))
      |> all
      >>| List.unzip
    in
    (result, memArgs), allocs
  ;;

  let allocationToMallocMemArg { binding; hostOrDevice; uses = _; type' }
    : host Acorn.Expr.memArg
    =
    { memBinding = binding; mem = Malloc { hostOrDevice; type' } }
  ;;
end

type ('l, 'e) captureAvoider =
  capturesToAvoid:Set.M(Identifier).t
  -> ('l Acorn.Expr.sansCaptures, 'e) AllocAcc.t
  -> ('l Acorn.Expr.sansCaptures, 'e) AllocAcc.t

let hostAvoidCaptures
  ~capturesToAvoid
  (prog : (host Acorn.Expr.sansCaptures, _) AllocAcc.t)
  =
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
    | _ :: _ ->
      if List.is_empty allocsToDeclare
      then result
      else Acorn.Expr.MemLet { memArgs = allocsToDeclare; body = result }
  in
  return (result, allocsToPropogate)
;;

let deviceAvoidCaptures ~capturesToAvoid prog =
  let open CompilerState in
  let open Let_syntax in
  let%bind result, acc = prog in
  let usesInAcc =
    acc
    |> List.map ~f:(fun (alloc : AllocAcc.allocation) -> alloc.uses)
    |> Set.union_list (module Identifier)
  in
  let intersection = Set.inter usesInAcc capturesToAvoid in
  match Set.to_list intersection with
  | [] -> return (result, acc)
  | captured :: _ ->
    let capturedStr = Identifier.show captured in
    CompilerState.error
      [%string
        "Cannot allocate array of unknown size inside kernel; allocation relies on \
         %{capturedStr}"]
;;

type targetAddr =
  | TargetValue of device Acorn.Mem.t
  | TargetValues of targetAddr option list
[@@deriving sexp_of]

type 'l allocResult =
  { expr : 'l Acorn.Expr.sansCaptures
  ; statement : 'l Acorn.Expr.statementSansCaptures
  }

let rec allocRequest
  : type l.
    (l, 'e) captureAvoider
    -> mallocLoc:Acorn.Mem.mallocHostOrDevice
    -> writeToAddr:targetAddr option
    -> memDeviceToL:(device Acorn.Mem.t -> l Acorn.Mem.t)
    -> l Corn.Expr.t
    -> (l allocResult, 'e) AllocAcc.t
  =
  fun avoidCaptures ~mallocLoc ~writeToAddr:targetAddr ~memDeviceToL expr ->
  let open AllocAcc in
  let open Let_syntax in
  let open Acorn in
  let alloc = allocRequest avoidCaptures ~mallocLoc ~memDeviceToL in
  let getExpr { expr; statement = _ } = expr in
  let getStatement { expr = _; statement } = statement in
  let allocStatement ~writeToAddr expr = alloc ~writeToAddr expr >>| getStatement in
  let allocExpr ~writeToAddr expr = alloc ~writeToAddr expr >>| getExpr in
  let rec partialUnwrittenExprToAllocResult
    : type l'.
      memDeviceToL:(device Mem.t -> l' Mem.t)
      -> targetAddr:targetAddr option
      -> exprToWriteExtractor:(l' Expr.sansCaptures -> (l', unit) Expr.t)
      -> l' Expr.sansCaptures
      -> (l' allocResult, 'e) AllocAcc.t
    =
    fun ~memDeviceToL ~targetAddr ~exprToWriteExtractor expr ->
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
                    [ Expr.putmem
                        ~expr:(exprToWriteExtractor (Ref ref))
                        ~addr:(memDeviceToL mem)
                    ]
                  ~expr:(Ref ref)
            }
      ; statement = Expr.putmem ~expr ~addr:(memDeviceToL mem)
      }
    | Some (TargetValues targetAddrs) ->
      let%bind binding = createId "expr-result" in
      let ref = Expr.{ id = binding; type' = Expr.type' expr } in
      let%map elements =
        targetAddrs
        |> List.mapi ~f:(fun index targetAddr ->
          partialUnwrittenExprToAllocResult
            ~memDeviceToL
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
    partialUnwrittenExprToAllocResult
      ~memDeviceToL
      ~targetAddr
      ~exprToWriteExtractor:(fun e -> e)
      expr
  in
  let writtenExprToAllocResult expr = { expr; statement = ComputeForSideEffects expr } in
  let statementToAllocResult ~mem statement =
    { expr = Expr.eseq ~statements:[ statement ] ~expr:(Expr.getmem (memDeviceToL mem))
    ; statement
    }
  in
  let rec getMemForResult ?(targetAddr = targetAddr) type' name =
    match targetAddr with
    | None -> malloc ~hostOrDevice:mallocLoc type' name
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
    : type lOuter lInner seqOrPar.
      ((lOuter, lInner, seqOrPar, unit) Expr.loopBlock -> lOuter Expr.sansCaptures)
      -> blocks:int
      -> (lOuter, 'e) captureAvoider
      -> Acorn.Mem.mallocHostOrDevice
      -> (device Acorn.Mem.t -> lOuter Acorn.Mem.t)
      -> (lInner, 'e) captureAvoider
      -> Acorn.Mem.mallocHostOrDevice
      -> (device Acorn.Mem.t -> lInner Acorn.Mem.t)
      -> (lOuter, lInner, seqOrPar) Corn.Expr.loopBlock
      -> (lOuter allocResult, _) AllocAcc.t
    =
    fun wrapLoopBlock
        ~blocks
        outerCaptureAvoider
        outerMallocLoc
        outerMemDeviceToL
        innerCaptureAvoider
        innerMallocLoc
        innerMemDeviceToL
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
        let%bind mem = malloc ~hostOrDevice:mallocLoc type' "map-mem" in
        createTargetAddrMapArgs type' (Some (TargetValue mem))
      | Some (TargetValue targetValue) ->
        let elementType =
          arrayElementType ~expectedSize:(convertShapeElement frameShape)
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
    let%bind mapResultsAddr, mapTargetAddrMemArgs, mapResultMem =
      createTargetAddrMapArgs (List.nth_exn type' 0) mapTargetAddr
    in
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
      allocRequest
        innerCaptureAvoider
        ~mallocLoc:innerMallocLoc
        ~writeToAddr:mapBodyTargetAddr
        ~memDeviceToL:innerMemDeviceToL
        mapBody
      >>| getExpr
      |> innerCaptureAvoider ~capturesToAvoid:bindingsForMapBody
      |> multiplyAllocations ~multiplier:(convertShapeElement frameShape)
    in
    let mapMemArgs =
      List.map mapTargetAddrMemArgs ~f:(fun { memBinding; mem } ->
        Acorn.Expr.{ memBinding; mem = outerMemDeviceToL mem })
      @ mapBodyMemArgs
    in
    let processReduce Corn.Expr.{ arg; zero; body; d; character; type' = reduceType } =
      let%map zero =
        zero
        |> Option.map ~f:(fun zero ->
          allocRequest
            outerCaptureAvoider
            ~mallocLoc:outerMallocLoc
            ~writeToAddr:None
            ~memDeviceToL:outerMemDeviceToL
            zero
          >>| getExpr)
        |> all_opt
      and body, bodyMemArgs =
        allocRequest
          innerCaptureAvoider
          ~mallocLoc:innerMallocLoc
          ~writeToAddr:None
          ~memDeviceToL:innerMemDeviceToL
          body
        >>| getExpr
        |> innerCaptureAvoider
             ~capturesToAvoid:
               (Set.of_list (module Identifier) [ arg.firstBinding; arg.secondBinding ])
        |> multiplyAllocations ~multiplier:(convertShapeElement frameShape)
      in
      Expr.
        { arg =
            { firstBinding = arg.firstBinding
            ; secondBinding = arg.secondBinding
            ; production = convertProductionTuple arg.production
            }
        ; zero
        ; mappedMemArgs = bodyMemArgs
        ; body
        ; d = convertDimension d
        ; character
        ; type' = canonicalizeType reduceType
        }
    in
    let processConsumer
      : type t.
        (lOuter, lInner, t) Corn.Expr.consumerOp option
        -> ((lOuter, lInner, t, unit) Expr.consumerOp option * bool, _) AllocAcc.t
      = function
      | None -> return (None, false)
      | Some (ReduceSeq reduce) ->
        let%map reduce = processReduce reduce in
        Some (Expr.ReduceSeq reduce), true
      | Some (ReducePar { reduce; outerBody }) ->
        let%bind reduce = processReduce reduce in
        let%bind outerBody, outerBodyMemArgs =
          allocRequest
            outerCaptureAvoider
            ~mallocLoc:outerMallocLoc
            ~writeToAddr:None
            ~memDeviceToL:outerMemDeviceToL
            outerBody
          >>| getExpr
          |> outerCaptureAvoider
               ~capturesToAvoid:
                 (Set.of_list
                    (module Identifier)
                    [ reduce.arg.firstBinding; reduce.arg.secondBinding ])
          |> multiplyAllocations ~multiplier:(Add (Index.dimensionConstant blocks))
        in
        let%map interimResultMem =
          malloc
            ~hostOrDevice:MallocDevice
            (Type.array
               ~element:(Expr.type' reduce.body)
               ~size:(Add (Index.dimensionConstant blocks)))
            "reduce-interim-result"
        in
        ( Some
            (Expr.ReducePar
               { reduce
               ; interimResultMem
               ; outerBody
               ; outerMappedMemArgs = outerBodyMemArgs
               })
        , true )
      | Some (Fold { zeroArg; arrayArgs; body; d; character; type' }) ->
        let%map zeroValue =
          allocRequest
            outerCaptureAvoider
            ~mallocLoc:outerMallocLoc
            ~writeToAddr:None
            ~memDeviceToL:outerMemDeviceToL
            zeroArg.zeroValue
          >>| getExpr
        and body, bodyMemArgs =
          allocRequest
            innerCaptureAvoider
            ~mallocLoc:innerMallocLoc
            ~writeToAddr:None
            ~memDeviceToL:innerMemDeviceToL
            body
          >>| getExpr
          |> innerCaptureAvoider
               ~capturesToAvoid:
                 (Set.of_list
                    (module Identifier)
                    ([ zeroArg.zeroBinding ]
                     @ List.map arrayArgs ~f:(fun arg -> arg.binding)))
          |> multiplyAllocations ~multiplier:(convertShapeElement frameShape)
        in
        ( Some
            (Expr.Fold
               { zeroArg = { zeroBinding = zeroArg.zeroBinding; zeroValue }
               ; arrayArgs =
                   List.map arrayArgs ~f:(fun { binding; production } ->
                     Expr.{ binding; production = convertProduction production })
               ; mappedMemArgs = bodyMemArgs
               ; body
               ; d = convertDimension d
               ; character
               ; type' = canonicalizeType type'
               })
        , true )
      | Some (Scatter { valuesArg; indicesArg; dIn; dOut; type' }) ->
        let type' = canonicalizeType type' in
        let%map mem =
          getMemForResult ~targetAddr:consumerTargetAddr type' "scatter-array"
        in
        ( Some
            (Expr.Scatter
               { valuesArg = convertProduction valuesArg
               ; indicesArg = convertProduction indicesArg
               ; dIn = convertDimension dIn
               ; dOut = convertDimension dOut
               ; mem = outerMemDeviceToL mem
               ; type'
               })
        , false )
    in
    let%bind consumer, consumerCopyRequired = processConsumer consumer in
    let loopBlock : (lOuter, lInner, seqOrPar, unit) Expr.loopBlock =
      { frameShape = convertShapeElement frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapMemArgs
      ; mapBodyMatcher
      ; mapResults
      ; mapResultMem = outerMemDeviceToL mapResultMem
      ; consumer
      ; type'
      }
    in
    if consumerCopyRequired
    then
      partialUnwrittenExprToAllocResult
        ~memDeviceToL:outerMemDeviceToL
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
           ~hostOrDevice:mallocLoc
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
       statementToAllocResult ~mem (ReifyShapeIndex { shape; mem = memDeviceToL mem }))
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
      (fun loopBlock -> Expr.LoopBlock loopBlock)
      ~blocks:0
      avoidCaptures
      mallocLoc
      memDeviceToL
      avoidCaptures
      mallocLoc
      memDeviceToL
      loopBlock
  | LoopKernel { kernel = loopBlock; blocks; threads } ->
    allocLoopBlock
      (fun loopBlock ->
        Expr.LoopKernel { kernel = loopBlock; captures = (); blocks; threads })
      ~blocks
      hostAvoidCaptures
      MallocHost
      Acorn.Mem.deviceToHost
      deviceAvoidCaptures
      MallocDevice
      (fun m -> m)
      loopBlock
  | MapKernel { kernel = mapKernel; blocks; threads } ->
    let rec allocMapKernel
      ~writeToAddr:targetAddr
      Corn.Expr.
        { frameShape; mapArgs; mapIotas; mapBody; mapBodyMatcher; mapResults; type' }
      : ((unit Expr.mapKernel, unit) Expr.kernel, _) AllocAcc.t
      =
      let type' = canonicalizeType type' in
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
          let%bind mem = malloc ~hostOrDevice:mallocLoc type' "map-mem" in
          createTargetAddrMapArgs type' (Some (TargetValue mem))
        | Some (TargetValue targetValue) ->
          let elementType =
            arrayElementType ~expectedSize:(convertShapeElement frameShape)
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
      let%bind mapResultTargetAddr, mapTargetAddrMemArgs, mapResultMem =
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
      let rec allocMapBody ~writeToAddr:targetAddr (mapBody : Corn.Expr.mapBody) =
        match mapBody with
        | MapBodyMap mapKernel ->
          let%map mapKernel =
            allocMapKernel ~writeToAddr:targetAddr mapKernel
            |> deviceAvoidCaptures ~capturesToAvoid:bindingsForMapBody
          in
          [], [ mapKernel.kernel ]
        | MapBodyExpr expr ->
          let%map expr =
            allocDevice ~writeToAddr:targetAddr expr
            |> deviceAvoidCaptures ~capturesToAvoid:bindingsForMapBody
          in
          [ expr.statement ], []
        | MapBodyValues elements ->
          let elementsAndTargetAddrs =
            match targetAddr with
            | None -> List.map elements ~f:(fun element -> element, None)
            | Some (TargetValue targetAddr) ->
              List.mapi elements ~f:(fun index element ->
                element, Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index)))
            | Some (TargetValues targetAddrs) -> List.zip_exn elements targetAddrs
          in
          let%map subStatements, subSubMaps =
            elementsAndTargetAddrs
            |> List.map ~f:(fun (element, targetAddr) ->
              allocMapBody ~writeToAddr:targetAddr element)
            |> all
            >>| List.unzip
          in
          List.concat subStatements, List.concat subSubMaps
        | MapBodyDeref { tuple; index } ->
          let rec mapBodyType : Corn.Expr.mapBody -> Type.t = function
            | MapBodyMap mapKernel -> canonicalizeType mapKernel.type'
            | MapBodyExpr expr -> canonicalizeType @@ Corn.Expr.type' expr
            | MapBodyValues elements -> Tuple (List.map elements ~f:mapBodyType)
            | MapBodyDeref { tuple; index } ->
              let tupleType = typeAsTuple @@ mapBodyType tuple in
              List.nth_exn tupleType index
          in
          let tupleSize = List.length @@ typeAsTuple (mapBodyType tuple) in
          let tupleTargetAddrs =
            List.init tupleSize ~f:(fun i -> if i = index then targetAddr else None)
          in
          allocMapBody ~writeToAddr:(Some (TargetValues tupleTargetAddrs)) tuple
      in
      let%map (mapBodyStatements, mapBodySubMaps), mapBodyMemArgs =
        allocMapBody ~writeToAddr:mapBodyTargetAddr mapBody
        |> multiplyAllocations ~multiplier:(convertShapeElement frameShape)
      in
      let mapMemArgs =
        List.map mapTargetAddrMemArgs ~f:(fun { memBinding; mem } ->
          Acorn.Expr.{ memBinding; mem = Mem.deviceToHost mem })
        @ mapBodyMemArgs
      in
      ({ kernel =
           { frameShape = convertShapeElement frameShape
           ; mapArgs
           ; mapIotas
           ; mapBody =
               { statement = Statements mapBodyStatements; subMaps = mapBodySubMaps }
           ; mapMemArgs
           ; mapBodyMatcher
           ; mapResults
           ; mapResultMem = Acorn.Mem.deviceToHost mapResultMem
           ; type'
           }
       ; captures = ()
       ; blocks
       ; threads
       }
        : (unit Expr.mapKernel, unit) Expr.kernel)
    in
    let%map mapKernel = allocMapKernel ~writeToAddr:targetAddr mapKernel in
    Acorn.Expr.
      { expr =
          Expr.eseq
            ~statements:[ MapKernel mapKernel ]
            ~expr:(Expr.getmem mapKernel.kernel.mapResultMem)
      ; statement = MapKernel mapKernel
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
          let%map mem =
            malloc ~hostOrDevice:mallocLoc reifiedShapeType "reify-index-array"
          in
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
  | SubArray { arrayArg; indexArg; type' } ->
    let%bind arrayArg = allocExpr ~writeToAddr:None arrayArg
    and indexArg = allocExpr ~writeToAddr:None indexArg in
    unwrittenExprToAllocResult
    @@ SubArray { arrayArg; indexArg; type' = canonicalizeType type' }
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

and allocHost ~writeToAddr expr =
  allocRequest
    hostAvoidCaptures
    ~mallocLoc:MallocHost
    ~writeToAddr
    ~memDeviceToL:Acorn.Mem.deviceToHost
    expr

and allocDevice ~writeToAddr expr =
  allocRequest
    deviceAvoidCaptures
    ~mallocLoc:MallocDevice
    ~writeToAddr
    ~memDeviceToL:(fun m -> m)
    expr
;;

let alloc (expr : Corn.t)
  : (Acorn.sansCaptures, ('s option, string) Source.annotate) CompilerState.u
  =
  let open CompilerState.Let_syntax in
  let%map result, acc = allocHost ~writeToAddr:None expr in
  Acorn.Expr.MemLet
    { memArgs = List.map acc ~f:AllocAcc.allocationToMallocMemArg; body = result.expr }
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
