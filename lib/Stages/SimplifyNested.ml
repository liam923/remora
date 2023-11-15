open! Base
open Nested

module Counts : sig
  type count =
    { count : int
    ; inLoop : bool
    }
  [@@deriving sexp_of]

  type t [@@deriving sexp_of]

  val empty : t
  val one : Identifier.t -> inLoop:bool -> t
  val get : t -> Identifier.t -> count
  val merge : t list -> t
end = struct
  type count =
    { count : int
    ; inLoop : bool
    }
  [@@deriving sexp_of]

  type t = count Map.M(Identifier).t [@@deriving sexp_of]

  let empty = Map.empty (module Identifier)
  let one id ~inLoop = Map.singleton (module Identifier) id { count = 1; inLoop }

  let get counts id =
    Map.find counts id |> Option.value ~default:{ count = 0; inLoop = false }
  ;;

  let merge countss =
    List.fold countss ~init:empty ~f:(fun acc more ->
      Map.merge_skewed acc more ~combine:(fun ~key:_ a b ->
        { count = a.count + b.count; inLoop = a.inLoop || b.inLoop }))
  ;;
end

(* Determine if an expression is a constant expression or if it requires
   computation. i.e. 7 and x are nonComputational,
   while (+ 7 x) is computational *)
let rec nonComputational : Expr.t -> bool = function
  | Ref _ -> true
  | Frame frame -> List.for_all frame.elements ~f:nonComputational
  | BoxValue { box; type' = _ } -> nonComputational box
  | IndexLet _ -> false
  | ReifyIndex _ -> false
  | Box _ -> false
  | Literal (IntLiteral _ | CharacterLiteral _ | BooleanLiteral _) -> true
  | Values values -> List.for_all values.elements ~f:nonComputational
  | TupleDeref { tuple; index = _; type' = _ } -> nonComputational tuple
  | Let _ -> false
  | LoopBlock _ -> false
  | ScalarPrimitive _ -> false
  | SubArray { arrayArg; indexArg; type' = _ } ->
    nonComputational arrayArg && nonComputational indexArg
  | Append _ -> false
  | Zip _ -> true
  | Unzip _ -> true
;;

let getCounts =
  let getCountsIndex inLoop : Index.t -> Counts.t = function
    | Shape elements ->
      elements
      |> List.map ~f:(function
        | ShapeRef ref -> Counts.one ref ~inLoop
        | Add { const = _; refs } ->
          refs |> Map.keys |> List.map ~f:(Counts.one ~inLoop) |> Counts.merge)
      |> Counts.merge
    | Dimension { const = _; refs } ->
      refs |> Map.keys |> List.map ~f:(Counts.one ~inLoop) |> Counts.merge
  in
  let rec getCountsProductionTuple inLoop : Expr.productionTuple -> Counts.t = function
    | ProductionTuple { elements; type' = _ } ->
      elements |> List.map ~f:(getCountsProductionTuple inLoop) |> Counts.merge
    | ProductionTupleAtom atom -> Counts.one atom.productionId ~inLoop
  in
  let rec getCountsExpr inLoop : Expr.t -> Counts.t = function
    | Ref { id; type' = _ } -> Counts.one id ~inLoop
    | Frame { elements; dimension = _; type' = _ } ->
      elements |> List.map ~f:(getCountsExpr inLoop) |> Counts.merge
    | BoxValue { box; type' = _ } -> getCountsExpr inLoop box
    | IndexLet { indexArgs; body; type' = _ } ->
      let indexValueCounts =
        List.map indexArgs ~f:(fun arg ->
          match arg.indexValue with
          | Runtime value -> getCountsExpr inLoop value
          | FromBox { box; i = _ } -> getCountsExpr inLoop box)
      and bodyCounts = getCountsExpr inLoop body in
      Counts.merge (bodyCounts :: indexValueCounts)
    | ReifyIndex { index; type' = _ } -> getCountsIndex inLoop index
    | Let { args; body; type' = _ } ->
      let argCounts = args |> List.map ~f:(fun arg -> getCountsExpr inLoop arg.value)
      and bodyCounts = getCountsExpr inLoop body in
      Counts.merge (bodyCounts :: argCounts)
    | LoopBlock
        { frameShape
        ; mapArgs
        ; mapIotas
        ; mapBody
        ; mapBodyMatcher = _
        ; mapResults
        ; consumer
        ; type' = _
        } ->
      let argsCounts =
        mapArgs
        |> List.map ~f:(fun { binding = _; ref } -> getCountsExpr inLoop (Ref ref))
        |> Counts.merge
      and iotaCounts =
        mapIotas
        |> List.map ~f:(fun { iota = _; nestIn } ->
          match nestIn with
          | Some nestIn -> Counts.one nestIn ~inLoop
          | None -> Counts.empty)
        |> Counts.merge
      and bodyCounts = getCountsExpr true mapBody
      and frameShapeCounts = getCountsIndex inLoop (Shape [ frameShape ])
      and mapResultsCounts =
        mapResults |> List.map ~f:(Counts.one ~inLoop) |> Counts.merge
      and consumerCounts =
        match consumer with
        | None -> Counts.empty
        | Some
            (Reduce
              { arg; body; zero; d; itemPad; associative = _; character = _; type' = _ })
          ->
          let argCounts = getCountsProductionTuple inLoop arg.production
          and bodyCounts = getCountsExpr true body
          and zeroCounts =
            zero
            |> Option.map ~f:(getCountsExpr inLoop)
            |> Option.value ~default:Counts.empty
          and dCounts = getCountsIndex inLoop (Dimension d)
          and itemPadCounts = getCountsIndex inLoop (Shape itemPad) in
          Counts.merge [ argCounts; bodyCounts; zeroCounts; dCounts; itemPadCounts ]
        | Some (Fold { zeroArg; arrayArgs; body; d; itemPad; character = _; type' = _ })
          ->
          let zeroCounts = getCountsExpr inLoop zeroArg.zeroValue
          and arrayCounts =
            arrayArgs
            |> List.map ~f:(fun { binding = _; production } ->
              Counts.one production.productionId ~inLoop)
            |> Counts.merge
          and bodyCounts = getCountsExpr true body
          and dCounts = getCountsIndex inLoop (Dimension d)
          and itemPadCounts = getCountsIndex inLoop (Shape itemPad) in
          Counts.merge [ zeroCounts; arrayCounts; bodyCounts; dCounts; itemPadCounts ]
        | Some (Scatter { valuesArg; indicesArg; dIn; dOut; type' = _ }) ->
          Counts.merge
            [ Counts.one valuesArg.productionId ~inLoop
            ; Counts.one indicesArg.productionId ~inLoop
            ; getCountsIndex inLoop (Dimension dIn)
            ; getCountsIndex inLoop (Dimension dOut)
            ]
      in
      Counts.merge
        [ argsCounts
        ; iotaCounts
        ; bodyCounts
        ; frameShapeCounts
        ; mapResultsCounts
        ; consumerCounts
        ]
    | Literal (IntLiteral _) -> Counts.empty
    | Literal (CharacterLiteral _) -> Counts.empty
    | Literal (BooleanLiteral _) -> Counts.empty
    | Box { indices; body; bodyType = _; type' = _ } ->
      let indexCounts = List.map indices ~f:(getCountsIndex inLoop)
      and bodyCounts = getCountsExpr inLoop body in
      Counts.merge (bodyCounts :: indexCounts)
    | ScalarPrimitive { op = _; args; type' = _ } ->
      args |> List.map ~f:(getCountsExpr inLoop) |> Counts.merge
    | Values { elements; type' = _ } ->
      elements |> List.map ~f:(getCountsExpr inLoop) |> Counts.merge
    | TupleDeref { tuple; index = _; type' = _ } -> getCountsExpr inLoop tuple
    | SubArray { arrayArg; indexArg; type' = _ } ->
      Counts.merge [ getCountsExpr inLoop arrayArg; getCountsExpr inLoop indexArg ]
    | Append { args; type' = _ } ->
      args |> List.map ~f:(getCountsExpr inLoop) |> Counts.merge
    | Zip { zipArg; nestCount = _; type' = _ } -> getCountsExpr inLoop zipArg
    | Unzip { unzipArg; type' = _ } -> getCountsExpr inLoop unzipArg
  in
  getCountsExpr false
;;

let rec subExpr subKey subValue : Expr.t -> Expr.t option =
  let open Expr in
  let open Option.Let_syntax in
  function
  | Ref { id; type' = _ } as expr ->
    return (if Identifier.equal id subKey then subValue else expr)
  | Frame { elements; dimension; type' } ->
    let%map elements = elements |> List.map ~f:(subExpr subKey subValue) |> Option.all in
    Frame { elements; dimension; type' }
  | BoxValue { box; type' } ->
    let%map box = subExpr subKey subValue box in
    BoxValue { box; type' }
  | IndexLet { indexArgs; body; type' } ->
    let%map indexArgs =
      indexArgs
      |> List.map ~f:(fun { indexBinding; indexValue; sort } ->
        let%map indexValue =
          match indexValue with
          | Runtime value ->
            let%map value = subExpr subKey subValue value in
            Runtime value
          | FromBox { box; i } ->
            let%map box = subExpr subKey subValue box in
            FromBox { box; i }
        in
        { indexBinding; indexValue; sort })
      |> Option.all
    and body = subExpr subKey subValue body in
    IndexLet { indexArgs; body; type' }
  | ReifyIndex { index; type' } -> return (ReifyIndex { index; type' })
  | Let { args; body; type' } ->
    let%map args =
      args
      |> List.map ~f:(fun { binding; value } ->
        let%map value = subExpr subKey subValue value in
        { binding; value })
      |> Option.all
    and body = subExpr subKey subValue body in
    Let { args; body; type' }
  | LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; consumer
      ; type'
      } ->
    let%map mapArgs =
      mapArgs
      |> List.map ~f:(fun { binding; ref } ->
        if Identifier.equal subKey ref.id then None else return { binding; ref })
      |> Option.all
    and mapBody = subExpr subKey subValue mapBody
    and consumer =
      let subProduction (p : Expr.production) =
        if Identifier.equal subKey p.productionId then None else return p
      in
      match consumer with
      | None -> return None
      | Some (Reduce { arg; body; zero; d; itemPad; associative; character; type' }) ->
        let rec productionTupleContainsIdInSubs : Expr.productionTuple -> bool = function
          | ProductionTuple { elements; type' = _ } ->
            List.exists elements ~f:productionTupleContainsIdInSubs
          | ProductionTupleAtom production ->
            Identifier.equal subKey production.productionId
        in
        let%map arg =
          if productionTupleContainsIdInSubs arg.production then None else return arg
        and body = subExpr subKey subValue body
        and zero =
          match zero with
          | Some zero ->
            let%map zero = subExpr subKey subValue zero in
            Some zero
          | None -> return None
        in
        Some (Reduce { arg; body; zero; d; itemPad; associative; character; type' })
      | Some (Fold { zeroArg; arrayArgs; body; d; itemPad; character; type' }) ->
        let%map zeroArg =
          let%map zeroValue = subExpr subKey subValue zeroArg.zeroValue in
          { zeroBinding = zeroArg.zeroBinding; zeroValue }
        and arrayArgs =
          arrayArgs
          |> List.map ~f:(fun { binding; production } ->
            let%map production = subProduction production in
            { binding; production })
          |> Option.all
        and body = subExpr subKey subValue body in
        Some (Fold { zeroArg; arrayArgs; body; d; itemPad; character; type' })
      | Some (Scatter { valuesArg; indicesArg; dIn; dOut; type' }) ->
        let%map valuesArg = subProduction valuesArg
        and indicesArg = subProduction indicesArg in
        Some (Scatter { valuesArg; indicesArg; dIn; dOut; type' })
    in
    LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; consumer
      ; type'
      }
  | Append { args; type' } ->
    let%map args = args |> List.map ~f:(subExpr subKey subValue) |> Option.all in
    Append { args; type' }
  | SubArray { arrayArg; indexArg; type' } ->
    let%map arrayArg = subExpr subKey subValue arrayArg
    and indexArg = subExpr subKey subValue indexArg in
    SubArray { arrayArg; indexArg; type' }
  | Literal (IntLiteral _ | CharacterLiteral _ | BooleanLiteral _) as lit -> return lit
  | Box { indices; body; bodyType; type' } ->
    let%map body = subExpr subKey subValue body in
    Box { indices; body; bodyType; type' }
  | ScalarPrimitive { op; args; type' } ->
    let%map args = args |> List.map ~f:(subExpr subKey subValue) |> Option.all in
    ScalarPrimitive { op; args; type' }
  | Values { elements; type' } ->
    let%map elements = elements |> List.map ~f:(subExpr subKey subValue) |> Option.all in
    Values { elements; type' }
  | TupleDeref { tuple; index; type' } ->
    let%map tuple = subExpr subKey subValue tuple in
    TupleDeref { tuple; index; type' }
  | Zip { zipArg; nestCount; type' } ->
    let%map zipArg = subExpr subKey subValue zipArg in
    Zip { zipArg; nestCount; type' }
  | Unzip { unzipArg; type' } ->
    let%map unzipArg = subExpr subKey subValue unzipArg in
    Unzip { unzipArg; type' }
;;

(* Perform the following optimizations:
   - Copy propogation
   - Delete unused variables
   - Inline variables only used once
   - Inline variables with constant value
   - Constant folding *)
let rec optimize : Expr.t -> Expr.t =
  let open Expr in
  function
  | Ref _ as ref -> ref
  | Frame { elements; dimension; type' } ->
    let elements = List.map elements ~f:optimize in
    Frame { elements; dimension; type' }
  | BoxValue { box; type' } ->
    let box = optimize box in
    (match box with
     | Box { indices = _; body; bodyType = _; type' = _ } -> body
     | _ -> BoxValue { box; type' })
  | IndexLet { indexArgs; body; type' } ->
    (* TODO: remove unused bindings and inline ones known statically *)
    let indexArgs =
      List.map indexArgs ~f:(fun { indexBinding; indexValue; sort } ->
        Expr.
          { indexBinding
          ; indexValue =
              (match indexValue with
               | Runtime value -> Runtime (optimize value)
               | FromBox { box; i } -> FromBox { box = optimize box; i })
          ; sort
          })
    in
    let body = optimize body in
    IndexLet { indexArgs; body; type' }
  | ReifyIndex { index = Dimension d; type' = _ } as original ->
    if Map.is_empty d.refs
    then (* The dimension is known, so we can sub it in *)
      Literal (IntLiteral d.const)
    else original
  | ReifyIndex { index = Shape shape; type' } as original ->
    let revDims =
      List.fold shape ~init:(Some []) ~f:(fun revDims nextDim ->
        match revDims, nextDim with
        | Some revDims, Add d when Map.is_empty d.refs -> Some (d.const :: revDims)
        | _ -> None)
    in
    (match revDims with
     | Some revDims ->
       let dims = List.rev revDims in
       Expr.Frame
         { elements = List.map dims ~f:(fun dim -> Literal (IntLiteral dim))
         ; dimension = List.length dims
         ; type'
         }
     | None -> original)
  | Append { args; type' } ->
    let args = List.map args ~f:optimize in
    let elementType =
      match type' with
      | Array array -> array.element
      | _ -> raise (Unreachable.Error "expected array as append type")
    in
    let rec reduceArgs args =
      match args with
      | Frame f1 :: Frame f2 :: restArgs ->
        let mergedArg =
          Frame
            { dimension = f1.dimension + f2.dimension
            ; elements = f1.elements @ f2.elements
            ; type' =
                Array
                  { element = elementType
                  ; size = Add (Index.dimensionConstant (f1.dimension + f2.dimension))
                  }
            }
        in
        reduceArgs (mergedArg :: restArgs)
      | arg :: restArgs -> arg :: reduceArgs restArgs
      | [] -> []
    in
    let args = reduceArgs args in
    (match args with
     | [] -> Frame { dimension = 0; elements = []; type' }
     | arg :: [] -> arg
     | _ :: _ :: _ as args -> Append { args; type' })
  | Let { args; body; type' } ->
    (* Do an initial simplification of the argument values and the body *)
    let args =
      List.map args ~f:(fun Expr.{ binding; value } ->
        let value = optimize value in
        Expr.{ binding; value })
    in
    let body = optimize body in
    (* Inline args that can be propogated and remove unused args. *)
    let bodyCounts = getCounts body in
    let argsRev, body =
      List.fold args ~init:([], body) ~f:(fun (argsAcc, body) arg ->
        let count = Counts.get bodyCounts arg.binding in
        if count.count = 0
        then
          (* No usages, so drop the arg *)
          (* DISCARD!!! *)
          argsAcc, body
        else if (count.count = 1 && not count.inLoop) || nonComputational arg.value
        then (
          (* The arg can be subbed into the body *)
          match subExpr arg.binding arg.value body with
          | Some body -> argsAcc, body
          | None ->
            (* Subbing into body failed, so keep the arg *)
            arg :: argsAcc, body)
        else (* Multiple usages, so do nothing *)
          arg :: argsAcc, body)
    in
    let args = List.rev argsRev in
    (* If args are now empty, just return the body, as the let is now redundant *)
    (match args with
     | [] -> body
     | args -> Let { args; body; type' })
  | LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; consumer
      ; type'
      } ->
    let mapBody = optimize mapBody in
    (* Simplify the args and iotas, removing unused ones *)
    let mapBodyCounts = getCounts mapBody in
    let mapArgs =
      List.filter mapArgs ~f:(fun arg ->
        (* DISCARD!!! *)
        (Counts.get mapBodyCounts arg.binding).count > 0)
    in
    let mapIotas =
      List.filter mapIotas ~f:(fun iota -> (Counts.get mapBodyCounts iota.iota).count > 0)
    in
    let consumer =
      match consumer with
      | None -> None
      | Some (Reduce { arg; body; zero; d; itemPad; associative; character; type' }) ->
        let body = optimize body
        and zero = Option.map zero ~f:optimize in
        Some (Reduce { arg; body; zero; d; itemPad; associative; character; type' })
      | Some (Fold { zeroArg; arrayArgs; body; d; itemPad; character; type' }) ->
        let body = optimize body
        and zeroArg = { zeroArg with zeroValue = optimize zeroArg.zeroValue } in
        (* Simplify the zero args, removing unused ones *)
        let bodyCounts = getCounts body in
        let arrayArgs =
          List.filter arrayArgs ~f:(fun arg ->
            (* DISCARD!!! *)
            (Counts.get bodyCounts arg.binding).count > 0)
        in
        Some (Fold { zeroArg; arrayArgs; body; d; itemPad; character; type' })
      | Some (Scatter { valuesArg = _; indicesArg = _; dIn = _; dOut = _; type' = _ }) as
        scatter -> scatter
    in
    LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; consumer
      ; type'
      }
  | SubArray { arrayArg; indexArg; type' } ->
    let arrayArg = optimize arrayArg
    and indexArg = optimize indexArg in
    SubArray { arrayArg; indexArg; type' }
  | Box { indices; body; bodyType; type' } ->
    let body = optimize body in
    Expr.Box { indices; body; bodyType; type' }
  | Literal _ as literal -> literal
  | ScalarPrimitive { op; args; type' } ->
    let args = List.map args ~f:optimize in
    (* Do constant folding: *)
    (match op, args with
     | Add, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (IntLiteral (a + b))
     | Add, [ Literal (IntLiteral 0); value ] | Add, [ value; Literal (IntLiteral 0) ] ->
       value
     | Sub, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (IntLiteral (a - b))
     | Sub, [ value; Literal (IntLiteral 0) ] -> value
     | Mul, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (IntLiteral (a * b))
     | Mul, [ Literal (IntLiteral 1); value ] | Mul, [ value; Literal (IntLiteral 1) ] ->
       value
     | Mul, [ Literal (IntLiteral 0); _ ] | Mul, [ _; Literal (IntLiteral 0) ] ->
       (* DISCARD!!! *)
       Literal (IntLiteral 0)
     | Div, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (IntLiteral (a / b))
     | Div, [ value; Literal (IntLiteral 1) ] -> value
     | _ -> ScalarPrimitive { op; args; type' })
  | Values { elements; type' } ->
    let elements = List.map elements ~f:optimize in
    let values = Expr.Values { elements; type' } in
    (match elements with
     | TupleDeref { tuple = Ref ref; index = 0; type' = _ } :: _ ->
       (* ex: (a.0, a.1, a.2) => a *)
       let sizesMatch =
         match ref.type' with
         | Tuple t -> List.length t = List.length elements
         | _ -> false
       in
       let allSequentialDerefs =
         elements
         |> List.mapi ~f:(fun i e -> i, e)
         |> List.for_all ~f:(fun (expectedIndex, e) ->
           match e with
           | TupleDeref { tuple = Ref eRef; index = eI; type' = _ }
             when Identifier.equal ref.id eRef.id && eI = expectedIndex -> true
           | _ -> false)
       in
       if sizesMatch && allSequentialDerefs then Ref ref else values
     | _ -> values)
  | TupleDeref { tuple; index; type' } ->
    let tuple = optimize tuple in
    (match tuple with
     | Values { elements; type' = _ } -> List.nth_exn elements index
     | _ -> TupleDeref { tuple; index; type' })
  | Zip { zipArg; nestCount = 0; type' = _ } -> optimize zipArg
  | Zip { zipArg; nestCount; type' } ->
    let zipArg = optimize zipArg in
    Zip { zipArg; nestCount; type' }
  | Unzip { unzipArg = Zip { zipArg; nestCount = _; type' = _ }; type' = _ } -> zipArg
  | Unzip { unzipArg; type' } ->
    let unzipArg = optimize unzipArg in
    (match Expr.type' unzipArg with
     | Tuple _ -> unzipArg
     | _ -> Unzip { unzipArg; type' })
;;

module TupleRequest = struct
  module T = struct
    type collectionType =
      | Array of Index.shapeElement
      | Sigma of Sort.t Type.param list
    [@@deriving compare, sexp_of, eq]

    type deref =
      { i : int
      ; rest : t
      }

    and t =
      | Element of deref
      | Elements of deref list
      | Whole
      | Collection of
          { subRequest : t
          ; collectionType : collectionType
          }
    [@@deriving compare, sexp_of]
  end

  include Comparator.Make (T)
  include T

  let isWhole = function
    | Whole -> true
    | _ -> false
  ;;

  let unexpected ~actual ~expected =
    let actualStr = actual |> sexp_of_t |> Sexp.to_string in
    Unreachable.Error [%string "Expected %{expected} request type, got %{actualStr}"]
  ;;
end

module ReduceTupleState = struct
  include State

  type cache =
    | TupleCache of
        { bindings : Identifier.t list
        ; subCaches : cache Map.M(Int).t
        }
    | CollectionCache of
        { subCache : cache
        ; collectionType : TupleRequest.collectionType
        }
  [@@deriving sexp_of]

  type state =
    { compilerState : CompilerState.state
    ; caches : cache Map.M(Identifier).t
    ; droppedAny : bool
    }

  type ('t, 'e) u = (state, 't, 'e) t

  let getCaches () = get () >>| fun state -> state.caches
  let setCaches caches = get () >>= fun state -> set { state with caches }
  let updateCaches ~f = getCaches () >>= fun caches -> setCaches (f caches)
  let markDrop () = get () >>= fun state -> set { state with droppedAny = true }

  let createId name =
    make ~f:(fun state ->
      State.run
        (Identifier.create
           name
           ~getCounter:(fun (s : state) -> s.compilerState.idCounter)
           ~setCounter:(fun (s : state) idCounter ->
             { s with compilerState = { s.compilerState with idCounter } }))
        state)
  ;;

  let toSimplifyState (s : ('t, _) u) : (CompilerState.state, 't * bool, _) State.t =
    State.make ~f:(fun compilerState ->
      let state =
        { compilerState; caches = Map.empty (module Identifier); droppedAny = false }
      in
      let state, res = run s state in
      state.compilerState, (res, state.droppedAny))
  ;;
end

let rec reduceTuplesType (request : TupleRequest.t) : Type.t -> Type.t = function
  | Array array -> Array (reduceTuplesArrayType request array)
  | Tuple elements as t ->
    (match request with
     | Whole -> t
     | Element { i; rest } ->
       let element = List.nth_exn elements i in
       reduceTuplesType rest element
     | Elements elementRequests ->
       Tuple
         (List.map elementRequests ~f:(fun { i; rest } ->
            let element = List.nth_exn elements i in
            reduceTuplesType rest element))
     | Collection _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"tuple"))
  | Literal _ as lit ->
    assert (TupleRequest.isWhole request);
    lit
  | Sigma sigma -> Sigma (reduceTuplesSigmaType request sigma)

and reduceTuplesArrayType (request : TupleRequest.t) ({ element; size } as t : Type.array)
  : Type.array
  =
  match request with
  | Collection { subRequest; collectionType = Array requestSize } ->
    assert (Index.equal_shapeElement size requestSize);
    { element = reduceTuplesType subRequest element; size }
  | Whole -> t
  | Element _ | Elements _ | Collection { subRequest = _; collectionType = Sigma _ } ->
    raise (TupleRequest.unexpected ~actual:request ~expected:"array")

and reduceTuplesSigmaType
  (request : TupleRequest.t)
  ({ parameters; body } as t : Type.sigma)
  : Type.sigma
  =
  match request with
  | Collection { subRequest; collectionType = Sigma requestParameters } ->
    assert (List.equal (Type.equal_param Sort.equal) parameters requestParameters);
    let body = reduceTuplesType subRequest body in
    { parameters; body }
  | Whole -> t
  | Element _ | Elements _ | Collection { subRequest = _; collectionType = Array _ } ->
    raise (TupleRequest.unexpected ~actual:request ~expected:"sigma")
;;

let rec createUnpackersFromCache
  (cache : ReduceTupleState.cache)
  (derefStack : [ `IntDeref of int | `CollectionUnzip ] list)
  ~(insideWhole : bool)
  : (Expr.t -> Expr.letArg) list
  =
  match cache with
  | TupleCache { bindings; subCaches } ->
    let insideWhole = insideWhole || not (List.is_empty bindings) in
    let refUnpacker =
      List.map bindings ~f:(fun id ->
        let rec deref value = function
          | `IntDeref index :: rest ->
            Expr.tupleDeref ~tuple:(Expr.unzip (deref value rest)) ~index
          | `CollectionUnzip :: rest -> deref value rest
          | [] -> value
        in
        fun masterRef -> Expr.{ binding = id; value = deref masterRef derefStack })
    in
    let subCacheList =
      subCaches
      |> Map.to_alist
      |> List.sort ~compare:(fun (ai, _) (bi, _) -> Int.compare ai bi)
    in
    let subUnpackers =
      if insideWhole
      then
        List.bind subCacheList ~f:(fun (index, subCache) ->
          createUnpackersFromCache subCache (`IntDeref index :: derefStack) ~insideWhole)
      else (
        match subCacheList with
        | [ (_, subCache) ] when not insideWhole ->
          (* if not inside a whole and there's only one element,
             the tuple is un-nested *)
          createUnpackersFromCache subCache derefStack ~insideWhole
        | subCaches ->
          subCaches
          |> List.mapi ~f:(fun index (_, subCache) ->
            createUnpackersFromCache subCache (`IntDeref index :: derefStack) ~insideWhole)
          |> List.concat)
    in
    refUnpacker @ subUnpackers
  | CollectionCache { subCache; collectionType = _ } ->
    createUnpackersFromCache subCache (`CollectionUnzip :: derefStack) ~insideWhole
;;

let rec createRequestFromCache (cache : ReduceTupleState.cache) : TupleRequest.t =
  match cache with
  | TupleCache { bindings = _ :: _; subCaches = _ } -> Whole
  | TupleCache { bindings = []; subCaches } ->
    let subCacheList =
      subCaches
      |> Map.to_alist
      |> List.sort ~compare:(fun (ai, _) (bi, _) -> Int.compare ai bi)
    in
    (match subCacheList with
     | [] -> Elements []
     | [ (i, subCache) ] ->
       let subRequest = createRequestFromCache subCache in
       Element { i; rest = subRequest }
     | _ :: _ :: _ as subCaches ->
       let elementRequests =
         List.map subCaches ~f:(fun (sourceI, subCache) ->
           let subRequest = createRequestFromCache subCache in
           TupleRequest.{ i = sourceI; rest = subRequest })
       in
       Elements elementRequests)
  | CollectionCache { subCache; collectionType } ->
    Collection { subRequest = createRequestFromCache subCache; collectionType }
;;

let rec reduceTuples (request : TupleRequest.t) =
  let open ReduceTupleState.Let_syntax in
  function
  | Expr.Ref { id = masterId; type' } ->
    let%bind caches = ReduceTupleState.getCaches () in
    let cache = Map.find caches masterId in
    let%bind value, cache =
      let rec resolveRequest
        (cache : ReduceTupleState.cache option)
        (request : TupleRequest.t)
        (elementType : Type.t)
        (typeCollectionWrapper : Type.t -> Type.t)
        (zipDepth : int)
        derefStack
        : (Expr.t * ReduceTupleState.cache, _) ReduceTupleState.u
        =
        let createId () =
          let derefString =
            derefStack
            |> List.rev
            |> List.map ~f:(fun i -> [%string "[%{i#Int}]"])
            |> String.concat
          in
          let varName = [%string "%{Identifier.name masterId}%{derefString}"] in
          ReduceTupleState.createId varName
        in
        match cache with
        | Some (TupleCache cache) ->
          (match request with
           | Whole ->
             let%map id = createId () in
             ( Expr.Ref { id; type' = typeCollectionWrapper elementType }
             , ReduceTupleState.TupleCache { cache with bindings = id :: cache.bindings }
             )
           | Element { i; rest = subRequest } ->
             let subCache = Map.find cache.subCaches i in
             let subType = reduceTuplesType (Element { i; rest = Whole }) elementType in
             let%map value, subCache =
               resolveRequest
                 subCache
                 subRequest
                 subType
                 typeCollectionWrapper
                 zipDepth
                 (i :: derefStack)
             in
             ( value
             , ReduceTupleState.TupleCache
                 { cache with subCaches = Map.set cache.subCaches ~key:i ~data:subCache }
             )
           | Elements elementRequests ->
             let%map elementsRev, cache =
               List.fold
                 elementRequests
                 ~init:(return ([], ReduceTupleState.TupleCache cache))
                 ~f:(fun curr nextRequest ->
                   let%bind currElementArrays, prevCache = curr in
                   let%map nextElementArray, nextCache =
                     resolveRequest
                       (Some prevCache)
                       (Element nextRequest)
                       elementType
                       typeCollectionWrapper
                       zipDepth
                       derefStack
                   in
                   nextElementArray :: currElementArrays, nextCache)
             in
             let elements = List.rev elementsRev in
             let tupleType =
               match elementType with
               | Tuple tuple -> tuple
               | _ -> raise (Unreachable.Error "Expected tuple type")
             in
             let stripCollections n type' =
               if n = 0
               then type'
               else (
                 match type' with
                 | Type.Array { element; size = _ } -> element
                 | Type.Sigma { parameters = _; body } -> body
                 | _ -> raise @@ Unimplemented.Error "Expected collection type")
             in
             let tuple = Expr.Values { elements; type' = tupleType } in
             let zippedTuple =
               Expr.Zip
                 { zipArg = tuple
                 ; nestCount = zipDepth
                 ; type' =
                     tupleType
                     |> List.map ~f:(stripCollections zipDepth)
                     |> Type.Tuple
                     |> typeCollectionWrapper
                 }
             in
             zippedTuple, cache
           | _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"tuple"))
        | Some (CollectionCache cache) ->
          let restRequest =
            match request with
            | Collection { subRequest; collectionType } ->
              assert (
                TupleRequest.equal_collectionType collectionType cache.collectionType);
              subRequest
            | Whole -> Whole
            | _ -> raise @@ TupleRequest.unexpected ~actual:request ~expected:"collection"
          in
          let restType, newTypeCollectionWrapper =
            match elementType with
            | Array { element; size } ->
              element, fun t -> typeCollectionWrapper @@ Array { element = t; size }
            | Sigma { parameters; body } ->
              body, fun t -> typeCollectionWrapper @@ Sigma { parameters; body = t }
            | _ -> raise @@ Unreachable.Error "Expected collection type"
          in
          let%map ref, subCache =
            resolveRequest
              (Some cache.subCache)
              restRequest
              restType
              newTypeCollectionWrapper
              (zipDepth + 1)
              derefStack
          in
          ref, ReduceTupleState.CollectionCache { cache with subCache }
        | None ->
          let rec makeEmptyCache (type' : Type.t) =
            match type' with
            | Array { element; size } ->
              ReduceTupleState.CollectionCache
                { subCache = makeEmptyCache element; collectionType = Array size }
            | Sigma { parameters; body } ->
              ReduceTupleState.CollectionCache
                { subCache = makeEmptyCache body; collectionType = Sigma parameters }
            | Literal _ | Tuple _ ->
              ReduceTupleState.TupleCache
                { bindings = []; subCaches = Map.empty (module Int) }
          in
          let cache = makeEmptyCache elementType in
          resolveRequest
            (Some cache)
            request
            elementType
            typeCollectionWrapper
            0
            derefStack
      in
      resolveRequest cache request type' (fun t -> t) 0 []
    in
    let%map () = ReduceTupleState.setCaches (Map.set caches ~key:masterId ~data:cache) in
    value
  | Frame { elements; dimension; type' } ->
    let elementRequest =
      match request with
      | Collection { subRequest; collectionType = Array _ } -> subRequest
      | Whole -> Whole
      | _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"array")
    in
    let%map elements =
      elements |> List.map ~f:(reduceTuples elementRequest) |> ReduceTupleState.all
    in
    let type' = reduceTuplesType request type' in
    Expr.Frame { elements; dimension; type' }
  | ReifyIndex _ as reifyIndex ->
    assert (TupleRequest.isWhole request);
    return reifyIndex
  | Append { args; type' } ->
    let%map args = args |> List.map ~f:(reduceTuples request) |> ReduceTupleState.all in
    let type' = reduceTuplesType request type' in
    Expr.Append { args; type' }
  | Box { indices; body; bodyType; type' } ->
    let bodyRequest =
      match request with
      | Collection { subRequest; collectionType = Sigma _ } -> subRequest
      | Whole -> Whole
      | _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"sigma")
    in
    let%map body = reduceTuples bodyRequest body in
    let type' = reduceTuplesSigmaType request type' in
    Expr.Box { indices; body; bodyType; type' }
  | Literal _ as literal ->
    assert (TupleRequest.isWhole request);
    return literal
  | TupleDeref { tuple; index; type' = _ } ->
    reduceTuples (Element { i = index; rest = request }) tuple
  | ScalarPrimitive { op; args; type' } ->
    let%map args = args |> List.map ~f:(reduceTuples Whole) |> ReduceTupleState.all in
    let type' = reduceTuplesType Whole type' in
    Expr.ScalarPrimitive { op; args; type' }
  | Values { elements; type' } ->
    (match request with
     | Whole ->
       let%map elements =
         elements |> List.map ~f:(reduceTuples Whole) |> ReduceTupleState.all
       in
       Expr.Values { elements; type' }
     | Element { i; rest } ->
       (* DISCARD!!! *)
       let value = List.nth_exn elements i in
       let%bind () =
         if List.length elements > 1 then ReduceTupleState.markDrop () else return ()
       in
       reduceTuples rest value
     | Elements elementRequests ->
       (* DISCARD!!! *)
       let oldElementCount = List.length elements in
       let%bind elements =
         elementRequests
         |> List.map ~f:(fun { i; rest } ->
           let value = List.nth_exn elements i in
           reduceTuples rest value)
         |> ReduceTupleState.all
       in
       let type' = List.map elements ~f:Expr.type' in
       let%map () =
         if List.length elementRequests < oldElementCount
         then ReduceTupleState.markDrop ()
         else return ()
       in
       Expr.Values { elements; type' }
     | Collection _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"tuple"))
  | BoxValue { box; type' } ->
    let boxType =
      match Expr.type' box with
      | Sigma sigma -> sigma
      | _ -> raise (Unreachable.Error "expected sigma type")
    in
    let%map box =
      reduceTuples
        (Collection { subRequest = request; collectionType = Sigma boxType.parameters })
        box
    in
    let type' = reduceTuplesType request type' in
    Expr.BoxValue { box; type' }
  | SubArray { arrayArg; indexArg; type' } ->
    let argType =
      match Expr.type' arrayArg with
      | Array array -> array
      | _ -> raise (Unreachable.Error "expected array type")
    in
    let%map arrayArg =
      reduceTuples
        (Collection { subRequest = request; collectionType = Array argType.size })
        arrayArg
    and indexArg = reduceTuples Whole indexArg in
    let type' = reduceTuplesType request type' in
    Expr.SubArray { arrayArg; indexArg; type' }
  | Zip { zipArg; nestCount; type' } ->
    let rec interchangeCollections nestCount wrapper request =
      if nestCount = 0
      then (
        match request with
        | TupleRequest.Whole -> TupleRequest.Whole
        | TupleRequest.Element { i; rest } ->
          TupleRequest.Element { i; rest = wrapper rest }
        | TupleRequest.Elements derefs ->
          TupleRequest.Elements
            (List.map derefs ~f:(fun { i; rest } ->
               TupleRequest.{ i; rest = wrapper rest }))
        | _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"tuple"))
      else (
        match request with
        | TupleRequest.Whole -> TupleRequest.Whole
        | TupleRequest.Collection { subRequest; collectionType } ->
          interchangeCollections
            (nestCount - 1)
            (fun r -> wrapper @@ Collection { subRequest = r; collectionType })
            subRequest
        | TupleRequest.Element _ | TupleRequest.Elements _ ->
          raise (TupleRequest.unexpected ~actual:request ~expected:"collection"))
    in
    let%map zipArg =
      reduceTuples (interchangeCollections nestCount (fun r -> r) request) zipArg
    in
    Expr.Zip { zipArg; nestCount; type' }
  | Unzip { unzipArg; type' } ->
    let nestCount, collectionRequestWrapper =
      let rec unzipType = function
        | Type.Tuple _ -> 0, fun r -> r
        | Type.Array { element; size } ->
          let subCount, subWrapper = unzipType element in
          ( subCount + 1
          , fun r ->
              TupleRequest.Collection
                { subRequest = subWrapper r; collectionType = Array size } )
        | Type.Sigma { parameters; body } ->
          let subCount, subWrapper = unzipType body in
          ( subCount + 1
          , fun r ->
              TupleRequest.Collection
                { subRequest = subWrapper r; collectionType = Sigma parameters } )
        | Type.Literal _ -> raise (Unreachable.Error "Unexpected literal type")
      in
      unzipType (Expr.type' unzipArg)
    in
    let rec stripCollections nestCount request =
      if nestCount = 0
      then request
      else (
        match request with
        | TupleRequest.Collection { subRequest; collectionType = _ } ->
          stripCollections (nestCount - 1) subRequest
        | TupleRequest.Whole -> Whole
        | _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"collection"))
    in
    (match request with
     | Whole ->
       let%map unzipArg = reduceTuples Whole unzipArg in
       Expr.Unzip { unzipArg; type' }
     | Element { i; rest } ->
       let request =
         collectionRequestWrapper (Element { i; rest = stripCollections nestCount rest })
       in
       reduceTuples request unzipArg
     | Elements elementRequests ->
       let unzipArgRequest =
         collectionRequestWrapper
           (Elements
              (List.map elementRequests ~f:(fun { i; rest } ->
                 TupleRequest.{ i; rest = stripCollections nestCount rest })))
       in
       let%map unzipArg = reduceTuples unzipArgRequest unzipArg in
       let type' =
         match reduceTuplesType request (Tuple type') with
         | Tuple tuple -> tuple
         | _ -> raise (Unreachable.Error "expected tuple type")
       in
       Expr.Unzip { unzipArg; type' }
     | Collection _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"tuple"))
  | IndexLet { indexArgs; body; type' } ->
    let%map body = reduceTuples request body in
    Expr.IndexLet { indexArgs; body; type' }
  | Let { args; body; type' } ->
    let%bind body = reduceTuples request body in
    let type' = reduceTuplesType request type' in
    let%bind caches = ReduceTupleState.getCaches () in
    let%bind args, unpackerss =
      args
      |> List.map ~f:(fun { binding; value } ->
        Map.find caches binding
        |> Option.map ~f:(fun cache ->
          let request = createRequestFromCache cache in
          let unpackersRaw = createUnpackersFromCache cache [] ~insideWhole:false in
          let%map value = reduceTuples request value in
          let ref = Expr.Ref { id = binding; type' = Expr.type' value } in
          let unpackers = List.map unpackersRaw ~f:(fun unpacker -> unpacker ref) in
          Expr.{ binding; value }, unpackers))
      |> List.filter_opt
      |> ReduceTupleState.all
      >>| List.unzip
    in
    let%map () =
      ReduceTupleState.updateCaches ~f:(fun caches ->
        args
        |> List.map ~f:(fun arg -> arg.binding)
        |> List.fold ~init:caches ~f:Map.remove)
    in
    let unpackers = List.bind unpackerss ~f:(fun e -> e) in
    Expr.Let { args; body = Let { args = unpackers; body; type' }; type' }
  | LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; consumer
      ; type' = _
      } ->
    let mapRequest, consumerRequest, wrapResult =
      let wrapperForPair block ~mapWrapper ~consumerWrapper =
        let%map blockBinding = ReduceTupleState.createId "loop-block-result" in
        let blockRef = Expr.Ref { id = blockBinding; type' = Expr.type' block } in
        Expr.let'
          ~args:[ { binding = blockBinding; value = block } ]
          ~body:
            (Expr.values
               [ mapWrapper @@ Expr.tupleDeref ~tuple:blockRef ~index:0
               ; consumerWrapper @@ Expr.tupleDeref ~tuple:blockRef ~index:1
               ])
      in
      let wrapperForMapOnly block ~mapWrapper ~consumerWrapper:_ =
        let mapValue = Expr.tupleDeref ~tuple:block ~index:0 in
        let%map mapBinding = ReduceTupleState.createId "map-result" in
        let mapRef = Expr.Ref { id = mapBinding; type' = Expr.type' mapValue } in
        Expr.let'
          ~args:[ { binding = mapBinding; value = mapValue } ]
          ~body:(mapWrapper mapRef)
      in
      let wrapperForConsumerOnly block ~mapWrapper:_ ~consumerWrapper =
        let consumerValue = Expr.tupleDeref ~tuple:block ~index:1 in
        let%map consumerBinding = ReduceTupleState.createId "consumer-result" in
        let consumerRef =
          Expr.Ref { id = consumerBinding; type' = Expr.type' consumerValue }
        in
        Expr.let'
          ~args:[ { binding = consumerBinding; value = consumerValue } ]
          ~body:(consumerWrapper consumerRef)
      in
      let wrapperForUnit _ ~mapWrapper:_ ~consumerWrapper:_ = return @@ Expr.values [] in
      match request with
      | Whole -> TupleRequest.Whole, TupleRequest.Whole, wrapperForPair
      | Element { i = 0; rest = mapRequest } ->
        mapRequest, TupleRequest.Elements [], wrapperForMapOnly
      | Element { i = 1; rest = consumerRequest } ->
        TupleRequest.Elements [], consumerRequest, wrapperForConsumerOnly
      | Element { i = _; rest = _ } ->
        raise (Unreachable.Error "invalid tuple index of loop block")
      | Elements [] -> TupleRequest.Elements [], TupleRequest.Elements [], wrapperForUnit
      | Elements [ { i = 0; rest = mapRequest } ] ->
        mapRequest, TupleRequest.Elements [], wrapperForMapOnly
      | Elements [ { i = 1; rest = consumerRequest } ] ->
        TupleRequest.Elements [], consumerRequest, wrapperForConsumerOnly
      | Elements [ { i = 0; rest = mapRequest }; { i = 1; rest = consumerRequest } ] ->
        mapRequest, consumerRequest, wrapperForPair
      | Elements _ -> raise (Unreachable.Error "invalid tuple indices of loop block")
      | Collection _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"tuple")
    in
    let%bind consumerUsages, consumer =
      match consumer, consumerRequest with
      | _, Elements [] | None, _ -> return (Set.empty (module Identifier), None)
      | Some (Reduce { arg; zero; d; body; itemPad; associative; character; type' }), _ ->
        let%bind zero =
          zero |> Option.map ~f:(reduceTuples Whole) |> ReduceTupleState.all_opt
        in
        let rec getUsagesInProductionTuple = function
          | Expr.ProductionTuple t ->
            t.elements
            |> List.map ~f:getUsagesInProductionTuple
            |> Set.union_list (module Identifier)
          | Expr.ProductionTupleAtom p -> Set.singleton (module Identifier) p.productionId
        in
        let usages = getUsagesInProductionTuple arg.production in
        let%bind body = reduceTuples Whole body in
        let%bind caches = ReduceTupleState.getCaches () in
        let unpackers =
          [ arg.firstBinding; arg.secondBinding ]
          |> List.map ~f:(fun binding ->
            Map.find caches binding
            |> Option.map ~f:(fun cache ->
              let unpackersRaw = createUnpackersFromCache cache [] ~insideWhole:true in
              let ref =
                Expr.Ref { id = binding; type' = Expr.productionTupleType arg.production }
              in
              let unpackers = List.map unpackersRaw ~f:(fun unpacker -> unpacker ref) in
              unpackers))
          |> List.filter_opt
          |> List.concat
        in
        let%map () =
          ReduceTupleState.setCaches
            (List.fold [ arg.firstBinding; arg.secondBinding ] ~init:caches ~f:Map.remove)
        in
        let reduce =
          Expr.Reduce
            { arg
            ; zero
            ; body = Expr.let' ~args:unpackers ~body
            ; d
            ; itemPad
            ; associative
            ; character
            ; type'
            }
        in
        usages, Some reduce
      | Some (Fold { zeroArg; arrayArgs; body; d; itemPad; character; type' }), _ ->
        let%bind body = reduceTuples Whole body in
        let%bind caches = ReduceTupleState.getCaches () in
        let bindings =
          (zeroArg.zeroBinding, Expr.type' zeroArg.zeroValue)
          :: List.map arrayArgs ~f:(fun { binding; production } ->
            let argType =
              match production.type' with
              | Array { element; size = _ } -> element
              | _ -> raise @@ Unreachable.Error "expected array type"
            in
            binding, argType)
        in
        let unpackers =
          bindings
          |> List.map ~f:(fun (binding, type') ->
            Map.find caches binding
            |> Option.map ~f:(fun cache ->
              let unpackersRaw = createUnpackersFromCache cache [] ~insideWhole:true in
              let ref = Expr.Ref { id = binding; type' } in
              let unpackers = List.map unpackersRaw ~f:(fun unpacker -> unpacker ref) in
              unpackers))
          |> List.filter_opt
          |> List.concat
        in
        let%map () =
          ReduceTupleState.updateCaches ~f:(fun caches ->
            bindings
            |> List.map ~f:(fun (binding, _) -> binding)
            |> List.fold ~init:caches ~f:Map.remove)
        in
        let fold =
          Expr.Fold
            { zeroArg
            ; arrayArgs
            ; body = Expr.let' ~args:unpackers ~body
            ; d
            ; itemPad
            ; character
            ; type'
            }
        in
        let usages =
          arrayArgs
          |> List.map ~f:(fun arg -> arg.production.productionId)
          |> Set.of_list (module Identifier)
        in
        usages, Some fold
      | ( (Some (Scatter { valuesArg; indicesArg; dIn = _; dOut = _; type' = _ }) as
           scatter)
        , _ ) ->
        let usages =
          Set.of_list
            (module Identifier)
            [ valuesArg.productionId; indicesArg.productionId ]
        in
        return (usages, scatter)
    in
    let consumerWrapper e = e in
    let mapBodyRequest, mapWrapper, mapBodyMatcher, mapResults =
      let resultIdsAndRequests, mapWrapper =
        let rec extractFromConstExpr nestCount typeWrapper request constExpr =
          match request with
          | TupleRequest.Whole -> constExpr
          | TupleRequest.Element { i; rest } ->
            Expr.tupleDeref
              ~tuple:(Expr.unzip (extractFromConstExpr 0 (fun t -> t) rest constExpr))
              ~index:i
          | TupleRequest.Elements derefs ->
            let values =
              List.map derefs ~f:(fun { i; rest } ->
                Expr.tupleDeref
                  ~tuple:(Expr.unzip (extractFromConstExpr 0 (fun t -> t) rest constExpr))
                  ~index:i)
            in
            let types =
              List.map values ~f:(fun value ->
                let rec getType n type' =
                  if n = 0
                  then type'
                  else (
                    match type' with
                    | Type.Array array -> getType (n - 1) array.element
                    | Type.Sigma sigma -> getType (n - 1) sigma.body
                    | _ -> raise @@ Unreachable.Error "expected collection type")
                in
                getType nestCount @@ Expr.type' value)
            in
            Expr.Zip
              { zipArg = Expr.values values
              ; nestCount
              ; type' = typeWrapper @@ Type.Tuple types
              }
          | TupleRequest.Collection { subRequest; collectionType } ->
            extractFromConstExpr
              (nestCount + 1)
              (fun t ->
                let innerType =
                  match collectionType with
                  | Array size -> Type.Array { element = t; size }
                  | Sigma parameters -> Type.Sigma { parameters; body = t }
                in
                typeWrapper innerType)
              subRequest
              constExpr
        in
        let getForDeref TupleRequest.{ i; rest } =
          let subRequest =
            match rest with
            | Whole -> TupleRequest.Whole
            | Collection { subRequest; collectionType = _ } -> subRequest
            | _ -> raise @@ TupleRequest.unexpected ~actual:rest ~expected:"collection"
          in
          let resultId = List.nth_exn mapResults i in
          if Set.mem consumerUsages resultId
          then (resultId, TupleRequest.Whole), extractFromConstExpr 0 (fun t -> t) rest
          else (resultId, subRequest), fun e -> e
        in
        match mapRequest with
        | Whole ->
          ( List.map mapResults ~f:(fun resultId -> resultId, TupleRequest.Whole)
          , fun e -> e )
        | Element deref ->
          let resultIdAndRequest, extractor = getForDeref deref in
          let mapWrapper mapResult =
            extractor @@ Expr.tupleDeref ~tuple:mapResult ~index:0
          in
          [ resultIdAndRequest ], mapWrapper
        | Elements derefs ->
          let resultIdsAndRequest, extractors =
            derefs |> List.map ~f:getForDeref |> List.unzip
          in
          let mapWrapper mapResult =
            extractors
            |> List.mapi ~f:(fun i extractor ->
              extractor @@ Expr.tupleDeref ~tuple:mapResult ~index:i)
            |> Expr.values
          in
          resultIdsAndRequest, mapWrapper
        | Collection _ ->
          raise @@ TupleRequest.unexpected ~actual:mapRequest ~expected:"tuple"
      in
      let mapResults, _ = List.unzip resultIdsAndRequests in
      let resultRequestsFromMap =
        List.map resultIdsAndRequests ~f:(fun (resultId, request) -> resultId, request)
      in
      let resultRequestsFromConsumer =
        consumerUsages
        |> Set.to_list
        |> List.map ~f:(fun resultId -> resultId, TupleRequest.Whole)
      in
      let resultRequests =
        Map.of_alist_reduce
          (module Identifier)
          (resultRequestsFromMap @ resultRequestsFromConsumer)
          ~f:(fun a _ -> a)
      in
      let rec makeMapBodyRequestAndMatcher (oldMatcher : Expr.tupleMatch)
        : TupleRequest.t * Expr.tupleMatch * bool
        =
        match oldMatcher with
        | Binding resultId ->
          (match Map.find resultRequests resultId with
           | Some request -> request, Binding resultId, true
           | None -> Whole, Binding resultId, false)
        | Unpack matchers ->
          let requestsAndMatchers =
            List.filter_mapi matchers ~f:(fun i matcher ->
              let request, subMatcher, used = makeMapBodyRequestAndMatcher matcher in
              if used then Some (i, request, subMatcher) else None)
          in
          (match requestsAndMatchers with
           | [] -> Elements [], Unpack [], false
           | [ (i, request, matcher) ] -> Element { i; rest = request }, matcher, true
           | _ ->
             let elementsRequests, matchers =
               requestsAndMatchers
               |> List.map ~f:(fun (i, request, matcher) ->
                 TupleRequest.{ i; rest = request }, matcher)
               |> List.unzip
             in
             Elements elementsRequests, Unpack matchers, true)
      in
      let mapBodyRequest, mapBodyMatcher, _ =
        makeMapBodyRequestAndMatcher mapBodyMatcher
      in
      mapBodyRequest, mapWrapper, mapBodyMatcher, mapResults
    in
    let%bind mapArgs, mapBody, blockUnpackers =
      let%bind body = reduceTuples mapBodyRequest mapBody in
      let%bind caches = ReduceTupleState.getCaches () in
      let%bind args, argUnpackerss, blockUnpackers =
        mapArgs
        |> List.map ~f:(fun { binding; ref } ->
          Map.find caches binding
          |> Option.map ~f:(fun cache ->
            let argArrayType =
              match ref.type' with
              | Array array -> array
              | _ -> raise @@ Unreachable.Error "expected array type"
            in
            let unpackersRaw = createUnpackersFromCache cache [] ~insideWhole:false in
            let argRef = Expr.Ref { id = binding; type' = argArrayType.element } in
            let unpackers = List.map unpackersRaw ~f:(fun unpacker -> unpacker argRef) in
            let argRequest =
              TupleRequest.Collection
                { subRequest = createRequestFromCache cache
                ; collectionType = Array argArrayType.size
                }
            in
            let%map value = reduceTuples argRequest (Expr.Ref ref)
            and valueBinding = ReduceTupleState.createId (Identifier.name ref.id) in
            let valueRef : Expr.ref = { id = valueBinding; type' = Expr.type' value } in
            let valueUnpacker : Expr.letArg = { binding = valueBinding; value } in
            Expr.{ binding; ref = valueRef }, unpackers, valueUnpacker))
        |> List.filter_opt
        |> ReduceTupleState.all
        >>| List.unzip3
      in
      let argUnpackers = List.concat argUnpackerss in
      let iotaUnpackers =
        mapIotas
        |> List.map ~f:(fun { iota; nestIn = _ } ->
          Map.find caches iota
          |> Option.map ~f:(fun cache ->
            createUnpackersFromCache cache [] ~insideWhole:false
            |> List.map ~f:(fun unpacker ->
              unpacker (Ref { id = iota; type' = Literal IntLiteral }))))
        |> List.filter_opt
        |> List.concat
      in
      let unpackers = argUnpackers @ iotaUnpackers in
      (* Remove the caches for variables bound only in the map *)
      let%map () =
        ReduceTupleState.updateCaches ~f:(fun caches ->
          List.map mapArgs ~f:(fun arg -> arg.binding)
          @ List.map mapIotas ~f:(fun i -> i.iota)
          |> List.fold ~init:caches ~f:Map.remove)
      in
      args, Expr.let' ~args:unpackers ~body, blockUnpackers
    in
    let rec extractTypesFromTupleType (matcher : Expr.tupleMatch) type' =
      match matcher with
      | Binding id -> [ id, type' ]
      | Unpack matchers ->
        (match type' with
         | Type.Tuple types ->
           List.zip_exn matchers types
           |> List.bind ~f:(fun (matcher, type') ->
             extractTypesFromTupleType matcher type')
         | _ -> raise @@ Unreachable.Error "expected tuple type")
    in
    let resultElementTypes =
      extractTypesFromTupleType mapBodyMatcher (Expr.type' mapBody)
    in
    let resultTypes =
      resultElementTypes
      |> List.map ~f:(fun (id, element) -> id, Type.Array { element; size = frameShape })
      |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
    in
    let mapResultType =
      Type.Tuple
        (List.map mapResults ~f:(fun resultId -> Map.find_exn resultTypes resultId))
    in
    let block =
      Expr.let'
        ~args:blockUnpackers
        ~body:
          (Expr.LoopBlock
             { frameShape
             ; mapArgs
             ; mapIotas
             ; mapBody
             ; mapBodyMatcher
             ; mapResults
             ; consumer
             ; type' =
                 [ mapResultType
                 ; consumer
                   |> Option.map ~f:Expr.consumerOpType
                   |> Option.value ~default:(Type.Tuple [])
                 ]
             })
    in
    wrapResult block ~mapWrapper ~consumerWrapper
;;

let simplify expr =
  let open State.Let_syntax in
  let rec loop expr =
    let unoptimized = expr in
    let optimized = optimize unoptimized in
    if Expr.equal unoptimized optimized
    then (
      let%bind reduced, droppedAny =
        reduceTuples Whole optimized |> ReduceTupleState.toSimplifyState
      in
      if droppedAny then loop reduced else return optimized)
    else loop optimized
  in
  loop expr
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nested.t
  type output = Nested.t
  type error = (SB.source option, string) Source.annotate

  let name = "Simplify Nested"

  let run input =
    CompilerPipeline.S.make ~f:(fun state -> State.run (simplify input) state)
  ;;
end
