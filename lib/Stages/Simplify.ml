open! Base
open Nested

module BindingSet = struct
  type t =
    | Bindings of Set.M(Identifier).t
    | All
  [@@deriving sexp_of]

  let union a b =
    match a, b with
    | Bindings a, Bindings b -> Bindings (Set.union a b)
    | All, _ | _, All -> All
  ;;

  let of_list l = Bindings (Set.of_list (module Identifier) l)
end

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
  val usesAny : t -> BindingSet.t -> bool
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

  let usesAny counts bindings =
    match bindings with
    | BindingSet.Bindings bindings ->
      let keys =
        counts
        |> Map.filter ~f:(fun c -> c.count > 0)
        |> Map.keys
        |> Set.of_list (module Identifier)
      in
      not (Set.are_disjoint keys bindings)
    | BindingSet.All -> true
  ;;
end

(* Determine if an expression is a constant expression or if it requires
   computation. i.e. 7 and x are nonComputational,
   while (+ 7 x) is computational *)
let rec nonComputational : Expr.t -> bool = function
  | Ref _ -> true
  | Frame _ -> false
  | BoxValue { box; type' = _ } -> nonComputational box
  | IndexLet _ -> false
  | ReifyIndex _ -> false
  | Box _ -> false
  | Literal (IntLiteral _ | FloatLiteral _ | CharacterLiteral _ | BooleanLiteral _) ->
    true
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
        | Some (Reduce { arg; body; zero; d; associative = _; character = _; type' = _ })
          ->
          let argCounts = getCountsProductionTuple inLoop arg.production
          and bodyCounts = getCountsExpr true body
          and zeroCounts =
            zero
            |> Option.map ~f:(getCountsExpr inLoop)
            |> Option.value ~default:Counts.empty
          and dCounts = getCountsIndex inLoop (Dimension d) in
          Counts.merge [ argCounts; bodyCounts; zeroCounts; dCounts ]
        | Some (Fold { zeroArg; arrayArgs; body; d; character = _; type' = _ }) ->
          let zeroCounts = getCountsExpr inLoop zeroArg.zeroValue
          and arrayCounts =
            arrayArgs
            |> List.map ~f:(fun { binding = _; production } ->
              Counts.one production.productionId ~inLoop)
            |> Counts.merge
          and bodyCounts = getCountsExpr true body
          and dCounts = getCountsIndex inLoop (Dimension d) in
          Counts.merge [ zeroCounts; arrayCounts; bodyCounts; dCounts ]
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
    | Literal (IntLiteral _ | FloatLiteral _ | CharacterLiteral _ | BooleanLiteral _) ->
      Counts.empty
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
      | Some (Reduce { arg; body; zero; d; associative; character; type' }) ->
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
        Some (Reduce { arg; body; zero; d; associative; character; type' })
      | Some (Fold { zeroArg; arrayArgs; body; d; character; type' }) ->
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
        Some (Fold { zeroArg; arrayArgs; body; d; character; type' })
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
  | Literal (IntLiteral _ | FloatLiteral _ | CharacterLiteral _ | BooleanLiteral _) as lit
    -> return lit
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
    (* TODO: if box is just a box, immediately unbox *)
    BoxValue { box; type' }
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
      | Some (Reduce { arg; body; zero; d; associative; character; type' }) ->
        let body = optimize body
        and zero = Option.map zero ~f:optimize in
        Some (Reduce { arg; body; zero; d; associative; character; type' })
      | Some (Fold { zeroArg; arrayArgs; body; d; character; type' }) ->
        let body = optimize body
        and zeroArg = { zeroArg with zeroValue = optimize zeroArg.zeroValue } in
        (* Simplify the zero args, removing unused ones *)
        let bodyCounts = getCounts body in
        let arrayArgs =
          List.filter arrayArgs ~f:(fun arg ->
            (* DISCARD!!! *)
            (Counts.get bodyCounts arg.binding).count > 0)
        in
        Some (Fold { zeroArg; arrayArgs; body; d; character; type' })
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
     | If, [ Literal (BooleanLiteral true); then'; _ ] -> then'
     | If, [ Literal (BooleanLiteral false); _; else' ] -> else'
     | And, [ Literal (BooleanLiteral true); value ]
     | And, [ value; Literal (BooleanLiteral true) ] -> value
     | And, [ Literal (BooleanLiteral false); _ ]
     | And, [ _; Literal (BooleanLiteral false) ] ->
       (* DISCARD!!! *)
       Literal (BooleanLiteral false)
     | Or, [ Literal (BooleanLiteral false); value ]
     | Or, [ value; Literal (BooleanLiteral false) ] -> value
     | Or, [ Literal (BooleanLiteral true); _ ] | Or, [ _; Literal (BooleanLiteral true) ]
       ->
       (* DISCARD!!! *)
       Literal (BooleanLiteral true)
     | Not, [ Literal (BooleanLiteral b) ] -> Literal (BooleanLiteral (not b))
     | Equal, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (BooleanLiteral (a = b))
     | Ne, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (BooleanLiteral (a <> b))
     | Lt, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (BooleanLiteral (a < b))
     | LtEq, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (BooleanLiteral (a <= b))
     | Gt, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (BooleanLiteral (a > b))
     | GtEq, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (BooleanLiteral (a >= b))
     | LtF, [ Literal (FloatLiteral a); Literal (FloatLiteral b) ] ->
       Literal (BooleanLiteral Float.(a < b))
     | LtEqF, [ Literal (FloatLiteral a); Literal (FloatLiteral b) ] ->
       Literal (BooleanLiteral Float.(a <= b))
     | GtF, [ Literal (FloatLiteral a); Literal (FloatLiteral b) ] ->
       Literal (BooleanLiteral Float.(a > b))
     | GtEqF, [ Literal (FloatLiteral a); Literal (FloatLiteral b) ] ->
       Literal (BooleanLiteral Float.(a >= b))
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

type hoisting =
  { variableDeclaration : Expr.letArg
  ; counts : Counts.t
  }
[@@deriving sexp_of]

let hoistDeclarationsMap l ~f =
  let results, hoistings = l |> List.map ~f |> List.unzip in
  results, List.join hoistings
;;

let minDimensionSize (dimension : Index.dimension) = dimension.const

let minShapeElementSize (se : Index.shapeElement) =
  match se with
  | Add d -> minDimensionSize d
  | ShapeRef _ -> 0
;;

(* Hoist variables that can be hoisted. Maps are also cleaned up while doing
   this. (nested maps with empty frames that can be flattened are, and maps
   with empty frames and no args are removed) *)
let rec hoistDeclarations : Expr.t -> Expr.t * hoisting list = function
  | Ref _ as ref -> ref, []
  | Frame { elements; dimension; type' } ->
    let elements, hoistings = hoistDeclarationsMap elements ~f:hoistDeclarations in
    Expr.Frame { elements; dimension; type' }, hoistings
  | BoxValue { box; type' } ->
    let box, hoistings = hoistDeclarations box in
    BoxValue { box; type' }, hoistings
  | IndexLet { indexArgs; body; type' } ->
    let bindings =
      indexArgs |> List.map ~f:(fun arg -> arg.indexBinding) |> BindingSet.of_list
    in
    let indexArgs, indexValueHoistings =
      hoistDeclarationsMap indexArgs ~f:(fun { indexBinding; indexValue; sort } ->
        let indexValue, hoistings =
          match indexValue with
          | Runtime value ->
            let value, hoistings = hoistDeclarations value in
            Expr.Runtime value, hoistings
          | FromBox { box; i } ->
            let box, hoistings = hoistDeclarations box in
            Expr.FromBox { box; i }, hoistings
        in
        Expr.{ indexBinding; indexValue; sort }, hoistings)
    in
    let body, bodyHoistings = hoistDeclarationsInBody body ~bindings in
    Expr.IndexLet { indexArgs; body; type' }, indexValueHoistings @ bodyHoistings
  | ReifyIndex _ as reify -> reify, []
  | Append { args; type' } ->
    let args, hoistings = hoistDeclarationsMap args ~f:hoistDeclarations in
    Append { args; type' }, hoistings
  | Let { args; body; type' = _ } ->
    let body, bodyHoistings = hoistDeclarations body in
    let args, argHoistings =
      hoistDeclarationsMap args ~f:(fun { binding; value } ->
        let value, hoistings = hoistDeclarations value in
        Expr.{ binding; value }, hoistings)
    in
    let argsAsHoistings =
      List.map args ~f:(fun arg ->
        { variableDeclaration = arg; counts = getCounts arg.value })
    in
    body, argHoistings @ argsAsHoistings @ bodyHoistings
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
    let mapBodyBindings =
      if minShapeElementSize frameShape > 0
      then (
        (* The loop will be run at least once, so hoisting out of the loop
           is ok *)
        let argBindings = List.map mapArgs ~f:(fun arg -> arg.binding) in
        let iotaBindings = List.map mapIotas ~f:(fun iota -> iota.iota) in
        argBindings @ iotaBindings |> BindingSet.of_list)
      else
        (* The loop could be run 0 times, so nothing can be hoisted out *)
        BindingSet.All
    in
    let mapBody, mapBodyHoistings =
      hoistDeclarationsInBody mapBody ~bindings:mapBodyBindings
    in
    let consumer, consumerHoistings =
      match consumer with
      | (None | Some (Scatter _)) as consumer -> consumer, []
      | Some (Reduce { arg; body; zero; d; associative; character; type' }) ->
        let bindings =
          if minDimensionSize d >= if Option.is_none zero then 2 else 1
          then BindingSet.of_list [ arg.firstBinding; arg.secondBinding ]
          else
            (* The body of the reduce is not guaranteed to run, so hoisting is unsafe *)
            BindingSet.All
        in
        let body, bodyHoistings = hoistDeclarationsInBody body ~bindings in
        let zero, zeroHoistings =
          match zero with
          | None -> None, []
          | Some zero ->
            let zero, hoistings = hoistDeclarations zero in
            Some zero, hoistings
        in
        ( Some (Expr.Reduce { arg; body; zero; d; associative; character; type' })
        , bodyHoistings @ zeroHoistings )
      | Some (Fold { zeroArg; arrayArgs; body; d; character; type' }) ->
        let bindings =
          if minDimensionSize d >= 1
          then
            zeroArg.zeroBinding :: List.map arrayArgs ~f:(fun arg -> arg.binding)
            |> BindingSet.of_list
          else
            (* The body of the fold is not guaranteed to run, so hoisting is unsafe *)
            BindingSet.All
        in
        let body, bodyHoistings = hoistDeclarationsInBody body ~bindings in
        let zeroArgValue, zeroArgHoistings = hoistDeclarations zeroArg.zeroValue in
        ( Some
            (Fold
               { zeroArg = { zeroBinding = zeroArg.zeroBinding; zeroValue = zeroArgValue }
               ; arrayArgs
               ; body
               ; d
               ; character
               ; type'
               })
        , zeroArgHoistings @ bodyHoistings )
    in
    ( LoopBlock
        { frameShape
        ; mapArgs
        ; mapIotas
        ; mapBody
        ; mapBodyMatcher
        ; mapResults
        ; consumer
        ; type'
        }
    , mapBodyHoistings @ consumerHoistings )
  | SubArray { arrayArg; indexArg; type' } ->
    let arrayArg, arrayHoistings = hoistDeclarations arrayArg
    and indexArg, indexHoistings = hoistDeclarations indexArg in
    SubArray { arrayArg; indexArg; type' }, arrayHoistings @ indexHoistings
  | Box { indices; body; bodyType; type' } ->
    let body, hoistings = hoistDeclarations body in
    Box { indices; body; bodyType; type' }, hoistings
  | Literal _ as literal -> literal, []
  | ScalarPrimitive { op; args; type' } ->
    let args, hoistings = hoistDeclarationsMap args ~f:hoistDeclarations in
    ScalarPrimitive { op; args; type' }, hoistings
  | Values { elements; type' } ->
    let elements, hoistings = hoistDeclarationsMap elements ~f:hoistDeclarations in
    Values { elements; type' }, hoistings
  | TupleDeref { tuple; index; type' } ->
    let tuple, hoistings = hoistDeclarations tuple in
    TupleDeref { tuple; index; type' }, hoistings
  | Zip { zipArg; nestCount; type' } ->
    let zipArg, hoistings = hoistDeclarations zipArg in
    Zip { zipArg; nestCount; type' }, hoistings
  | Unzip { unzipArg; type' } ->
    let unzipArg, hoistings = hoistDeclarations unzipArg in
    Unzip { unzipArg; type' }, hoistings

and hoistDeclarationsInBody body ~bindings : Expr.t * hoisting list =
  (* Simplify the body *)
  let body, bodyHoistings = hoistDeclarations body in
  (* Find which hoistings need to be declared due to a dependency on bindings.
     This can be indirect through another hoisting since hoistings can depend
     on each other. *)
  let findHoistingsToDeclare hoistings =
    let rec loop ~toDeclare ~queue ~barrier =
      let newDelcarations, queue =
        List.partition_map queue ~f:(fun hoisting ->
          if Counts.usesAny hoisting.counts barrier
          then First hoisting
          else Second hoisting)
      in
      match newDelcarations with
      | [] -> toDeclare, queue
      | _ :: _ ->
        let toDeclare = newDelcarations @ toDeclare in
        let barrier =
          List.fold toDeclare ~init:barrier ~f:(fun barrier dec ->
            match barrier with
            | All -> All
            | Bindings bindings ->
              Bindings (Set.add bindings dec.variableDeclaration.binding))
        in
        loop ~toDeclare ~queue ~barrier
    in
    loop ~toDeclare:[] ~queue:hoistings ~barrier:bindings
  in
  let hoistingsToDeclare, hoistingsToPropogate = findHoistingsToDeclare bodyHoistings in
  (* Hoistings in hoistingsToDeclare can depend on each other.
     resolveHoistingDependencies creates a list of lists of the hoistings,
     where each list of hoistings depends only on the lists following it *)
  let resolveHoistingDependencies hoistings =
    let rec loop hoistings undeclared declared =
      match hoistings with
      | [] -> declared
      | _ :: _ as hoistings ->
        (* Hoistings can't depend on each other cyclicly, so there is always
           going to be at least one declarable *)
        let declarable, undeclarable =
          List.partition_map hoistings ~f:(fun h ->
            if Counts.usesAny h.counts (Bindings undeclared) then Second h else First h)
        in
        assert (not (List.is_empty declarable));
        let newUndeclared =
          List.fold declarable ~init:undeclared ~f:(fun undeclared declaration ->
            Set.remove undeclared declaration.variableDeclaration.binding)
        in
        loop undeclarable newUndeclared (declarable :: declared)
    in
    loop
      hoistings
      (hoistings
       |> List.map ~f:(fun h -> h.variableDeclaration.binding)
       |> Set.of_list (module Identifier))
      []
  in
  let hoistingsToDeclare = resolveHoistingDependencies hoistingsToDeclare in
  (* Modify the body to declare the hoistings *)
  let body =
    List.fold hoistingsToDeclare ~init:body ~f:(fun body hoistings ->
      Let
        { args = List.map hoistings ~f:(fun h -> h.variableDeclaration)
        ; body
        ; type' = Expr.type' body
        })
  in
  body, hoistingsToPropogate
;;

module HoistState = struct
  include State

  type state = CompilerState.state
  type ('a, 'e) u = (state, 'a, 'e) t

  let createId name =
    make ~f:(fun state ->
      State.run
        (Identifier.create
           name
           ~getCounter:(fun (s : state) -> s.idCounter)
           ~setCounter:(fun s idCounter -> { s with idCounter }))
        state)
  ;;

  let toSimplifyState (s : ('t, _) u) : (CompilerState.state, 't, _) State.t = s
end

let hoistExpressionsMap l ~f =
  let open HoistState.Let_syntax in
  let%map results, hoistings = l |> List.map ~f |> HoistState.all |> HoistState.unzip in
  results, List.join hoistings
;;

(* Hoist expressions that remain constant over a loop out of the loop.
   loopBarreir is the set of variables that are declared by the loop surrounding
   the current expression. Expressions will be hoisted iff they don't use
   any variables in the loopBarrier AND the expression is "computational".
   (see the function nonComputational) *)
let rec hoistExpressions loopBarrier (expr : Expr.t)
  : (Expr.t * hoisting list, _) HoistState.u
  =
  let open HoistState.Let_syntax in
  let counts = getCounts expr in
  if (not (Counts.usesAny counts loopBarrier)) && not (nonComputational expr)
  then (
    (* This expression can and should be hoisted, so create a variable for it *)
    let%map id = HoistState.createId "hoistedExp" in
    let hoisting = { variableDeclaration = { binding = id; value = expr }; counts } in
    Expr.Ref { id; type' = Expr.type' expr }, [ hoisting ])
  else (
    match expr with
    | Ref _ as ref -> return (ref, [])
    | Frame { elements; dimension; type' } ->
      let%map elements, hoistings =
        hoistExpressionsMap elements ~f:(hoistExpressions loopBarrier)
      in
      Expr.Frame { elements; dimension; type' }, hoistings
    | BoxValue { box; type' } ->
      let%map box, hoistings = hoistExpressions loopBarrier box in
      Expr.BoxValue { box; type' }, hoistings
    | IndexLet { indexArgs; body; type' } ->
      let bindings =
        indexArgs |> List.map ~f:(fun arg -> arg.indexBinding) |> BindingSet.of_list
      in
      let%map indexArgs, indexValueHoistings =
        hoistExpressionsMap indexArgs ~f:(fun { indexBinding; indexValue; sort } ->
          let%map indexValue, hoistings =
            match indexValue with
            | Runtime value ->
              let%map value, hoistings = hoistExpressions loopBarrier value in
              Expr.Runtime value, hoistings
            | FromBox { box; i } ->
              let%map box, hoistings = hoistExpressions loopBarrier box in
              Expr.FromBox { box; i }, hoistings
          in
          Expr.{ indexBinding; indexValue; sort }, hoistings)
      and body, bodyHoistings = hoistExpressionsInBody loopBarrier body ~bindings in
      Expr.IndexLet { indexArgs; body; type' }, indexValueHoistings @ bodyHoistings
    | ReifyIndex _ as reify -> return (reify, [])
    | Append { args; type' } ->
      let%map args, hoistings =
        hoistExpressionsMap args ~f:(hoistExpressions loopBarrier)
      in
      Expr.Append { args; type' }, hoistings
    | Let { args; body; type' } ->
      let%bind args, argHoistings =
        hoistExpressionsMap args ~f:(fun { binding; value } ->
          let%map value, hoistings = hoistExpressions loopBarrier value in
          Expr.{ binding; value }, hoistings)
      in
      let bindings = args |> List.map ~f:(fun arg -> arg.binding) |> BindingSet.of_list in
      let%map body, bodyHoistings = hoistExpressionsInBody loopBarrier body ~bindings in
      Expr.Let { args; body; type' }, argHoistings @ bodyHoistings
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
      let bindings =
        if minShapeElementSize frameShape > 0
        then (
          (* The loop will be run at least once, so hoisting out of the loop
             is ok *)
          let argBindings = List.map mapArgs ~f:(fun arg -> arg.binding) in
          let iotaBindings = List.map mapIotas ~f:(fun iota -> iota.iota) in
          argBindings @ iotaBindings |> BindingSet.of_list)
        else
          (* The loop could be run 0 times, so nothing can be hoisted out *)
          BindingSet.All
      in
      (* update the loop barrier to be the args of the map *)
      let bodyLoopBarrier = bindings in
      let%bind mapBody, mapBodyHoistings =
        hoistExpressionsInBody bodyLoopBarrier mapBody ~bindings
      in
      let%map consumer, consumerHoistings =
        match consumer with
        | (None | Some (Scatter _)) as consumer -> return (consumer, [])
        | Some (Reduce { arg; zero; body; d; associative; character; type' }) ->
          let bindings =
            if minDimensionSize d >= if Option.is_some zero then 2 else 1
            then BindingSet.of_list [ arg.firstBinding; arg.secondBinding ]
            else
              (* The body of the reduce is not guaranteed to run, so hoisting is unsafe *)
              BindingSet.All
          in
          let%map body, bodyHoistings = hoistExpressionsInBody bindings body ~bindings
          and zero, zeroHoistings =
            match zero with
            | None -> return (None, [])
            | Some zero ->
              let%map zero, hoistings = hoistExpressions loopBarrier zero in
              Some zero, hoistings
          in
          ( Some (Expr.Reduce { arg; zero; body; d; associative; character; type' })
          , bodyHoistings @ zeroHoistings )
        | Some (Fold { zeroArg; arrayArgs; body; d; character; type' }) ->
          let%bind zeroArgValue, zeroArgsHoistings =
            hoistExpressions loopBarrier zeroArg.zeroValue
          in
          let bindings =
            if minDimensionSize d >= 1
            then
              zeroArg.zeroBinding :: List.map arrayArgs ~f:(fun arg -> arg.binding)
              |> BindingSet.of_list
            else
              (* The body of the fold is not guaranteed to run, so hoisting is unsafe *)
              BindingSet.All
          in
          let%map body, bodyHoistings = hoistExpressionsInBody bindings body ~bindings in
          ( Some
              (Expr.Fold
                 { zeroArg =
                     { zeroBinding = zeroArg.zeroBinding; zeroValue = zeroArgValue }
                 ; arrayArgs
                 ; body
                 ; d
                 ; character
                 ; type'
                 })
          , zeroArgsHoistings @ bodyHoistings )
      in
      ( Expr.LoopBlock
          { frameShape
          ; mapArgs
          ; mapIotas
          ; mapBody
          ; mapBodyMatcher
          ; mapResults
          ; consumer
          ; type'
          }
      , mapBodyHoistings @ consumerHoistings )
    (*
       | ArrayPrimitive (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' }) ->
        let%map valuesArg, valuesHoistings = hoistExpressions loopBarrier valuesArg
        and indicesArg, indicesHoistings = hoistExpressions loopBarrier indicesArg in
        ( Expr.ArrayPrimitive
            (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' })
        , valuesHoistings @ indicesHoistings ) *)
    | SubArray { arrayArg; indexArg; type' } ->
      let%map arrayArg, arrayHoistings = hoistExpressions loopBarrier arrayArg
      and indexArg, indexHoistings = hoistExpressions loopBarrier indexArg in
      Expr.SubArray { arrayArg; indexArg; type' }, arrayHoistings @ indexHoistings
    | Box { indices; body; bodyType; type' } ->
      let%map body, hoistings = hoistExpressions loopBarrier body in
      Expr.Box { indices; body; bodyType; type' }, hoistings
    | Literal _ as literal -> return (literal, [])
    | ScalarPrimitive { op; args; type' } ->
      let%map args, hoistings =
        hoistExpressionsMap args ~f:(hoistExpressions loopBarrier)
      in
      Expr.ScalarPrimitive { op; args; type' }, hoistings
    | Values { elements; type' } ->
      let%map elements, hoistings =
        hoistExpressionsMap elements ~f:(hoistExpressions loopBarrier)
      in
      Expr.Values { elements; type' }, hoistings
    | TupleDeref { tuple; index; type' } ->
      let%map tuple, hoistings = hoistExpressions loopBarrier tuple in
      Expr.TupleDeref { tuple; index; type' }, hoistings
    | Zip { zipArg; nestCount; type' } ->
      let%map zipArg, hoistings = hoistExpressions loopBarrier zipArg in
      Expr.Zip { zipArg; nestCount; type' }, hoistings
    | Unzip { unzipArg; type' } ->
      let%map unzipArg, hoistings = hoistExpressions loopBarrier unzipArg in
      Expr.Unzip { unzipArg; type' }, hoistings)

and hoistExpressionsInBody loopBarrier body ~bindings =
  let open HoistState.Let_syntax in
  (* Hoist from the body, and then determine which hoistings can be propogated
     outside the body, and which need to be declared in the body *)
  let extendedLoopBarrier = BindingSet.union loopBarrier bindings in
  let%bind body, bodyHoistings = hoistExpressions extendedLoopBarrier body in
  let hoistingsToDeclare, hoistingsToPropogate =
    List.partition_map bodyHoistings ~f:(fun hoisting ->
      if Counts.usesAny hoisting.counts bindings
      then First hoisting.variableDeclaration
      else Second hoisting)
  in
  (* The hoistings' values may have expression of their own that can be hoisted
     beyond this body *)
  let%map hoistingsToDeclare, moreHoistingsToPropogate =
    hoistExpressionsMap hoistingsToDeclare ~f:(fun { binding; value } ->
      let%map value, hoistings = hoistExpressions loopBarrier value in
      Expr.{ binding; value }, hoistings)
  in
  (* Declare the hoistings that need to be declared. *)
  let body =
    match hoistingsToDeclare with
    | [] ->
      (* None to declare *)
      body
    | _ :: _ as hoistingsToDeclare ->
      (* There are some to be declared, so wrap the body in a map *)
      Expr.Let { args = hoistingsToDeclare; body; type' = Expr.type' body }
  in
  body, moreHoistingsToPropogate @ hoistingsToPropogate
;;

(* let simplify expr =
  let open State.Let_syntax in
  let rec loop expr =
    let unoptimized = expr in
    let optimized = optimize unoptimized in
    if Expr.equal unoptimized optimized
    then (
      let%bind { res = reduced; droppedAny } = TupleReduce.reduceTuples optimized in
      if droppedAny then loop reduced else return optimized)
    else loop optimized
  in
  loop expr
;; *)

let simplify expr =
  let open State.Let_syntax in
  let rec loop expr =
    (* Hoist variables that can be hoisted *)
    let decHoisted, hoistings = hoistDeclarationsInBody expr ~bindings:All in
    assert (List.length hoistings = 0);
    (* Perform standard optimizations *)
    let optimized = optimize decHoisted in
    (* Re-hoist and re-optimize if anything changed *)
    if Expr.equal decHoisted optimized
    then (
      (* Hoist expressions that can be hoisted *)
      let%bind exprHoisted, hoistings =
        hoistExpressionsInBody All optimized ~bindings:All |> HoistState.toSimplifyState
      in
      assert (List.length hoistings = 0);
      (* Reduce tuples (remove unused elements) *)
      let%bind { res = reduced; droppedAny } = TupleReduce.reduceTuples exprHoisted in
      (* If reducing tuples did anything, loop. Otherwise, return *)
      if droppedAny then loop reduced else return exprHoisted)
    else loop optimized
  in
  loop expr
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nested.t
  type output = Nested.t
  type error = (SB.source option, string) Source.annotate

  let name = "Simplify"

  let run input =
    CompilerPipeline.S.make ~f:(fun state -> State.run (simplify input) state)
  ;;
end
