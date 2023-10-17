open! Base
open Nested

module Counts : sig
  type t [@@deriving sexp_of]

  val empty : t
  val one : Identifier.t -> t
  val get : t -> Identifier.t -> int
  val merge : t list -> t
end = struct
  type t = int Map.M(Identifier).t [@@deriving sexp_of]

  let empty = Map.empty (module Identifier)
  let one id = Map.singleton (module Identifier) id 1
  let get counts id = Map.find counts id |> Option.value ~default:0

  let merge countss =
    List.fold countss ~init:empty ~f:(fun acc more ->
      Map.merge_skewed acc more ~combine:(fun ~key:_ a b -> a + b))
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
  | ConsumerBlock _ -> false
  | ScalarPrimitive _ -> false
  | SubArray { arrayArg; indexArg; type' = _ } ->
    nonComputational arrayArg && nonComputational indexArg
  | Append _ -> false
;;

let getCounts =
  let getCountsIndex : Index.t -> Counts.t = function
    | Shape elements ->
      elements
      |> List.map ~f:(function
        | ShapeRef ref -> Counts.one ref
        | Add { const = _; refs } ->
          refs |> Map.keys |> List.map ~f:Counts.one |> Counts.merge)
      |> Counts.merge
    | Dimension { const = _; refs } ->
      refs |> Map.keys |> List.map ~f:Counts.one |> Counts.merge
  in
  let rec getCountsProductionTuple : Expr.productionTuple -> Counts.t = function
    | ProductionTuple { elements; type' = _ } ->
      elements |> List.map ~f:getCountsProductionTuple |> Counts.merge
    | ProductionTupleAtom atom -> Counts.one atom.productionId
  in
  let rec getCountsExpr : Expr.t -> Counts.t = function
    | Ref { id; type' = _ } -> Counts.one id
    | Frame { elements; dimension = _; type' = _ } ->
      elements |> List.map ~f:getCountsExpr |> Counts.merge
    | BoxValue { box; type' = _ } -> getCountsExpr box
    | IndexLet { indexArgs; body; type' = _ } ->
      let indexValueCounts =
        List.map indexArgs ~f:(fun arg ->
          match arg.indexValue with
          | Runtime value -> getCountsExpr value
          | FromBox { box; i = _ } -> getCountsExpr box)
      and bodyCounts = getCountsExpr body in
      Counts.merge (bodyCounts :: indexValueCounts)
    | ReifyIndex { index; type' = _ } -> getCountsIndex index
    | Let { args; body; type' = _ } ->
      let argCounts = args |> List.map ~f:(fun arg -> getCountsExpr arg.value)
      and bodyCounts = getCountsExpr body in
      Counts.merge (bodyCounts :: argCounts)
    | ConsumerBlock
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
        |> List.map ~f:(fun { binding = _; ref } -> getCountsExpr (Ref ref))
        |> Counts.merge
      and iotaCounts =
        mapIotas
        |> List.map ~f:(fun { iota = _; nestIn } ->
          match nestIn with
          | Some nestIn -> Counts.one nestIn
          | None -> Counts.empty)
        |> Counts.merge
      and bodyCounts = getCountsExpr mapBody
      and frameShapeCounts = getCountsIndex (Shape [ frameShape ])
      and mapResultsCounts = mapResults |> List.map ~f:Counts.one |> Counts.merge
      and consumerCounts =
        match consumer with
        | None -> Counts.empty
        | Some
            (Reduce
              { arg; body; zero; d; itemPad; associative = _; character = _; type' = _ })
          ->
          let argCounts = getCountsProductionTuple arg.production
          and bodyCounts = getCountsExpr body
          and zeroCounts =
            zero |> Option.map ~f:getCountsExpr |> Option.value ~default:Counts.empty
          and dCounts = getCountsIndex (Dimension d)
          and itemPadCounts = getCountsIndex (Shape itemPad) in
          Counts.merge [ argCounts; bodyCounts; zeroCounts; dCounts; itemPadCounts ]
        | Some (Fold { zeroArg; arrayArgs; body; d; itemPad; character = _; type' = _ })
          ->
          let zeroCounts = getCountsExpr zeroArg.zeroValue
          and arrayCounts =
            arrayArgs
            |> List.map ~f:(fun { binding = _; production } ->
              Counts.one production.productionId)
            |> Counts.merge
          and bodyCounts = getCountsExpr body
          and dCounts = getCountsIndex (Dimension d)
          and itemPadCounts = getCountsIndex (Shape itemPad) in
          Counts.merge [ zeroCounts; arrayCounts; bodyCounts; dCounts; itemPadCounts ]
        | Some (Scatter { valuesArg; indicesArg; dIn; dOut; type' = _ }) ->
          Counts.merge
            [ Counts.one valuesArg.productionId
            ; Counts.one indicesArg.productionId
            ; getCountsIndex (Dimension dIn)
            ; getCountsIndex (Dimension dOut)
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
      let indexCounts = List.map indices ~f:getCountsIndex
      and bodyCounts = getCountsExpr body in
      Counts.merge (bodyCounts :: indexCounts)
    | ScalarPrimitive { op = _; args; type' = _ } ->
      args |> List.map ~f:getCountsExpr |> Counts.merge
    | Values { elements; type' = _ } ->
      elements |> List.map ~f:getCountsExpr |> Counts.merge
    | TupleDeref { tuple; index = _; type' = _ } -> getCountsExpr tuple
    | SubArray { arrayArg; indexArg; type' = _ } ->
      Counts.merge [ getCountsExpr arrayArg; getCountsExpr indexArg ]
    | Append { args; type' = _ } -> args |> List.map ~f:getCountsExpr |> Counts.merge
  in
  getCountsExpr
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
  | ConsumerBlock
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
      | None -> None
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
        and zero = Option.map zero ~f:(subExpr subKey subValue) in
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
    ConsumerBlock
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
        if count = 0
        then
          (* No usages, so drop the arg *)
          (* DISCARD!!! *)
          argsAcc, body
        else if count = 1 || nonComputational arg.value
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
  | ConsumerBlock
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
        Counts.get mapBodyCounts arg.binding > 0)
    in
    let mapIotas =
      List.filter mapIotas ~f:(fun iota -> Counts.get mapBodyCounts iota.iota > 0)
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
            Counts.get bodyCounts arg.binding > 0)
        in
        Some (Fold { zeroArg; arrayArgs; body; d; itemPad; character; type' })
      | Some (Scatter { valuesArg = _; indicesArg = _; dIn = _; dOut = _; type' = _ }) as
        scatter -> scatter
    in
    ConsumerBlock
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
    Values { elements; type' }
  | TupleDeref { tuple; index; type' } ->
    let tuple = optimize tuple in
    (match tuple with
     | Values { elements; type' = _ } -> List.nth_exn elements index
     | _ -> TupleDeref { tuple; index; type' })
;;

let simplify expr =
  let open State.Let_syntax in
  let rec loop expr =
    let unoptimized = expr in
    let optimized = optimize unoptimized in
    if Expr.equal unoptimized optimized then return optimized else loop optimized
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
