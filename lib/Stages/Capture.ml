open! Base
open Acorn

module Captures = struct
  type t = Expr.captures

  let empty =
    Expr.
      { exprCaptures = Set.empty (module Identifier)
      ; indexCaptures = Set.empty (module Identifier)
      }
  ;;

  let merge (a : t) (b : t) =
    Expr.
      { exprCaptures = Set.union a.exprCaptures b.exprCaptures
      ; indexCaptures = Set.union a.indexCaptures b.indexCaptures
      }
  ;;

  let diff (a : t) b =
    Expr.
      { exprCaptures = Set.diff a.exprCaptures b
      ; indexCaptures = Set.diff a.indexCaptures b
      }
  ;;

  let merge_list = List.fold ~init:empty ~f:merge
  let getList l ~f = List.map l ~f |> merge_list
  let getOpt l ~f = Option.map l ~f |> Option.value ~default:empty
  let ( + ) = merge
  let ( - ) = diff

  let of_expr id =
    Expr.
      { exprCaptures = Set.singleton (module Identifier) id
      ; indexCaptures = Set.empty (module Identifier)
      }
  ;;

  let of_index index =
    Expr.
      { exprCaptures = Set.empty (module Identifier)
      ; indexCaptures = Set.singleton (module Identifier) index
      }
  ;;

  let getInDim Index.{ const = _; refs } = refs |> Map.keys |> getList ~f:of_index

  let getInShapeElement = function
    | Index.Add dim -> getInDim dim
    | Index.ShapeRef ref -> of_index ref
  ;;

  let getInShape = getList ~f:getInShapeElement

  let getInIndex = function
    | Index.Shape shape -> getInShape shape
    | Index.Dimension dim -> getInDim dim
  ;;

  let rec getInExpr : (Expr.device, unit) Acorn.Expr.t -> t = function
    | Ref { id; type' = _ } -> of_expr id
    | BoxValue { box; type' = _ } -> getInExpr box
    | IndexLet { indexArgs; body; type' = _ } ->
      let argCaptures =
        getList indexArgs ~f:(fun arg ->
          match arg.indexValue with
          | Runtime e -> getInExpr e
          | FromBox { box; i = _ } -> getInExpr box)
      in
      let bindings =
        indexArgs
        |> List.map ~f:(fun arg -> arg.indexBinding)
        |> Set.of_list (module Identifier)
      in
      let bodyCaptures = getInExpr body - bindings in
      argCaptures + bodyCaptures
    | MemLet { memArgs = _; body } -> getInExpr body
    | Let { args; body } ->
      let argCaptures = getList args ~f:(fun arg -> getInExpr arg.value) in
      let bindings =
        args |> List.map ~f:(fun arg -> arg.binding) |> Set.of_list (module Identifier)
      in
      let bodyCaptures = getInExpr body - bindings in
      argCaptures + bodyCaptures
    | LoopBlock
        { frameShape
        ; mapArgs
        ; mapIotas
        ; mapMemArgs = _
        ; mapBody
        ; mapBodyMatcher = _
        ; mapResults = _
        ; mapResultMem = _
        ; consumer
        ; type' = _
        } ->
      let iotaCaptures =
        mapIotas
        |> List.map ~f:(fun iota -> iota.nestIn)
        |> List.filter_opt
        |> getList ~f:of_expr
      in
      let bodyBindings =
        List.map mapArgs ~f:(fun arg -> arg.binding)
        @ List.map mapIotas ~f:(fun iota -> iota.iota)
        |> Set.of_list (module Identifier)
      in
      let bodyCaptures = getInExpr mapBody - bodyBindings in
      let consumerCaptures =
        match consumer with
        | None -> empty
        | Some
            (ReduceSeq
              { arg; zero; mappedMemArgs = _; body; d; itemPad; character = _; type' = _ })
          ->
          let zeroCaptures = getOpt zero ~f:getInExpr in
          let argBindings =
            Set.of_list (module Identifier) [ arg.firstBinding; arg.secondBinding ]
          in
          let bodyCaptures = getInExpr body - argBindings in
          zeroCaptures + bodyCaptures + getInDim d + getInShape itemPad
        | Some
            (Fold
              { zeroArg
              ; arrayArgs
              ; mappedMemArgs = _
              ; body
              ; d
              ; itemPad
              ; character = _
              ; type' = _
              }) ->
          let zeroCaptures = getInExpr zeroArg.zeroValue in
          let argBindings =
            zeroArg.zeroBinding :: List.map arrayArgs ~f:(fun arg -> arg.binding)
            |> Set.of_list (module Identifier)
          in
          let bodyCaptures = getInExpr body - argBindings in
          zeroCaptures + bodyCaptures + getInDim d + getInShape itemPad
        | Some (Scatter { valuesArg = _; indicesArg = _; dIn; dOut; mem = _; type' = _ })
          -> getInDim dIn + getInDim dOut
      in
      getInShapeElement frameShape + iotaCaptures + bodyCaptures + consumerCaptures
    | Box { indices; body; bodyType = _; type' = _ } ->
      getList indices ~f:getInIndex + getInExpr body
    | Values { elements; type' = _ } -> getList elements ~f:getInExpr
    | ScalarPrimitive { op = _; args; type' = _ } -> getList args ~f:getInExpr
    | TupleDeref { tuple; index = _; type' = _ } -> getInExpr tuple
    | SubArray { arrayArg; indexArg; type' = _ } ->
      getInExpr arrayArg + getInExpr indexArg
    | Eseq { statement; expr; type' = _ } -> getInStatement statement + getInExpr expr
    | ReifyDimensionIndex { dimIndex } -> getInDim dimIndex
    | Literal _ -> empty
    | Getmem { addr = _; type' = _ } -> empty

  and getInStatement : (Expr.device, unit) Acorn.Expr.statement -> t = function
    | Putmem { addr = _; expr; type' = _ } -> getInExpr expr
    | ComputeForSideEffects expr -> getInExpr expr
    | Statements statements -> getList statements ~f:getInStatement
    | SLet { args; body } ->
      getList args ~f:(fun arg -> getInExpr arg.value) + getInStatement body
    | SMemLet { memArgs = _; body } -> getInStatement body
    | ReifyShapeIndexToArray { shapeIndex; mem = _ } -> getInShape shapeIndex
    | ReifyShapeIndexToBox { shapeIndex; mem = _ } -> getInShape shapeIndex
  ;;
end

let rec annotateExpr : type l. l Expr.sansCaptures -> l Expr.withCaptures = function
  | BoxValue { box; type' } -> BoxValue { box = annotateExpr box; type' }
  | IndexLet { indexArgs; body; type' } ->
    IndexLet
      { indexArgs =
          List.map indexArgs ~f:(function
            | { indexBinding; indexValue = Runtime v; sort } ->
              Expr.{ indexBinding; indexValue = Runtime (annotateExpr v); sort }
            | { indexBinding; indexValue = FromBox { box; i }; sort } ->
              Expr.
                { indexBinding; indexValue = FromBox { box = annotateExpr box; i }; sort })
      ; body = annotateExpr body
      ; type'
      }
  | MemLet { memArgs; body } -> MemLet { memArgs; body = annotateExpr body }
  | Let { args; body } ->
    Let { args = List.map args ~f:annotateArg; body = annotateExpr body }
  | LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapMemArgs
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; mapResultMem
      ; consumer
      ; type'
      } ->
    let consumer =
      Option.map consumer ~f:(function
        | ReduceSeq { arg; zero; mappedMemArgs; body; d; itemPad; character; type' } ->
          Expr.ReduceSeq
            { arg
            ; zero = Option.map zero ~f:annotateExpr
            ; mappedMemArgs
            ; body = annotateExpr body
            ; d
            ; itemPad
            ; character
            ; type'
            }
        | Fold { zeroArg; arrayArgs; mappedMemArgs; body; d; itemPad; character; type' }
          ->
          Expr.Fold
            { zeroArg = { zeroArg with zeroValue = annotateExpr zeroArg.zeroValue }
            ; arrayArgs
            ; mappedMemArgs
            ; body = annotateExpr body
            ; d
            ; itemPad
            ; character
            ; type'
            }
        | Scatter { valuesArg; indicesArg; dIn; dOut; mem; type' } ->
          Expr.Scatter { valuesArg; indicesArg; dIn; dOut; mem; type' })
    in
    LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapMemArgs
      ; mapBody = annotateExpr mapBody
      ; mapBodyMatcher
      ; mapResults
      ; mapResultMem
      ; consumer
      ; type'
      }
  | LoopKernel
      { kernel =
          { frameShape
          ; mapArgs
          ; mapIotas
          ; mapMemArgs
          ; mapBody
          ; mapBodyMatcher
          ; mapResults
          ; mapResultMem
          ; consumer
          ; type'
          }
      ; captures = ()
      } ->
    let mapBodyBindings =
      List.map mapArgs ~f:(fun arg -> arg.binding)
      @ List.map mapIotas ~f:(fun iota -> iota.iota)
      |> Set.of_list (module Identifier)
    in
    let mapBodyCaptures = Captures.(getInExpr mapBody - mapBodyBindings) in
    let consumerCaptures, consumer =
      match consumer with
      | None -> Captures.empty, None
      | Some (ReducePar { arg; zero; mappedMemArgs; body; d; itemPad; character; type' })
        ->
        let bindings =
          Set.of_list (module Identifier) [ arg.firstBinding; arg.secondBinding ]
        in
        ( Captures.(getInExpr body - bindings)
        , Some
            (Expr.ReducePar
               { arg
               ; zero = Option.map zero ~f:annotateExpr
               ; mappedMemArgs
               ; body = annotateExpr body
               ; d
               ; itemPad
               ; character
               ; type'
               }) )
      | Some (Scatter { valuesArg; indicesArg; dIn; dOut; mem; type' }) ->
        ( Captures.empty
        , Some (Expr.Scatter { valuesArg; indicesArg; dIn; dOut; mem; type' }) )
    in
    LoopKernel
      { kernel =
          { frameShape
          ; mapArgs
          ; mapIotas
          ; mapMemArgs
          ; mapBody = annotateExpr mapBody
          ; mapBodyMatcher
          ; mapResults
          ; mapResultMem
          ; consumer
          ; type'
          }
      ; captures = Captures.(mapBodyCaptures + consumerCaptures)
      }
  | Box { indices; body; bodyType; type' } ->
    Box { indices; body = annotateExpr body; bodyType; type' }
  | Values { elements; type' } ->
    Values { elements = List.map elements ~f:annotateExpr; type' }
  | ScalarPrimitive { op; args; type' } ->
    ScalarPrimitive { op; args = List.map args ~f:annotateExpr; type' }
  | TupleDeref { tuple; index; type' } ->
    TupleDeref { tuple = annotateExpr tuple; index; type' }
  | SubArray { arrayArg; indexArg; type' } ->
    SubArray { arrayArg = annotateExpr arrayArg; indexArg = annotateExpr indexArg; type' }
  | IfParallelismHitsCutoff { parallelism; cutoff; then'; else'; type' } ->
    IfParallelismHitsCutoff
      { parallelism
      ; cutoff
      ; then' = annotateExpr then'
      ; else' = annotateExpr else'
      ; type'
      }
  | Eseq { statement; expr; type' } ->
    Eseq { statement = annotateStatement statement; expr = annotateExpr expr; type' }
  | (Ref _ | ReifyDimensionIndex _ | Literal _ | Getmem _) as expr -> expr

and annotateStatement
  : type l. l Expr.statementSansCaptures -> l Expr.statementWithCaptures
  = function
  | Putmem { addr; expr; type' } -> Putmem { addr; expr = annotateExpr expr; type' }
  | MapKernel { kernel; captures = () } ->
    let rec annotateMapKernel
      Expr.
        { frameShape
        ; mapArgs
        ; mapIotas
        ; mapMemArgs
        ; mapBody
        ; mapBodyMatcher
        ; mapResults
        ; mapResultMem
        ; type'
        }
      : Expr.captures * Expr.captures Expr.mapKernel
      =
      let mapBodyBindings =
        List.map mapArgs ~f:(fun arg -> arg.binding)
        @ List.map mapIotas ~f:(fun iota -> iota.iota)
        |> Set.of_list (module Identifier)
      in
      let subCaptures, subMaps =
        mapBody.subMaps |> List.map ~f:annotateMapKernel |> List.unzip
      in
      let bodyCaptures = Captures.getInStatement mapBody.statement in
      let captures = Captures.(merge_list subCaptures + bodyCaptures - mapBodyBindings) in
      ( captures
      , { frameShape
        ; mapArgs
        ; mapIotas
        ; mapMemArgs
        ; mapBody = { statement = annotateStatement mapBody.statement; subMaps }
        ; mapBodyMatcher
        ; mapResults
        ; mapResultMem
        ; type'
        } )
    in
    let captures, kernel = annotateMapKernel kernel in
    MapKernel { kernel; captures }
  | ComputeForSideEffects expr -> ComputeForSideEffects (annotateExpr expr)
  | Statements statements -> Statements (List.map statements ~f:annotateStatement)
  | SLet { args; body } ->
    SLet { args = List.map args ~f:annotateArg; body = annotateStatement body }
  | SMemLet { memArgs; body } -> SMemLet { memArgs; body = annotateStatement body }
  | (ReifyShapeIndexToArray _ | ReifyShapeIndexToBox _) as statement -> statement

and annotateArg : type l. (l, unit) Expr.letArg -> (l, Expr.captures) Expr.letArg =
  fun { binding; value } -> { binding; value = annotateExpr value }
;;

let annotateCaptures = annotateExpr

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = sansCaptures
  type output = withCaptures
  type error = (SB.source option, string) Source.annotate

  let name = "Capture"
  let run input = CompilerPipeline.S.return @@ annotateCaptures input
end
