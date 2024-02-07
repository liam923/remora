open! Base
open Acorn

module Captures = struct
  type t = Expr.captures

  let empty =
    Expr.
      { exprCaptures = Map.empty (module Identifier)
      ; indexCaptures = Map.empty (module Identifier)
      ; memCaptures = Map.empty (module Identifier)
      }
  ;;

  let merge (a : t) (b : t) =
    Expr.
      { exprCaptures =
          Map.merge_skewed a.exprCaptures b.exprCaptures ~combine:(fun ~key:_ a _ -> a)
      ; memCaptures =
          Map.merge_skewed a.memCaptures b.memCaptures ~combine:(fun ~key:_ a _ -> a)
      ; indexCaptures =
          Map.merge_skewed a.indexCaptures b.indexCaptures ~combine:(fun ~key:_ a _ -> a)
      }
  ;;

  let diff (a : t) b =
    Expr.
      { exprCaptures = Set.fold b ~init:a.exprCaptures ~f:Map.remove
      ; memCaptures = Set.fold b ~init:a.memCaptures ~f:Map.remove
      ; indexCaptures = Set.fold b ~init:a.indexCaptures ~f:Map.remove
      }
  ;;

  let merge_list = List.fold ~init:empty ~f:merge
  let getList l ~f = List.map l ~f |> merge_list
  let getOpt l ~f = Option.map l ~f |> Option.value ~default:empty
  let ( + ) = merge
  let ( - ) = diff

  let of_ref (ref : Expr.ref) =
    Expr.
      { exprCaptures = Map.singleton (module Identifier) ref.id ref.type'
      ; memCaptures = Map.empty (module Identifier)
      ; indexCaptures = Map.empty (module Identifier)
      }
  ;;

  let of_memRef (ref : Mem.ref) =
    Expr.
      { exprCaptures = Map.empty (module Identifier)
      ; memCaptures = Map.singleton (module Identifier) ref.id ref.type'
      ; indexCaptures = Map.empty (module Identifier)
      }
  ;;

  let of_index sort index =
    Expr.
      { exprCaptures = Map.empty (module Identifier)
      ; memCaptures = Map.empty (module Identifier)
      ; indexCaptures = Map.singleton (module Identifier) index sort
      }
  ;;

  let getInDim Index.{ const = _; refs; lens } =
    (refs |> Map.keys |> getList ~f:(of_index Sort.Dim))
    + (lens |> Map.keys |> getList ~f:(of_index Sort.Shape))
  ;;

  let getInShapeElement = function
    | Index.Add dim -> getInDim dim
    | Index.ShapeRef ref -> of_index Sort.Shape ref
  ;;

  let getInShape = getList ~f:getInShapeElement

  let getInIndex = function
    | Index.Dimension dim -> getInDim dim
    | Index.Shape shape -> getInShape shape
  ;;

  let rec getInType = function
    | Type.Atom (Literal _) -> empty
    | Type.Array { element; shape } ->
      getInType (Atom element) + getInShape (NeList.to_list shape)
    | Type.Atom (Sigma { parameters; body }) ->
      let bindings =
        parameters
        |> List.map ~f:(fun param -> param.binding)
        |> Set.of_list (module Identifier)
      in
      getInType body - bindings
    | Type.Tuple elements -> getList elements ~f:getInType
  ;;

  let rec getInMem : Acorn.Mem.t -> t = function
    | Ref ref -> of_memRef ref
    | TupleDeref { tuple; index = _; type' = _ } -> getInMem tuple
    | Values { elements; type' = _ } -> getList elements ~f:getInMem
    | Index { mem; offset; type' } -> getInMem mem + getInDim offset + getInType type'
  ;;

  let rec getInExpr : (device, unit) Acorn.Expr.t -> t = function
    | Ref ref -> of_ref ref
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
    | MallocLet { memArgs; body } ->
      let argCaptures = getList memArgs ~f:(fun arg -> getInType arg.memType) in
      let bindings =
        memArgs
        |> List.map ~f:(fun arg -> arg.memBinding)
        |> Set.of_list (module Identifier)
      in
      let bodyCaptures = getInExpr body - bindings in
      argCaptures + bodyCaptures
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
        ; mapMemArgs
        ; mapBody
        ; mapBodyMatcher = _
        ; mapResults = _
        ; mapResultMemFinal
        ; consumer
        ; type' = _
        } ->
      let iotaCaptures =
        mapIotas
        |> List.map ~f:(fun iota -> iota.nestIn)
        |> List.filter_opt
        |> getList ~f:(fun id -> of_ref { id; type' = Atom (Literal IntLiteral) })
      in
      let bodyBindings =
        List.map mapArgs ~f:(fun arg -> arg.binding)
        @ List.map mapMemArgs ~f:(fun arg -> arg.memBinding)
        @ List.map mapIotas ~f:(fun iota -> iota.iota)
        |> Set.of_list (module Identifier)
      in
      let mapArgCaptures = getList mapArgs ~f:(fun arg -> of_ref arg.ref) in
      let mapMemArgCaptures = getList mapMemArgs ~f:(fun arg -> getInMem arg.mem) in
      let bodyCaptures = getInExpr mapBody - bodyBindings in
      let consumerCaptures =
        match consumer with
        | Nothing -> empty
        | Just (ReduceSeq { arg; zero; body; d; type' = _ }) ->
          let zeroCaptures = getInExpr zero in
          let argBindings =
            Set.of_list (module Identifier) [ arg.firstBinding; arg.secondBinding ]
          in
          let bodyCaptures = getInExpr body - argBindings in
          zeroCaptures + bodyCaptures + getInDim d
        | Just (ScanSeq { arg; zero; body; d; scanResultMemFinal; type' = _ }) ->
          let zeroCaptures = getInExpr zero in
          let argBindings =
            Set.of_list (module Identifier) [ arg.firstBinding; arg.secondBinding ]
          in
          let bodyCaptures = getInExpr body - argBindings in
          zeroCaptures + bodyCaptures + getInDim d + getInMem scanResultMemFinal
        | Just
            (Fold
              { zeroArg
              ; arrayArgs
              ; mappedMemArgs = _
              ; body
              ; reverse = _
              ; d
              ; character = _
              ; type' = _
              }) ->
          let zeroCaptures = getInExpr zeroArg.zeroValue in
          let argBindings =
            zeroArg.zeroBinding :: List.map arrayArgs ~f:(fun arg -> arg.binding)
            |> Set.of_list (module Identifier)
          in
          let bodyCaptures = getInExpr body - argBindings in
          zeroCaptures + bodyCaptures + getInDim d
        | Just
            (Scatter
              { valuesArg = _
              ; indicesArg = _
              ; dIn
              ; dOut
              ; memInterim = _
              ; memFinal = _
              ; type' = _
              }) -> getInDim dIn + getInDim dOut
      in
      getInShapeElement frameShape
      + mapArgCaptures
      + mapMemArgCaptures
      + iotaCaptures
      + bodyCaptures
      + consumerCaptures
      + getInMem mapResultMemFinal
    | Box { indices; body; type' = _ } ->
      getList indices ~f:(fun { expr; index } -> getInExpr expr + getInIndex index)
      + getInExpr body
    | Values { elements; type' = _ } -> getList elements ~f:getInExpr
    | ScalarPrimitive { op = _; args; type' = _ } -> getList args ~f:getInExpr
    | TupleDeref { tuple; index = _; type' = _ } -> getInExpr tuple
    | ContiguousSubArray { arrayArg; indexArg; originalShape; resultShape; type' } ->
      getInExpr arrayArg
      + getInExpr indexArg
      + getInShape originalShape
      + getInShape resultShape
      + getInType type'
    | Eseq { statement; expr; type' = _ } -> getInStatement statement + getInExpr expr
    | ReifyDimensionIndex { dim } -> getInDim dim
    | Literal _ -> empty
    | Getmem { addr; type' = _ } -> getInMem addr

  and getInStatement : (device, unit) Acorn.Expr.statement -> t = function
    | Putmem { addr; expr; type' = _ } -> getInMem addr + getInExpr expr
    | ComputeForSideEffects expr -> getInExpr expr
    | Statements statements -> getList statements ~f:getInStatement
    | SLet { args; body } ->
      getList args ~f:(fun arg -> getInExpr arg.value) + getInStatement body
    | SMallocLet { memArgs; body } ->
      let argCaptures = getList memArgs ~f:(fun arg -> getInType arg.memType) in
      let bindings =
        memArgs
        |> List.map ~f:(fun arg -> arg.memBinding)
        |> Set.of_list (module Identifier)
      in
      let bodyCaptures = getInStatement body - bindings in
      argCaptures + bodyCaptures
    | ReifyShapeIndex { shape; mem } -> getInShape shape + getInMem mem
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
  | MallocLet { memArgs; body } -> MallocLet { memArgs; body = annotateExpr body }
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
      ; mapResultMemFinal
      ; consumer
      ; type'
      } ->
    let consumer =
      Maybe.map consumer ~f:(function
        | ReduceSeq { arg; zero; body; d; type' } ->
          Expr.ReduceSeq
            { arg; zero = annotateExpr zero; body = annotateExpr body; d; type' }
        | ScanSeq { arg; zero; body; d; scanResultMemFinal; type' } ->
          Expr.ScanSeq
            { arg
            ; zero = annotateExpr zero
            ; body = annotateExpr body
            ; d
            ; scanResultMemFinal
            ; type'
            }
        | Fold { zeroArg; arrayArgs; mappedMemArgs; reverse; body; d; character; type' }
          ->
          Expr.Fold
            { zeroArg = { zeroArg with zeroValue = annotateExpr zeroArg.zeroValue }
            ; arrayArgs
            ; mappedMemArgs
            ; reverse
            ; body = annotateExpr body
            ; d
            ; character
            ; type'
            }
        | Scatter { valuesArg; indicesArg; dIn; dOut; memInterim; memFinal; type' } ->
          Expr.Scatter { valuesArg; indicesArg; dIn; dOut; memInterim; memFinal; type' })
    in
    LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapMemArgs
      ; mapBody = annotateExpr mapBody
      ; mapBodyMatcher
      ; mapResults
      ; mapResultMemFinal
      ; consumer
      ; type'
      }
  | LoopKernel
      { kernel =
          { loopBlock =
              { frameShape
              ; mapArgs
              ; mapIotas
              ; mapMemArgs
              ; mapBody
              ; mapBodyMatcher
              ; mapResults
              ; mapResultMemFinal
              ; consumer
              ; type'
              }
          ; mapResultMemDeviceInterim
          }
      ; captures = ()
      ; blocks
      ; threads
      } ->
    let mapBodyBindings =
      List.map mapArgs ~f:(fun arg -> arg.binding)
      @ List.map mapMemArgs ~f:(fun arg -> arg.memBinding)
      @ List.map mapIotas ~f:(fun iota -> iota.iota)
      |> Set.of_list (module Identifier)
    in
    let mapArgCaptures =
      Captures.getList mapArgs ~f:(fun arg -> Captures.of_ref arg.ref)
    in
    let mapMemArgCaptures =
      Captures.getList mapMemArgs ~f:(fun arg -> Captures.getInMem arg.mem)
    in
    let mapIotaCaptures =
      Captures.getList mapIotas ~f:(fun iota ->
        Captures.getOpt iota.nestIn ~f:(fun id ->
          Captures.of_ref { id; type' = Atom (Literal IntLiteral) }))
    in
    let mapBodyCaptures = Captures.(getInExpr mapBody - mapBodyBindings) in
    let consumerCaptures, consumer =
      match consumer with
      | Just
          (ReducePar
            { reduce = { arg; zero; body; d; type' }
            ; interimResultMemDeviceInterim
            ; interimResultMemHostFinal
            ; outerBody
            }) ->
        let bindings =
          Set.of_list (module Identifier) [ arg.firstBinding; arg.secondBinding ]
        in
        ( Captures.(getInExpr body - bindings + getInDim d)
        , Maybe.Just
            (Expr.ReducePar
               { reduce =
                   { arg; zero = annotateExpr zero; body = annotateExpr body; d; type' }
               ; interimResultMemDeviceInterim
               ; interimResultMemHostFinal
               ; outerBody = annotateExpr outerBody
               }) )
      | Just
          (ScanPar
            { scan = { arg; zero; body; d; scanResultMemFinal; type' }
            ; scanResultMemDeviceInterim
            }) ->
        let bindings =
          Set.of_list (module Identifier) [ arg.firstBinding; arg.secondBinding ]
        in
        ( Captures.(getInExpr body - bindings + getInDim d)
        , Maybe.Just
            (Expr.ScanPar
               { scan =
                   { arg
                   ; zero = annotateExpr zero
                   ; body = annotateExpr body
                   ; d
                   ; scanResultMemFinal
                   ; type'
                   }
               ; scanResultMemDeviceInterim
               }) )
      | Just (Scatter { valuesArg; indicesArg; dIn; dOut; memInterim; memFinal; type' })
        ->
        ( Captures.empty
        , Maybe.Just
            (Expr.Scatter
               { valuesArg; indicesArg; dIn; dOut; memInterim; memFinal; type' }) )
    in
    LoopKernel
      { kernel =
          { loopBlock =
              { frameShape
              ; mapArgs
              ; mapIotas
              ; mapMemArgs
              ; mapBody = annotateExpr mapBody
              ; mapBodyMatcher
              ; mapResults
              ; mapResultMemFinal
              ; consumer
              ; type'
              }
          ; mapResultMemDeviceInterim
          }
      ; captures =
          Captures.(
            mapArgCaptures
            + mapMemArgCaptures
            + mapIotaCaptures
            + mapBodyCaptures
            + consumerCaptures)
      ; blocks
      ; threads
      }
  | Box { indices; body; type' } ->
    Box
      { indices =
          List.map indices ~f:(fun { expr; index } ->
            Expr.{ expr = annotateExpr expr; index })
      ; body = annotateExpr body
      ; type'
      }
  | Values { elements; type' } ->
    Values { elements = List.map elements ~f:annotateExpr; type' }
  | ScalarPrimitive { op; args; type' } ->
    ScalarPrimitive { op; args = List.map args ~f:annotateExpr; type' }
  | TupleDeref { tuple; index; type' } ->
    TupleDeref { tuple = annotateExpr tuple; index; type' }
  | ContiguousSubArray { arrayArg; indexArg; originalShape; resultShape; type' } ->
    ContiguousSubArray
      { arrayArg = annotateExpr arrayArg
      ; indexArg = annotateExpr indexArg
      ; originalShape
      ; resultShape
      ; type'
      }
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
  | MapKernel
      { kernel = { map = mapKernel; mapResultMemDeviceInterim; mapResultMemHostFinal }
      ; captures = ()
      ; blocks
      ; threads
      } ->
    let rec annotateMapInKernel
      Expr.{ frameShape; mapArgs; mapIotas; mapMemArgs; mapBody; type' }
      : Expr.captures * Expr.captures Expr.mapInKernel
      =
      let mapBodyBindings =
        List.map mapArgs ~f:(fun arg -> arg.binding)
        @ List.map mapMemArgs ~f:(fun arg -> arg.memBinding)
        @ List.map mapIotas ~f:(fun iota -> iota.iota)
        |> Set.of_list (module Identifier)
      in
      let mapArgCaptures =
        Captures.getList mapArgs ~f:(fun arg -> Captures.of_ref arg.ref)
      in
      let mapMemArgCaptures =
        Captures.getList mapMemArgs ~f:(fun arg -> Captures.getInMem arg.mem)
      in
      let mapIotaCaptures =
        Captures.getList mapIotas ~f:(fun iota ->
          Captures.getOpt iota.nestIn ~f:(fun id ->
            Captures.of_ref { id; type' = Atom (Literal IntLiteral) }))
      in
      let bodyCaptures, mapBody =
        match mapBody with
        | MapBodyStatement statement ->
          ( Captures.getInStatement statement
          , Expr.MapBodyStatement (annotateStatement statement) )
        | MapBodySubMaps subMaps ->
          let captures, subMaps =
            subMaps |> List.map ~f:annotateMapInKernel |> List.unzip
          in
          Captures.merge_list captures, Expr.MapBodySubMaps subMaps
      in
      let captures =
        Captures.(
          mapArgCaptures
          + mapMemArgCaptures
          + mapIotaCaptures
          + (bodyCaptures - mapBodyBindings))
      in
      captures, { frameShape; mapArgs; mapIotas; mapMemArgs; mapBody; type' }
    in
    let captures, mapKernel = annotateMapInKernel mapKernel in
    MapKernel
      { kernel = { map = mapKernel; mapResultMemDeviceInterim; mapResultMemHostFinal }
      ; captures
      ; blocks
      ; threads
      }
  | ComputeForSideEffects expr -> ComputeForSideEffects (annotateExpr expr)
  | Statements statements -> Statements (List.map statements ~f:annotateStatement)
  | SLet { args; body } ->
    SLet { args = List.map args ~f:annotateArg; body = annotateStatement body }
  | SMallocLet { memArgs; body } -> SMallocLet { memArgs; body = annotateStatement body }
  | ReifyShapeIndex _ as statement -> statement

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
