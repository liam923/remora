open! Base

(* The Corn language represents a flattened, kernelized Remora program *)

module Index = Nested.Index
module Type = Nested.Type

module Expr = struct
  type host = Host
  type device = Device
  type parallel = Parallel
  type sequential = Sequential
  type ref = Nested.Expr.ref
  type reduceCharacter = Nested.Expr.reduceCharacter
  type foldCharacter = Nested.Expr.foldCharacter

  type 'l frame =
    { dimension : int
    ; elements : 'l t list
    ; type' : Type.t
    }

  and 'l boxValue =
    { box : 'l t
    ; type' : Type.t
    }

  and 'l indexValue =
    | Runtime of 'l t
    | FromBox of
        { box : 'l t
        ; i : int
        }

  and 'l indexArg =
    { indexBinding : Identifier.t
    ; indexValue : 'l indexValue
    ; sort : Sort.t
    }

  and 'l indexLet =
    { indexArgs : 'l indexArg list
    ; body : 'l t
    ; type' : Type.t
    }

  and reifyIndex = Nested.Expr.reifyIndex

  and 'l letArg =
    { binding : Identifier.t
    ; value : 'l t
    }

  and 'l let' =
    { args : 'l letArg list
    ; body : 'l t
    ; type' : Type.t
    }

  and 'l box =
    { indices : Index.t list
    ; body : 'l t
    ; bodyType : Type.t
    ; type' : Type.sigma
    }

  and scalarOp = Nested.Expr.scalarOp

  and 'l scalarPrimitive =
    { op : scalarOp
    ; args : 'l t list
    ; type' : Type.t
    }

  and tupleMatch = Nested.Expr.tupleMatch
  and mapArg = Nested.Expr.mapArg
  and mapIota = Nested.Expr.mapIota

  (** returns a tuple of (map results (tuple of arrays, not array of tuples), consumer result (unit if None)) *)
  and ('lOuter, 'lInner, 'p, 'e) loopBlock =
    { frameShape : Index.shapeElement
    ; mapArgs : mapArg list
    ; mapIotas : mapIota list
    ; mapBody : 'lInner t
    ; mapBodyMatcher : tupleMatch
    ; mapResults : Identifier.t list
    ; consumer : (('lOuter, 'lInner, 'p) consumerOp, 'e) Maybe.t
    ; type' : Type.tuple
    }

  and 'l foldZeroArg =
    { zeroBinding : Identifier.t
    ; zeroValue : 'l t
    }

  and production = Nested.Expr.production
  and foldArrayArg = Nested.Expr.foldArrayArg
  and productionTuple = Nested.Expr.productionTuple
  and reduceArg = Nested.Expr.reduceArg

  and ('lOuter, 'lInner) reduce =
    { arg : reduceArg
    ; zero : 'lOuter t option
    ; body : 'lInner t
    ; d : Index.dimension
    ; character : reduceCharacter
    ; type' : Type.t
    }

  and ('lOuter, 'lInner) parReduce =
    { reduce : ('lOuter, 'lInner) reduce
    ; outerBody : 'lOuter t
    }

  and ('lOuter, 'lInner) fold =
    { zeroArg : 'lOuter foldZeroArg
    ; arrayArgs : foldArrayArg list
    ; body : 'lInner t
    ; d : Index.dimension
    ; character : foldCharacter
    ; type' : Type.t
    }

  and scatter =
    { valuesArg : production
    ; indicesArg : production
    ; dIn : Index.dimension
    ; dOut : Index.dimension
    ; type' : Type.t
    }

  and ('lOuter, 'lInner, 'p) consumerOp =
    | ReduceSeq : ('lOuter, 'lInner) reduce -> ('lOuter, 'lInner, sequential) consumerOp
    | ReducePar : (host, device) parReduce -> (host, device, parallel) consumerOp
    | Scatter : scatter -> _ consumerOp
    | Fold : ('lOuter, 'lInner) fold -> ('lOuter, 'lInner, sequential) consumerOp

  and mapBodySubMap =
    | MapBodyMap of mapKernel
    | MapBodyValues of mapBodySubMap list
    | MapBodyDeref of
        { tuple : mapBodySubMap
        ; index : int
        }

  and mapBody =
    | MapBodyExpr of device t
    | MapBodySubMap of mapBodySubMap

  and mapKernel =
    { frameShape : Index.shapeElement
    ; mapArgs : mapArg list
    ; mapIotas : mapIota list
    ; mapBody : mapBody
    ; mapBodyMatcher : tupleMatch
    ; mapResults : Identifier.t list
    ; type' : Type.t
    }

  and 'k kernel =
    { kernel : 'k
    ; blocks : int
    ; threads : int
    }

  and 'l values =
    { elements : 'l t list
    ; type' : Type.tuple
    }

  and 'l tupleDeref =
    { index : int
    ; tuple : 'l t
    ; type' : Type.t
    }

  and literal = Nested.Expr.literal

  and 'l subArray =
    { arrayArg : 'l t
    ; indexArg : 'l t
    ; type' : Type.t
    }

  and 'l append =
    { args : 'l t list
    ; type' : Type.t
    }

  (** Zip collections together, going `nestCount` deep. A "collection" is
      a box or array. The arg is expected to be a tuple *)
  and 'l zip =
    { zipArg : 'l t
    ; nestCount : int
    ; type' : Type.t
    }

  (** Unzip nested collections, recursively entering collections until a tuple
      is reached *)
  and 'l unzip =
    { unzipArg : 'l t
    ; type' : Type.tuple
    }

  and parallelism =
    | KnownParallelism of int
    | Parallelism of
        { shape : Index.shapeElement
        ; rest : parallelism
        }
    | MaxParallelism of parallelism list

  and ifParallelismHitsCutoff =
    { parallelism : parallelism
    ; cutoff : int
    ; then' : host t
    ; else' : host t
    ; type' : Type.t
    }

  and _ t =
    | Ref : ref -> _ t
    | Frame : 'l frame -> 'l t
    | BoxValue : 'l boxValue -> 'l t
    | IndexLet : 'l indexLet -> 'l t
    | ReifyIndex : reifyIndex -> _ t
    | Let : 'l let' -> 'l t
    | LoopBlock : ('l, 'l, sequential, 'e) loopBlock -> 'l t
    | LoopKernel : (host, device, parallel, Maybe.doesExist) loopBlock kernel -> host t
    | MapKernel : mapKernel kernel -> host t
    | Box : 'l box -> 'l t
    | Literal : literal -> _ t
    | Values : 'l values -> 'l t
    | ScalarPrimitive : 'l scalarPrimitive -> 'l t
    | TupleDeref : 'l tupleDeref -> 'l t
    | SubArray : 'l subArray -> 'l t
    | Append : 'l append -> 'l t
    | Zip : 'l zip -> 'l t
    | Unzip : 'l unzip -> 'l t
    | IfParallelismHitsCutoff : ifParallelismHitsCutoff -> host t

  let type' : type l. l t -> Type.t = function
    | Box box -> Sigma box.type'
    | Literal (IntLiteral _) -> Literal IntLiteral
    | Literal (FloatLiteral _) -> Literal FloatLiteral
    | Literal (CharacterLiteral _) -> Literal CharacterLiteral
    | Literal (BooleanLiteral _) -> Literal BooleanLiteral
    | ScalarPrimitive scalarPrimitive -> scalarPrimitive.type'
    | TupleDeref tupleDeref -> tupleDeref.type'
    | Values values -> Tuple values.type'
    | Ref ref -> ref.type'
    | Frame frame -> frame.type'
    | BoxValue boxValue -> boxValue.type'
    | IndexLet indexLet -> indexLet.type'
    | Let let' -> let'.type'
    | ReifyIndex reifyIndex -> reifyIndex.type'
    | LoopBlock loopBlock -> Tuple loopBlock.type'
    | LoopKernel loopKernel -> Tuple loopKernel.kernel.type'
    | MapKernel mapKernel -> mapKernel.kernel.type'
    | SubArray subArray -> subArray.type'
    | Append append -> append.type'
    | Zip zip -> zip.type'
    | Unzip unzip -> Tuple unzip.type'
    | IfParallelismHitsCutoff ifParallelismHitsCutoff -> ifParallelismHitsCutoff.type'
  ;;

  let consumerOpType = function
    | `Reduce reduce -> reduce.type'
    | `Fold fold -> fold.type'
    | `Scatter scatter -> scatter.type'
  ;;

  let productionTupleType : productionTuple -> Type.t = function
    | ProductionTuple productionTuple -> productionTuple.type'
    | ProductionTupleAtom productionTupleAtom -> productionTupleAtom.type'
  ;;

  let values elements = Values { elements; type' = List.map elements ~f:type' }
  let let' ~args ~body = Let { args; body; type' = type' body }

  let tupleDeref ~tuple ~index =
    TupleDeref
      { tuple
      ; index
      ; type' =
          (match type' tuple with
           | Tuple types -> List.nth_exn types index
           | _ -> raise (Unreachable.Error "Expected tuple type"))
      }
  ;;

  let unzip unzipArg =
    let rec unzipType type' =
      match type' with
      | Type.Array { element; size } ->
        unzipType element |> List.map ~f:(fun e -> Type.Array { element = e; size })
      | Type.Sigma { parameters; body } ->
        unzipType body |> List.map ~f:(fun e -> Type.Sigma { parameters; body = e })
      | Type.Tuple t -> t
      | Type.Literal _ -> raise (Unreachable.Error "Expected collection or tuple type")
    in
    Unzip { unzipArg; type' = unzipType @@ type' unzipArg }
  ;;

  module Sexp_of = struct
    let sexp_of_host Host = Sexp.Atom "host"
    let sexp_of_device Device = Sexp.Atom "device"
    let sexp_of_parallel Parallel = Sexp.Atom "parallel"
    let sexp_of_sequential Sequential = Sexp.Atom "sequential"
    let sexp_of_ref = [%sexp_of: Nested.Expr.ref]

    let rec sexp_of_frame : type a. (a -> Sexp.t) -> a frame -> Sexp.t =
      fun sexp_of_a { dimension = _; elements; type' = _ } ->
      Sexp.List (Sexp.Atom "frame" :: List.map elements ~f:(sexp_of_t sexp_of_a))

    and sexp_of_box : type a. (a -> Sexp.t) -> a box -> Sexp.t =
      fun sexp_of_a { indices; body; bodyType = _; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "box"
        ; Sexp.List (List.map indices ~f:Index.sexp_of_t)
        ; sexp_of_t sexp_of_a body
        ]

    and sexp_of_literal = [%sexp_of: Nested.Expr.literal]
    and sexp_of_scalarOp = [%sexp_of: Nested.Expr.scalarOp]

    and sexp_of_scalarPrimitive : type a. (a -> Sexp.t) -> a scalarPrimitive -> Sexp.t =
      fun sexp_of_a { op; args; type' = _ } ->
      Sexp.List (sexp_of_scalarOp op :: List.map args ~f:(sexp_of_t sexp_of_a))

    and sexp_of_tupleDeref : type a. (a -> Sexp.t) -> a tupleDeref -> Sexp.t =
      fun sexp_of_a { tuple; index; type' = _ } ->
      Sexp.List [ Sexp.Atom [%string "#%{index#Int}"]; sexp_of_t sexp_of_a tuple ]

    and sexp_of_values : type a. (a -> Sexp.t) -> a values -> Sexp.t =
      fun sexp_of_a { elements; type' = _ } ->
      Sexp.List (Sexp.Atom "values" :: List.map elements ~f:(sexp_of_t sexp_of_a))

    and sexp_of_boxValue : type a. (a -> Sexp.t) -> a boxValue -> Sexp.t =
      fun sexp_of_a { box; type' = _ } ->
      Sexp.List [ Sexp.Atom "unbox"; sexp_of_t sexp_of_a box ]

    and sexp_of_indexLet : type a. (a -> Sexp.t) -> a indexLet -> Sexp.t =
      fun sexp_of_a { indexArgs; body; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "index-let"
        ; Sexp.List
            (List.map indexArgs ~f:(fun { indexBinding; indexValue; sort = _ } ->
               Sexp.List
                 (Sexp.Atom (Identifier.show indexBinding)
                  ::
                  (match indexValue with
                   | Runtime v -> [ Sexp.Atom "runtime-value"; sexp_of_t sexp_of_a v ]
                   | FromBox { box; i } ->
                     [ Sexp.Atom [%string "box-index-%{i#Int}"]; sexp_of_t sexp_of_a box ])
                 )))
        ; sexp_of_t sexp_of_a body
        ]

    and sexp_of_letArg : type a. (a -> Sexp.t) -> a letArg -> Sexp.t =
      fun sexp_of_a { binding; value } ->
      Sexp.List [ Sexp.Atom (Identifier.show binding); sexp_of_t sexp_of_a value ]

    and sexp_of_let : type a. (a -> Sexp.t) -> a let' -> Sexp.t =
      fun sexp_of_a { args; body; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "let"
        ; Sexp.List (List.map args ~f:(sexp_of_letArg sexp_of_a))
        ; sexp_of_t sexp_of_a body
        ]

    and sexp_of_reifyIndex = [%sexp_of: Nested.Expr.reifyIndex]
    and sexp_of_tupleMatch = [%sexp_of: Nested.Expr.tupleMatch]
    and sexp_of_productionTuple = [%sexp_of: Nested.Expr.productionTuple]

    and sexp_of_reduce
      : type a b. (a -> Sexp.t) -> (b -> Sexp.t) -> (a, b) reduce -> Sexp.t
      =
      fun sexp_of_a sexp_of_b { arg; zero; body; d = _; character; type' = _ } ->
      let characterName =
        match character with
        | Reduce -> "reduce"
        | Scan -> "scan"
        | OpenScan -> "open-scan"
      in
      let zeroName =
        match zero with
        | Some _ -> "-zero"
        | None -> ""
      in
      let opName = [%string "%{characterName}%{zeroName}"] in
      Sexp.List
        ([ Sexp.Atom opName ]
         @ (zero |> Option.map ~f:(sexp_of_t sexp_of_a) |> Option.to_list)
         @ [ Sexp.List
               [ Sexp.Atom (Identifier.show arg.firstBinding)
               ; Sexp.Atom (Identifier.show arg.secondBinding)
               ; sexp_of_productionTuple arg.production
               ]
           ; sexp_of_t sexp_of_b body
           ])

    and sexp_of_parReduce
      : type a b. (a -> Sexp.t) -> (b -> Sexp.t) -> (a, b) parReduce -> Sexp.t
      =
      fun sexp_of_a sexp_of_b { reduce; outerBody } ->
      Sexp.List
        [ sexp_of_reduce sexp_of_a sexp_of_b reduce
        ; Sexp.List [ Sexp.Atom "outer-body"; sexp_of_t sexp_of_a outerBody ]
        ]

    and sexp_of_fold : type a b. (a -> Sexp.t) -> (b -> Sexp.t) -> (a, b) fold -> Sexp.t =
      fun sexp_of_a sexp_of_b { zeroArg; arrayArgs; body; d = _; character; type' = _ } ->
      let opName =
        match character with
        | Fold -> "fold"
        | Trace -> "trace"
        | OpenTrace -> "open-trace"
      in
      Sexp.List
        [ Sexp.Atom opName
        ; Sexp.List
            [ Sexp.Atom (Identifier.show zeroArg.zeroBinding)
            ; sexp_of_t sexp_of_a zeroArg.zeroValue
            ]
        ; Sexp.List
            (List.map arrayArgs ~f:(fun arrayArg ->
               Sexp.List
                 [ Sexp.Atom (Identifier.show arrayArg.binding)
                 ; Sexp.Atom (Identifier.show arrayArg.production.productionId)
                 ]))
        ; sexp_of_t sexp_of_b body
        ]

    and sexp_of_scatter { valuesArg; indicesArg; dIn; dOut; type' = _ } =
      Sexp.List
        [ Sexp.Atom "scatter"
        ; Index.sexp_of_dimension dIn
        ; Index.sexp_of_dimension dOut
        ; Sexp.Atom (Identifier.show valuesArg.productionId)
        ; Sexp.Atom (Identifier.show indicesArg.productionId)
        ]

    and sexp_of_consumerOp
      : type a b p.
        (a -> Sexp.t) -> (b -> Sexp.t) -> (p -> Sexp.t) -> (a, b, p) consumerOp -> Sexp.t
      =
      fun sexp_of_a sexp_of_b _ -> function
      | ReduceSeq reduce -> sexp_of_reduce sexp_of_a sexp_of_b reduce
      | ReducePar reduce -> sexp_of_parReduce sexp_of_host sexp_of_device reduce
      | Fold fold -> sexp_of_fold sexp_of_a sexp_of_b fold
      | Scatter scatter -> sexp_of_scatter scatter

    and sexp_of_mapArg = [%sexp_of: Nested.Expr.mapArg]
    and sexp_of_mapIota = [%sexp_of: Nested.Expr.mapIota]

    and sexp_of_loopBlock
      : type a b p e x.
        ?kernel:bool
        -> (a -> Sexp.t)
        -> (b -> Sexp.t)
        -> (p -> Sexp.t)
        -> x
        -> (a, b, p, e) loopBlock
        -> Sexp.t
      =
      fun ?(kernel = false)
          sexp_of_a
          sexp_of_b
          sexp_of_p
          _
          { frameShape
          ; mapArgs
          ; mapIotas
          ; mapBody
          ; mapBodyMatcher
          ; mapResults
          ; consumer
          ; type' = _
          } ->
      Sexp.List
        [ Sexp.Atom (if kernel then "loop-kernel" else "loop-block")
        ; Sexp.List [ Sexp.Atom "frame-shape"; Index.sexp_of_shapeElement frameShape ]
        ; Sexp.List
            ([ Sexp.Atom "map"; Sexp.List (List.map mapArgs ~f:sexp_of_mapArg) ]
             @ (if List.length mapIotas > 0
                then
                  [ Sexp.List (Sexp.Atom "iota" :: List.map mapIotas ~f:sexp_of_mapIota) ]
                else [])
             @ [ sexp_of_t sexp_of_b mapBody ])
        ; Sexp.List [ Sexp.Atom "body-matcher"; sexp_of_tupleMatch mapBodyMatcher ]
        ; Sexp.List
            [ Sexp.Atom "map-result"
            ; Sexp.List
                (List.map mapResults ~f:(fun id -> Sexp.Atom (Identifier.show id)))
            ]
        ; Sexp.List
            [ Sexp.Atom "consumer"
            ; (match consumer with
               | Just consumer ->
                 sexp_of_consumerOp sexp_of_a sexp_of_b sexp_of_p consumer
               | Nothing -> sexp_of_values sexp_of_a { elements = []; type' = [] })
            ]
        ]

    and sexp_of_mapBodySubMap = function
      | MapBodyMap mapKernel -> sexp_of_mapKernel mapKernel
      | MapBodyValues values ->
        Sexp.List (Sexp.Atom "values" :: List.map values ~f:sexp_of_mapBodySubMap)
      | MapBodyDeref { tuple; index } ->
        Sexp.List
          [ Sexp.Atom (String.concat [ "#"; Int.to_string index ])
          ; sexp_of_mapBodySubMap tuple
          ]

    and sexp_of_mapBody = function
      | MapBodyExpr expr -> sexp_of_t sexp_of_device expr
      | MapBodySubMap subMap -> sexp_of_mapBodySubMap subMap

    and sexp_of_mapKernel
      ({ frameShape; mapArgs; mapIotas; mapBody; mapBodyMatcher; mapResults; type' = _ } :
        mapKernel)
      =
      Sexp.List
        ([ Sexp.Atom "map-kernel"
         ; Sexp.List [ Sexp.Atom "frame-shape"; Index.sexp_of_shapeElement frameShape ]
         ; Sexp.List
             (List.map mapArgs ~f:(fun { binding; ref } ->
                Sexp.List
                  [ Sexp.Atom (Identifier.show binding)
                  ; Sexp.Atom (Identifier.show ref.id)
                  ]))
         ]
         @ (if List.length mapIotas > 0
            then
              [ Sexp.List
                  (Sexp.Atom "iota"
                   :: List.map mapIotas ~f:(function
                     | { iota; nestIn = None } -> Sexp.Atom (Identifier.show iota)
                     | { iota; nestIn = Some parent } ->
                       Sexp.List
                         [ Sexp.Atom (Identifier.show iota)
                         ; Sexp.Atom ":"
                         ; Sexp.Atom (Identifier.show parent)
                         ]))
              ]
            else [])
         @ (Sexp.List [ Sexp.Atom "body-matcher"; sexp_of_tupleMatch mapBodyMatcher ]
            :: Sexp.List
                 [ Sexp.Atom "map-result"
                 ; Sexp.List
                     (List.map mapResults ~f:(fun id -> Sexp.Atom (Identifier.show id)))
                 ]
            :: [ sexp_of_mapBody mapBody ]))

    and sexp_of_kernel : type k. (k -> Sexp.t) -> k kernel -> Sexp.t =
      fun sexp_of_k { kernel; blocks; threads } ->
      Sexp.List
        [ Sexp.Atom "kernel"
        ; Sexp.List [ Sexp.Atom "blocks"; Int.sexp_of_t blocks ]
        ; Sexp.List [ Sexp.Atom "threads"; Int.sexp_of_t threads ]
        ; sexp_of_k kernel
        ]

    and sexp_of_subArray : type a. (a -> Sexp.t) -> a subArray -> Sexp.t =
      fun sexp_of_a { arrayArg; indexArg; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "index"; sexp_of_t sexp_of_a arrayArg; sexp_of_t sexp_of_a indexArg ]

    and sexp_of_append : type a. (a -> Sexp.t) -> a append -> Sexp.t =
      fun sexp_of_a { args; type' = _ } ->
      Sexp.List (Sexp.Atom "++" :: List.map args ~f:(sexp_of_t sexp_of_a))

    and sexp_of_zip : type a. (a -> Sexp.t) -> a zip -> Sexp.t =
      fun sexp_of_a { zipArg; nestCount; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "zip"
        ; Sexp.List [ Sexp.Atom "nests"; Sexp.Atom (Int.to_string nestCount) ]
        ; sexp_of_t sexp_of_a zipArg
        ]

    and sexp_of_unzip : type a. (a -> Sexp.t) -> a unzip -> Sexp.t =
      fun sexp_of_a { unzipArg; type' = _ } ->
      Sexp.List [ Sexp.Atom "unzip"; sexp_of_t sexp_of_a unzipArg ]

    and sexp_of_parallelism = function
      | KnownParallelism n -> Sexp.Atom (Int.to_string n)
      | Parallelism { shape; rest } ->
        Sexp.List
          [ Sexp.Atom "Parallelism"
          ; Index.sexp_of_shapeElement shape
          ; sexp_of_parallelism rest
          ]
      | MaxParallelism pars ->
        Sexp.List (Sexp.Atom "MaxParallelism" :: List.map pars ~f:sexp_of_parallelism)

    and sexp_of_ifParallelismHitsCutoff { parallelism; cutoff; then'; else'; type' = _ } =
      Sexp.List
        [ Sexp.Atom "if"
        ; Sexp.List
            [ Sexp.Atom ">="
            ; Sexp.List
                [ Sexp.Atom "parallelism-factor"; sexp_of_parallelism parallelism ]
            ; Sexp.Atom (Int.to_string cutoff)
            ]
        ; sexp_of_t sexp_of_host then'
        ; sexp_of_t sexp_of_host else'
        ]

    and sexp_of_t : type a. (a -> Sexp.t) -> a t -> Sexp.t =
      fun sexp_of_a expr ->
      match expr with
      | Box box -> sexp_of_box sexp_of_a box
      | Literal lit -> sexp_of_literal lit
      | ScalarPrimitive scalarPrimitive ->
        sexp_of_scalarPrimitive sexp_of_a scalarPrimitive
      | TupleDeref tupleDeref -> sexp_of_tupleDeref sexp_of_a tupleDeref
      | Values values -> sexp_of_values sexp_of_a values
      | Ref ref -> sexp_of_ref ref
      | Frame frame -> sexp_of_frame sexp_of_a frame
      | BoxValue boxValue -> sexp_of_boxValue sexp_of_a boxValue
      | IndexLet indexLet -> sexp_of_indexLet sexp_of_a indexLet
      | Let let' -> sexp_of_let sexp_of_a let'
      | ReifyIndex reifyIndex -> sexp_of_reifyIndex reifyIndex
      | LoopBlock loopBlock ->
        sexp_of_loopBlock sexp_of_a sexp_of_a sexp_of_sequential () loopBlock
      | LoopKernel loopKernel ->
        sexp_of_kernel
          (sexp_of_loopBlock
             ~kernel:true
             sexp_of_host
             sexp_of_device
             sexp_of_parallel
             (sexp_of_consumerOp sexp_of_host sexp_of_device sexp_of_parallel))
          loopKernel
      | MapKernel mapKernel -> sexp_of_kernel sexp_of_mapKernel mapKernel
      | SubArray subArray -> sexp_of_subArray sexp_of_a subArray
      | Append append -> sexp_of_append sexp_of_a append
      | Zip zip -> sexp_of_zip sexp_of_a zip
      | Unzip unzip -> sexp_of_unzip sexp_of_a unzip
      | IfParallelismHitsCutoff ifParallelismHitsCutoff ->
        sexp_of_ifParallelismHitsCutoff ifParallelismHitsCutoff
    ;;
  end

  include Sexp_of
end

type t = Expr.host Expr.t

let sexp_of_t = Expr.sexp_of_t Expr.sexp_of_host
