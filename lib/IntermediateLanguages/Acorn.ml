open! Base

(* The Acorn language represents a flattened, kernelized Remora program,
   which explicit memory allocations and kernels annotated with values
   they capture (A for allocate + Corn) *)

module Index = Corn.Index
module Type = Corn.Type

module Mem = struct
  type mallocHostOrDevice =
    | MallocHost
    | MallocDevice
  [@@deriving sexp_of]

  type t =
    | Ref of
        { id : Identifier.t
        ; type' : Type.t
        }
    | TupleDeref of
        { tuple : t
        ; index : int
        ; type' : Type.t
        }
    | Values of
        { elements : t list
        ; type' : Type.t
        }
    | Malloc of
        { hostOrDevice : mallocHostOrDevice
        ; type' : Type.t
        }
    | Index of
        { mem : t
        ; offset : Index.dimension
        ; elementType : Type.t
        ; type' : Type.t
        }
  [@@deriving sexp_of]

  let type' = function
    | Ref ref -> ref.type'
    | TupleDeref tupleDeref -> tupleDeref.type'
    | Values values -> values.type'
    | Malloc malloc -> malloc.type'
    | Index index -> index.type'
  ;;

  let tupleDeref ~tuple ~index =
    let rec extractElementType = function
      | Type.Array { element; size } ->
        Type.Array { element = extractElementType element; size }
      | Type.Sigma { parameters; body } ->
        Type.Sigma { parameters; body = extractElementType body }
      | Type.Tuple types -> List.nth_exn types index
      | Literal _ -> raise (Unreachable.Error "Expected tuple type")
    in
    match tuple with
    | Values { elements; type' = _ } -> List.nth_exn elements index
    | _ -> TupleDeref { tuple; index; type' = extractElementType (type' tuple) }
  ;;

  let values elements = Values { elements; type' = Tuple (List.map elements ~f:type') }
end

module Expr = struct
  type host = Corn.Expr.host
  type device = Corn.Expr.device
  type parallel = Corn.Expr.parallel
  type sequential = Corn.Expr.sequential

  type ref =
    { id : Identifier.t
    ; type' : Type.t
    }

  type reduceCharacter = Corn.Expr.reduceCharacter

  type foldCharacter = Corn.Expr.foldCharacter

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

  and reifyShapeIndex =
    { shapeIndex : Index.shape
    ; mem : Mem.t
    }

  and reifyDimensionIndex = { dimIndex : Index.dimension }

  and 'l letArg =
    { binding : Identifier.t
    ; value : 'l t
    }

  and ('l, 's) let' =
    { args : 'l letArg list
    ; body : 's
    }

  and memArg =
    { memBinding : Identifier.t
    ; mem : Mem.t
    }

  and ('l, 's) memLet =
    { memArgs : memArg list
    ; body : 's
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
  and ('lOuter, 'lInner, 'p) loopBlock =
    { frameShape : Index.shapeElement
    ; mapArgs : mapArg list
    ; mapMemArgs : memArg list
    ; mapIotas : mapIota list
    ; mapBody : 'lInner statement
    ; mapBodyMatcher : tupleMatch
    ; mapResults : Identifier.t list
    ; mapResultMem : Mem.t
    ; consumer : ('lOuter, 'lInner, 'p) consumerOp option
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
    ; mappedMemArgs : memArg list
    ; body : 'lInner t
    ; d : Index.dimension
    ; itemPad : Index.shape
    ; character : reduceCharacter
    ; type' : Type.t
    }

  and ('lOuter, 'lInner) fold =
    { zeroArg : 'lOuter foldZeroArg
    ; arrayArgs : foldArrayArg list
    ; mappedMemArgs : memArg list
    ; body : 'lInner t
    ; d : Index.dimension
    ; itemPad : Index.shape
    ; character : foldCharacter
    ; type' : Type.t
    }

  and scatter =
    { valuesArg : production
    ; indicesArg : production
    ; dIn : Index.dimension
    ; dOut : Index.dimension
    ; mem : Mem.t
    ; type' : Type.t
    }

  and ('lOuter, 'lInner, 'p) consumerOp =
    | ReduceSeq : ('lOuter, 'lInner) reduce -> ('lOuter, 'lInner, sequential) consumerOp
    | ReducePar : (host, device) reduce -> (host, device, parallel) consumerOp
    | Scatter : scatter -> (_, _, _) consumerOp
    | Fold : ('lOuter, 'lInner) fold -> ('lOuter, 'lInner, sequential) consumerOp

  and mapBody =
    { statements : device statement list
    ; subMaps : mapKernel list
    }

  and mapKernel =
    { frameShape : Index.shapeElement
    ; mapArgs : mapArg list
    ; mapMemArgs : memArg list
    ; mapIotas : mapIota list
    ; mapBody : mapBody
    ; mapBodyMatcher : tupleMatch
    ; mapResults : Identifier.t list
    ; mapResultMem : Mem.t
    ; type' : Type.t
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

  and parallelism = Corn.Expr.parallelism

  and ifParallelismHitsCutoff =
    { parallelism : parallelism
    ; cutoff : int
    ; then' : host t
    ; else' : host t
    ; type' : Type.t
    }

  and 'k kernel =
    { kernel : 'k
    ; memCaptures : Set.M(Identifier).t
    ; indexCaptures : Set.M(Identifier).t
    ; exprCaptures : Set.M(Identifier).t
    }

  and 'l eseq =
    { statement : 'l statement
    ; expr : 'l t
    ; type' : Type.t
    }

  and 'l putmem =
    { expr : 'l t
    ; addr : Mem.t
    ; type' : Type.t
    }

  and getmem =
    { addr : Mem.t
    ; type' : Type.t
    }

  and _ t =
    | Ref : ref -> _ t
    | BoxValue : 'l boxValue -> 'l t
    | IndexLet : 'l indexLet -> 'l t
    | MemLet : ('l, 'l t) memLet -> 'l t
    | ReifyDimensionIndex : reifyDimensionIndex -> _ t
    | LoopBlock : ('l, 'l, sequential) loopBlock -> 'l t
    | LoopKernel : (host, device, parallel) loopBlock kernel -> host t
    | Let : ('l, 'l t) let' -> 'l t
    | Box : 'l box -> 'l t
    | Literal : literal -> _ t
    | Values : 'l values -> 'l t
    | ScalarPrimitive : 'l scalarPrimitive -> 'l t
    | TupleDeref : 'l tupleDeref -> 'l t
    | SubArray : 'l subArray -> 'l t
    | IfParallelismHitsCutoff : ifParallelismHitsCutoff -> host t
    | Eseq : 'l eseq -> 'l t
    | Getmem : getmem -> _ t

  and _ statement =
    | Putmem : 'l putmem -> 'l statement
    | MapKernel : mapKernel kernel -> host statement
    | ComputeForSideEffects : 'l t -> 'l statement
    | Statements : 'l statement list -> 'l statement
    | SLet : ('l, 'l statement) let' -> 'l statement
    | SMemLet : ('l, 'l statement) memLet -> 'l statement
    | ReifyShapeIndexToArray : reifyShapeIndex -> _ statement
    | ReifyShapeIndexToBox : reifyShapeIndex -> _ statement

  let rec type' : type l. l t -> Type.t = function
    | Box box -> Sigma box.type'
    | Literal (IntLiteral _) -> Literal IntLiteral
    | Literal (CharacterLiteral _) -> Literal CharacterLiteral
    | Literal (BooleanLiteral _) -> Literal BooleanLiteral
    | ScalarPrimitive scalarPrimitive -> scalarPrimitive.type'
    | TupleDeref tupleDeref -> tupleDeref.type'
    | Values values -> Tuple values.type'
    | Ref ref -> ref.type'
    | BoxValue boxValue -> boxValue.type'
    | IndexLet indexLet -> indexLet.type'
    | Let let' -> type' let'.body
    | MemLet memLet -> type' memLet.body
    | ReifyDimensionIndex _ -> Literal IntLiteral
    | LoopBlock loopBlock -> Tuple loopBlock.type'
    | LoopKernel loopKernel -> Tuple loopKernel.kernel.type'
    | SubArray subArray -> subArray.type'
    | IfParallelismHitsCutoff ifParallelismHitsCutoff -> ifParallelismHitsCutoff.type'
    | Eseq eseq -> eseq.type'
    | Getmem getmem -> getmem.type'
  ;;

  let rec eseq ~statements ~expr =
    match statements with
    | [] -> expr
    | first :: rest ->
      Eseq { statement = first; expr = eseq ~statements:rest ~expr; type' = type' expr }
  ;;

  let putmem ~expr ~addr = Putmem { expr; addr; type' = type' expr }
  let getmem mem = Getmem { addr = mem; type' = Mem.type' mem }

  let tupleDeref ~tuple ~index =
    let rec extractElementType = function
      | Type.Array { element; size } ->
        Type.Array { element = extractElementType element; size }
      | Type.Sigma { parameters; body } ->
        Type.Sigma { parameters; body = extractElementType body }
      | Type.Literal _ -> raise @@ Unreachable.Error "expected tuple type"
      | Type.Tuple elements -> List.nth_exn elements index
    in
    TupleDeref { tuple; index; type' = extractElementType (type' tuple) }
  ;;

  module Sexp_of = struct
    let sexp_of_host (Host : host) = Sexp.Atom "host"
    let sexp_of_device (Device : device) = Sexp.Atom "device"
    let sexp_of_parallel (Parallel : parallel) = Sexp.Atom "parallel"
    let sexp_of_sequential (Sequential : sequential) = Sexp.Atom "sequential"
    let sexp_of_ref { id; type' = _ } = [%sexp_of: Identifier.t] id

    let rec sexp_of_box : type a. (a -> Sexp.t) -> a box -> Sexp.t =
      fun sexp_of_a { indices; body; bodyType = _; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "box"
        ; Sexp.List (List.map indices ~f:Index.sexp_of_t)
        ; sexp_of_t sexp_of_a body
        ]

    and sexp_of_literal = [%sexp_of: Nested.Expr.literal]

    and sexp_of_scalarPrimitive : type a. (a -> Sexp.t) -> a scalarPrimitive -> Sexp.t =
      fun sexp_of_a { op; args; type' = _ } ->
      let opString =
        match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Equal -> "="
      in
      Sexp.List (Sexp.Atom opString :: List.map args ~f:(sexp_of_t sexp_of_a))

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

    and sexp_of_let : type a s. (a -> Sexp.t) -> (s -> Sexp.t) -> (a, s) let' -> Sexp.t =
      fun sexp_of_a sexp_of_s { args; body } ->
      Sexp.List
        [ Sexp.Atom "let"
        ; Sexp.List (List.map args ~f:(sexp_of_letArg sexp_of_a))
        ; sexp_of_s body
        ]

    and sexp_of_reifyShapeIndex ?(toStr = "") { shapeIndex; mem } =
      Sexp.List
        [ Sexp.Atom [%string "reify-shape-index%{toStr}"]
        ; Index.sexp_of_shape shapeIndex
        ; Sexp.List [ Sexp.Atom "mem"; Mem.sexp_of_t mem ]
        ]

    and sexp_of_reifyDimensionIndex { dimIndex } =
      Sexp.List [ Sexp.Atom "reify-dimension-index"; Index.sexp_of_dimension dimIndex ]

    and sexp_of_memArg { memBinding; mem } =
      Sexp.List [ Sexp.Atom (Identifier.show memBinding); Mem.sexp_of_t mem ]

    and sexp_of_memLet
      : type a s. (a -> Sexp.t) -> (s -> Sexp.t) -> (a, s) memLet -> Sexp.t
      =
      fun _ sexp_of_s { memArgs; body } ->
      Sexp.List
        [ Sexp.Atom "mem-let"
        ; Sexp.List (List.map memArgs ~f:sexp_of_memArg)
        ; sexp_of_s body
        ]

    and sexp_of_tupleMatch = [%sexp_of: Nested.Expr.tupleMatch]
    and sexp_of_productionTuple = [%sexp_of: Nested.Expr.productionTuple]

    and sexp_of_reduce
      : type a b. (a -> Sexp.t) -> (b -> Sexp.t) -> (a, b) reduce -> Sexp.t
      =
      fun sexp_of_a
          sexp_of_b
          { arg; zero; mappedMemArgs; body; d = _; itemPad; character; type' = _ } ->
      let characterName =
        match character with
        | `Reduce -> "reduce"
        | `Scan -> "scan"
        | `OpenScan -> "open-scan"
      in
      let zeroName =
        match zero with
        | Some _ -> "-zero"
        | None -> ""
      in
      let opName = [%string "%{characterName}%{zeroName}"] in
      Sexp.List
        ([ Sexp.Atom opName; Index.sexp_of_shape itemPad ]
         @ (zero |> Option.map ~f:(sexp_of_t sexp_of_a) |> Option.to_list)
         @ (match mappedMemArgs with
            | [] -> []
            | _ ->
              [ Sexp.List
                  [ Sexp.Atom "mapped-mem-args"
                  ; Sexp.List (List.map mappedMemArgs ~f:sexp_of_memArg)
                  ]
              ])
         @ [ Sexp.List
               [ Sexp.Atom (Identifier.show arg.firstBinding)
               ; Sexp.Atom (Identifier.show arg.secondBinding)
               ; sexp_of_productionTuple arg.production
               ]
           ; sexp_of_t sexp_of_b body
           ])

    and sexp_of_fold : type a b. (a -> Sexp.t) -> (b -> Sexp.t) -> (a, b) fold -> Sexp.t =
      fun sexp_of_a
          sexp_of_b
          { zeroArg
          ; arrayArgs
          ; mappedMemArgs
          ; body
          ; d = _
          ; itemPad
          ; character
          ; type' = _
          } ->
      let opName =
        match character with
        | `Fold -> "fold"
        | `Trace -> "trace"
        | `OpenTrace -> "open-trace"
      in
      Sexp.List
        [ Sexp.Atom opName
        ; Index.sexp_of_shape itemPad
        ; Sexp.List
            [ Sexp.Atom (Identifier.show zeroArg.zeroBinding)
            ; sexp_of_t sexp_of_a zeroArg.zeroValue
            ]
        ; Sexp.List
            (List.map arrayArgs ~f:(fun arrayArg ->
               Sexp.List
                 ([ Sexp.Atom (Identifier.show arrayArg.binding)
                  ; Sexp.Atom (Identifier.show arrayArg.production.productionId)
                  ]
                  @ List.map mappedMemArgs ~f:sexp_of_memArg)))
        ; sexp_of_t sexp_of_b body
        ]

    and sexp_of_scatter { valuesArg; indicesArg; dIn; dOut; mem; type' = _ } =
      Sexp.List
        [ Sexp.Atom "scatter"
        ; Sexp.List [ Sexp.Atom "mem"; Mem.sexp_of_t mem ]
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
      | ReducePar reduce -> sexp_of_reduce sexp_of_host sexp_of_device reduce
      | Fold fold -> sexp_of_fold sexp_of_a sexp_of_b fold
      | Scatter scatter -> sexp_of_scatter scatter

    and sexp_of_mapArg = [%sexp_of: Nested.Expr.mapArg]
    and sexp_of_mapIota = [%sexp_of: Nested.Expr.mapIota]

    and sexp_of_loopBlock
      : type a b p.
        (a -> Sexp.t) -> (b -> Sexp.t) -> (p -> Sexp.t) -> (a, b, p) loopBlock -> Sexp.t
      =
      fun sexp_of_a
          sexp_of_b
          sexp_of_p
          { frameShape
          ; mapArgs
          ; mapMemArgs
          ; mapIotas
          ; mapBody
          ; mapBodyMatcher
          ; mapResults
          ; mapResultMem
          ; consumer
          ; type' = _
          } ->
      Sexp.List
        [ Sexp.Atom "loop"
        ; Sexp.List [ Sexp.Atom "frame-shape"; Index.sexp_of_shapeElement frameShape ]
        ; Sexp.List
            ([ Sexp.Atom "map"
             ; Sexp.List
                 (List.map mapArgs ~f:sexp_of_mapArg
                  @ List.map mapMemArgs ~f:sexp_of_memArg)
             ]
             @ (if List.length mapIotas > 0
                then
                  [ Sexp.List (Sexp.Atom "iota" :: List.map mapIotas ~f:sexp_of_mapIota) ]
                else [])
             @ [ sexp_of_statement sexp_of_b mapBody ])
        ; Sexp.List [ Sexp.Atom "body-matcher"; sexp_of_tupleMatch mapBodyMatcher ]
        ; Sexp.List
            [ Sexp.Atom "map-result"
            ; Sexp.List
                (List.map mapResults ~f:(fun id -> Sexp.Atom (Identifier.show id)))
            ]
        ; Sexp.List [ Sexp.Atom "map-result-mem"; Mem.sexp_of_t mapResultMem ]
        ; Sexp.List
            [ Sexp.Atom "consumer"
            ; (match consumer with
               | Some consumer ->
                 sexp_of_consumerOp sexp_of_a sexp_of_b sexp_of_p consumer
               | None -> sexp_of_values sexp_of_a { elements = []; type' = [] })
            ]
        ]

    and sexp_of_mapBody { statements; subMaps } =
      Sexp.List
        [ Sexp.List
            (Sexp.Atom "statements"
             :: List.map statements ~f:(sexp_of_statement sexp_of_device))
        ; Sexp.List (Sexp.Atom "sub-maps" :: List.map subMaps ~f:sexp_of_mapKernel)
        ]

    and sexp_of_mapKernel
      ({ frameShape
       ; mapArgs
       ; mapMemArgs
       ; mapIotas
       ; mapBody
       ; mapBodyMatcher
       ; mapResults
       ; mapResultMem
       ; type' = _
       } :
        mapKernel)
      =
      Sexp.List
        ([ Sexp.Atom "map-kernel"
         ; Sexp.List [ Sexp.Atom "frame-shape"; Index.sexp_of_shapeElement frameShape ]
         ; Sexp.List
             (List.map mapArgs ~f:sexp_of_mapArg @ List.map mapMemArgs ~f:sexp_of_memArg)
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
            :: Sexp.List [ Sexp.Atom "map-result-mem"; Mem.sexp_of_t mapResultMem ]
            :: [ sexp_of_mapBody mapBody ]))

    and sexp_of_subArray : type a. (a -> Sexp.t) -> a subArray -> Sexp.t =
      fun sexp_of_a { arrayArg; indexArg; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "index"; sexp_of_t sexp_of_a arrayArg; sexp_of_t sexp_of_a indexArg ]

    and sexp_of_parallelism = Corn.Expr.sexp_of_parallelism

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

    and sexp_of_kernel : type k. (k -> Sexp.t) -> k kernel -> Sexp.t =
      fun sexp_of_k { kernel; memCaptures; indexCaptures; exprCaptures } ->
      Sexp.List
        [ Sexp.Atom "kernel"
        ; Sexp.List
            [ Sexp.Atom "capturing"
            ; Sexp.List
                [ Sexp.List
                    [ Sexp.Atom "mems"; [%sexp_of: Set.M(Identifier).t] memCaptures ]
                ; Sexp.List
                    [ Sexp.Atom "indices"; [%sexp_of: Set.M(Identifier).t] indexCaptures ]
                ; Sexp.List
                    [ Sexp.Atom "exprs"; [%sexp_of: Set.M(Identifier).t] exprCaptures ]
                ]
            ]
        ; sexp_of_k kernel
        ]

    and sexp_of_eseq : type a. (a -> Sexp.t) -> a eseq -> Sexp.t =
      fun sexp_of_a eseq ->
      let rec flattenEseq { statement; expr; type' = _ } =
        match expr with
        | Eseq eseq ->
          let restStatements, expr = flattenEseq eseq in
          statement :: restStatements, expr
        | _ -> [ statement ], expr
      in
      let statements, expr = flattenEseq eseq in
      Sexp.List
        ((Sexp.Atom "begin" :: List.map statements ~f:(sexp_of_statement sexp_of_a))
         @ [ sexp_of_t sexp_of_a expr ])

    and sexp_of_getmem ({ addr; type' = _ } : getmem) =
      Sexp.List [ Sexp.Atom "getmem"; Mem.sexp_of_t addr ]

    and sexp_of_putmem : type a. (a -> Sexp.t) -> a putmem -> Sexp.t =
      fun sexp_of_a { expr; addr; type' = _ } ->
      Sexp.List [ Sexp.Atom "putmem"; sexp_of_t sexp_of_a expr; Mem.sexp_of_t addr ]

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
      | BoxValue boxValue -> sexp_of_boxValue sexp_of_a boxValue
      | IndexLet indexLet -> sexp_of_indexLet sexp_of_a indexLet
      | Let let' -> sexp_of_let sexp_of_a (sexp_of_t sexp_of_a) let'
      | ReifyDimensionIndex reifyDimensionIndex ->
        sexp_of_reifyDimensionIndex reifyDimensionIndex
      | LoopBlock loopBlock ->
        sexp_of_loopBlock sexp_of_a sexp_of_a sexp_of_sequential loopBlock
      | LoopKernel loopKernel ->
        sexp_of_kernel
          (sexp_of_loopBlock sexp_of_host sexp_of_device sexp_of_parallel)
          loopKernel
      | SubArray subArray -> sexp_of_subArray sexp_of_a subArray
      | IfParallelismHitsCutoff ifParallelismHitsCutoff ->
        sexp_of_ifParallelismHitsCutoff ifParallelismHitsCutoff
      | Eseq eseq -> sexp_of_eseq sexp_of_a eseq
      | Getmem getmem -> sexp_of_getmem getmem
      | MemLet memLet -> sexp_of_memLet sexp_of_a (sexp_of_t sexp_of_a) memLet

    and sexp_of_statement : type a. (a -> Sexp.t) -> a statement -> Sexp.t =
      fun sexp_of_a statement ->
      match statement with
      | Putmem putmem -> sexp_of_putmem sexp_of_a putmem
      | MapKernel mapKernel -> sexp_of_kernel sexp_of_mapKernel mapKernel
      | ComputeForSideEffects expr ->
        Sexp.List [ Sexp.Atom "do-expr"; sexp_of_t sexp_of_a expr ]
      | Statements statements ->
        Sexp.List
          (Sexp.Atom "begin-do" :: List.map statements ~f:(sexp_of_statement sexp_of_a))
      | SLet let' -> sexp_of_let sexp_of_a (sexp_of_statement sexp_of_a) let'
      | SMemLet memLet -> sexp_of_memLet sexp_of_a (sexp_of_statement sexp_of_a) memLet
      | ReifyShapeIndexToArray reifyShapeIndexToArray ->
        sexp_of_reifyShapeIndex ~toStr:"-to-array" reifyShapeIndexToArray
      | ReifyShapeIndexToBox reifyShapeIndexToBox ->
        sexp_of_reifyShapeIndex ~toStr:"-to-box" reifyShapeIndexToBox
    ;;
  end

  include Sexp_of
end

type t = Expr.host Expr.t [@@deriving sexp_of]
