open! Base

(* The Acorn language represents a flattened, kernelized Remora program,
   which explicit memory allocations and kernels annotated with values
   they capture (A for allocate + Corn) *)

module Index = struct
  type dimension =
    { const : int
    ; refs : int Map.M(Identifier).t
        (* lens is like refs, but instead of ids refering to dimension variables, they are
           shape variables and represent the lengths of them *)
    ; lens : int Map.M(Identifier).t
    }
  [@@deriving compare, equal]

  type shapeElement =
    | Add of dimension
    | ShapeRef of Identifier.t
  [@@deriving compare, equal]

  type shape = shapeElement list [@@deriving compare, equal]
  type neShape = shapeElement NeList.t [@@deriving compare, equal]

  type t =
    | Dimension of dimension
    | Shape of shape
  [@@deriving compare, equal]

  let dimensionConstant n =
    { const = n
    ; refs = Map.empty (module Identifier)
    ; lens = Map.empty (module Identifier)
    }
  ;;

  let dimensionRef r =
    { const = 0
    ; refs = Map.singleton (module Identifier) r 1
    ; lens = Map.empty (module Identifier)
    }
  ;;

  let dimensionLen l =
    { const = 0
    ; refs = Map.empty (module Identifier)
    ; lens = Map.singleton (module Identifier) l 1
    }
  ;;

  let addDimensions a b =
    { const = a.const + b.const
    ; refs = Map.merge_skewed a.refs b.refs ~combine:(fun ~key:_ a b -> a + b)
    ; lens = Map.merge_skewed a.lens b.lens ~combine:(fun ~key:_ a b -> a + b)
    }
  ;;

  let sort = function
    | Dimension _ -> Sort.Dim
    | Shape _ -> Sort.Shape
  ;;

  let sexp_of_dimension ({ const; refs; lens } : dimension) =
    match Map.to_alist refs, Map.to_alist lens with
    | [], [] -> Sexp.Atom (Int.to_string const)
    | [ (ref, 1) ], [] when const = 0 -> Sexp.Atom (Identifier.show ref)
    | [], [ (ref, 1) ] when const = 0 ->
      Sexp.List [ Sexp.Atom "len"; Sexp.Atom (Identifier.show ref) ]
    | refs, lens ->
      Sexp.List
        ([ Sexp.Atom "+"; Sexp.Atom (Int.to_string const) ]
         @ List.bind refs ~f:(fun (ref, count) ->
           let refSexp = Sexp.Atom (Identifier.show ref) in
           List.init count ~f:(fun _ -> refSexp))
         @ List.bind lens ~f:(fun (ref, count) ->
           let refSexp = Sexp.List [ Sexp.Atom "len"; Sexp.Atom (Identifier.show ref) ] in
           List.init count ~f:(fun _ -> refSexp)))
  ;;

  let sexp_of_shapeElement = function
    | Add dimension -> sexp_of_dimension dimension
    | ShapeRef ref -> Sexp.Atom (Identifier.show ref)
  ;;

  let sexp_of_shape shape =
    Sexp.List (Sexp.Atom "shape" :: List.map shape ~f:sexp_of_shapeElement)
  ;;

  let sexp_of_neShape shape = sexp_of_shape @@ NeList.to_list shape

  let sexp_of_t = function
    | Shape shape -> sexp_of_shape shape
    | Dimension dimension -> sexp_of_dimension dimension
  ;;
end

module Type = struct
  module T = struct
    type array =
      { element : atom
      ; shape : Index.neShape
      }

    and sigmaParam = Corn.Type.sigmaParam

    and sigma =
      { parameters : sigmaParam list
      ; body : t
      }

    and literal = Corn.Type.literal

    and atom =
      | Sigma of sigma
      | Literal of literal

    and tuple = t list

    and t =
      | Tuple of tuple
      | Atom of atom
      | Array of array
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let rec array ~element ~size =
    match element with
    | Array innerArray ->
      Array { element = innerArray.element; shape = NeList.cons size innerArray.shape }
    | Atom atom -> Array { element = atom; shape = [ size ] }
    | Tuple elements -> Tuple (List.map elements ~f:(fun t -> array ~element:t ~size))
  ;;

  let arrayOfShape ~element ~shape =
    match shape with
    | [] -> Atom element
    | head :: rest -> Array { element; shape = head :: rest }
  ;;
end

type host = Corn.Expr.host
type device = Corn.Expr.device

let sexp_of_host (Host : host) = Sexp.Atom "host"
let sexp_of_device (Device : device) = Sexp.Atom "device"

module Mem = struct
  type mallocHostOrDevice =
    | MallocHost
    | MallocDevice
  [@@deriving sexp_of]

  (** A Mem.t represents a location in memory, on either host or device *)
  type 'l t =
    | Ref :
        { id : Identifier.t
        ; type' : Type.t
        }
        -> _ t
    | TupleDeref :
        { tuple : 'l t
        ; index : int
        ; type' : Type.t
        }
        -> 'l t
    | Values :
        { elements : 'l t list
        ; type' : Type.t
        }
        -> 'l t
    | Malloc :
        { hostOrDevice : mallocHostOrDevice
        ; type' : Type.t
        }
        -> host t
    | Index :
        { mem : 'l t
        ; offset : Index.dimension
        ; type' : Type.t
        }
        -> 'l t
        (** mem is expected to have an array type (or a tuple of arrays) where the
            first shape element is a single dimension. offset is an index of an element
            in that array *)
  [@@deriving sexp_of]

  let type' : type l. l t -> Type.t = function
    | Ref ref -> ref.type'
    | TupleDeref tupleDeref -> tupleDeref.type'
    | Values values -> values.type'
    | Malloc malloc -> malloc.type'
    | Index index -> index.type'
  ;;

  let rec deviceToHost : device t -> host t = function
    | Ref ref -> Ref ref
    | TupleDeref { tuple; index; type' } ->
      TupleDeref { tuple = deviceToHost tuple; index; type' }
    | Values { elements; type' } ->
      Values { elements = List.map elements ~f:deviceToHost; type' }
    | Index { mem; offset; type' } -> Index { mem = deviceToHost mem; offset; type' }
  ;;

  let tupleDeref : type l. tuple:l t -> index:int -> l t =
    fun ~tuple ~index ->
    let extractElementType = function
      | Type.Tuple types -> List.nth_exn types index
      | Type.Array _ | Type.Atom _ -> raise (Unreachable.Error "Expected tuple type")
    in
    match tuple with
    | Values { elements; type' = _ } -> List.nth_exn elements index
    | _ -> TupleDeref { tuple; index; type' = extractElementType (type' tuple) }
  ;;

  let values : type l. l t list -> l t =
    fun elements -> Values { elements; type' = Tuple (List.map elements ~f:type') }
  ;;
end

module Expr = struct
  type parallel = Corn.Expr.parallel
  type sequential = Corn.Expr.sequential

  type ref =
    { id : Identifier.t
    ; type' : Type.t
    }

  type 'l reduceCharacter =
    | Reduce
    | Scan of 'l Mem.t
    | OpenScan of 'l Mem.t

  type 'l foldCharacter =
    | Fold
    | Trace of 'l Mem.t
    | OpenTrace of 'l Mem.t

  and ('l, 'c) boxValue =
    { box : ('l, 'c) t
    ; type' : Type.t
    }

  and ('l, 'c) indexValue =
    | Runtime of ('l, 'c) t
    | FromBox of
        { box : ('l, 'c) t
        ; i : int
        }

  and ('l, 'c) indexArg =
    { indexBinding : Identifier.t
    ; indexValue : ('l, 'c) indexValue
    ; sort : Sort.t
    }

  and ('l, 'c) indexLet =
    { indexArgs : ('l, 'c) indexArg list
    ; body : ('l, 'c) t
    ; type' : Type.t
    }

  and 'l reifyShapeIndex =
    { shape : Index.shape
    ; mem : 'l Mem.t
    }

  and reifyDimensionIndex = { dim : Index.dimension }

  and ('l, 'c) letArg =
    { binding : Identifier.t
    ; value : ('l, 'c) t
    }

  and ('l, 's, 'c) let' =
    { args : ('l, 'c) letArg list
    ; body : 's
    }

  and 'l memArg =
    { memBinding : Identifier.t
    ; mem : 'l Mem.t
    }

  and ('l, 's) memLet =
    { memArgs : 'l memArg list
    ; body : 's
    }

  and ('l, 'c) boxIndex =
    { expr : ('l, 'c) t
    ; index : Index.t
    }

  and ('l, 'c) box =
    { indices : ('l, 'c) boxIndex list
    ; body : ('l, 'c) t
    ; type' : Type.sigma
    }

  and scalarOp = Nested.Expr.scalarOp

  and ('l, 'c) scalarPrimitive =
    { op : scalarOp
    ; args : ('l, 'c) t list
    ; type' : Type.t
    }

  and tupleMatch = Nested.Expr.tupleMatch

  and mapArg =
    { binding : Identifier.t
    ; ref : ref
    }

  and mapIota = Nested.Expr.mapIota

  (** returns a tuple of (map results (tuple of arrays, not array of tuples), consumer result (unit if None)) *)
  and ('lOuter, 'lInner, 'p, 'c) loopBlock =
    { frameShape : Index.shapeElement
    ; mapArgs : mapArg list
    ; mapMemArgs : 'lOuter memArg list
    ; mapIotas : mapIota list
    ; mapBody : ('lInner, 'c) t
    ; mapBodyMatcher : tupleMatch
    ; mapResults : Identifier.t list
    ; mapResultMem : 'lOuter Mem.t (** where the result of the map should be written *)
    ; consumer : ('lOuter, 'lInner, 'p, 'c) consumerOp option
    ; type' : Type.tuple
    }

  and ('l, 'c) foldZeroArg =
    { zeroBinding : Identifier.t
    ; zeroValue : ('l, 'c) t
    }

  and production =
    { productionId : Identifier.t
    ; type' : Type.t
    }

  and foldArrayArg =
    { binding : Identifier.t
    ; production : production
    }

  and productionTuple =
    | ProductionTuple of
        { elements : productionTuple list
        ; type' : Type.t
        }
    | ProductionTupleAtom of production

  and reduceArg =
    { firstBinding : Identifier.t
    ; secondBinding : Identifier.t
    ; production : productionTuple
    }

  and ('lOuter, 'lInner, 'c) reduce =
    { arg : reduceArg
    ; zero : ('lOuter, 'c) t option
    ; mappedMemArgs : 'lOuter memArg list
    ; body : ('lInner, 'c) t
    ; d : Index.dimension
    ; character : 'lOuter reduceCharacter
    ; type' : Type.t
    }

  (** Represents a reduce that happens in parallel. While most of the reduction occurs
      on device, the last few steps occur on host. *)
  and ('lOuter, 'lInner, 'c) parReduce =
    { reduce : ('lOuter, 'lInner, 'c) reduce
    ; interimResultMem : 'lOuter Mem.t
        (** Where to write the results of the reduction steps that occur on device *)
    ; outerBody : ('lOuter, 'c) t (** The last few steps, which occur on host *)
    ; outerMappedMemArgs : 'lOuter memArg list
    }

  and ('lOuter, 'lInner, 'c) fold =
    { zeroArg : ('lOuter, 'c) foldZeroArg
    ; arrayArgs : foldArrayArg list
    ; mappedMemArgs : 'lOuter memArg list
    ; body : ('lInner, 'c) t
    ; d : Index.dimension
    ; character : 'lOuter foldCharacter
    ; type' : Type.t
    }

  and 'l scatter =
    { valuesArg : production
    ; indicesArg : production
    ; dIn : Index.dimension
    ; dOut : Index.dimension
    ; mem : 'l Mem.t
    ; type' : Type.t
    }

  and ('lOuter, 'lInner, 'p, 'c) consumerOp =
    | ReduceSeq :
        ('lOuter, 'lInner, 'c) reduce
        -> ('lOuter, 'lInner, sequential, 'c) consumerOp
    | ReducePar : (host, device, 'c) parReduce -> (host, device, parallel, 'c) consumerOp
    | Scatter : 'l scatter -> ('l, _, _, _) consumerOp
    | Fold : ('lOuter, 'lInner, 'c) fold -> ('lOuter, 'lInner, sequential, 'c) consumerOp

  and 'c mapBody =
    { statement : (device, 'c) statement
    ; subMaps : 'c mapKernel list
    }

  and 'c mapKernel =
    { frameShape : Index.shapeElement
    ; mapArgs : mapArg list
    ; mapMemArgs : host memArg list
    ; mapIotas : mapIota list
    ; mapBody : 'c mapBody
    ; mapBodyMatcher : tupleMatch
    ; mapResults : Identifier.t list
    ; mapResultMem : host Mem.t
    ; type' : Type.t
    }

  and ('l, 'c) values =
    { elements : ('l, 'c) t list
    ; type' : Type.tuple
    }

  and ('l, 'c) tupleDeref =
    { index : int
    ; tuple : ('l, 'c) t
    ; type' : Type.t
    }

  and literal = Nested.Expr.literal

  and ('l, 'c) subArray =
    { arrayArg : ('l, 'c) t
    ; indexArg : ('l, 'c) t
    ; type' : Type.t
    }

  and parallelism =
    | KnownParallelism of int
    | Parallelism of
        { shape : Index.shapeElement
        ; rest : parallelism
        }
    | MaxParallelism of parallelism list

  and 'c ifParallelismHitsCutoff =
    { parallelism : parallelism
    ; cutoff : int
    ; then' : (host, 'c) t
    ; else' : (host, 'c) t
    ; type' : Type.t
    }

  and ('k, 'c) kernel =
    { kernel : 'k
    ; captures : 'c
    ; blocks : int
    ; threads : int
    }

  and ('l, 'c) eseq =
    { statement : ('l, 'c) statement
    ; expr : ('l, 'c) t
    ; type' : Type.t
    }

  and ('l, 'c) putmem =
    { expr : ('l, 'c) t
    ; addr : 'l Mem.t
    ; type' : Type.t
    }

  and 'l getmem =
    { addr : 'l Mem.t
    ; type' : Type.t
    }

  and (_, _) t =
    | Ref : ref -> (_, _) t
    | BoxValue : ('l, 'c) boxValue -> ('l, 'c) t
    | IndexLet : ('l, 'c) indexLet -> ('l, 'c) t
    | MemLet : ('l, ('l, 'c) t) memLet -> ('l, 'c) t
    | ReifyDimensionIndex : reifyDimensionIndex -> (_, _) t
    | LoopBlock : ('l, 'l, sequential, 'c) loopBlock -> ('l, 'c) t
    | LoopKernel : ((host, device, parallel, 'c) loopBlock, 'c) kernel -> (host, 'c) t
    | Let : ('l, ('l, 'c) t, 'c) let' -> ('l, 'c) t
    | Box : ('l, 'c) box -> ('l, 'c) t
    | Literal : literal -> (_, _) t
    | Values : ('l, 'c) values -> ('l, 'c) t
    | ScalarPrimitive : ('l, 'c) scalarPrimitive -> ('l, 'c) t
    | TupleDeref : ('l, 'c) tupleDeref -> ('l, 'c) t
    | SubArray : ('l, 'c) subArray -> ('l, 'c) t
    | IfParallelismHitsCutoff : 'c ifParallelismHitsCutoff -> (host, 'c) t
    | Eseq : ('l, 'c) eseq -> ('l, 'c) t
    | Getmem : 'l getmem -> ('l, _) t

  and (_, _) statement =
    | Putmem : ('l, 'c) putmem -> ('l, 'c) statement
    | MapKernel : ('c mapKernel, 'c) kernel -> (host, 'c) statement
    | ComputeForSideEffects : ('l, 'c) t -> ('l, 'c) statement
    | Statements : ('l, 'c) statement list -> ('l, 'c) statement
    | SLet : ('l, ('l, 'c) statement, 'c) let' -> ('l, 'c) statement
    | SMemLet : ('l, ('l, 'c) statement) memLet -> ('l, 'c) statement
    | ReifyShapeIndex : 'l reifyShapeIndex -> ('l, _) statement

  type captures =
    { exprCaptures : Set.M(Identifier).t
    ; indexCaptures : Set.M(Identifier).t
    }

  type 'l sansCaptures = ('l, unit) t
  type 'l withCaptures = ('l, captures) t
  type 'l statementSansCaptures = ('l, unit) statement
  type 'l statementWithCaptures = ('l, captures) statement

  let rec type' : type l c. (l, c) t -> Type.t = function
    | Box box -> Atom (Sigma box.type')
    | Literal (IntLiteral _) -> Atom (Literal IntLiteral)
    | Literal (CharacterLiteral _) -> Atom (Literal CharacterLiteral)
    | Literal (BooleanLiteral _) -> Atom (Literal BooleanLiteral)
    | ScalarPrimitive scalarPrimitive -> scalarPrimitive.type'
    | TupleDeref tupleDeref -> tupleDeref.type'
    | Values values -> Tuple values.type'
    | Ref ref -> ref.type'
    | BoxValue boxValue -> boxValue.type'
    | IndexLet indexLet -> indexLet.type'
    | Let let' -> type' let'.body
    | MemLet memLet -> type' memLet.body
    | ReifyDimensionIndex _ -> Atom (Literal IntLiteral)
    | LoopBlock loopBlock -> Tuple loopBlock.type'
    | LoopKernel loopKernel -> Tuple loopKernel.kernel.type'
    | SubArray subArray -> subArray.type'
    | IfParallelismHitsCutoff ifParallelismHitsCutoff -> ifParallelismHitsCutoff.type'
    | Eseq eseq -> eseq.type'
    | Getmem getmem -> getmem.type'
  ;;

  let eseq ~statements ~expr =
    match statements with
    | [] -> expr
    | [ statement ] -> Eseq { statement; expr; type' = type' expr }
    | statements -> Eseq { statement = Statements statements; expr; type' = type' expr }
  ;;

  let putmem ~expr ~addr = Putmem { expr; addr; type' = type' expr }
  let getmem mem = Getmem { addr = mem; type' = Mem.type' mem }

  let tupleDeref ~tuple ~index =
    let extractElementType = function
      | Type.Atom _ | Type.Array _ -> raise @@ Unreachable.Error "expected tuple type"
      | Type.Tuple elements -> List.nth_exn elements index
    in
    TupleDeref { tuple; index; type' = extractElementType (type' tuple) }
  ;;

  module Sexp_of = struct
    let sexp_of_parallel (Parallel : parallel) = Sexp.Atom "parallel"
    let sexp_of_sequential (Sequential : sequential) = Sexp.Atom "sequential"
    let sexp_of_ref { id; type' = _ } = Sexp.Atom (Identifier.show id)

    let rec sexp_of_box : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) box -> Sexp.t
      =
      fun sexp_of_a sexp_of_c { indices; body; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "box"
        ; Sexp.List (List.map indices ~f:(sexp_of_boxIndex sexp_of_a sexp_of_c))
        ; sexp_of_t sexp_of_a sexp_of_c body
        ]

    and sexp_of_boxIndex
      : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) boxIndex -> Sexp.t
      =
      fun sexp_of_a sexp_of_c { expr; index } ->
      Sexp.List
        [ Sexp.List [ Sexp.Atom "expr"; sexp_of_t sexp_of_a sexp_of_c expr ]
        ; Sexp.List [ Sexp.Atom "index"; Index.sexp_of_t index ]
        ]

    and sexp_of_literal = [%sexp_of: Nested.Expr.literal]

    and sexp_of_scalarPrimitive
      : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) scalarPrimitive -> Sexp.t
      =
      fun sexp_of_a sexp_of_c { op; args; type' = _ } ->
      let opString =
        match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Equal -> "="
      in
      Sexp.List (Sexp.Atom opString :: List.map args ~f:(sexp_of_t sexp_of_a sexp_of_c))

    and sexp_of_tupleDeref
      : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) tupleDeref -> Sexp.t
      =
      fun sexp_of_a sexp_of_c { tuple; index; type' = _ } ->
      Sexp.List
        [ Sexp.Atom [%string "#%{index#Int}"]; sexp_of_t sexp_of_a sexp_of_c tuple ]

    and sexp_of_values
      : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) values -> Sexp.t
      =
      fun sexp_of_a sexp_of_c { elements; type' = _ } ->
      Sexp.List
        (Sexp.Atom "values" :: List.map elements ~f:(sexp_of_t sexp_of_a sexp_of_c))

    and sexp_of_boxValue
      : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) boxValue -> Sexp.t
      =
      fun sexp_of_a sexp_of_c { box; type' = _ } ->
      Sexp.List [ Sexp.Atom "unbox"; sexp_of_t sexp_of_a sexp_of_c box ]

    and sexp_of_indexLet
      : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) indexLet -> Sexp.t
      =
      fun sexp_of_a sexp_of_c { indexArgs; body; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "index-let"
        ; Sexp.List
            (List.map indexArgs ~f:(fun { indexBinding; indexValue; sort = _ } ->
               Sexp.List
                 (Sexp.Atom (Identifier.show indexBinding)
                  ::
                  (match indexValue with
                   | Runtime v ->
                     [ Sexp.Atom "runtime-value"; sexp_of_t sexp_of_a sexp_of_c v ]
                   | FromBox { box; i } ->
                     [ Sexp.Atom [%string "box-index-%{i#Int}"]
                     ; sexp_of_t sexp_of_a sexp_of_c box
                     ]))))
        ; sexp_of_t sexp_of_a sexp_of_c body
        ]

    and sexp_of_letArg
      : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) letArg -> Sexp.t
      =
      fun sexp_of_a sexp_of_c { binding; value } ->
      Sexp.List
        [ Sexp.Atom (Identifier.show binding); sexp_of_t sexp_of_a sexp_of_c value ]

    and sexp_of_let
      : type a s c.
        (a -> Sexp.t) -> (s -> Sexp.t) -> (c -> Sexp.t) -> (a, s, c) let' -> Sexp.t
      =
      fun sexp_of_a sexp_of_s sexp_of_c { args; body } ->
      Sexp.List
        [ Sexp.Atom "let"
        ; Sexp.List (List.map args ~f:(sexp_of_letArg sexp_of_a sexp_of_c))
        ; sexp_of_s body
        ]

    and sexp_of_reifyShapeIndex : type a. (a -> Sexp.t) -> a reifyShapeIndex -> Sexp.t =
      fun sexp_of_a { shape; mem } ->
      Sexp.List
        [ Sexp.Atom [%string "reify-shape-index"]
        ; Index.sexp_of_shape shape
        ; Sexp.List [ Sexp.Atom "mem"; Mem.sexp_of_t sexp_of_a mem ]
        ]

    and sexp_of_reifyDimensionIndex { dim } =
      Sexp.List [ Sexp.Atom "reify-dimension-index"; Index.sexp_of_dimension dim ]

    and sexp_of_memArg : type a. (a -> Sexp.t) -> a memArg -> Sexp.t =
      fun sexp_of_a { memBinding; mem } ->
      Sexp.List [ Sexp.Atom (Identifier.show memBinding); Mem.sexp_of_t sexp_of_a mem ]

    and sexp_of_memLet
      : type a s. (a -> Sexp.t) -> (s -> Sexp.t) -> (a, s) memLet -> Sexp.t
      =
      fun sexp_of_a sexp_of_s { memArgs; body } ->
      Sexp.List
        [ Sexp.Atom "mem-let"
        ; Sexp.List (List.map memArgs ~f:(sexp_of_memArg sexp_of_a))
        ; sexp_of_s body
        ]

    and sexp_of_tupleMatch = [%sexp_of: Nested.Expr.tupleMatch]

    and sexp_of_productionTuple = function
      | ProductionTuple { elements; type' = _ } ->
        Sexp.List (List.map elements ~f:sexp_of_productionTuple)
      | ProductionTupleAtom p -> Sexp.Atom (Identifier.show p.productionId)

    and sexp_of_reduce
      : type a b c.
        (a -> Sexp.t) -> (b -> Sexp.t) -> (c -> Sexp.t) -> (a, b, c) reduce -> Sexp.t
      =
      fun sexp_of_a
          sexp_of_b
          sexp_of_c
          { arg; zero; mappedMemArgs; body; d = _; character; type' = _ } ->
      let characterName =
        match character with
        | Reduce -> "reduce"
        | Scan _ -> "scan"
        | OpenScan _ -> "open-scan"
      in
      let zeroName =
        match zero with
        | Some _ -> "-zero"
        | None -> ""
      in
      let opName = [%string "%{characterName}%{zeroName}"] in
      Sexp.List
        ([ Sexp.Atom opName ]
         @ (zero |> Option.map ~f:(sexp_of_t sexp_of_a sexp_of_c) |> Option.to_list)
         @ (match mappedMemArgs with
            | [] -> []
            | _ ->
              [ Sexp.List
                  [ Sexp.Atom "mapped-mem-args"
                  ; Sexp.List (List.map mappedMemArgs ~f:(sexp_of_memArg sexp_of_a))
                  ]
              ])
         @ [ Sexp.List
               [ Sexp.Atom (Identifier.show arg.firstBinding)
               ; Sexp.Atom (Identifier.show arg.secondBinding)
               ; sexp_of_productionTuple arg.production
               ]
           ; sexp_of_t sexp_of_b sexp_of_c body
           ])

    and sexp_of_parReduce
      : type a b c.
        (a -> Sexp.t) -> (b -> Sexp.t) -> (c -> Sexp.t) -> (a, b, c) parReduce -> Sexp.t
      =
      fun sexp_of_a
          sexp_of_b
          sexp_of_c
          { reduce; interimResultMem; outerBody; outerMappedMemArgs } ->
      Sexp.List
        [ sexp_of_reduce sexp_of_a sexp_of_b sexp_of_c reduce
        ; Sexp.List
            [ Sexp.Atom "interim-result-mem"; Mem.sexp_of_t sexp_of_a interimResultMem ]
        ; Sexp.List [ Sexp.Atom "outer-body"; sexp_of_t sexp_of_a sexp_of_c outerBody ]
        ; Sexp.List
            [ Sexp.Atom "mapped-mem-args"
            ; Sexp.List (List.map outerMappedMemArgs ~f:(sexp_of_memArg sexp_of_a))
            ]
        ]

    and sexp_of_fold
      : type a b c.
        (a -> Sexp.t) -> (b -> Sexp.t) -> (c -> Sexp.t) -> (a, b, c) fold -> Sexp.t
      =
      fun sexp_of_a
          sexp_of_b
          sexp_of_c
          { zeroArg; arrayArgs; mappedMemArgs; body; d = _; character; type' = _ } ->
      let opName =
        match character with
        | Fold -> "fold"
        | Trace _ -> "trace"
        | OpenTrace _ -> "open-trace"
      in
      Sexp.List
        [ Sexp.Atom opName
        ; Sexp.List
            [ Sexp.Atom (Identifier.show zeroArg.zeroBinding)
            ; sexp_of_t sexp_of_a sexp_of_c zeroArg.zeroValue
            ]
        ; Sexp.List
            (List.map arrayArgs ~f:(fun arrayArg ->
               Sexp.List
                 ([ Sexp.Atom (Identifier.show arrayArg.binding)
                  ; Sexp.Atom (Identifier.show arrayArg.production.productionId)
                  ]
                  @ List.map mappedMemArgs ~f:(sexp_of_memArg sexp_of_a))))
        ; sexp_of_t sexp_of_b sexp_of_c body
        ]

    and sexp_of_scatter : type a. (a -> Sexp.t) -> a scatter -> Sexp.t =
      fun sexp_of_a { valuesArg; indicesArg; dIn; dOut; mem; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "scatter"
        ; Sexp.List [ Sexp.Atom "mem"; Mem.sexp_of_t sexp_of_a mem ]
        ; Index.sexp_of_dimension dIn
        ; Index.sexp_of_dimension dOut
        ; Sexp.Atom (Identifier.show valuesArg.productionId)
        ; Sexp.Atom (Identifier.show indicesArg.productionId)
        ]

    and sexp_of_consumerOp
      : type a b p c.
        (a -> Sexp.t)
        -> (b -> Sexp.t)
        -> (p -> Sexp.t)
        -> (c -> Sexp.t)
        -> (a, b, p, c) consumerOp
        -> Sexp.t
      =
      fun sexp_of_a sexp_of_b _ sexp_of_c -> function
      | ReduceSeq reduce -> sexp_of_reduce sexp_of_a sexp_of_b sexp_of_c reduce
      | ReducePar reduce -> sexp_of_parReduce sexp_of_host sexp_of_device sexp_of_c reduce
      | Fold fold -> sexp_of_fold sexp_of_a sexp_of_b sexp_of_c fold
      | Scatter scatter -> sexp_of_scatter sexp_of_a scatter

    and sexp_of_mapArg { binding; ref } =
      Sexp.List
        [ Sexp.Atom (Identifier.show binding); Sexp.Atom (Identifier.show ref.id) ]

    and sexp_of_mapIota = [%sexp_of: Nested.Expr.mapIota]

    and sexp_of_loopBlock
      : type a b p c.
        (a -> Sexp.t)
        -> (b -> Sexp.t)
        -> (p -> Sexp.t)
        -> (c -> Sexp.t)
        -> (a, b, p, c) loopBlock
        -> Sexp.t
      =
      fun sexp_of_a
          sexp_of_b
          sexp_of_p
          sexp_of_c
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
                  @ List.map mapMemArgs ~f:(sexp_of_memArg sexp_of_a))
             ]
             @ (if List.length mapIotas > 0
                then
                  [ Sexp.List (Sexp.Atom "iota" :: List.map mapIotas ~f:sexp_of_mapIota) ]
                else [])
             @ [ sexp_of_t sexp_of_b sexp_of_c mapBody ])
        ; Sexp.List [ Sexp.Atom "body-matcher"; sexp_of_tupleMatch mapBodyMatcher ]
        ; Sexp.List
            [ Sexp.Atom "map-result"
            ; Sexp.List
                (List.map mapResults ~f:(fun id -> Sexp.Atom (Identifier.show id)))
            ]
        ; Sexp.List [ Sexp.Atom "map-result-mem"; Mem.sexp_of_t sexp_of_a mapResultMem ]
        ; Sexp.List
            [ Sexp.Atom "consumer"
            ; (match consumer with
               | Some consumer ->
                 sexp_of_consumerOp sexp_of_a sexp_of_b sexp_of_p sexp_of_c consumer
               | None -> sexp_of_values sexp_of_a sexp_of_c { elements = []; type' = [] })
            ]
        ]

    and sexp_of_mapBody : type c. (c -> Sexp.t) -> c mapBody -> Sexp.t =
      fun sexp_of_c { statement; subMaps } ->
      Sexp.List
        [ Sexp.List
            [ Sexp.Atom "statement"
            ; sexp_of_statement sexp_of_device sexp_of_c statement
            ]
        ; Sexp.List
            (Sexp.Atom "sub-maps" :: List.map subMaps ~f:(sexp_of_mapKernel sexp_of_c))
        ]

    and sexp_of_mapKernel : type c. (c -> Sexp.t) -> c mapKernel -> Sexp.t =
      fun sexp_of_c
          { frameShape
          ; mapArgs
          ; mapMemArgs
          ; mapIotas
          ; mapBody
          ; mapBodyMatcher
          ; mapResults
          ; mapResultMem
          ; type' = _
          } ->
      Sexp.List
        ([ Sexp.Atom "map-kernel"
         ; Sexp.List [ Sexp.Atom "frame-shape"; Index.sexp_of_shapeElement frameShape ]
         ; Sexp.List
             (List.map mapArgs ~f:sexp_of_mapArg
              @ List.map mapMemArgs ~f:(sexp_of_memArg sexp_of_host))
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
            :: Sexp.List
                 [ Sexp.Atom "map-result-mem"; Mem.sexp_of_t sexp_of_host mapResultMem ]
            :: [ sexp_of_mapBody sexp_of_c mapBody ]))

    and sexp_of_subArray
      : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) subArray -> Sexp.t
      =
      fun sexp_of_a sexp_of_c { arrayArg; indexArg; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "index"
        ; sexp_of_t sexp_of_a sexp_of_c arrayArg
        ; sexp_of_t sexp_of_a sexp_of_c indexArg
        ]

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

    and sexp_of_ifParallelismHitsCutoff
      : type c. (c -> Sexp.t) -> c ifParallelismHitsCutoff -> Sexp.t
      =
      fun sexp_of_c { parallelism; cutoff; then'; else'; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "if"
        ; Sexp.List
            [ Sexp.Atom ">="
            ; Sexp.List
                [ Sexp.Atom "parallelism-factor"; sexp_of_parallelism parallelism ]
            ; Sexp.Atom (Int.to_string cutoff)
            ]
        ; sexp_of_t sexp_of_host sexp_of_c then'
        ; sexp_of_t sexp_of_host sexp_of_c else'
        ]

    and sexp_of_kernel
      : type k c. (k -> Sexp.t) -> (c -> Sexp.t) -> (k, c) kernel -> Sexp.t
      =
      fun sexp_of_k sexp_of_c { kernel; captures; blocks; threads } ->
      Sexp.List
        ((Sexp.Atom "kernel"
          ::
          (match sexp_of_c captures with
           | Sexp.List [] -> []
           | capturesSexp -> [ Sexp.Atom "captures"; capturesSexp ]))
         @ [ Sexp.List [ Sexp.Atom "blocks"; Int.sexp_of_t blocks ]
           ; Sexp.List [ Sexp.Atom "threads"; Int.sexp_of_t threads ]
           ]
         @ (sexp_of_k kernel :: []))

    and sexp_of_eseq : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) eseq -> Sexp.t =
      fun sexp_of_a sexp_of_c eseq ->
      let rec flattenEseq { statement; expr; type' = _ } =
        match expr with
        | Eseq eseq ->
          let restStatements, expr = flattenEseq eseq in
          statement :: restStatements, expr
        | _ -> [ statement ], expr
      in
      let statements, expr = flattenEseq eseq in
      Sexp.List
        ((Sexp.Atom "begin"
          :: List.map statements ~f:(sexp_of_statement sexp_of_a sexp_of_c))
         @ [ sexp_of_t sexp_of_a sexp_of_c expr ])

    and sexp_of_getmem : type a. (a -> Sexp.t) -> a getmem -> Sexp.t =
      fun sexp_of_a { addr; type' = _ } ->
      Sexp.List [ Sexp.Atom "getmem"; Mem.sexp_of_t sexp_of_a addr ]

    and sexp_of_putmem
      : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) putmem -> Sexp.t
      =
      fun sexp_of_a sexp_of_c { expr; addr; type' = _ } ->
      Sexp.List
        [ Sexp.Atom "putmem"
        ; sexp_of_t sexp_of_a sexp_of_c expr
        ; Mem.sexp_of_t sexp_of_a addr
        ]

    and sexp_of_t : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) t -> Sexp.t =
      fun sexp_of_a sexp_of_c expr ->
      match expr with
      | Box box -> sexp_of_box sexp_of_a sexp_of_c box
      | Literal lit -> sexp_of_literal lit
      | ScalarPrimitive scalarPrimitive ->
        sexp_of_scalarPrimitive sexp_of_a sexp_of_c scalarPrimitive
      | TupleDeref tupleDeref -> sexp_of_tupleDeref sexp_of_a sexp_of_c tupleDeref
      | Values values -> sexp_of_values sexp_of_a sexp_of_c values
      | Ref ref -> sexp_of_ref ref
      | BoxValue boxValue -> sexp_of_boxValue sexp_of_a sexp_of_c boxValue
      | IndexLet indexLet -> sexp_of_indexLet sexp_of_a sexp_of_c indexLet
      | Let let' -> sexp_of_let sexp_of_a (sexp_of_t sexp_of_a sexp_of_c) sexp_of_c let'
      | ReifyDimensionIndex reifyDimensionIndex ->
        sexp_of_reifyDimensionIndex reifyDimensionIndex
      | LoopBlock loopBlock ->
        sexp_of_loopBlock sexp_of_a sexp_of_a sexp_of_sequential sexp_of_c loopBlock
      | LoopKernel loopKernel ->
        sexp_of_kernel
          (sexp_of_loopBlock sexp_of_host sexp_of_device sexp_of_parallel sexp_of_c)
          sexp_of_c
          loopKernel
      | SubArray subArray -> sexp_of_subArray sexp_of_a sexp_of_c subArray
      | IfParallelismHitsCutoff ifParallelismHitsCutoff ->
        sexp_of_ifParallelismHitsCutoff sexp_of_c ifParallelismHitsCutoff
      | Eseq eseq -> sexp_of_eseq sexp_of_a sexp_of_c eseq
      | Getmem getmem -> sexp_of_getmem sexp_of_a getmem
      | MemLet memLet -> sexp_of_memLet sexp_of_a (sexp_of_t sexp_of_a sexp_of_c) memLet

    and sexp_of_statement
      : type a c. (a -> Sexp.t) -> (c -> Sexp.t) -> (a, c) statement -> Sexp.t
      =
      fun sexp_of_a sexp_of_c statement ->
      match statement with
      | Putmem putmem -> sexp_of_putmem sexp_of_a sexp_of_c putmem
      | MapKernel mapKernel ->
        sexp_of_kernel (sexp_of_mapKernel sexp_of_c) sexp_of_c mapKernel
      | ComputeForSideEffects expr ->
        Sexp.List [ Sexp.Atom "do-expr"; sexp_of_t sexp_of_a sexp_of_c expr ]
      | Statements statements ->
        Sexp.List
          (Sexp.Atom "begin-do"
           :: List.map statements ~f:(sexp_of_statement sexp_of_a sexp_of_c))
      | SLet let' ->
        sexp_of_let sexp_of_a (sexp_of_statement sexp_of_a sexp_of_c) sexp_of_c let'
      | SMemLet memLet ->
        sexp_of_memLet sexp_of_a (sexp_of_statement sexp_of_a sexp_of_c) memLet
      | ReifyShapeIndex reifyShapeIndex ->
        sexp_of_reifyShapeIndex sexp_of_a reifyShapeIndex
    ;;

    let sexp_of_captures { exprCaptures; indexCaptures } =
      Sexp.List
        [ Sexp.List
            [ Sexp.Atom "expr-captures"; [%sexp_of: Set.M(Identifier).t] exprCaptures ]
        ; Sexp.List
            [ Sexp.Atom "index-captures"; [%sexp_of: Set.M(Identifier).t] indexCaptures ]
        ]
    ;;
  end

  include Sexp_of
end

type 'c t = (host, 'c) Expr.t [@@deriving sexp_of]
type sansCaptures = unit t [@@deriving sexp_of]
type withCaptures = Expr.captures t [@@deriving sexp_of]

module SansCaptures = struct
  type t = sansCaptures [@@deriving sexp_of]
end

module WithCaptures = struct
  type t = withCaptures [@@deriving sexp_of]
end
