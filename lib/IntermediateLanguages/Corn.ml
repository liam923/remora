open! Base

(* The Corn language represents a flattened, kernelized Remora program *)

module Index = Nested.Index

module Type = struct
  type array =
    { element : t
    ; size : Index.shape
    }

  and 't param = 't Typed.param

  and sigma =
    { parameters : Sort.t param list
    ; body : t
    }

  and tuple = t list
  and literal = Nucleus.Type.literal

  and t =
    | Array of array
    | Sigma of sigma
    | Literal of literal
    | Tuple of tuple
  [@@deriving sexp_of, equal, compare]
end

module Expr = struct
  type host = Host [@@deriving sexp_of]
  type device = Device [@@deriving sexp_of]

  type ref =
    { id : Identifier.t
    ; type' : Type.t
    }
  [@@deriving sexp_of]

  type reduceCharacter =
    [ `Reduce
    | `Scan
    | `OpenScan
    ]
  [@@deriving sexp_of]

  type foldCharacter =
    [ `Fold
    | `Trace
    | `OpenTrace
    ]
  [@@deriving sexp_of]

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

  and reifyIndex =
    { index : Index.t
    ; type' : Type.t
    }

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

  and scalarOp = Nucleus.Expr.scalarOp

  and 'l scalarPrimitive =
    { op : scalarOp
    ; args : 'l t list
    ; type' : Type.t
    }

  and tupleMatch =
    | Binding of Identifier.t
    | Unpack of tupleMatch list

  and mapArg =
    { binding : Identifier.t
    ; ref : ref
    }

  and mapIota =
    { iota : Identifier.t
    ; nestIn : Identifier.t option
    }

  (** returns a tuple of (map results (tuple of arrays, not array of tuples), consumer result (unit if None)) *)
  and ('l, 'consumerOp) loopBlock =
    { frameShape : Index.shapeElement
    ; mapArgs : mapArg list
    ; mapIotas : mapIota list
    ; mapBody : 'l t
    ; mapBodyMatcher : tupleMatch
    ; mapResults : Identifier.t list
    ; consumer : 'consumerOp option
    ; type' : Type.tuple
    }

  and 'l foldZeroArg =
    { zeroBinding : Identifier.t
    ; zeroValue : 'l t
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

  and 'l reduce =
    { arg : reduceArg
    ; zero : 'l t option
    ; body : 'l t
    ; d : Index.dimension
    ; itemPad : Index.shape
    ; associative : bool
    ; character : reduceCharacter
    ; type' : Type.t
    }

  and 'l fold =
    { zeroArg : 'l foldZeroArg
    ; arrayArgs : foldArrayArg list
    ; body : 'l t
    ; d : Index.dimension
    ; itemPad : Index.shape
    ; character : foldCharacter
    ; type' : Type.t
    }

  and 'l scatter =
    { valuesArg : production
    ; indicesArg : production
    ; dIn : Index.dimension
    ; dOut : Index.dimension
    ; type' : Type.t
    }

  and 'l parallelizableConsumerOp =
    [ `Reduce of 'l reduce
    | `Scatter of 'l scatter
    ]

  and 'l consumerOp =
    [ `Reduce of 'l reduce
    | `Scatter of 'l scatter
    | `Fold of 'l fold
    ]

  and mapTuple =
    | MapValue of mapKernel
    | MapValues of mapTuple list

  and kernelMapBody =
    | Expr of device t
    | MapTuple of mapTuple

  and mapKernel =
    { frameShape : Index.shapeElement
    ; mapArgs : mapArg list
    ; mapIotas : mapIota list
    ; mapBody : kernelMapBody
    ; mapBodyMatcher : tupleMatch
    ; mapResults : Identifier.t list
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

  and literal = Nucleus.Expr.literal

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

  and _ t =
    | Ref : ref -> _ t
    | Frame : 'l frame -> 'l t
    | BoxValue : 'l boxValue -> 'l t
    | IndexLet : 'l indexLet -> 'l t
    | ReifyIndex : reifyIndex -> _ t
    | Let : 'l let' -> 'l t
    | LoopBlock : ('l, 'l consumerOp) loopBlock -> 'l t
    | LoopKernel : (device, device parallelizableConsumerOp) loopBlock -> host t
    | MapKernel : mapKernel -> host t
    | Box : 'l box -> 'l t
    | Literal : literal -> _ t
    | Values : 'l values -> 'l t
    | ScalarPrimitive : 'l scalarPrimitive -> 'l t
    | TupleDeref : 'l tupleDeref -> 'l t
    | SubArray : 'l subArray -> 'l t
    | Append : 'l append -> 'l t
    | Zip : 'l zip -> 'l t
    | Unzip : 'l unzip -> 'l t
  [@@deriving sexp_of]

  let type' : _ t -> Type.t = function
    | Box box -> Sigma box.type'
    | Literal (IntLiteral _) -> Literal IntLiteral
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
    | LoopKernel loopKernel -> Tuple loopKernel.type'
    | MapKernel mapKernel -> mapKernel.type'
    | SubArray subArray -> subArray.type'
    | Append append -> append.type'
    | Zip zip -> zip.type'
    | Unzip unzip -> Tuple unzip.type'
  ;;

  let consumerOpType = function
    | `Reduce reduce -> reduce.type'
    | `Fold fold -> fold.type'
    | `Scatter scatter -> scatter.type'
  ;;

  let productionTupleType = function
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
end

type t = Expr.host Expr.t

let sexp_of_t = Expr.sexp_of_t
