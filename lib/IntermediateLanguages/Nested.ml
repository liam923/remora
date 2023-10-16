open! Base

(* The Nucleus language represents a monomorphized Remora program where
   all function calls have been inlined (besides intrinsic ones like map) *)

module Index = Nucleus.Index

module Type = struct
  type array =
    { element : t
    ; size : Index.shapeElement
    }

  and 't param = 't Typed.param

  and 't abstraction =
    { parameters : 't param list
    ; body : t
    }

  and sigma = Sort.t abstraction
  and tuple = t list
  and literal = Nucleus.Type.literal

  and t =
    | Array of array
    | Sigma of sigma
    | Literal of literal
    | Tuple of tuple
  [@@deriving sexp_of]
end

module Expr = struct
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
  [@@deriving sexp_of, equal]

  type foldCharacter =
    [ `Fold
    | `Trace
    | `OpenTrace
    ]
  [@@deriving sexp_of, equal]

  type frame =
    { dimensions : int list
    ; elements : t list
    ; type' : Type.t
    }

  and boxValue =
    { box : t
    ; type' : Type.t
    }

  and indexValue =
    | Runtime of t
    | FromBox of
        { box : t
        ; i : int
        }

  and indexArg =
    { indexBinding : Identifier.t
    ; indexValue : indexValue
    ; sort : Sort.t
    }

  and indexLet =
    { indexArgs : indexArg list
    ; body : t
    ; type' : Type.t
    }

  and reifyIndex =
    { index : Index.t
    ; type' : Type.t
    }

  and letArg =
    { binding : Identifier.t
    ; value : t
    }

  and let' =
    { args : letArg list
    ; body : t
    ; type' : Type.t
    }

  and box =
    { indices : Index.t list
    ; body : t
    ; bodyType : Type.t
    ; type' : Type.sigma
    }

  and scalarOp = Nucleus.Expr.scalarOp

  and scalarPrimitive =
    { op : scalarOp
    ; args : t list
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
  and consumerBlock =
    { frameShape : Index.shapeElement
    ; mapArgs : mapArg list
    ; mapIotas : mapIota list
    ; mapBody : t
    ; mapBodyMatcher : tupleMatch
    ; mapResults : Identifier.t list
    ; consumer : consumerOp option
    ; type' : Type.tuple
    }

  and foldZeroArg =
    { zeroBinding : Identifier.t
    ; zeroValue : t
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

  and consumerOp =
    | Reduce of
        { arg : reduceArg
        ; zero : t option
        ; body : t
        ; d : Index.dimension
        ; itemPad : Index.shape
        ; associative : bool
        ; character : reduceCharacter
        ; type' : Type.t
        }
    | Fold of
        { zeroArg : foldZeroArg
        ; arrayArgs : foldArrayArg list
        ; body : t
        ; d : Index.dimension
        ; itemPad : Index.shape
        ; character : foldCharacter
        ; type' : Type.t
        }
    | Scatter of
        { valuesArg : production
        ; indicesArg : production
        ; dIn : Index.dimension
        ; dOut : Index.dimension
        ; type' : Type.t
        }

  and values =
    { elements : t list
    ; type' : Type.tuple
    }

  and tupleDeref =
    { index : int
    ; tuple : t
    ; type' : Type.t
    }

  and literal = Nucleus.Expr.literal

  and subArray =
    { arrayArg : t
    ; indexArg : t
    ; type' : Type.t
    }

  and append =
    { args : t list
    ; type' : Type.t
    }

  and t =
    | Ref of ref
    | Frame of frame
    | BoxValue of boxValue
    | IndexLet of indexLet
    | ReifyIndex of reifyIndex
    | Let of let'
    | ConsumerBlock of consumerBlock
    | Box of box
    | Literal of literal
    | Values of values
    | ScalarPrimitive of scalarPrimitive
    | TupleDeref of tupleDeref
    | SubArray of subArray
    | Append of append
  [@@deriving sexp_of]

  let type' : t -> Type.t = function
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
    | ConsumerBlock consumerBlock -> Tuple consumerBlock.type'
    | SubArray subArray -> subArray.type'
    | Append append -> append.type'
  ;;

  let consumerOpType = function
    | Reduce reduce -> reduce.type'
    | Fold fold -> fold.type'
    | Scatter scatter -> scatter.type'
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
end

type t = Expr.t [@@deriving sexp_of]
