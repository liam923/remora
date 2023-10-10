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

  and letBinding =
    { binding : Identifier.t
    ; value : t
    }

  and let' =
    { args : letBinding list
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

  (** A consumer block has the semantics of performing all the map operations in
      the production list and then performing the (optional) consumption. Then
      the body is performed and returned. Producer may use corresponding
      elements of previous productions in the list. The result of productions
      are optionally available via bindings to the consumption and to the body.
      The consumption's result is also available to the body. *)
  and consumerBlock =
    { frameShape : Index.shapeElement
        (** The shape which is being operated over. For the productions,
            this is the frame of the mapping *)
    ; producers : boundProducerOp list
    ; consumer : boundConsumerOp option
    ; body : t
    ; type' : Type.t
    }

  (** Represents a reading of an array or *)
  and boundProducerOp =
    { op : producerOp (** The expression that generates values of an array *)
    ; localBinding : Identifier.t
        (** Elements of basis are available to the bodies of subsequent
            mapping productions via this id *)
    ; consumerBinding : Identifier.t
        (** The result of this production can be input to the consumer via this id *)
    ; bodyBinding : Identifier.t option
        (** The result of this production can be used in the body of the consumer via this id *)
    ; type' : Type.t (** The type of the resulting array *)
    }

  and producerOp =
    | Read of t (** Read simply reads values of an array. *)
    | Map of
        { iotaVar : Identifier.t option
        ; body : t
        } (** Map generates values via the body expression *)

  and boundConsumerOp =
    { op : consumerOp
    ; binding : Identifier.t
    }

  and arg =
    { binding : Identifier.t
    ; production : ref
    }

  and reduceArg =
    { firstBinding : Identifier.t
    ; secondBinding : Identifier.t
    ; production : ref
    }

  and consumerOp =
    | Reduce of
        { args : reduceArg list
        ; zero : t option
        ; body : t
        ; d : Index.dimension
        ; itemPad : Index.shape
        ; cellShape : Index.shape
        ; associative : bool
        ; character : [ `Reduce | `Scan | `OpenScan ]
        ; type' : Type.t
        }
    | Fold of
        { zeroArgs : arg list
        ; arrayArgs : arg list
        ; body : t
        ; d : Index.dimension
        ; itemPad : Index.shape
        ; cellShape : Index.shape
        ; character : [ `Fold | `Trace | `OpenTrace ]
        ; type' : Type.t
        }
    | Scatter of
        { valuesArg : ref
        ; indicesArg : ref
        ; dIn : Index.dimension
        ; dOut : Index.dimension
        ; cellShape : Index.shape
        ; type' : Type.t
        }

  and values =
    { elements : t list
    ; type' : Type.tuple
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

  and shapeElementSize = Index.shapeElement

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
    | SubArray of subArray
    | Append of append
    | ShapeElementSize of shapeElementSize
  [@@deriving sexp_of]

  let type' : t -> Type.t = function
    | Box box -> Sigma box.type'
    | Literal (IntLiteral _) -> Literal IntLiteral
    | Literal (CharacterLiteral _) -> Literal CharacterLiteral
    | Literal (BooleanLiteral _) -> Literal BooleanLiteral
    | ScalarPrimitive scalarPrimitive -> scalarPrimitive.type'
    | Values values -> Tuple values.type'
    | Ref ref -> ref.type'
    | Frame frame -> frame.type'
    | BoxValue boxValue -> boxValue.type'
    | IndexLet indexLet -> indexLet.type'
    | Let let' -> let'.type'
    | ReifyIndex reifyIndex -> reifyIndex.type'
    | ConsumerBlock consumerBlock -> consumerBlock.type'
    | SubArray subArray -> subArray.type'
    | Append append -> append.type'
    | ShapeElementSize _ -> Literal IntLiteral
  ;;
end

type t = Expr.t [@@deriving sexp_of]
