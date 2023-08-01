open! Base

(* The InlineNucleus language represents a monomorphized Remora program where
   all function calls have been inlined (besides intrinsic ones like map) *)

type 't param = 't Nucleus.param [@@deriving sexp, compare, equal]

module Index = Nucleus.Index

module Type = struct
  type array =
    { element : atom
    ; shape : Index.shape
    }

  and 't abstraction =
    { parameters : 't param list
    ; body : array
    }

  and sigma = Sort.t abstraction

  and literal =
    | IntLiteral
    | CharacterLiteral
    | Unit

  and atom =
    | Sigma of sigma
    | Literal of literal

  and t =
    | Array of array
    | Atom of atom
  [@@deriving sexp, compare, equal]

  let kind = function
    | Array _ -> Kind.Array
    | Atom _ -> Kind.Atom
  ;;
end

module Expr = struct
  type ref =
    { id : Identifier.t
    ; type' : Type.array
    }
  [@@deriving sexp]

  type scalar =
    { element : atom
    ; type' : Type.array
    }

  and frame =
    { dimensions : int list
    ; elements : array list
    ; type' : Type.array
    }

  and unboxBinding =
    { binding : Identifier.t
    ; box : array
    }

  and unbox =
    { indexBindings : Identifier.t list
    ; boxBindings : unboxBinding list
    ; body : array
    ; type' : Type.array
    }

  and box =
    { indices : Index.t list
    ; body : array
    ; bodyType : Type.array
    ; type' : Type.sigma
    }

  and tupleLet =
    { params : Type.atom param list
    ; value : array
    ; body : array
    ; type' : Type.array
    }

  and primitiveOp =
    | Add
    | Sub
    | Mul
    | Div

  and primitiveCall =
    { op : primitiveOp
    ; args : array list
    ; type' : Type.array
    }

  and mapArg =
    { binding : Identifier.t
    ; value : array
    }

  and reduceArg =
    { firstBinding : Identifier.t
    ; secondBinding : Identifier.t
    ; value : array
    }

  and intrinsicCall =
    | Map of
        { args : mapArg list
        ; body : array
        ; frameShape : Index.shape
        ; type' : Type.array
        }
    | Length of
        { arg : array
        ; t : Type.atom
        ; d : Index.dimension
        ; cellShape : Index.shape
        ; type' : Type.array
        }
    | Reduce of
        { args : reduceArg list
        ; body : array
        ; t : Type.atom
        ; dSub1 : Index.dimension
        ; itemPad : Index.shape
        ; cellShape : Index.shape
        ; type' : Type.array
        }

  and literal =
    | IntLiteral of int
    | CharacterLiteral of char
    | Unit

  and array =
    | Ref of ref
    | Scalar of scalar
    | Frame of frame
    | Unbox of unbox
    | PrimitiveCall of primitiveCall
    | IntrinsicCall of intrinsicCall

  and atom =
    | Box of box
    | Literal of literal

  and t =
    | Array of array
    | Atom of atom
  [@@deriving sexp]

  let atomType : atom -> Type.atom = function
    | Box box -> Sigma box.type'
    | Literal (IntLiteral _) -> Literal IntLiteral
    | Literal (CharacterLiteral _) -> Literal CharacterLiteral
    | Literal Unit -> Literal Unit
  ;;

  let arrayType : array -> Type.array = function
    | Ref ref -> ref.type'
    | Scalar scalar -> scalar.type'
    | Frame frame -> frame.type'
    | Unbox unbox -> unbox.type'
    | PrimitiveCall primitiveCall -> primitiveCall.type'
    | IntrinsicCall instrinsicCall ->
      (match instrinsicCall with
       | Map map -> map.type'
       | Length length -> length.type'
       | Reduce reduce -> reduce.type')
  ;;
end

type t = Expr.array [@@deriving sexp]
