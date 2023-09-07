open! Base

(* The Nucleus language represents a monomorphized Remora program where
   all function calls have been inlined (besides intrinsic ones like map) *)

type 't param = 't Typed.param [@@deriving sexp, compare, equal]

module Index = Typed.Index

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
    | BooleanLiteral
    | UnitLiteral

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
  [@@deriving sexp, equal]

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

  and primitiveOp =
    | Add
    | Sub
    | Mul
    | Div
    | Equal

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
        { frameShape : Index.shape
        ; args : mapArg list
        ; body : array
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
    | Scan of
        { args : reduceArg list
        ; body : array
        ; t : Type.atom
        ; dSub1 : Index.dimension
        ; itemPad : Index.shape
        ; cellShape : Index.shape
        ; type' : Type.array
        }
    | Filter of
        { array : array
        ; flags : array
        ; t : Type.atom
        ; d : Index.dimension
        ; cellShape : Index.shape
        ; type' : Type.array
        }
    | Append of
        { arg1 : array
        ; arg2 : array
        ; t : Type.atom
        ; d1 : Index.dimension
        ; d2 : Index.dimension
        ; cellShape : Index.shape
        ; type' : Type.array
        }

  and literal =
    | IntLiteral of int
    | CharacterLiteral of char
    | BooleanLiteral of bool
    | UnitLiteral

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
  [@@deriving sexp, equal]

  let atomType : atom -> Type.atom = function
    | Box box -> Sigma box.type'
    | Literal (IntLiteral _) -> Literal IntLiteral
    | Literal (CharacterLiteral _) -> Literal CharacterLiteral
    | Literal (BooleanLiteral _) -> Literal BooleanLiteral
    | Literal UnitLiteral -> Literal UnitLiteral
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
       | Reduce reduce -> reduce.type'
       | Scan scan -> scan.type'
       | Filter filter -> filter.type'
       | Append append -> append.type')
  ;;
end

type t = Expr.array [@@deriving sexp]
