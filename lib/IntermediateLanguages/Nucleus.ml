open! Base

(* The Nucleus language represents a monomorphized Remora program where
   all function calls have been inlined (besides intrinsic ones like map) *)

module Index = Typed.Index

module Type = struct
  type array =
    { element : atom
    ; shape : Index.shape
    }

  and sigmaParam =
    { binding : Identifier.t
    ; bound : Sort.t
    }

  and sigma =
    { parameters : sigmaParam list
    ; body : array
    }

  and tuple = atom list

  and literal =
    | IntLiteral
    | FloatLiteral
    | CharacterLiteral
    | BooleanLiteral

  and atom =
    | Sigma of sigma
    | Literal of literal
    | Tuple of tuple

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

  type atomicArray =
    { element : atom
    ; type' : Type.array
    }

  and arrayAtomic =
    { array : array
    ; type' : Type.atom
    }

  and frame =
    { dimensions : int list
    ; elements : array list
    ; type' : Type.array
    }

  and boxValue =
    { box : array
    ; type' : Type.array
    }

  and indexValue =
    | Runtime of array
    | FromBox of
        { box : array
        ; i : int
        }

  and indexArg =
    { indexBinding : Identifier.t
    ; indexValue : indexValue
    ; sort : Sort.t
    }

  and indexLet =
    { indexArgs : indexArg list
    ; body : array
    ; type' : Type.array
    }

  and reifyIndex =
    { index : Index.t
    ; type' : Type.array
    }

  and box =
    { indices : Index.t list
    ; body : array
    ; bodyType : Type.array
    ; type' : Type.sigma
    }

  and scalarOp =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | AddF
    | SubF
    | MulF
    | DivF
    | IntToBool
    | BoolToInt
    | IntToFloat
    | FloatToInt
    | Equal

  and atomicPrimitive =
    { op : scalarOp
    ; args : atom list
    ; type' : Type.atom
    }

  and arg =
    { binding : Identifier.t
    ; value : array
    }

  and reduceArg =
    { firstBinding : Identifier.t
    ; secondBinding : Identifier.t
    ; value : array
    }

  and reduceCharacter = Typed.Expr.reduceCharacter
  and foldCharacter = Typed.Expr.foldCharacter

  and arrayPrimitive =
    | Map of
        { frameShape : Index.shape
        ; args : arg list
        ; iotaVar : Identifier.t option
             [@default None] [@sexp_drop_if fun i -> Option.is_none i]
        ; body : array
        ; type' : Type.array
        }
    | Reduce of
        { arg : reduceArg
        ; zero : array option
        ; body : array
        ; d : Index.dimension
        ; cellShape : Index.shape
        ; associative : bool
        ; character : reduceCharacter
        ; type' : Type.array
        }
    | Fold of
        { zeroArg : arg
        ; arrayArgs : arg list
        ; body : array
        ; d : Index.dimension
        ; cellShape : Index.shape
        ; character : foldCharacter
        ; type' : Type.array
        }
    | Append of
        { arg1 : array
        ; arg2 : array
        ; d1 : Index.dimension
        ; d2 : Index.dimension
        ; cellShape : Index.shape
        ; type' : Type.array
        }
    | Index of
        { arrayArg : array
        ; indexArg : array
        ; s : Index.shape
        ; cellShape : Index.shape
        ; l : Index.dimension
        ; type' : Type.array
        }
    | Scatter of
        { valuesArg : array
        ; indicesArg : array
        ; dIn : Index.dimension
        ; dOut : Index.dimension
        ; cellShape : Index.shape
        ; type' : Type.array
        }

  and values =
    { elements : atom list
    ; type' : Type.tuple
    }

  and tupleDeref =
    { tuple : atom
    ; index : int
    ; type' : Type.atom
    }

  and literal =
    | IntLiteral of int
    | FloatLiteral of float
    | CharacterLiteral of char
    | BooleanLiteral of bool

  and array =
    | Ref of ref
    | AtomAsArray of atomicArray
    | Frame of frame
    | BoxValue of boxValue
    | IndexLet of indexLet
    | ReifyIndex of reifyIndex
    | ArrayPrimitive of arrayPrimitive

  and atom =
    | Box of box
    | Literal of literal
    | Values of values
    | ArrayAsAtom of arrayAtomic
    | AtomicPrimitive of atomicPrimitive
    | TupleDeref of tupleDeref

  and t =
    | Array of array
    | Atom of atom
  [@@deriving sexp, equal]

  let atomType : atom -> Type.atom = function
    | Box box -> Sigma box.type'
    | Literal (IntLiteral _) -> Literal IntLiteral
    | Literal (FloatLiteral _) -> Literal FloatLiteral
    | Literal (CharacterLiteral _) -> Literal CharacterLiteral
    | Literal (BooleanLiteral _) -> Literal BooleanLiteral
    | ArrayAsAtom arrayAtomic -> arrayAtomic.type'
    | AtomicPrimitive atomicPrimitive -> atomicPrimitive.type'
    | Values values -> Tuple values.type'
    | TupleDeref tupleDeref -> tupleDeref.type'
  ;;

  let arrayType : array -> Type.array = function
    | Ref ref -> ref.type'
    | AtomAsArray atomicArray -> atomicArray.type'
    | Frame frame -> frame.type'
    | BoxValue boxValue -> boxValue.type'
    | IndexLet indexLet -> indexLet.type'
    | ReifyIndex reifyIndex -> reifyIndex.type'
    | ArrayPrimitive arrayPrimitive ->
      (match arrayPrimitive with
       | Map map -> map.type'
       | Reduce reduce -> reduce.type'
       | Fold fold -> fold.type'
       | Append append -> append.type'
       | Index index -> index.type'
       | Scatter scatter -> scatter.type')
  ;;
end

type t = Expr.array [@@deriving sexp]
