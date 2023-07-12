open! Base

(* The AST language represents a typed Remora program with no optimizations
   yet performed*)

module Identifier = struct
  module T = struct
    type t =
      { name : string
      ; id : int
      }
    [@@deriving compare, sexp, equal]
  end

  include T
  include Comparator.Make (T)
end

type 't param =
  { binding : Identifier.t
  ; bound : 't
  }
[@@deriving sexp]

module Index = struct
  type dimension =
    { const : int
    ; refs : int Map.M(Identifier).t
    }
  [@@deriving sexp]

  type shapeElement =
    | Add of dimension
    | ShapeRef of Identifier.t
  [@@deriving sexp]

  type shape = shapeElement list [@@deriving sexp]

  type t =
    | Dimension of dimension
    | Shape of shape
  [@@deriving sexp]

  let dimensionConstant n = { const = n; refs = Map.empty (module Identifier) }
  let dimensionRef r = { const = 0; refs = Map.singleton (module Identifier) r 1 }

  let sort = function
    | Dimension _ -> Sort.Dim
    | Shape _ -> Sort.Shape
  ;;
end

module Type = struct
  type arr =
    { element : atom
    ; shape : Index.shape
    }

  and func =
    { parameters : array list
    ; return : array
    }

  and 't abstraction =
    { parameters : 't param list
    ; body : array
    }

  and forall = Kind.t abstraction
  and pi = Sort.t abstraction
  and sigma = Sort.t abstraction
  and tuple = atom list

  and array =
    | ArrayRef of Identifier.t
    | Arr of arr

  and atom =
    | AtomRef of Identifier.t
    | Func of func
    | Forall of forall
    | Pi of pi
    | Sigma of sigma
    | Tuple of tuple

  and t =
    | Array of array
    | Atom of atom
  [@@deriving sexp]

  let kind = function
    | Array _ -> Kind.Array
    | Atom _ -> Kind.Atom
  ;;
end

module Expr = struct
  type ref =
    { id : Identifier.t
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }
  [@@deriving sexp]

  type arr =
    { dimensions : int list
    ; elements : atom list
    ; type' : Type.arr [@sexp_drop_if fun _ -> true]
    }

  and frame =
    { dimensions : int list
    ; arrays : array list
    ; type' : Type.arr [@sexp_drop_if fun _ -> true]
    }

  and termApplication =
    { func : array
    ; args : array list
    ; type' : Type.arr [@sexp_drop_if fun _ -> true]
    }

  and typeApplication =
    { tFunc : array
    ; args : Type.t list
    ; type' : Type.arr [@sexp_drop_if fun _ -> true]
    }

  and indexApplication =
    { iFunc : array
    ; args : Index.t list
    ; type' : Type.arr [@sexp_drop_if fun _ -> true]
    }

  and unbox =
    { indexBindings : Identifier.t list
    ; valueBinding : Identifier.t
    ; box : array
    ; body : array
    ; type' : Type.arr [@sexp_drop_if fun _ -> true]
    }

  and termLambda =
    { params : Type.array param list
    ; body : t
    ; type' : Type.func [@sexp_drop_if fun _ -> true]
    }

  and typeLambda =
    { params : Kind.t param list
    ; body : array
    ; type' : Type.forall [@sexp_drop_if fun _ -> true]
    }

  and indexLambda =
    { params : Sort.t param list
    ; body : array
    ; type' : Type.pi [@sexp_drop_if fun _ -> true]
    }

  and box =
    { indices : Index.t list
    ; body : array
    ; bodyType : Type.array
    ; type' : Type.sigma [@sexp_drop_if fun _ -> true]
    }

  and let' =
    { binding : Identifier.t
    ; value : array
    ; body : array
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }

  and tupleLet =
    { params : Type.atom param list
    ; value : array
    ; body : array
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }

  and tuple =
    { elements : atom list
    ; type' : Type.tuple [@sexp_drop_if fun _ -> true]
    }

  and literalValue =
    | IntLiteral of int
    | CharacterLiteral of char

  and literal =
    { value : literalValue
    ; type' : Type.atom [@sexp_drop_if fun _ -> true]
    }

  and array =
    | Ref of ref
    | Arr of arr
    | Frame of frame
    | TermApplication of termApplication
    | TypeApplication of typeApplication
    | IndexApplication of indexApplication
    | Unbox of unbox
    | Let of let'
    | TupleLet of tupleLet

  and atom =
    | TermLambda of termLambda
    | TypeLambda of typeLambda
    | IndexLambda of indexLambda
    | Box of box
    | Tuple of tuple
    | Literal of literal

  and t =
    | Array of array
    | Atom of atom
  [@@deriving sexp]

  let atomType : atom -> Type.atom = function
    | TermLambda termLambda -> Func termLambda.type'
    | TypeLambda typeLambda -> Forall typeLambda.type'
    | IndexLambda indexLambda -> Pi indexLambda.type'
    | Box box -> Sigma box.type'
    | Tuple tuple -> Tuple tuple.type'
    | Literal literal -> literal.type'
  ;;

  let arrayType : array -> Type.array = function
    | Ref ref -> ref.type'
    | Arr arr -> Arr arr.type'
    | Frame frame -> Arr frame.type'
    | TermApplication termApplication -> Arr termApplication.type'
    | TypeApplication typeApplication -> Arr typeApplication.type'
    | IndexApplication indexApplication -> Arr indexApplication.type'
    | Unbox unbox -> Arr unbox.type'
    | Let let' -> let'.type'
    | TupleLet tupleLet -> tupleLet.type'
  ;;

  let type' : t -> Type.t = function
    | Array array -> Array (arrayType array)
    | Atom atom -> Atom (atomType atom)
  ;;
end
