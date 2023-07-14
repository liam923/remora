open! Base

(* The MonoNucleus language represents a monomorphized Remora program *)

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
  type array =
    { element : atom
    ; shape : Index.shape
    }

  and func =
    { parameters : array list
    ; return : array
    }

  and sigma =
    { parameters : Sort.t param list
    ; body : array
    }

  and tuple = atom list

  and literal =
    | IntLiteral
    | CharacterLiteral

  and atom =
    | Func of func
    | Sigma of sigma
    | Tuple of tuple
    | Literal of literal

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
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }

  and frame =
    { dimensions : int list
    ; arrays : array list
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }

  and termApplication =
    { func : array
    ; args : array list
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }

  and unbox =
    { indexBindings : Identifier.t list
    ; valueBinding : Identifier.t
    ; box : array
    ; body : array
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }

  and termLambda =
    { params : Type.array param list
    ; body : array
    ; type' : Type.func [@sexp_drop_if fun _ -> true]
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
    | Unbox of unbox
    | Let of let'
    | TupleLet of tupleLet

  and atom =
    | TermLambda of termLambda
    | Box of box
    | Tuple of tuple
    | Literal of literal

  and t =
    | Array of array
    | Atom of atom
  [@@deriving sexp]

  let atomType : atom -> Type.atom = function
    | TermLambda termLambda -> Func termLambda.type'
    | Box box -> Sigma box.type'
    | Tuple tuple -> Tuple tuple.type'
    | Literal literal -> literal.type'
  ;;

  let arrayType : array -> Type.array = function
    | Ref ref -> ref.type'
    | Arr arr -> arr.type'
    | Frame frame -> frame.type'
    | TermApplication termApplication -> termApplication.type'
    | Unbox unbox -> unbox.type'
    | Let let' -> let'.type'
    | TupleLet tupleLet -> tupleLet.type'
  ;;

  let type' : t -> Type.t = function
    | Array array -> Array (arrayType array)
    | Atom atom -> Atom (atomType atom)
  ;;
end

type t = Expr.t [@@deriving sexp]

module ShowStage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type error = (SB.source option, string) Source.annotate
  type input = t
  type output = string

  let name = "Print MonoNucleus"
  let run input = CompilerPipeline.S.return (Sexp.to_string_hum ([%sexp_of: t] input))
end
