(* open! Base

(* The DefunNucleus language represents a monomorphized, defunctionalized Remora program *)

type 't param = 't MonoNucleus.param [@@deriving sexp]

module Index = MonoNucleus.Index
module Type = MonoNucleus.Type

module Expr = struct
  type ref =
    { id : Identifier.t
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }
  [@@deriving sexp]

  type functionRef =
    { id : Identifier.t
    ; type' : Type.func [@sexp_drop_if fun _ -> true]
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

  and literalValue = MonoNucleus.Expr.literalValue
  and literal = MonoNucleus.Expr.literal

  and array =
    | Ref of ref
    | Arr of arr
    | Frame of frame
    | TermApplication of termApplication
    | Unbox of unbox
    | Let of let'
    | TupleLet of tupleLet

  and atom =
    | FunctionRef of functionRef
    | Box of box
    | Tuple of tuple
    | Literal of literal

  and t =
    | Array of array
    | Atom of atom
  [@@deriving sexp]

  let atomType : atom -> Type.atom = function
    | FunctionRef ref -> Type.Func ref.type'
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

type func =
  { params : Type.array param list
  ; body : Expr.array
  ; type' : Type.func [@sexp_drop_if fun _ -> true]
  }
[@@deriving sexp]

type t =
  { functions : func Map.M(Identifier).t
  ; body : Expr.t
  }
[@@deriving sexp]

module ShowStage (SB : Source.BuilderT) = struct
  type error = (SB.source option, string) Source.annotate
  type input = t
  type output = string

  let name = "Print DefunNucleus"
  let run input = MResult.MOk (Sexp.to_string_hum ([%sexp_of: t] input))
end *)
