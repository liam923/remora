open! Base

module Name = struct
  module T = struct
    type t =
      | UniqueName of Identifier.t
      | StrName of string
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let show = function
    | UniqueName id -> Identifier.show id
    | StrName str -> str
  ;;
end

type name = Name.t

and fun' =
  { args : funArg list
  ; returnType : type'
  ; body : block
  ; isKernel : bool
  }

and struct' = structField list

and 't declaration =
  { name : name
  ; value : 't
  }

and funArg =
  { name : name
  ; type' : type'
  }

and structField =
  { name : name
  ; type' : type'
  }

and type' =
  | Char
  | Int64
  | Float64
  | Bool
  | Ptr of type'
  | TypeRef of name

and literal =
  | Char of char
  | Int64 of int
  | Float64 of float
  | Bool of bool

and expr =
  | Literal of literal
  | VarRef of name
  | FieldDeref of
      { value : expr
      ; fieldName : name
      }
  | ArrayDeref of
      { value : expr
      ; index : expr
      }
  | PtrDeref of expr
  | Ternary of
      { cond : expr
      ; then' : expr
      ; else' : expr
      }
  | FunCall of
      { fun' : name
      ; args : expr list
      }
  | KernelLaunch of
      { kernel : name
      ; blocks : int
      ; threads : int
      ; args : expr list
      }
  | Binop of
      { op : string
      ; arg1 : expr
      ; arg2 : expr
      }

and statement =
  | Return of expr
  | Define of
      { name : name
      ; type' : type'
      ; value : expr option
      }
  | Assign of expr * expr
  | Ite of
      { cond : expr
      ; thenBranch : block
      ; elseBranch : block
      }
  | Eval of expr

and block = statement list

and program =
  { funDecs : fun' declaration list
  ; structDecs : struct' declaration list
  ; main : block option
  }

and t = program [@@deriving sexp_of]