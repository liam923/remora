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
  { params : funParam list
  ; returnType : type' option
  ; body : block
  ; isKernel : bool
  }

and struct' = structField list

and 't declaration =
  { name : name
  ; value : 't
  }

and funParam =
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
  | TypeInstantiation of
      { base : type'
      ; args : type' list
      }

and literal =
  | CharLiteral of char
  | Int64Literal of int
  | Float64Literal of float
  | BoolLiteral of bool

and expr =
  | Literal of literal
  | VarRef of name
  | FieldDeref of
      { value : expr
      ; fieldName : name
      }
  | PtrFieldDeref of
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
      ; typeArgs : type' list option
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
  | StructConstructor of
      { type' : type'
      ; args : expr list
      }
  | Arr of expr list

and statement =
  | Return of expr
  | Define of
      { name : name
      ; type' : type' option
      ; value : expr option
      }
  | Assign of
      { lhs : expr
      ; rhs : expr
      }
  | Ite of
      { cond : expr
      ; thenBranch : block
      ; elseBranch : block
      }
  | Eval of expr
  | StrStatement of string
  | ForLoop of
      { loopVar : name
      ; loopVarType : type'
      ; initialValue : expr
      ; cond : expr
      ; loopVarUpdate : varUpdate
      ; body : block
      }

and varUpdate = IncrementOne
and block = statement list

and program =
  { includes : string list
  ; prelude : string list
  ; funDecs : fun' declaration list
  ; structDecs : struct' declaration list
  ; main : block option
  }

and t = program [@@deriving sexp_of]

let fieldDeref ~value ~fieldName ~valueIsPtr =
  if valueIsPtr
  then PtrFieldDeref { value; fieldName }
  else FieldDeref { value; fieldName }
;;
