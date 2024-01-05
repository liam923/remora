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

and funType =
  | Host
  | Device
  | HostOrDevice
  | Kernel

and fun' =
  { params : funParam list
  ; returnType : type' option
  ; body : block
  ; funType : funType
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
  | Cast of
      { type' : type'
      ; value : expr
      }

and statement =
  | Return of expr
  | Define of
      { name : name
      ; type' : type' option
      ; value : expr option
      }
  | DefineDetail of
      { attributes : string list
      ; name : name
      ; type' : type' option
      ; value : expr option
      ; dims : expr list
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
  | SyncThreads
  | Block of block
  | Comment of string

and varUpdate =
  | IncrementOne
  | Increment of expr

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

module Syntax = struct
  let ( + ) arg1 arg2 = Binop { op = "+"; arg1; arg2 }
  let ( - ) arg1 arg2 = Binop { op = "-"; arg1; arg2 }
  let ( * ) arg1 arg2 = Binop { op = "*"; arg1; arg2 }
  let ( / ) arg1 arg2 = Binop { op = "/"; arg1; arg2 }
  let ( % ) arg1 arg2 = Binop { op = "%"; arg1; arg2 }
  let ( > ) arg1 arg2 = Binop { op = ">"; arg1; arg2 }
  let ( >= ) arg1 arg2 = Binop { op = ">="; arg1; arg2 }
  let ( < ) arg1 arg2 = Binop { op = "<"; arg1; arg2 }
  let ( <= ) arg1 arg2 = Binop { op = "<="; arg1; arg2 }
  let ( << ) arg1 arg2 = Binop { op = "<<"; arg1; arg2 }
  let ( == ) arg1 arg2 = Binop { op = "=="; arg1; arg2 }
  let ( && ) arg1 arg2 = Binop { op = "&&"; arg1; arg2 }
  let intLit i = Literal (Int64Literal i)
  let charLit c = Literal (CharLiteral c)
  let refStr str = VarRef (StrName str)
  let refId id = VarRef (UniqueName id)
  let ( %-> ) value fieldName = FieldDeref { value; fieldName }
  let ( := ) lhs rhs = Assign { lhs; rhs }

  let callBuiltin name ?(typeArgs = None) args =
    FunCall { fun' = StrName name; typeArgs; args }
  ;;

  let arrayDeref value index = ArrayDeref { value; index }

  let rec arrayDerefs value indices =
    match indices with
    | [] -> value
    | head :: rest -> arrayDerefs (arrayDeref value head) rest
  ;;

  let ternary ~cond ~then' ~else' = Ternary { cond; then'; else' }
end
