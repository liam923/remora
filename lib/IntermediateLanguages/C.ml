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
  | PtrRef of expr
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
      ; blocks : expr
      ; threads : expr
      ; args : expr list
      }
  | Binop of
      { op : string
      ; arg1 : expr
      ; arg2 : expr
      }
  | PrefixOp of
      { op : string
      ; arg : expr
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
  | WhileLoop of
      { cond : expr
      ; body : block
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
  | DecrementOne
  | Increment of expr
  | ShiftRight of expr
  | ShiftLeft of expr

and block = statement list

and program =
  { includes : string list
  ; prelude : string list
  ; funDecs : fun' declaration list
  ; structDecs : struct' declaration list
  ; main : block option
  }

and t = program [@@deriving sexp_of]

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
  let ( >> ) arg1 arg2 = Binop { op = ">>"; arg1; arg2 }
  let ( == ) arg1 arg2 = Binop { op = "=="; arg1; arg2 }
  let ( != ) arg1 arg2 = Binop { op = "!="; arg1; arg2 }
  let ( && ) arg1 arg2 = Binop { op = "&&"; arg1; arg2 }
  let ( || ) arg1 arg2 = Binop { op = "||"; arg1; arg2 }
  let pp arg = PrefixOp { op = "++"; arg }
  let not arg = PrefixOp { op = "!"; arg }
  let intLit i = Literal (Int64Literal i)
  let charLit c = Literal (CharLiteral c)
  let refStr str = VarRef (StrName str)
  let refId id = VarRef (UniqueName id)
  let ( %-> ) value fieldName = PtrFieldDeref { value; fieldName }
  let ( %. ) value fieldName = FieldDeref { value; fieldName }

  let fieldDeref value fieldName ~inPtr =
    if inPtr then value %-> fieldName else value %. fieldName
  ;;

  let ( := ) lhs rhs = Assign { lhs; rhs }
  let eval expr = Eval expr
  let callBuiltin name ?typeArgs args = FunCall { fun' = StrName name; typeArgs; args }
  let call name ?typeArgs args = FunCall { fun' = name; typeArgs; args }
  let initStruct type' args = StructConstructor { type'; args }

  let rec arrayDeref value indices =
    match indices with
    | [] -> value
    | head :: rest -> arrayDeref (ArrayDeref { value; index = head }) rest
  ;;

  let ternary ~cond ~then' ~else' = Ternary { cond; then'; else' }
end
