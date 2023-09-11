open! Base

(* The AST language represents a parsed, untyped Remora program *)

type ('s, 't) param =
  { binding : ('s, string) Source.annotate
  ; bound : 't
  }
[@@deriving sexp_of]

type ('s, 't) paramList = ('s, ('s, ('s, 't) param) Source.annotate list) Source.annotate
[@@deriving sexp_of]

module Index = struct
  type ref = string
  and 's dimension = int
  and 's shape = 's t list
  and 's add = 's t list
  and 's append = 's t list
  and 's slice = 's t list

  and 's raw =
    | Ref of ref
    | Dimension of 's dimension
    | Shape of 's shape
    | Add of 's add
    | Append of 's append
    | Slice of 's slice

  and 's t = ('s, 's raw) Source.annotate [@@deriving sexp_of]
end

module Type = struct
  type ref = string

  and 's arr =
    { element : 's t
    ; shape : 's Index.t
    }

  and 's func =
    { parameters : ('s, 's t list) Source.annotate
    ; return : 's t
    }

  and ('s, 't) abstraction =
    { parameters : ('s, 't) paramList
    ; body : 's t
    }

  and 's forall = ('s, ('s, Kind.t) Source.annotate) abstraction
  and 's pi = ('s, ('s, Sort.t) Source.annotate) abstraction
  and 's sigma = ('s, ('s, Sort.t) Source.annotate) abstraction
  and 's tuple = 's t list

  and 's raw =
    | Ref of ref
    | Arr of 's arr
    | Func of 's func
    | Forall of 's forall
    | Pi of 's pi
    | Sigma of 's sigma
    | Tuple of 's tuple

  and 's t = ('s, 's raw) Source.annotate [@@deriving sexp_of]
end

module Expr = struct
  type ref = string

  and 's arrOrFrame =
    { dimensions : ('s, ('s, int) Source.annotate list) Source.annotate
    ; elements : ('s, 's t NeList.t) Source.annotate
    }

  and 's emptyArrOrFrame =
    { dimensions : ('s, ('s, int) Source.annotate list) Source.annotate
    ; elementType : 's Type.t
    }

  and 's termApplication =
    { func : 's t
    ; args : ('s, 's t list) Source.annotate
    }

  and 's typeApplication =
    { tFunc : 's t
    ; args : ('s, 's Type.t list) Source.annotate
    }

  and 's indexApplication =
    { iFunc : 's t
    ; args : ('s, 's Index.t list) Source.annotate
    }

  and 's unboxParam =
    { binding : ('s, string) Source.annotate
    ; bound : ('s, Sort.t) Source.annotate option
    }

  and 's unbox =
    { indexBindings : ('s, ('s, 's unboxParam) Source.annotate list) Source.annotate
    ; valueBinding : ('s, string) Source.annotate
    ; box : 's t
    ; body : 's t
    }

  and 's termLambda =
    { params : ('s, 's Type.t) paramList
    ; body : 's t
    }

  and 's typeLambda =
    { params : ('s, ('s, Kind.t) Source.annotate) paramList
    ; body : 's t
    }

  and 's indexLambda =
    { params : ('s, ('s, Sort.t) Source.annotate) paramList
    ; body : 's t
    }

  and 's boxElement =
    { indices : ('s, 's Index.t list) Source.annotate
    ; body : 's t
    }

  and 's boxes =
    { params : ('s, ('s, Sort.t) Source.annotate) paramList
    ; elementType : 's Type.t
    ; dimensions : ('s, ('s, int) Source.annotate list) Source.annotate
    ; elements : ('s, ('s, 's boxElement) Source.annotate list) Source.annotate
    }

  and 's let' =
    { param : ('s, ('s, 's Type.t option) param) Source.annotate
    ; value : 's t
    ; body : 's t
    }

  and 's reshape =
    { newShape : 's Index.t
    ; value : 's t
    }

  and 's reifyShape = 's Index.t
  and 's reifyDimension = 's Index.t

  and 's tupleLet =
    { params : ('s, 's Type.t option) paramList
    ; value : 's t
    ; body : 's t
    }

  and 's tuple = ('s, 's t list) Source.annotate

  and 's raw =
    | Ref of ref
    | Arr of 's arrOrFrame
    | EmptyArr of 's emptyArrOrFrame
    | Frame of 's arrOrFrame
    | EmptyFrame of 's emptyArrOrFrame
    | TermApplication of 's termApplication
    | TypeApplication of 's typeApplication
    | IndexApplication of 's indexApplication
    | Unbox of 's unbox
    | TermLambda of 's termLambda
    | TypeLambda of 's typeLambda
    | IndexLambda of 's indexLambda
    | Boxes of 's boxes
    | Let of 's let'
    | Reshape of 's reshape
    | ReifyShape of 's reifyShape
    | ReifyDimension of 's reifyDimension
    | TupleLet of 's tupleLet
    | Tuple of 's tuple
    | IntLiteral of int
    | CharacterLiteral of char
    | BooleanLiteral of bool

  and 's t = ('s, 's raw) Source.annotate [@@deriving sexp_of]
end

type 's t = 's Expr.t [@@deriving sexp_of]
