open! Base

module Kind = struct
  type t =
    | Atom
    | Array
  [@@deriving eq, sexp]
end

module Sort = struct
  type t =
    | Dim
    | Shape
  [@@deriving eq, sexp]
end

module Untyped = struct
  type ('s, 't) param =
    { binding : ('s, string) Source.annotate
    ; bound : 't
    }
  [@@deriving sexp_of]

  type ('s, 't) paramList =
    ('s, ('s, ('s, 't) param) Source.annotate list) Source.annotate
  [@@deriving sexp_of]

  module Index = struct
    type ref = string
    and 's dimension = int
    and 's shape = 's t list
    and 's add = 's t list
    and 's append = 's t list

    and 's raw =
      | Ref of ref
      | Dimension of 's dimension
      | Shape of 's shape
      | Add of 's add
      | Append of 's append

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
      | TupleLet of 's tupleLet
      | Tuple of 's tuple
      | IntLiteral of int
      | CharacterLiteral of char

    and 's t = ('s, 's raw) Source.annotate [@@deriving sexp_of]
  end
end

module Typed = struct
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

    and literal =
      | Integer
      | Character

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
      ; type' : Type.array
      }
    [@@deriving sexp]

    type arr =
      { dimensions : int list
      ; elements : atom list
      ; type' : Type.arr
      }

    and frame =
      { dimensions : int list
      ; arrays : array list
      ; type' : Type.arr
      }

    and termApplication =
      { func : array
      ; args : array list
      ; type' : Type.arr
      }

    and typeApplication =
      { tFunc : array
      ; args : Type.t list
      ; type' : Type.arr
      }

    and indexApplication =
      { iFunc : array
      ; args : Index.t list
      ; type' : Type.arr
      }

    and unbox =
      { indexBindings : Identifier.t list
      ; valueBinding : Identifier.t
      ; box : array
      ; body : array
      ; type' : Type.arr
      }

    and termLambda =
      { params : Type.array param list
      ; body : t
      ; type' : Type.func
      }

    and typeLambda =
      { params : Kind.t param list
      ; body : array
      ; type' : Type.forall
      }

    and indexLambda =
      { params : Sort.t param list
      ; body : array
      ; type' : Type.pi
      }

    and box =
      { indices : Index.t list
      ; body : array
      ; bodyType : Type.array
      ; type' : Type.sigma
      }

    and let' =
      { binding : Identifier.t
      ; value : array
      ; body : array
      ; type' : Type.array
      }

    and tupleLet =
      { params : Type.atom param list
      ; value : array
      ; body : array
      ; type' : Type.array
      }

    and tuple =
      { elements : atom list
      ; type' : Type.tuple
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
      | IntLiteral of int
      | CharacterLiteral of char

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
      | IntLiteral _ -> Literal Integer
      | CharacterLiteral _ -> Literal Character
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
end
