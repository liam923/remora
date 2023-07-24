open! Base

(* The Nucleus language represents a typed Remora program with no optimizations
   yet performed*)

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
    | IntLiteral
    | CharacterLiteral

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
    ; body : array
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

  and literal =
    | IntLiteral of int
    | CharacterLiteral of char

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
    | Literal (IntLiteral _) -> Literal IntLiteral
    | Literal (CharacterLiteral _) -> Literal CharacterLiteral
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

type t = Expr.t [@@deriving sexp]

module ShowStage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type error = (SB.source option, string) Source.annotate
  type input = t
  type output = string

  let name = "Print Nucleus"
  let run input = CompilerPipeline.S.return (Sexp.to_string_hum ([%sexp_of: t] input))
end

module Canonical : sig
  module Index : sig
    type t [@@deriving compare, sexp, equal]
    type comparator_witness

    val comparator : (t, comparator_witness) Comparator.t
    val from : Index.t -> t
  end

  module Type : sig
    type t [@@deriving compare, sexp, equal]
    type comparator_witness

    val comparator : (t, comparator_witness) Comparator.t
    val from : Type.t -> t
  end
end = struct
  module Ref = struct
    module T = struct
      type t = int * int [@@deriving compare, sexp, equal]
    end

    include T
    include Comparator.Make (T)

    let default = -1, 0
    let getInEnv env i = Map.find env i |> Option.value ~default
  end

  module Index = struct
    module T = struct
      type dimension =
        { const : int
        ; refs : int Map.M(Ref).t
        }
      [@@deriving compare, sexp, equal]

      type shapeElement =
        | Add of dimension
        | ShapeRef of Ref.t
      [@@deriving compare, sexp, equal]

      type shape = shapeElement list [@@deriving compare, sexp, equal]

      type t =
        | Dimension of dimension
        | Shape of shape
      [@@deriving compare, sexp, equal]
    end

    include T
    include Comparator.Make (T)

    let dimensionFrom env ({ const; refs } : Index.dimension) : dimension =
      { const
      ; refs =
          Map.fold
            refs
            ~init:(Map.empty (module Ref))
            ~f:(fun ~key:ref ~data:count acc ->
              Map.set acc ~key:(Ref.getInEnv env ref) ~data:count)
      }
    ;;

    let shapeElementFrom env = function
      | Index.Add dimension -> Add (dimensionFrom env dimension)
      | Index.ShapeRef ref -> ShapeRef (Ref.getInEnv env ref)
    ;;

    let shapeFrom env = List.map ~f:(shapeElementFrom env)

    let indexFrom env : Index.t -> t = function
      | Index.Dimension dimension -> Dimension (dimensionFrom env dimension)
      | Index.Shape shapeElements -> Shape (shapeFrom env shapeElements)
    ;;

    let from index = indexFrom (Map.empty (module Identifier)) index
  end

  module Type = struct
    module T = struct
      type arr =
        { element : atom
        ; shape : Index.shape
        }

      and func =
        { parameters : array list
        ; return : array
        }

      and 't abstraction =
        { parameters : 't list
        ; body : array
        }

      and forall = Kind.t abstraction
      and pi = Sort.t abstraction
      and sigma = Sort.t abstraction
      and tuple = atom list

      and literal =
        | IntLiteral
        | CharacterLiteral

      and array =
        | ArrayRef of Ref.t
        | Arr of arr

      and atom =
        | AtomRef of Ref.t
        | Func of func
        | Forall of forall
        | Pi of pi
        | Sigma of sigma
        | Tuple of tuple
        | Literal of literal

      and t =
        | Array of array
        | Atom of atom
      [@@deriving compare, sexp, equal]
    end

    include T
    include Comparator.Make (T)

    let rec arrayFrom env depth = function
      | Type.ArrayRef ref -> ArrayRef (Ref.getInEnv env ref)
      | Type.Arr { element; shape } ->
        Arr { element = atomFrom env depth element; shape = Index.shapeFrom env shape }

    and atomFrom env depth = function
      | Type.AtomRef ref -> AtomRef (Ref.getInEnv env ref)
      | Type.Func { parameters; return } ->
        Func
          { parameters = List.map parameters ~f:(arrayFrom env depth)
          ; return = arrayFrom env depth return
          }
      | Type.Forall { parameters; body } ->
        Forall
          { parameters = List.map parameters ~f:(fun p -> p.bound)
          ; body =
              (let env, _ =
                 List.fold parameters ~init:(env, 0) ~f:(fun (env, count) param ->
                     Map.set env ~key:param.binding ~data:(depth, count), count + 1)
               in
               let depth = depth + 1 in
               arrayFrom env depth body)
          }
      | Type.Pi { parameters; body } ->
        Pi
          { parameters = List.map parameters ~f:(fun p -> p.bound)
          ; body =
              (let env, _ =
                 List.fold parameters ~init:(env, 0) ~f:(fun (env, count) param ->
                     Map.set env ~key:param.binding ~data:(depth, count), count + 1)
               in
               let depth = depth + 1 in
               arrayFrom env depth body)
          }
      | Type.Sigma { parameters; body } ->
        Sigma
          { parameters = List.map parameters ~f:(fun p -> p.bound)
          ; body =
              (let env, _ =
                 List.fold parameters ~init:(env, 0) ~f:(fun (env, count) param ->
                     Map.set env ~key:param.binding ~data:(depth, count), count + 1)
               in
               let depth = depth + 1 in
               arrayFrom env depth body)
          }
      | Type.Tuple elements -> Tuple (List.map elements ~f:(atomFrom env depth))
      | Type.Literal IntLiteral -> Literal IntLiteral
      | Type.Literal CharacterLiteral -> Literal CharacterLiteral

    and typeFrom env depth = function
      | Type.Array array -> Array (arrayFrom env depth array)
      | Type.Atom atom -> Atom (atomFrom env depth atom)
    ;;

    let from type' = typeFrom (Map.empty (module Identifier)) 0 type'
  end
end
