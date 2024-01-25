open! Base

(* The Nucleus language represents a typed Remora program with no optimizations
   yet performed*)

type 't param =
  { binding : Identifier.t
  ; bound : 't
  }
[@@deriving sexp, compare, equal]

module Index = struct
  type dimension =
    { const : int
    ; refs : int Map.M(Identifier).t
    }
  [@@deriving sexp, compare, equal]

  type shapeElement =
    | Add of dimension
    | ShapeRef of Identifier.t
  [@@deriving sexp, compare, equal]

  module ShapeElement = struct
    module T = struct
      type t = shapeElement [@@deriving sexp, compare, equal]
    end

    include T
    include Comparator.Make (T)
  end

  type shape = shapeElement list [@@deriving sexp, compare, equal]

  type t =
    | Dimension of dimension
    | Shape of shape
  [@@deriving sexp, compare, equal]

  let dimensionConstant n = { const = n; refs = Map.empty (module Identifier) }
  let dimensionRef r = { const = 0; refs = Map.singleton (module Identifier) r 1 }

  let addDimensions a b =
    { const = a.const + b.const
    ; refs = Map.merge_skewed a.refs b.refs ~combine:(fun ~key:_ a b -> a + b)
    }
  ;;

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

  and literal =
    | IntLiteral
    | FloatLiteral
    | CharacterLiteral
    | BooleanLiteral

  and array =
    | ArrayRef of Identifier.t
    | Arr of arr

  and atom =
    | AtomRef of Identifier.t
    | Func of func
    | Forall of forall
    | Pi of pi
    | Sigma of sigma
    | Literal of literal

  and t =
    | Array of array
    | Atom of atom
  [@@deriving sexp, compare, equal]

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
  [@@deriving sexp, compare, equal]

  type reduceCharacter =
    | Reduce
    | Scan
  [@@deriving compare, sexp, equal]

  type foldCharacter =
    | Fold
    | Trace
  [@@deriving compare, sexp, equal]

  type primitiveFuncName =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | AddF
    | SubF
    | MulF
    | DivF
    | And
    | Or
    | Not
    | IntToFloat
    | FloatToInt
    | IntToBool
    | BoolToInt
    | Equal
    | Ne
    | Gt
    | GtEq
    | Lt
    | LtEq
    | GtF
    | GtEqF
    | LtF
    | LtEqF
    | Reduce of { character : reduceCharacter }
    | Fold of { character : foldCharacter }
    | Append
    | ContiguousSubArray
    | Scatter
    | If
    | LibFun of
        { name : string
        ; libName : string
        ; argTypes : Type.array list
        ; retType : Type.array
        }
  [@@deriving compare, sexp, equal]

  type primitiveValName = Iota [@@deriving compare, sexp, equal]

  type primitiveName =
    | Func of primitiveFuncName
    | Val of primitiveValName
  [@@deriving compare, sexp, equal]

  type scalar =
    { element : atom
    ; type' : Type.arr [@sexp_drop_if fun _ -> true]
    }

  and frame =
    { dimensions : int list
    ; elements : array list
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
    { indexBindings : (Identifier.t * Sort.t) list
    ; valueBinding : Identifier.t
    ; box : array
    ; body : array
    ; type' : Type.arr [@sexp_drop_if fun _ -> true]
    }

  and reifyIndex =
    { index : Index.t
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

  and primitive =
    { name : primitiveName
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }

  and lift =
    { indexBinding : Identifier.t
    ; indexValue : array
    ; sort : Sort.t
    ; frameShape : Index.shape
    ; body : array
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }

  and literal =
    | IntLiteral of int
    | FloatLiteral of float
    | CharacterLiteral of char
    | BooleanLiteral of bool

  and array =
    | Ref of ref
    | Scalar of scalar
    | Frame of frame
    | TermApplication of termApplication
    | TypeApplication of typeApplication
    | IndexApplication of indexApplication
    | Unbox of unbox
    | ReifyIndex of reifyIndex
    | Let of let'
    | Primitive of primitive
    | Lift of lift

  and atom =
    | TermLambda of termLambda
    | TypeLambda of typeLambda
    | IndexLambda of indexLambda
    | Box of box
    | Literal of literal

  and t =
    | Array of array
    | Atom of atom
  [@@deriving sexp, compare, equal]

  let atomType : atom -> Type.atom = function
    | TermLambda termLambda -> Func termLambda.type'
    | TypeLambda typeLambda -> Forall typeLambda.type'
    | IndexLambda indexLambda -> Pi indexLambda.type'
    | Box box -> Sigma box.type'
    | Literal (IntLiteral _) -> Literal IntLiteral
    | Literal (FloatLiteral _) -> Literal FloatLiteral
    | Literal (CharacterLiteral _) -> Literal CharacterLiteral
    | Literal (BooleanLiteral _) -> Literal BooleanLiteral
  ;;

  let arrayType : array -> Type.array = function
    | Ref ref -> ref.type'
    | Scalar scalar -> Arr scalar.type'
    | Frame frame -> Arr frame.type'
    | TermApplication termApplication -> Arr termApplication.type'
    | TypeApplication typeApplication -> Arr typeApplication.type'
    | IndexApplication indexApplication -> Arr indexApplication.type'
    | Unbox unbox -> Arr unbox.type'
    | ReifyIndex reifyIndex -> Arr reifyIndex.type'
    | Let let' -> let'.type'
    | Primitive primitive -> primitive.type'
    | Lift lift -> lift.type'
  ;;

  let type' : t -> Type.t = function
    | Array array -> Array (arrayType array)
    | Atom atom -> Atom (atomType atom)
  ;;

  let replaceTypeOfArray array type' =
    match array with
    | Ref ref -> Ref { ref with type' = Arr type' }
    | Scalar scalar -> Scalar { scalar with type' }
    | Frame frame -> Frame { frame with type' }
    | TermApplication termApplication -> TermApplication { termApplication with type' }
    | TypeApplication typeApplication -> TypeApplication { typeApplication with type' }
    | IndexApplication indexApplication ->
      IndexApplication { indexApplication with type' }
    | Unbox unbox -> Unbox { unbox with type' }
    | ReifyIndex reifyIndex -> ReifyIndex { reifyIndex with type' }
    | Let let' -> Let { let' with type' = Arr type' }
    | Primitive primitive -> Primitive { primitive with type' = Arr type' }
    | Lift lift -> Lift { lift with type' = Arr type' }
  ;;
end

type t = Expr.array [@@deriving sexp]

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

      and literal =
        | IntLiteral
        | FloatLiteral
        | CharacterLiteral
        | BooleanLiteral

      and array =
        | ArrayRef of Ref.t
        | Arr of arr

      and atom =
        | AtomRef of Ref.t
        | Func of func
        | Forall of forall
        | Pi of pi
        | Sigma of sigma
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
      | Type.Literal IntLiteral -> Literal IntLiteral
      | Type.Literal FloatLiteral -> Literal FloatLiteral
      | Type.Literal CharacterLiteral -> Literal CharacterLiteral
      | Type.Literal BooleanLiteral -> Literal BooleanLiteral

    and typeFrom env depth = function
      | Type.Array array -> Array (arrayFrom env depth array)
      | Type.Atom atom -> Atom (atomFrom env depth atom)
    ;;

    let from type' = typeFrom (Map.empty (module Identifier)) 0 type'
  end
end

module Substitute = struct
  module Index = struct
    let subIndicesIntoDim indices ({ const; refs } : Index.dimension) =
      let open Index in
      Map.fold
        refs
        ~init:{ const; refs = Map.empty (Map.comparator_s refs) }
        ~f:(fun ~key:idBeingSubbed ~data:subMultiplier acc ->
          match Map.find indices idBeingSubbed with
          | Some (Dimension sub) ->
            (* need to combine togeth acc and sub, with sub repeated subMultiplier times *)
            let sub =
              { const = sub.const * subMultiplier
              ; refs = Map.map sub.refs ~f:(( * ) subMultiplier)
              }
            in
            let combinedConsts = acc.const + sub.const in
            let combinedRefs =
              Map.fold
                sub.refs
                ~init:acc.refs
                ~f:(fun ~key:ref ~data:count combinedRefs ->
                  Map.update combinedRefs ref ~f:(fun prevCount ->
                    Option.value prevCount ~default:0 + count))
            in
            { const = combinedConsts; refs = combinedRefs }
          | Some (Shape _) -> raise @@ Unreachable.Error "tried to sub shape for dim"
          | None ->
            { const = acc.const
            ; refs = Map.set acc.refs ~key:idBeingSubbed ~data:subMultiplier
            })
    ;;

    let subIndicesIntoShape indices shape =
      let open Index in
      List.bind shape ~f:(function
        | Add dim -> [ Add (subIndicesIntoDim indices dim) ]
        | ShapeRef id as ref ->
          (match Map.find indices id with
           | Some (Shape shape) -> shape
           | Some (Dimension _) -> raise @@ Unreachable.Error "tried to sub dim for shape"
           | None -> [ ref ]))
    ;;

    let subIndicesIntoIndex indices =
      let open Index in
      function
      | Shape shape -> Shape (subIndicesIntoShape indices shape)
      | Dimension dim -> Dimension (subIndicesIntoDim indices dim)
    ;;
  end

  module Type = struct
    let rec subIndicesIntoArray indices =
      let open Type in
      function
      | ArrayRef _ as ref -> ref
      | Arr arr -> Arr (subIndicesIntoArr indices arr)

    and subIndicesIntoArr indices Type.{ element; shape } =
      Type.
        { element = subIndicesIntoAtom indices element
        ; shape = Index.subIndicesIntoShape indices shape
        }

    and subIndicesIntoAtom indices =
      let open Type in
      function
      | AtomRef _ as ref -> ref
      | Func func -> Func (subIndicesIntoFunc indices func)
      | Forall forall -> Forall (subIndicesIntoForall indices forall)
      | Pi pi -> Pi (subIndicesIntoPi indices pi)
      | Sigma sigma -> Sigma (subIndicesIntoSigma indices sigma)
      | Literal IntLiteral -> Literal IntLiteral
      | Literal FloatLiteral -> Literal FloatLiteral
      | Literal CharacterLiteral -> Literal CharacterLiteral
      | Literal BooleanLiteral -> Literal BooleanLiteral

    and subIndicesIntoFunc indices Type.{ parameters; return } =
      Type.
        { parameters = List.map parameters ~f:(subIndicesIntoArray indices)
        ; return = subIndicesIntoArray indices return
        }

    and subIndicesIntoForall indices Type.{ parameters; body } =
      Type.{ parameters; body = subIndicesIntoArray indices body }

    and subIndicesIntoPi indices Type.{ parameters; body } =
      Type.{ parameters; body = subIndicesIntoArray indices body }

    and subIndicesIntoSigma indices Type.{ parameters; body } =
      Type.{ parameters; body = subIndicesIntoArray indices body }

    and subIndicesIntoType indices =
      let open Type in
      function
      | Array array -> Array (subIndicesIntoArray indices array)
      | Atom atom -> Atom (subIndicesIntoAtom indices atom)
    ;;

    let rec subTypesIntoArray types =
      let open Type in
      function
      | ArrayRef id as ref ->
        (match Map.find types id with
         | Some (Array arrayType) -> arrayType
         | Some (Atom _) -> ref
         | None -> ref)
      | Arr arr -> Arr (subTypesIntoArr types arr)

    and subTypesIntoArr types Type.{ element; shape } =
      Type.{ element = subTypesIntoAtom types element; shape }

    and subTypesIntoAtom types =
      let open Type in
      function
      | AtomRef id as ref ->
        (match Map.find types id with
         | Some (Atom atomType) -> atomType
         | Some (Array _) -> ref
         | None -> ref)
      | Func { parameters; return } ->
        Func
          { parameters = List.map parameters ~f:(subTypesIntoArray types)
          ; return = subTypesIntoArray types return
          }
      | Forall forall -> Forall (subTypesIntoForall types forall)
      | Pi pi -> Pi (subTypesIntoPi types pi)
      | Sigma sigma -> Sigma (subTypesIntoSigma types sigma)
      | Literal IntLiteral -> Literal IntLiteral
      | Literal FloatLiteral -> Literal FloatLiteral
      | Literal CharacterLiteral -> Literal CharacterLiteral
      | Literal BooleanLiteral -> Literal BooleanLiteral

    and subTypesIntoFunc types Type.{ parameters; return } =
      Type.
        { parameters = List.map parameters ~f:(subTypesIntoArray types)
        ; return = subTypesIntoArray types return
        }

    and subTypesIntoForall types Type.{ parameters; body } =
      Type.{ parameters; body = subTypesIntoArray types body }

    and subTypesIntoPi types Type.{ parameters; body } =
      Type.{ parameters; body = subTypesIntoArray types body }

    and subTypesIntoSigma types Type.{ parameters; body } =
      Type.{ parameters; body = subTypesIntoArray types body }

    and subTypesIntoType types =
      let open Type in
      function
      | Array array -> Array (subTypesIntoArray types array)
      | Atom atom -> Atom (subTypesIntoAtom types atom)
    ;;
  end

  module Expr = struct
    let rec subTypesIntoArray types =
      let open Expr in
      function
      | Ref _ as ref -> ref
      | Scalar { element; type' } ->
        Scalar
          { element = subTypesIntoAtom types element
          ; type' = Type.subTypesIntoArr types type'
          }
      | Frame { elements; dimensions; type' } ->
        Frame
          { elements = List.map elements ~f:(subTypesIntoArray types)
          ; dimensions
          ; type' = Type.subTypesIntoArr types type'
          }
      | TermApplication { func; args; type' } ->
        TermApplication
          { func = subTypesIntoArray types func
          ; args = List.map args ~f:(subTypesIntoArray types)
          ; type' = Type.subTypesIntoArr types type'
          }
      | TypeApplication { tFunc; args; type' } ->
        TypeApplication
          { tFunc = subTypesIntoArray types tFunc
          ; args = List.map args ~f:(Type.subTypesIntoType types)
          ; type' = Type.subTypesIntoArr types type'
          }
      | IndexApplication { iFunc; args; type' } ->
        IndexApplication
          { iFunc = subTypesIntoArray types iFunc
          ; args
          ; type' = Type.subTypesIntoArr types type'
          }
      | Unbox { indexBindings; valueBinding; box; body; type' } ->
        Unbox
          { indexBindings
          ; valueBinding
          ; box = subTypesIntoArray types box
          ; body = subTypesIntoArray types body
          ; type' = Type.subTypesIntoArr types type'
          }
      | ReifyIndex { index; type' } ->
        ReifyIndex { index; type' = Type.subTypesIntoArr types type' }
      | Let { binding; value; body; type' } ->
        Let
          { binding
          ; value = subTypesIntoArray types value
          ; body = subTypesIntoArray types body
          ; type' = Type.subTypesIntoArray types type'
          }
      | Primitive { name; type' } ->
        Primitive { name; type' = Type.subTypesIntoArray types type' }
      | Lift { indexBinding; indexValue; sort; frameShape; body; type' } ->
        Lift
          { indexBinding
          ; indexValue = subTypesIntoArray types indexValue
          ; sort
          ; frameShape
          ; body = subTypesIntoArray types body
          ; type' = Type.subTypesIntoArray types type'
          }

    and subTypesIntoAtom types = function
      | TermLambda lambda -> TermLambda (subTypesIntoTermLambda types lambda)
      | TypeLambda { params; body; type' } ->
        TypeLambda
          { params
          ; body = subTypesIntoArray types body
          ; type' = Type.subTypesIntoForall types type'
          }
      | IndexLambda { params; body; type' } ->
        IndexLambda
          { params
          ; body = subTypesIntoArray types body
          ; type' = Type.subTypesIntoPi types type'
          }
      | Box { indices; body; bodyType; type' } ->
        Box
          { indices
          ; body = subTypesIntoArray types body
          ; bodyType = Type.subTypesIntoArray types bodyType
          ; type' = Type.subTypesIntoSigma types type'
          }
      | Literal _ as literal -> literal

    and subTypesIntoTermLambda types { params; body; type' } =
      { params =
          List.map params ~f:(fun { binding; bound } ->
            { binding; bound = Type.subTypesIntoArray types bound })
      ; body = subTypesIntoArray types body
      ; type' = Type.subTypesIntoFunc types type'
      }
    ;;

    let rec subIndicesIntoArray indices =
      let open Expr in
      function
      | Ref _ as ref -> ref
      | Scalar { element; type' } ->
        Scalar
          { element = subIndicesIntoAtom indices element
          ; type' = Type.subIndicesIntoArr indices type'
          }
      | Frame { elements; dimensions; type' } ->
        Frame
          { elements = List.map elements ~f:(subIndicesIntoArray indices)
          ; dimensions
          ; type' = Type.subIndicesIntoArr indices type'
          }
      | TermApplication { func; args; type' } ->
        TermApplication
          { func = subIndicesIntoArray indices func
          ; args = List.map args ~f:(subIndicesIntoArray indices)
          ; type' = Type.subIndicesIntoArr indices type'
          }
      | TypeApplication { tFunc; args; type' } ->
        TypeApplication
          { tFunc = subIndicesIntoArray indices tFunc
          ; args = List.map args ~f:(Type.subIndicesIntoType indices)
          ; type' = Type.subIndicesIntoArr indices type'
          }
      | IndexApplication { iFunc; args; type' } ->
        IndexApplication
          { iFunc = subIndicesIntoArray indices iFunc
          ; args = List.map args ~f:(Index.subIndicesIntoIndex indices)
          ; type' = Type.subIndicesIntoArr indices type'
          }
      | Unbox { indexBindings; valueBinding; box; body; type' } ->
        Unbox
          { indexBindings
          ; valueBinding
          ; box = subIndicesIntoArray indices box
          ; body = subIndicesIntoArray indices body
          ; type' = Type.subIndicesIntoArr indices type'
          }
      | ReifyIndex { index; type' } ->
        ReifyIndex
          { index = Index.subIndicesIntoIndex indices index
          ; type' = Type.subIndicesIntoArr indices type'
          }
      | Let { binding; value; body; type' } ->
        Let
          { binding
          ; value = subIndicesIntoArray indices value
          ; body = subIndicesIntoArray indices body
          ; type' = Type.subIndicesIntoArray indices type'
          }
      | Primitive { name; type' } ->
        Primitive { name; type' = Type.subIndicesIntoArray indices type' }
      | Lift { indexBinding; indexValue; sort; frameShape; body; type' } ->
        Lift
          { indexBinding
          ; indexValue = subIndicesIntoArray indices indexValue
          ; sort
          ; frameShape = Index.subIndicesIntoShape indices frameShape
          ; body = subIndicesIntoArray indices body
          ; type' = Type.subIndicesIntoArray indices type'
          }

    and subIndicesIntoAtom indices = function
      | TermLambda lambda -> TermLambda (subIndicesIntoTermLambda indices lambda)
      | TypeLambda { params; body; type' } ->
        TypeLambda
          { params
          ; body = subIndicesIntoArray indices body
          ; type' = Type.subIndicesIntoForall indices type'
          }
      | IndexLambda { params; body; type' } ->
        IndexLambda
          { params
          ; body = subIndicesIntoArray indices body
          ; type' = Type.subIndicesIntoPi indices type'
          }
      | Box { indices = boxIndices; body; bodyType; type' } ->
        Box
          { indices = boxIndices
          ; body = subIndicesIntoArray indices body
          ; bodyType = Type.subIndicesIntoArray indices bodyType
          ; type' = Type.subIndicesIntoSigma indices type'
          }
      | Literal _ as literal -> literal

    and subIndicesIntoTermLambda indices { params; body; type' } =
      { params =
          List.map params ~f:(fun { binding; bound } ->
            { binding; bound = Type.subIndicesIntoArray indices bound })
      ; body = subIndicesIntoArray indices body
      ; type' = Type.subIndicesIntoFunc indices type'
      }
    ;;
  end
end
