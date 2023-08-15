open! Base

(* The ExplicitNucleus language represents a Remora program where maps have been made explicit *)

type 't param = 't Nucleus.param [@@deriving sexp, eq]

module Index = Nucleus.Index
module Type = Nucleus.Type

module Expr = struct
  type ref =
    { id : Identifier.t
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }
  [@@deriving sexp, eq]

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
    ; args : ref list
    ; type' : Type.arr
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

  and mapArg =
    { binding : Identifier.t
    ; value : array
    }

  and primitive = Nucleus.Expr.primitive
  and literal = Nucleus.Expr.literal

  and map =
    { args : mapArg list
    ; body : array
    ; frameShape : Index.shape
    ; type' : Type.array
    }

  and array =
    | Ref of ref
    | Scalar of scalar
    | Frame of frame
    | TermApplication of termApplication
    | TypeApplication of typeApplication
    | IndexApplication of indexApplication
    | Unbox of unbox
    | TupleLet of tupleLet
    | Primitive of primitive
    | Map of map

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
  [@@deriving sexp, eq]

  let atomType : atom -> Type.atom = function
    | TermLambda termLambda -> Func termLambda.type'
    | TypeLambda typeLambda -> Forall typeLambda.type'
    | IndexLambda indexLambda -> Pi indexLambda.type'
    | Box box -> Sigma box.type'
    | Tuple tuple -> Tuple tuple.type'
    | Literal (IntLiteral _) -> Literal IntLiteral
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
    | TupleLet tupleLet -> tupleLet.type'
    | Primitive primitive -> primitive.type'
    | Map map -> map.type'
  ;;
end

type t = Expr.array [@@deriving sexp]

module Substitute = struct
  module Index = Nucleus.Substitute.Index
  module Type = Nucleus.Substitute.Type

  module Expr = struct
    let rec subTypesIntoArray types =
      let open Expr in
      function
      | Ref { id; type' } -> Ref { id; type' = Type.subTypesIntoArray types type' }
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
          ; args = List.map args ~f:(subTypesIntoRef types)
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
      | TupleLet { params; value; body; type' } ->
        TupleLet
          { params =
              List.map params ~f:(fun { binding; bound } : 'u param ->
                { binding; bound = Type.subTypesIntoAtom types bound })
          ; value = subTypesIntoArray types value
          ; body = subTypesIntoArray types body
          ; type' = Type.subTypesIntoArray types type'
          }
      | Primitive { func; type' } ->
        Primitive { func; type' = Type.subTypesIntoArray types type' }
      | Map { args; body; frameShape; type' } ->
        Map
          { args =
              List.map args ~f:(fun { binding; value } ->
                { binding; value = subTypesIntoArray types value })
          ; body = subTypesIntoArray types body
          ; frameShape
          ; type' = Type.subTypesIntoArray types type'
          }

    and subTypesIntoRef types { id; type' } =
      { id; type' = Type.subTypesIntoArray types type' }

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
      | Tuple { elements; type' } ->
        Tuple
          { elements = List.map elements ~f:(subTypesIntoAtom types)
          ; type' = Type.subTypesIntoTuple types type'
          }
      | Literal _ as literal -> literal

    and subTypesIntoTermLambda types { params; body; type' } =
      { params =
          List.map params ~f:(fun { binding; bound } : 'v param ->
            { binding; bound = Type.subTypesIntoArray types bound })
      ; body = subTypesIntoArray types body
      ; type' = Type.subTypesIntoFunc types type'
      }
    ;;

    let rec subIndicesIntoArray indices =
      let open Expr in
      function
      | Ref { id; type' } -> Ref { id; type' = Type.subIndicesIntoArray indices type' }
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
          ; args = List.map args ~f:(subIndicesIntoRef indices)
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
      | TupleLet { params; value; body; type' } ->
        TupleLet
          { params =
              List.map params ~f:(fun { binding; bound } : 'u param ->
                { binding; bound = Type.subIndicesIntoAtom indices bound })
          ; value = subIndicesIntoArray indices value
          ; body = subIndicesIntoArray indices body
          ; type' = Type.subIndicesIntoArray indices type'
          }
      | Primitive { func; type' } ->
        Primitive { func; type' = Type.subIndicesIntoArray indices type' }
      | Map { args; body; frameShape; type' } ->
        Map
          { args =
              List.map args ~f:(fun { binding; value } ->
                { binding; value = subIndicesIntoArray indices value })
          ; body = subIndicesIntoArray indices body
          ; frameShape = Index.subIndicesIntoShape indices frameShape
          ; type' = Type.subIndicesIntoArray indices type'
          }

    and subIndicesIntoRef indices { id; type' } =
      { id; type' = Type.subIndicesIntoArray indices type' }

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
      | Tuple { elements; type' } ->
        Tuple
          { elements = List.map elements ~f:(subIndicesIntoAtom indices)
          ; type' = Type.subIndicesIntoTuple indices type'
          }
      | Literal _ as literal -> literal

    and subIndicesIntoTermLambda indices { params; body; type' } =
      { params =
          List.map params ~f:(fun { binding; bound } : 'v param ->
            { binding; bound = Type.subIndicesIntoArray indices bound })
      ; body = subIndicesIntoArray indices body
      ; type' = Type.subIndicesIntoFunc indices type'
      }
    ;;
  end
end
