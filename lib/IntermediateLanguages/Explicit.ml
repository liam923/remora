open! Base

(* The Explicit language represents a Remora program where maps have been made explicit *)

type 't param = 't Typed.param [@@deriving sexp, eq, compare]

module Index = Typed.Index
module Type = Typed.Type

module Expr = struct
  type ref =
    { id : Identifier.t
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }

  and scalar =
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
    ; type' : Type.array
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

  and boxValue =
    { box : array
    ; type' : Type.arr [@sexp_drop_if fun _ -> true]
    }

  and indexValue =
    | Runtime of array
    | FromBox of
        { box : array
        ; i : int
        }

  and indexArg =
    { indexBinding : Identifier.t
    ; indexValue : indexValue
    ; sort : Sort.t
    }

  and indexLet =
    { indexArgs : indexArg list
    ; body : array
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
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

  and mapArg =
    { binding : Identifier.t
    ; value : array
    }

  and primitive = Typed.Expr.primitive
  and literal = Typed.Expr.literal

  and contiguousSubArray =
    { arrayArg : array
    ; indexArg : array
    ; originalShape : Index.shape
    ; resultShape : Index.shape
    ; cellShape : Index.shape
    ; l : Index.dimension
    ; type' : Type.array [@sexp_drop_if fun _ -> true]
    }

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
    | BoxValue of boxValue
    | IndexLet of indexLet
    | ReifyIndex of reifyIndex
    | Primitive of primitive
    | ContiguousSubArray of contiguousSubArray
    | Map of map

  and atom =
    | TermLambda of termLambda
    | TypeLambda of typeLambda
    | IndexLambda of indexLambda
    | Box of box
    | Literal of literal

  and t =
    | Array of array
    | Atom of atom
  [@@deriving sexp, eq, compare]

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
    | TermApplication termApplication -> termApplication.type'
    | TypeApplication typeApplication -> Arr typeApplication.type'
    | IndexApplication indexApplication -> Arr indexApplication.type'
    | BoxValue boxValue -> Arr boxValue.type'
    | IndexLet indexLet -> indexLet.type'
    | ReifyIndex reifyIndex -> Arr reifyIndex.type'
    | Primitive primitive -> primitive.type'
    | ContiguousSubArray contiguousSubArray -> contiguousSubArray.type'
    | Map map -> map.type'
  ;;
end

type t = Expr.array [@@deriving sexp]

module Substitute = struct
  module Index = Typed.Substitute.Index
  module Type = Typed.Substitute.Type

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
          ; type' = Type.subTypesIntoArray types type'
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
      | BoxValue { box; type' } ->
        BoxValue
          { box = subTypesIntoArray types box; type' = Type.subTypesIntoArr types type' }
      | IndexLet { indexArgs; body; type' } ->
        IndexLet
          { indexArgs =
              List.map indexArgs ~f:(fun { indexBinding; indexValue; sort } ->
                { indexBinding
                ; indexValue =
                    (match indexValue with
                     | Runtime v -> Runtime (subTypesIntoArray types v)
                     | FromBox { box; i } ->
                       FromBox { box = subTypesIntoArray types box; i })
                ; sort
                })
          ; body = subTypesIntoArray types body
          ; type' = Type.subTypesIntoArray types type'
          }
      | ReifyIndex { index; type' } ->
        ReifyIndex { index; type' = Type.subTypesIntoArr types type' }
      | Primitive { name; type' } ->
        Primitive { name; type' = Type.subTypesIntoArray types type' }
      | Map { args; body; frameShape; type' } ->
        Map
          { args =
              List.map args ~f:(fun { binding; value } ->
                { binding; value = subTypesIntoArray types value })
          ; body = subTypesIntoArray types body
          ; frameShape
          ; type' = Type.subTypesIntoArray types type'
          }
      | ContiguousSubArray
          { arrayArg; indexArg; originalShape; resultShape; cellShape; l; type' } ->
        ContiguousSubArray
          { arrayArg = subTypesIntoArray types arrayArg
          ; indexArg = subTypesIntoArray types indexArg
          ; originalShape
          ; resultShape
          ; cellShape
          ; l
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
          ; type' = Type.subIndicesIntoArray indices type'
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
      | BoxValue { box; type' } ->
        BoxValue
          { box = subIndicesIntoArray indices box
          ; type' = Type.subIndicesIntoArr indices type'
          }
      | IndexLet { indexArgs; body; type' } ->
        IndexLet
          { indexArgs =
              List.map indexArgs ~f:(fun { indexBinding; indexValue; sort } ->
                { indexBinding
                ; indexValue =
                    (match indexValue with
                     | Runtime v -> Runtime (subIndicesIntoArray indices v)
                     | FromBox { box; i } ->
                       FromBox { box = subIndicesIntoArray indices box; i })
                ; sort
                })
          ; body = subIndicesIntoArray indices body
          ; type' = Type.subIndicesIntoArray indices type'
          }
      | ReifyIndex { index; type' } ->
        ReifyIndex
          { index = Index.subIndicesIntoIndex indices index
          ; type' = Type.subIndicesIntoArr indices type'
          }
      | Primitive { name; type' } ->
        Primitive { name; type' = Type.subIndicesIntoArray indices type' }
      | ContiguousSubArray
          { arrayArg; indexArg; originalShape; resultShape; cellShape; l; type' } ->
        ContiguousSubArray
          { arrayArg = subIndicesIntoArray indices arrayArg
          ; indexArg = subIndicesIntoArray indices indexArg
          ; originalShape = Index.subIndicesIntoShape indices originalShape
          ; resultShape = Index.subIndicesIntoShape indices resultShape
          ; cellShape = Index.subIndicesIntoShape indices cellShape
          ; l = Index.subIndicesIntoDim indices l
          ; type' = Type.subIndicesIntoArray indices type'
          }
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
      | Literal _ as literal -> literal

    and subIndicesIntoTermLambda indices { params; body; type' } =
      { params =
          List.map params ~f:(fun { binding; bound } : 'v param ->
            { binding; bound = Type.subIndicesIntoArray indices bound })
      ; body = subIndicesIntoArray indices body
      ; type' = Type.subIndicesIntoFunc indices type'
      }
    ;;

    let rec subRefsIntoArray refs =
      let open Expr in
      function
      | Ref ref -> Ref (subRefsIntoRef refs ref)
      | Scalar { element; type' } ->
        Scalar { element = subRefsIntoAtom refs element; type' }
      | Frame { elements; dimensions; type' } ->
        Frame
          { elements = List.map elements ~f:(subRefsIntoArray refs); dimensions; type' }
      | TermApplication { func; args; type' } ->
        TermApplication
          { func = subRefsIntoArray refs func
          ; args = List.map args ~f:(subRefsIntoRef refs)
          ; type'
          }
      | TypeApplication { tFunc; args; type' } ->
        TypeApplication { tFunc = subRefsIntoArray refs tFunc; args; type' }
      | IndexApplication { iFunc; args; type' } ->
        IndexApplication { iFunc = subRefsIntoArray refs iFunc; args; type' }
      | BoxValue { box; type' } -> BoxValue { box = subRefsIntoArray refs box; type' }
      | IndexLet { indexArgs; body; type' } ->
        IndexLet
          { indexArgs =
              List.map indexArgs ~f:(fun { indexBinding; indexValue; sort } ->
                { indexBinding
                ; indexValue =
                    (match indexValue with
                     | Runtime v -> Runtime (subRefsIntoArray refs v)
                     | FromBox { box; i } ->
                       FromBox { box = subRefsIntoArray refs box; i })
                ; sort
                })
          ; body = subRefsIntoArray refs body
          ; type'
          }
      | ReifyIndex { index; type' } -> ReifyIndex { index; type' }
      | Primitive { name; type' } -> Primitive { name; type' }
      | ContiguousSubArray
          { arrayArg; indexArg; originalShape; resultShape; cellShape; l; type' } ->
        ContiguousSubArray
          { arrayArg = subRefsIntoArray refs arrayArg
          ; indexArg = subRefsIntoArray refs indexArg
          ; originalShape
          ; resultShape
          ; cellShape
          ; l
          ; type'
          }
      | Map { args; body; frameShape; type' } ->
        Map
          { args =
              List.map args ~f:(fun { binding; value } ->
                { binding; value = subRefsIntoArray refs value })
          ; body = subRefsIntoArray refs body
          ; frameShape
          ; type'
          }

    and subRefsIntoAtom refs =
      let open Expr in
      function
      | TermLambda { params; body; type' } ->
        TermLambda { params; body = subRefsIntoArray refs body; type' }
      | TypeLambda { params; body; type' } ->
        TypeLambda { params; body = subRefsIntoArray refs body; type' }
      | IndexLambda { params; body; type' } ->
        IndexLambda { params; body = subRefsIntoArray refs body; type' }
      | Box { indices; body; bodyType; type' } ->
        Box { indices; body = subRefsIntoArray refs body; bodyType; type' }
      | Literal _ as literal -> literal

    and subRefsIntoRef refs { id; type' } =
      { id = Map.find refs id |> Option.value ~default:id; type' }
    ;;
  end
end
