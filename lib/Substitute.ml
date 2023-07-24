open! Base

let subIndicesIntoDimIndex indices ({ const; refs } : Nucleus.Index.dimension) =
  let open Nucleus.Index in
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
          Map.fold sub.refs ~init:acc.refs ~f:(fun ~key:ref ~data:count combinedRefs ->
              Map.update combinedRefs ref ~f:(fun prevCount ->
                  Option.value prevCount ~default:0 + count))
        in
        { const = combinedConsts; refs = combinedRefs }
      | Some (Shape _) -> acc
      | None -> acc)
;;

open! Base

let subIndicesIntoShapeIndex indices shape =
  let open Nucleus.Index in
  List.bind shape ~f:(function
      | Add dim -> [ Add (subIndicesIntoDimIndex indices dim) ]
      | ShapeRef id as ref ->
        (match Map.find indices id with
        | Some (Shape shape) -> shape
        | Some (Dimension _) -> [ ref ]
        | None -> [ ref ]))
;;

let subIndicesIntoIndex indices =
  let open Nucleus.Index in
  function
  | Shape shape -> Shape (subIndicesIntoShapeIndex indices shape)
  | Dimension dim -> Dimension (subIndicesIntoDimIndex indices dim)
;;

let rec subIndicesIntoArrayType indices =
  let open Nucleus.Type in
  function
  | ArrayRef _ as ref -> ref
  | Arr arr -> Arr (subIndicesIntoArrType indices arr)

and subIndicesIntoArrType indices Nucleus.Type.{ element; shape } =
  Nucleus.Type.
    { element = subIndicesIntoAtomType indices element
    ; shape = subIndicesIntoShapeIndex indices shape
    }

and subIndicesIntoAtomType indices =
  let open Nucleus.Type in
  function
  | AtomRef _ as ref -> ref
  | Func func -> Func (subIndicesIntoFuncType indices func)
  | Forall forall -> Forall (subIndicesIntoForallType indices forall)
  | Pi pi -> Pi (subIndicesIntoPiType indices pi)
  | Sigma sigma -> Sigma (subIndicesIntoSigmaType indices sigma)
  | Tuple tuple -> Tuple (subIndicesIntoTupleType indices tuple)
  | Literal IntLiteral -> Literal IntLiteral
  | Literal CharacterLiteral -> Literal CharacterLiteral

and subIndicesIntoFuncType indices Nucleus.Type.{ parameters; return } =
  Nucleus.Type.
    { parameters = List.map parameters ~f:(subIndicesIntoArrayType indices)
    ; return = subIndicesIntoArrayType indices return
    }

and subIndicesIntoForallType indices Nucleus.Type.{ parameters; body } =
  Nucleus.Type.{ parameters; body = subIndicesIntoArrayType indices body }

and subIndicesIntoPiType indices Nucleus.Type.{ parameters; body } =
  Nucleus.Type.{ parameters; body = subIndicesIntoArrayType indices body }

and subIndicesIntoSigmaType indices Nucleus.Type.{ parameters; body } =
  Nucleus.Type.{ parameters; body = subIndicesIntoArrayType indices body }

and subIndicesIntoTupleType indices = List.map ~f:(subIndicesIntoAtomType indices)

and subIndicesIntoType indices =
  let open Nucleus.Type in
  function
  | Array array -> Array (subIndicesIntoArrayType indices array)
  | Atom atom -> Atom (subIndicesIntoAtomType indices atom)
;;

let rec subTypesIntoArrayType types =
  let open Nucleus.Type in
  function
  | ArrayRef id as ref ->
    (match Map.find types id with
    | Some (Array arrayType) -> arrayType
    | Some (Atom _) -> ref
    | None -> ref)
  | Arr arr -> Arr (subTypesIntoArrType types arr)

and subTypesIntoArrType types Nucleus.Type.{ element; shape } =
  Nucleus.Type.{ element = subTypesIntoAtomType types element; shape }

and subTypesIntoAtomType types =
  let open Nucleus.Type in
  function
  | AtomRef id as ref ->
    (match Map.find types id with
    | Some (Atom atomType) -> atomType
    | Some (Array _) -> ref
    | None -> ref)
  | Func { parameters; return } ->
    Func
      { parameters = List.map parameters ~f:(subTypesIntoArrayType types)
      ; return = subTypesIntoArrayType types return
      }
  | Forall forall -> Forall (subTypesIntoForallType types forall)
  | Pi pi -> Pi (subTypesIntoPiType types pi)
  | Sigma sigma -> Sigma (subTypesIntoSigmaType types sigma)
  | Tuple tuple -> Tuple (subTypesIntoTupleType types tuple)
  | Literal IntLiteral -> Literal IntLiteral
  | Literal CharacterLiteral -> Literal CharacterLiteral

and subTypesIntoFuncType types Nucleus.Type.{ parameters; return } =
  Nucleus.Type.
    { parameters = List.map parameters ~f:(subTypesIntoArrayType types)
    ; return = subTypesIntoArrayType types return
    }

and subTypesIntoForallType types Nucleus.Type.{ parameters; body } =
  Nucleus.Type.{ parameters; body = subTypesIntoArrayType types body }

and subTypesIntoPiType types Nucleus.Type.{ parameters; body } =
  Nucleus.Type.{ parameters; body = subTypesIntoArrayType types body }

and subTypesIntoSigmaType types Nucleus.Type.{ parameters; body } =
  Nucleus.Type.{ parameters; body = subTypesIntoArrayType types body }

and subTypesIntoTupleType types = List.map ~f:(subTypesIntoAtomType types)

and subTypesIntoType types =
  let open Nucleus.Type in
  function
  | Array array -> Array (subTypesIntoArrayType types array)
  | Atom atom -> Atom (subTypesIntoAtomType types atom)
;;

let rec subTypesIntoArrayExpr types =
  let open Nucleus.Expr in
  function
  | Ref _ as ref -> ref
  | Scalar { element; type' } ->
    Scalar
      { element = subTypesIntoAtomExpr types element
      ; type' = subTypesIntoArrType types type'
      }
  | Frame { elements; dimensions; type' } ->
    Frame
      { elements = List.map elements ~f:(subTypesIntoArrayExpr types)
      ; dimensions
      ; type' = subTypesIntoArrType types type'
      }
  | TermApplication { func; args; type' } ->
    TermApplication
      { func = subTypesIntoArrayExpr types func
      ; args = List.map args ~f:(subTypesIntoArrayExpr types)
      ; type' = subTypesIntoArrType types type'
      }
  | TypeApplication { tFunc; args; type' } ->
    TypeApplication
      { tFunc = subTypesIntoArrayExpr types tFunc
      ; args = List.map args ~f:(subTypesIntoType types)
      ; type' = subTypesIntoArrType types type'
      }
  | IndexApplication { iFunc; args; type' } ->
    IndexApplication
      { iFunc = subTypesIntoArrayExpr types iFunc
      ; args
      ; type' = subTypesIntoArrType types type'
      }
  | Unbox { indexBindings; valueBinding; box; body; type' } ->
    Unbox
      { indexBindings
      ; valueBinding
      ; box = subTypesIntoArrayExpr types box
      ; body = subTypesIntoArrayExpr types body
      ; type' = subTypesIntoArrType types type'
      }
  | Let { binding; value; body; type' } ->
    Let
      { binding
      ; value = subTypesIntoArrayExpr types value
      ; body = subTypesIntoArrayExpr types body
      ; type' = subTypesIntoArrayType types type'
      }
  | TupleLet { params; value; body; type' } ->
    TupleLet
      { params =
          List.map params ~f:(fun { binding; bound } ->
              Nucleus.{ binding; bound = subTypesIntoAtomType types bound })
      ; value = subTypesIntoArrayExpr types value
      ; body = subTypesIntoArrayExpr types body
      ; type' = subTypesIntoArrayType types type'
      }

and subTypesIntoAtomExpr types = function
  | TermLambda { params; body; type' } ->
    TermLambda
      { params =
          List.map params ~f:(fun { binding; bound } ->
              Nucleus.{ binding; bound = subTypesIntoArrayType types bound })
      ; body = subTypesIntoArrayExpr types body
      ; type' = subTypesIntoFuncType types type'
      }
  | TypeLambda { params; body; type' } ->
    TypeLambda
      { params
      ; body = subTypesIntoArrayExpr types body
      ; type' = subTypesIntoForallType types type'
      }
  | IndexLambda { params; body; type' } ->
    IndexLambda
      { params
      ; body = subTypesIntoArrayExpr types body
      ; type' = subTypesIntoPiType types type'
      }
  | Box { indices; body; bodyType; type' } ->
    Box
      { indices
      ; body = subTypesIntoArrayExpr types body
      ; bodyType = subTypesIntoArrayType types bodyType
      ; type' = subTypesIntoSigmaType types type'
      }
  | Tuple { elements; type' } ->
    Tuple
      { elements = List.map elements ~f:(subTypesIntoAtomExpr types)
      ; type' = subTypesIntoTupleType types type'
      }
  | Literal _ as literal -> literal
;;

let rec subIndicesIntoArrayExpr indices =
  let open Nucleus.Expr in
  function
  | Ref _ as ref -> ref
  | Scalar { element; type' } ->
    Scalar
      { element = subIndicesIntoAtomExpr indices element
      ; type' = subIndicesIntoArrType indices type'
      }
  | Frame { elements; dimensions; type' } ->
    Frame
      { elements = List.map elements ~f:(subIndicesIntoArrayExpr indices)
      ; dimensions
      ; type' = subIndicesIntoArrType indices type'
      }
  | TermApplication { func; args; type' } ->
    TermApplication
      { func = subIndicesIntoArrayExpr indices func
      ; args = List.map args ~f:(subIndicesIntoArrayExpr indices)
      ; type' = subIndicesIntoArrType indices type'
      }
  | TypeApplication { tFunc; args; type' } ->
    TypeApplication
      { tFunc = subIndicesIntoArrayExpr indices tFunc
      ; args = List.map args ~f:(subIndicesIntoType indices)
      ; type' = subIndicesIntoArrType indices type'
      }
  | IndexApplication { iFunc; args; type' } ->
    IndexApplication
      { iFunc = subIndicesIntoArrayExpr indices iFunc
      ; args = List.map args ~f:(subIndicesIntoIndex indices)
      ; type' = subIndicesIntoArrType indices type'
      }
  | Unbox { indexBindings; valueBinding; box; body; type' } ->
    Unbox
      { indexBindings
      ; valueBinding
      ; box = subIndicesIntoArrayExpr indices box
      ; body = subIndicesIntoArrayExpr indices body
      ; type' = subIndicesIntoArrType indices type'
      }
  | Let { binding; value; body; type' } ->
    Let
      { binding
      ; value = subIndicesIntoArrayExpr indices value
      ; body = subIndicesIntoArrayExpr indices body
      ; type' = subIndicesIntoArrayType indices type'
      }
  | TupleLet { params; value; body; type' } ->
    TupleLet
      { params =
          List.map params ~f:(fun { binding; bound } ->
              Nucleus.{ binding; bound = subIndicesIntoAtomType indices bound })
      ; value = subIndicesIntoArrayExpr indices value
      ; body = subIndicesIntoArrayExpr indices body
      ; type' = subIndicesIntoArrayType indices type'
      }

and subIndicesIntoAtomExpr indices = function
  | TermLambda { params; body; type' } ->
    TermLambda
      { params =
          List.map params ~f:(fun { binding; bound } ->
              Nucleus.{ binding; bound = subIndicesIntoArrayType indices bound })
      ; body = subIndicesIntoArrayExpr indices body
      ; type' = subIndicesIntoFuncType indices type'
      }
  | TypeLambda { params; body; type' } ->
    TypeLambda
      { params
      ; body = subIndicesIntoArrayExpr indices body
      ; type' = subIndicesIntoForallType indices type'
      }
  | IndexLambda { params; body; type' } ->
    IndexLambda
      { params
      ; body = subIndicesIntoArrayExpr indices body
      ; type' = subIndicesIntoPiType indices type'
      }
  | Box { indices = boxIndices; body; bodyType; type' } ->
    Box
      { indices = boxIndices
      ; body = subIndicesIntoArrayExpr indices body
      ; bodyType = subIndicesIntoArrayType indices bodyType
      ; type' = subIndicesIntoSigmaType indices type'
      }
  | Tuple { elements; type' } ->
    Tuple
      { elements = List.map elements ~f:(subIndicesIntoAtomExpr indices)
      ; type' = subIndicesIntoTupleType indices type'
      }
  | Literal _ as literal -> literal
;;
