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

let rec subIndicesIntoArrayType indices =
  let open Nucleus.Type in
  function
  | ArrayRef _ as ref -> ref
  | Arr { element; shape } ->
    Arr
      { element = subIndicesIntoAtomType indices element
      ; shape = subIndicesIntoShapeIndex indices shape
      }

and subIndicesIntoAtomType indices =
  let open Nucleus.Type in
  function
  | AtomRef _ as ref -> ref
  | Func { parameters; return } ->
    Func
      { parameters = List.map parameters ~f:(subIndicesIntoArrayType indices)
      ; return = subIndicesIntoArrayType indices return
      }
  | Forall { parameters; body } ->
    Forall { parameters; body = subIndicesIntoArrayType indices body }
  | Pi { parameters; body } ->
    Pi { parameters; body = subIndicesIntoArrayType indices body }
  | Sigma { parameters; body } ->
    Sigma { parameters; body = subIndicesIntoArrayType indices body }
  | Tuple elements -> Tuple (List.map elements ~f:(subIndicesIntoAtomType indices))
  | Literal IntLiteral -> Literal IntLiteral
  | Literal CharacterLiteral -> Literal CharacterLiteral
;;

let rec subTypesIntoArrayType types =
  let open Nucleus.Type in
  function
  | ArrayRef id as ref ->
    (match Map.find types id with
    | Some (Array arrayType) -> arrayType
    | Some (Atom _) -> ref
    | None -> ref)
  | Arr { element; shape } -> Arr { element = subTypesIntoAtomType types element; shape }

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
  | Forall { parameters; body } ->
    Forall { parameters; body = subTypesIntoArrayType types body }
  | Pi { parameters; body } -> Pi { parameters; body = subTypesIntoArrayType types body }
  | Sigma { parameters; body } ->
    Sigma { parameters; body = subTypesIntoArrayType types body }
  | Tuple elements -> Tuple (List.map elements ~f:(subTypesIntoAtomType types))
  | Literal IntLiteral -> Literal IntLiteral
  | Literal CharacterLiteral -> Literal CharacterLiteral
;;
