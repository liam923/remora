open! Base
open Source

type 't expectedActual =
  { expected : 't
  ; actual : 't
  }

type error =
  | UnboundIndexVariable of string
  | UnexpectedSort of
      { expected : Sort.t
      ; actual : Sort.t
      }
  | DuplicateTypeParameterName of string
  | UnboundTypeVariable of string
  | UnexpectedKind of
      { expected : Kind.t
      ; actual : Kind.t
      }
  | DuplicateIndexParameterName of string
  | UnboundVariable of string
  | DuplicateParameterName of string
  | WrongNumberOfElementsInArray of int expectedActual
  | WrongNumberOfArraysInFrame of int expectedActual
  | WrongNumberOfElementsInBoxes of int expectedActual
  | ArrayTypeDisagreement of
      { firstElementType : Core.Type.t
      ; gotElementType : Core.Type.t
      }
  | FrameTypeDisagreement of
      { firstArrayType : Core.Type.t
      ; gotArrayType : Core.Type.t
      }
  | SigmaBodyTypeDisagreement of Core.Type.t expectedActual
  | ExpectedAtomicExpr of { actual : Core.Type.t }
  | ExpectedArrType of { actual : Core.Type.t }
  | ExpectedTupleType of { actual : Core.Type.t }
  | ExpectedSigmaType of { actual : Core.Type.t }
  | ExpectedFuncType of { actual : Core.Type.t }
  | ExpectedValue
  | ExpectedForall of { actual : Core.Type.t }
  | WrongNumberOfArguments of int expectedActual
  | WrongNumberOfBindings of int expectedActual
  | WrongNumberOfUnboxParameters of int expectedActual
  | DuplicateTupleBindingName of string
  | LetTypeDisagreement of Core.Type.t expectedActual
  | TupleLetTypeDisagreement of Core.Type.t expectedActual
  | UnexpectedSortBoundInUnbox of Sort.t expectedActual
  | EscapingRef of string
  | ArgumentTypeDisagreement of Core.Type.t expectedActual
  | CellShapeDisagreement of Core.Index.shape expectedActual
  | PrincipalFrameDisagreement of Core.Index.shape expectedActual

module Show : sig
  val sort : Sort.t -> string
  val kind : Kind.t -> string
  val shape : Core.Index.shape -> string
  val type' : Core.Type.t -> string
end = struct
  let sort = function
    | Sort.Dim -> "Dim"
    | Sort.Shape -> "Shape"
  ;;

  let kind = function
    | Kind.Atom -> "Atom"
    | Kind.Array -> "Array"
  ;;

  let showDimension ({ const; refs } : Core.Index.dimension) =
    let constElement = if const = 0 then None else Some (Int.to_string const) in
    let refElements =
      refs
      |> Map.to_alist
      |> List.bind ~f:(fun (id, count) -> List.init count ~f:(fun _ -> Some id.name))
    in
    let elements = constElement :: refElements |> List.filter_opt in
    match elements with
    | [] -> "0"
    | [ e ] -> e
    | elems ->
      let joinedElems = String.concat ~sep:" " elems in
      [%string "(+ %{joinedElems})"]
  ;;

  let showShapeElement = function
    | Core.Index.Add dim -> showDimension dim
    | Core.Index.ShapeRef ref -> ref.name
  ;;

  let showList ?(prependSpace = false) shower list =
    if prependSpace
    then list |> List.bind ~f:(fun e -> [ " "; shower e ]) |> String.concat
    else list |> List.map ~f:shower |> String.concat ~sep:" "
  ;;

  let shape elements = [%string "[%{showList showShapeElement elements}]"]
  let showParam (p : 't Core.param) = p.binding.name

  let rec showArray =
    let open Core.Type in
    function
    | ArrayRef ref -> ref.name
    | Arr { element; shape } ->
      if List.is_empty shape
      then showAtom element
      else (
        let shapeString = showList ~prependSpace:true showShapeElement shape in
        [%string "[%{showAtom element}%{shapeString}]"])

  and showAtom =
    let open Core.Type in
    function
    | AtomRef ref -> ref.name
    | Func { parameters; return } ->
      [%string "(→ (%{showList showArray parameters}) %{showArray return})"]
    | Forall { parameters; body } ->
      let paramsString = parameters |> showList showParam in
      [%string "(∀ (%{paramsString}) %{showArray body})"]
    | Pi { parameters; body } ->
      let paramsString = parameters |> showList showParam in
      [%string "(Π (%{paramsString}) %{showArray body})"]
    | Sigma { parameters; body } ->
      let paramsString = parameters |> showList showParam in
      [%string "(Σ (%{paramsString}) %{showArray body})"]
    | Tuple [ element ] -> [%string "(%{showAtom element} *)"]
    | Tuple elements ->
      let elementsString = elements |> List.map ~f:showAtom |> String.concat ~sep:" * " in
      [%string "(%{elementsString})"]

  and type' : Core.Type.t -> string = function
    | Array array -> showArray array
    | Atom atom -> showAtom atom
  ;;
end

let errorMessage = function
  | UnboundIndexVariable name -> [%string "Unbound index variable `%{name}`"]
  | UnboundTypeVariable name -> [%string "Unbound type variable `%{name}`"]
  | UnboundVariable name -> [%string "Unbound variable `%{name}`"]
  | UnexpectedSort { expected; actual } ->
    [%string
      "Unexpected sort: expected `%{Show.sort expected}`, got `%{Show.sort actual}`"]
  | UnexpectedKind { expected; actual } ->
    [%string
      "Unexpected sort: expected `%{Show.kind expected}`, got `%{Show.kind actual}`"]
  | DuplicateTypeParameterName name -> [%string "Duplicate type parameter name `%{name}`"]
  | DuplicateIndexParameterName name ->
    [%string "Duplicate index parameter name `%{name}`"]
  | DuplicateParameterName name -> [%string "Duplicate parameter name `%{name}`"]
  | WrongNumberOfElementsInArray { expected; actual } ->
    [%string "Expected %{expected#Int} elements in array, got %{actual#Int}"]
  | WrongNumberOfArraysInFrame { expected; actual } ->
    [%string "Expected %{expected#Int} arrays in frame, got %{actual#Int}"]
  | WrongNumberOfElementsInBoxes { expected; actual } ->
    [%string "Expected %{expected#Int} elements in boxes, got %{actual#Int}"]
  | ArrayTypeDisagreement { firstElementType; gotElementType } ->
    [%string
      "Mismatched array elements; first element is type `%{Show.type' \
       firstElementType}`, got `%{Show.type' gotElementType}`"]
  | FrameTypeDisagreement { firstArrayType; gotArrayType } ->
    [%string
      "Mismatched frame arrays; first array is type `%{Show.type' firstArrayType}`, got \
       `%{Show.type' gotArrayType}`"]
  | SigmaBodyTypeDisagreement { expected; actual } ->
    [%string "Expected type `%{Show.type' expected}`, got `%{Show.type' actual}`"]
  | ExpectedAtomicExpr { actual } ->
    [%string "Expected atomic expression, got type `%{Show.type' actual}`"]
  | ExpectedArrType { actual } ->
    [%string "Expected an Arr type, got `%{Show.type' actual}`"]
  | ExpectedTupleType { actual } ->
    [%string "Expected a Tuple type, got `%{Show.type' actual}`"]
  | ExpectedSigmaType { actual } ->
    [%string "Expected a Sigma type, got `%{Show.type' actual}`"]
  | ExpectedFuncType { actual } ->
    [%string "Expected a function type, got `%{Show.type' actual}`"]
  | ExpectedValue -> "Expected a value"
  | ExpectedForall { actual } ->
    [%string "Expected an express with a Forall type, got `%{Show.type' actual}`"]
  | WrongNumberOfArguments { expected; actual } ->
    [%string "Expected %{expected#Int} arguements, got %{actual#Int}"]
  | WrongNumberOfBindings { expected; actual } ->
    [%string "Expected %{expected#Int} bindings for tuple, got %{actual#Int}"]
  | WrongNumberOfUnboxParameters { expected; actual } ->
    [%string "Expected %{expected#Int} parameters for unboxing, got %{actual#Int}"]
  | DuplicateTupleBindingName name -> [%string "Duplicate variable name `%{name}`"]
  | LetTypeDisagreement { expected; actual } ->
    [%string
      "Let expected a value of type `%{Show.type' expected}`, got `%{Show.type' actual}`"]
  | TupleLetTypeDisagreement { expected; actual } ->
    [%string
      "Tuple-Let expected an element of type `%{Show.type' expected}`, got `%{Show.type' \
       actual}`"]
  | UnexpectedSortBoundInUnbox { expected; actual } ->
    [%string
      "Unexpected bound: expected `%{Show.sort expected}`, got `%{Show.sort actual}`"]
  | EscapingRef ref -> [%string "The variable `%{ref}` escapes the box context"]
  | ArgumentTypeDisagreement { expected; actual } ->
    [%string
      "Function expected argument with element type `%{Show.type' expected}`, got \
       `%{Show.type' actual}`"]
  | CellShapeDisagreement { expected; actual } ->
    [%string
      "Function expected argument with cell shape `%{Show.shape expected}`, got \
       `%{Show.shape actual}`"]
  | PrincipalFrameDisagreement { expected; actual } ->
    [%string
      "Function call has principal frame `%{Show.shape expected}`, got frame \
       `%{Show.shape actual}`"]
;;

let errorType = function
  | UnboundIndexVariable _ | UnexpectedSort _ -> `Sort
  | UnboundTypeVariable _
  | UnexpectedKind _
  | DuplicateTypeParameterName _
  | DuplicateIndexParameterName _ -> `Kind
  | UnboundVariable _
  | DuplicateParameterName _
  | WrongNumberOfElementsInArray _
  | WrongNumberOfArraysInFrame _
  | WrongNumberOfElementsInBoxes _
  | ArrayTypeDisagreement _
  | FrameTypeDisagreement _
  | SigmaBodyTypeDisagreement _
  | ExpectedAtomicExpr _
  | ExpectedArrType _
  | ExpectedTupleType _
  | ExpectedSigmaType _
  | ExpectedFuncType _
  | ExpectedValue
  | ExpectedForall _
  | WrongNumberOfArguments _
  | WrongNumberOfBindings _
  | WrongNumberOfUnboxParameters _
  | DuplicateTupleBindingName _
  | LetTypeDisagreement _
  | TupleLetTypeDisagreement _
  | UnexpectedSortBoundInUnbox _
  | EscapingRef _
  | ArgumentTypeDisagreement _
  | CellShapeDisagreement _
  | PrincipalFrameDisagreement _ -> `Type
;;

type ('s, 't) checkResult = ('t, ('s, error) Source.annotate) MResult.t
type state = { idCounter : int }

module CheckerStateT = struct
  include StateT.Make2WithError (MResult)

  type nonrec state = state

  let ok = return
  let err error = returnF (MResult.err error)
  let errs errors = returnF (MResult.Errors errors)

  let ofOption o ~err:error =
    match o with
    | Some v -> return v
    | None -> err error
  ;;

  let require b error = if b then return () else err error

  let traverseOpt = function
    | Some r -> map r ~f:(fun e -> Some e)
    | None -> return None
  ;;

  let createId name =
    make ~f:(fun state ->
        ( { idCounter = state.idCounter + 1 }
        , Core.Identifier.{ name; id = state.idCounter + 1 } ))
  ;;
end

open CheckerStateT.Let_syntax

let ok = CheckerStateT.ok

type ('s, 't) checkerStateT = (state, 't, ('s, error) Source.annotate) CheckerStateT.t

type ('t, 'u) processedParams =
  { typedParams : 't Core.param list
  ; extendedEnv : 'u Environment.entry Map.M(String).t
  }

type ('s, 't, 'u) paramsToMapResult =
  { typedParamsReversed : 't Core.param list
  ; entries : 'u Environment.entry Map.M(String).t
  ; dups : 's Map.M(String).t
  }

(** Extend the environment with the bindings specified by the parameters,
    and convert the untyped parameters into typed parameters. Any duplicate
    parameter names are detected and converted into erros via makeError *)
let processParams env params ~makeError ~boundToEnvEntry
    : ('s, ('t, 'u) processedParams) checkerStateT
  =
  (* Loops over the params, creating environment entries, converting into
      typed params, and detecting duplicates *)
  let rec collapseParams
      (params : ('s, ('s, 't) Ast.param) Source.annotate list)
      (acc : ('s, ('s, 't, 'u) paramsToMapResult) checkerStateT)
      : ('s, ('s, 't, 'u) paramsToMapResult) checkerStateT
    =
    match params with
    | [] -> acc
    | { elem = { binding; bound }; source = _ } :: rest ->
      let name = binding.elem in
      let%bind { typedParamsReversed = oldParams; entries = oldEntries; dups = oldDups } =
        acc
      in
      let%bind id = CheckerStateT.createId name in
      let typedParam : 't Core.param = { binding = id; bound } in
      let newParams = typedParam :: oldParams in
      let entry = Environment.{ e = boundToEnvEntry bound; id = typedParam.binding } in
      (match Map.add oldEntries ~key:name ~data:entry with
      | `Ok newEntries ->
        collapseParams
          rest
          (ok { typedParamsReversed = newParams; entries = newEntries; dups = oldDups })
      | `Duplicate ->
        let newDups = Map.set oldDups ~key:name ~data:binding.source in
        collapseParams
          rest
          (ok { typedParamsReversed = newParams; entries = oldEntries; dups = newDups }))
  in
  let%bind { typedParamsReversed; entries; dups } =
    collapseParams
      params
      (ok
         { typedParamsReversed = []
         ; entries = Map.empty (module String)
         ; dups = Map.empty (module String)
         })
  in
  let typedParams = List.rev typedParamsReversed in
  match NeList.of_list (Map.to_alist dups) with
  | None ->
    let extendedEnv =
      Map.merge_skewed env entries ~combine:(fun ~key:_ _ newEntry -> newEntry)
    in
    ok { typedParams; extendedEnv }
  | Some dups ->
    CheckerStateT.errs
      (NeList.map dups ~f:(fun (name, source) -> { elem = makeError name; source }))
;;

module SortChecker = struct
  let rec check (env : Environment.t) { elem = index; source } =
    let module U = Ast.Index in
    let module T = Core.Index in
    match index with
    | U.Ref name ->
      Map.find env.sorts name
      |> Option.map ~f:(fun entry ->
             match entry.e with
             | Sort.Dim -> T.Dimension (T.dimensionRef entry.id)
             | Sort.Shape -> T.Shape [ T.ShapeRef entry.id ])
      |> CheckerStateT.ofOption ~err:{ source; elem = UnboundIndexVariable name }
    | U.Dimension dim -> ok (T.Dimension (T.dimensionConstant dim))
    | U.Shape indices ->
      let%map indices = CheckerStateT.all (List.map indices ~f:(checkAndExpectDim env)) in
      T.Shape (List.map ~f:(fun d -> T.Add d) indices)
    | U.Add indices ->
      let%map indices = CheckerStateT.all (List.map indices ~f:(checkAndExpectDim env)) in
      let flattenedDimension =
        indices
        |> List.fold_left
             ~init:
               ({ const = 0; refs = Map.empty (module Core.Identifier) } : T.dimension)
             ~f:(fun m1 m2 ->
               { const = m1.const + m2.const
               ; refs =
                   Map.merge_skewed m1.refs m2.refs ~combine:(fun ~key:_ a b -> a + b)
               })
      in
      T.Dimension flattenedDimension
    | U.Append indices ->
      let%map nestedIndices =
        indices |> List.map ~f:(checkAndExpectShape env) |> CheckerStateT.all
      in
      let indices = List.join nestedIndices in
      T.Shape indices
    | U.Slice indices ->
      let%map indices = indices |> List.map ~f:(check env) |> CheckerStateT.all in
      let shapeElements =
        List.bind indices ~f:(function
            | Shape elements -> elements
            | Dimension dim -> [ Add dim ])
      in
      T.Shape shapeElements

  and checkAndExpectDim env index =
    match%bind check env index with
    | Core.Index.Dimension d -> ok d
    | Core.Index.Shape _ ->
      CheckerStateT.err
        { source = index.source
        ; elem = UnexpectedSort { expected = Sort.Dim; actual = Sort.Shape }
        }

  and checkAndExpectShape env index =
    match%bind check env index with
    | Core.Index.Dimension _ ->
      CheckerStateT.err
        { source = index.source
        ; elem = UnexpectedSort { expected = Sort.Dim; actual = Sort.Shape }
        }
    | Core.Index.Shape s -> ok s
  ;;

  let checkAndExpect sort env index =
    match sort with
    | Sort.Dim -> checkAndExpectDim env index >>| fun r -> Core.Index.Dimension r
    | Sort.Shape -> checkAndExpectShape env index >>| fun r -> Core.Index.Shape r
  ;;
end

module KindChecker = struct
  let rec check (env : Environment.t) { elem = type'; source } =
    let module U = Ast.Type in
    let module T = Core.Type in
    match type' with
    | U.Ref name ->
      Map.find env.kinds name
      |> Option.map ~f:(fun entry ->
             match entry.e with
             | Kind.Array -> T.Array (T.ArrayRef entry.id)
             | Kind.Atom -> T.Atom (T.AtomRef entry.id))
      |> CheckerStateT.ofOption ~err:{ source; elem = UnboundTypeVariable name }
    | U.Arr { element; shape } ->
      let%map element = checkAndExpectAtom env element
      and shape = SortChecker.checkAndExpectShape env shape in
      T.Array (T.Arr { element; shape })
    | U.Func { parameters; return } ->
      let%map parameters =
        parameters.elem |> List.map ~f:(checkAndExpectArray env) |> CheckerStateT.all
      and return = checkAndExpectArray env return in
      T.Atom (T.Func { parameters; return })
    | U.Forall { parameters; body } ->
      let parameters =
        (* The parameter's have source-annotated bounds; remove the source annotations *)
        List.map parameters.elem ~f:(function
            | { elem = { binding; bound = { elem = bound; source = _ } }; source } ->
            { elem = ({ binding; bound } : ('s, Kind.t) Ast.param); source })
      in
      let%bind { typedParams = parameters; extendedEnv = extendedKinds } =
        processParams
          env.kinds
          parameters
          ~makeError:(fun name -> DuplicateTypeParameterName name)
          ~boundToEnvEntry:(fun b -> b)
      in
      let extendedEnv = { env with kinds = extendedKinds } in
      let%map body = checkAndExpectArray extendedEnv body in
      T.Atom (T.Forall { parameters; body })
    | U.Pi { parameters; body } ->
      let parameters =
        (* The parameter's have source-annotated bounds; remove the source annotations *)
        List.map parameters.elem ~f:(function
            | { elem = { binding; bound = { elem = bound; source = _ } }; source } ->
            { elem = ({ binding; bound } : ('s, Sort.t) Ast.param); source })
      in
      let%bind { typedParams = parameters; extendedEnv = extendeSorts } =
        processParams
          env.sorts
          parameters
          ~makeError:(fun name -> DuplicateIndexParameterName name)
          ~boundToEnvEntry:(fun b -> b)
      in
      let extendedEnv = { env with sorts = extendeSorts } in
      let%map body = checkAndExpectArray extendedEnv body in
      T.Atom (T.Pi { parameters; body })
    | U.Sigma { parameters; body } ->
      let parameters =
        (* The parameter's have source-annotated bounds; remove the source annotations *)
        List.map parameters.elem ~f:(function
            | { elem = { binding; bound = { elem = bound; source = _ } }; source } ->
            { elem = ({ binding; bound } : ('s, Sort.t) Ast.param); source })
      in
      let%bind { typedParams = parameters; extendedEnv = extendeSorts } =
        processParams
          env.sorts
          parameters
          ~makeError:(fun name -> DuplicateIndexParameterName name)
          ~boundToEnvEntry:(fun b -> b)
      in
      let extendedEnv = { env with sorts = extendeSorts } in
      let%map body = checkAndExpectArray extendedEnv body in
      T.Atom (T.Sigma { parameters; body })
    | U.Tuple elements ->
      let%map kindedElements =
        elements |> List.map ~f:(checkAndExpectAtom env) |> CheckerStateT.all
      in
      T.Atom (T.Tuple kindedElements)

  and checkAndExpectArray env type' =
    let open Core.Type in
    match%bind check env type' with
    | Array array -> ok array
    | Atom atom -> ok (Arr { element = atom; shape = [] })

  and checkAndExpectAtom env type' =
    let open Core.Type in
    match%bind check env type' with
    | Atom atom -> ok atom
    | Array _ ->
      CheckerStateT.err
        { source = type'.source
        ; elem = UnexpectedKind { expected = Kind.Atom; actual = Kind.Array }
        }
  ;;

  let checkAndExpect kind env type' =
    match kind with
    | Kind.Array -> checkAndExpectArray env type' >>| fun r -> Core.Type.Array r
    | Kind.Atom -> checkAndExpectAtom env type' >>| fun r -> Core.Type.Atom r
  ;;
end

module TypeChecker = struct
  let zipLists ~expected ~actual ~makeError =
    match List.zip expected actual with
    | Ok zipped -> ok zipped
    | Unequal_lengths ->
      CheckerStateT.err
        (makeError { expected = List.length expected; actual = List.length actual })
  ;;

  (** Compare two types to check that they are equal*)
  let eqType (a : Core.Type.t) (b : Core.Type.t) : bool =
    let open Core in
    (* Booleans are represented as options in order to be able to use let syntax *)
    let boolToOpt = function
      | true -> Some ()
      | false -> None
    in
    let optToBool : unit option -> bool = Option.is_some in
    let open Option.Let_syntax in
    let rec compareIndices a b bToA : unit option =
      let open Index in
      match a with
      | Dimension a ->
        let%bind b =
          match b with
          | Dimension b -> Some b
          | _ -> None
        in
        let mapKeys m ~f =
          let rec loop alist acc =
            match alist with
            | [] -> acc
            | (key, data) :: rest -> loop rest (Map.set acc ~key:(f key) ~data)
          in
          loop (Map.to_alist m) (Map.empty (Map.comparator_s m))
        in
        let rec compareRefs (aRefList, aRefs) (bRefList, bRefs) =
          match aRefList, bRefList with
          | [], [] -> true
          | [], bRefList -> compareRefs (bRefList, bRefs) ([], aRefs)
          | (ref, aCount) :: aRefListRest, bRefList ->
            let bCount = ref |> Map.find bRefs |> Option.value ~default:0 in
            aCount = bCount && compareRefs (aRefListRest, aRefs) (bRefList, bRefs)
        in
        let aRefs = a.refs in
        let bRefs =
          mapKeys b.refs ~f:(fun ref -> ref |> Map.find bToA |> Option.value ~default:ref)
        in
        boolToOpt
          (compareRefs (Map.to_alist aRefs, aRefs) (Map.to_alist bRefs, bRefs)
          && a.const = b.const)
      | Shape a ->
        let%bind b =
          match b with
          | Shape b -> Some b
          | _ -> None
        in
        let compareShapeElement a b bToA : unit option =
          match a with
          | Add a ->
            let%bind b =
              match b with
              | Add b -> Some b
              | _ -> None
            in
            compareIndices (Dimension a) (Dimension b) bToA
          | ShapeRef a ->
            let%bind b =
              match b with
              | ShapeRef b -> Some b
              | _ -> None
            in
            let b = Map.find bToA b |> Option.value ~default:b in
            boolToOpt (Identifier.equal a b)
        in
        boolToOpt
          (List.equal (fun ae be -> optToBool (compareShapeElement ae be bToA)) a b)
    in
    (* Forall, Pi, and Sigma are all very similar, so compareTypeAbstractions
     pulls out this commonality*)
    let rec compareTypeAbstractions
              : 't.
                't Type.abstraction
                -> 't Type.abstraction
                -> Identifier.t Map.M(Identifier).t
                -> ('t -> 't -> bool)
                -> unit option
      =
     fun a b bToA boundEq ->
      let open Type in
      let%bind zippedParams =
        match List.zip a.parameters b.parameters with
        | Ok zp -> Some zp
        | Unequal_lengths -> None
      in
      let%bind newBToA =
        List.fold zippedParams ~init:(Some bToA) ~f:(fun bToA (aParam, bParam) ->
            let%map bToA = bToA
            and () = boolToOpt (boundEq aParam.bound bParam.bound) in
            Map.set bToA ~key:bParam.binding ~data:aParam.binding)
      in
      compareTypes (Type.Array a.body) (Type.Array b.body) newBToA
    and compareTypes a b bToA : unit option =
      let open Type in
      match a with
      | Array (ArrayRef a) ->
        let%bind b =
          match b with
          | Array (ArrayRef b) -> Some b
          | _ -> None
        in
        let b = Map.find bToA b |> Option.value ~default:b in
        boolToOpt (Identifier.equal a b)
      | Array (Arr a) ->
        let%bind b =
          match b with
          | Array (Arr b) -> Some b
          | _ -> None
        in
        let%bind () = compareTypes (Atom a.element) (Atom b.element) bToA in
        compareIndices (Index.Shape a.shape) (Index.Shape b.shape) bToA
      | Atom (AtomRef a) ->
        let%bind b =
          match b with
          | Atom (AtomRef b) -> Some b
          | _ -> None
        in
        let b = Map.find bToA b |> Option.value ~default:b in
        boolToOpt (Identifier.equal a b)
      | Atom (Func a) ->
        let%bind b =
          match b with
          | Atom (Func b) -> Some b
          | _ -> None
        in
        let%bind () =
          boolToOpt
            (List.equal
               (fun aParam bParam ->
                 optToBool (compareTypes (Array aParam) (Array bParam) bToA))
               a.parameters
               b.parameters)
        in
        compareTypes (Array a.return) (Array b.return) bToA
      | Atom (Forall a) ->
        let%bind b =
          match b with
          | Atom (Forall b) -> Some b
          | _ -> None
        in
        compareTypeAbstractions a b bToA Kind.equal
      | Atom (Pi a) ->
        let%bind b =
          match b with
          | Atom (Pi b) -> Some b
          | _ -> None
        in
        compareTypeAbstractions a b bToA Sort.equal
      | Atom (Sigma a) ->
        let%bind b =
          match b with
          | Atom (Sigma b) -> Some b
          | _ -> None
        in
        compareTypeAbstractions a b bToA Sort.equal
      | Atom (Tuple a) ->
        let%bind b =
          match b with
          | Atom (Tuple b) -> Some b
          | _ -> None
        in
        boolToOpt
          (List.equal
             (fun aParam bParam ->
               optToBool (compareTypes (Atom aParam) (Atom bParam) bToA))
             a
             b)
    in
    let result : unit option = compareTypes a b (Map.empty (module Identifier)) in
    (* Convert from option back to boolean *)
    optToBool result
  ;;

  let requireType ~expected ~actual ~makeError =
    if eqType expected actual
    then ok ()
    else CheckerStateT.err (makeError { expected; actual })
  ;;

  let checkForArrType source type' =
    let open Core.Type in
    match type' with
    | Arr arr -> ok arr
    | ArrayRef _ ->
      CheckerStateT.err { source; elem = ExpectedArrType { actual = Array type' } }
  ;;

  let checkForTupleType source type' =
    let open Core.Type in
    match type' with
    | Tuple tup -> ok tup
    | _ -> CheckerStateT.err { source; elem = ExpectedTupleType { actual = Atom type' } }
  ;;

  let checkForSigmaType source type' =
    let open Core.Type in
    match type' with
    | Sigma sigma -> ok sigma
    | _ -> CheckerStateT.err { source; elem = ExpectedSigmaType { actual = Atom type' } }
  ;;

  let checkForFuncType source type' =
    let open Core.Type in
    match type' with
    | Func func -> ok func
    | _ -> CheckerStateT.err { source; elem = ExpectedFuncType { actual = Atom type' } }
  ;;

  (* Check if an expr is a value. This corresponds to Val and Atval in the
   formal syntax definition *)
  let rec requireValue { elem = expr; source } =
    let open Ast.Expr in
    let err = CheckerStateT.err { elem = ExpectedValue; source } in
    match expr with
    | Ref _ -> ok ()
    | Arr arr ->
      arr.elements.elem
      |> NeList.map ~f:requireValue
      |> CheckerStateT.allNE
      |> CheckerStateT.ignore_m
    | EmptyArr _ -> err
    | Frame _ -> err
    | EmptyFrame _ -> err
    | TermApplication _ -> err
    | TypeApplication _ -> err
    | IndexApplication _ -> err
    | Unbox _ -> err
    | TermLambda _ -> ok ()
    | TypeLambda tl -> requireValue tl.body
    | IndexLambda il -> requireValue il.body
    | Boxes boxes ->
      boxes.elements.elem
      |> List.map ~f:(fun b -> requireValue b.elem.body)
      |> CheckerStateT.all_unit
    | Let _ -> err
    | TupleLet _ -> err
    | Tuple elements ->
      elements.elem
      |> List.map ~f:requireValue
      |> CheckerStateT.all
      |> CheckerStateT.ignore_m
    | IntLiteral _ -> ok ()
    | CharacterLiteral _ -> ok ()
  ;;

  let subIndicesIntoDimIndex indices ({ const; refs } : Core.Index.dimension) =
    let open Core.Index in
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

  let subIndicesIntoShapeIndex indices shape =
    let open Core.Index in
    List.bind shape ~f:(function
        | Add dim -> [ Add (subIndicesIntoDimIndex indices dim) ]
        | ShapeRef id as ref ->
          (match Map.find indices id with
          | Some (Shape shape) -> shape
          | Some (Dimension _) -> [ ref ]
          | None -> [ ref ]))
  ;;

  let rec subIndicesIntoArrayType indices =
    let open Core.Type in
    function
    | ArrayRef _ as ref -> ref
    | Arr { element; shape } ->
      Arr
        { element = subIndicesIntoAtomType indices element
        ; shape = subIndicesIntoShapeIndex indices shape
        }

  and subIndicesIntoAtomType indices =
    let open Core.Type in
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
  ;;

  let rec subTypesIntoArrayType types =
    let open Core.Type in
    function
    | ArrayRef id as ref ->
      (match Map.find types id with
      | Some (Array arrayType) -> arrayType
      | Some (Atom _) -> ref
      | None -> ref)
    | Arr { element; shape } ->
      Arr { element = subTypesIntoAtomType types element; shape }

  and subTypesIntoAtomType types =
    let open Core.Type in
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
    | Pi { parameters; body } ->
      Pi { parameters; body = subTypesIntoArrayType types body }
    | Sigma { parameters; body } ->
      Sigma { parameters; body = subTypesIntoArrayType types body }
    | Tuple elements -> Tuple (List.map elements ~f:(subTypesIntoAtomType types))
  ;;

  let findEscapingRefs (env : Environment.t) type' =
    let checkIfEscaping env (ref : Core.Identifier.t) =
      if Map.mem env ref.name then [] else [ ref ]
    in
    let rec findInIndex (env : Environment.t) =
      let open Core.Index in
      function
      | Shape shapeElements ->
        List.bind shapeElements ~f:(function
            | Add dim -> findInIndex env (Dimension dim)
            | ShapeRef ref -> checkIfEscaping env.sorts ref)
      | Dimension { const = _; refs } ->
        Map.keys refs |> List.bind ~f:(checkIfEscaping env.sorts)
    in
    let rec findInType (env : Environment.t) =
      let open Core.Type in
      function
      | Array (ArrayRef ref) -> checkIfEscaping env.kinds ref
      | Array (Arr { element; shape }) ->
        findInType env (Atom element) @ findInIndex env (Core.Index.Shape shape)
      | Atom (AtomRef ref) -> checkIfEscaping env.kinds ref
      | Atom (Func { parameters; return }) ->
        List.bind parameters ~f:(fun a -> findInType env (Array a))
        @ findInType env (Array return)
      | Atom (Forall { parameters; body }) ->
        let extendedEnv =
          { env with
            kinds =
              List.fold parameters ~init:env.kinds ~f:(fun env param ->
                  Map.set
                    env
                    ~key:param.binding.name
                    ~data:{ e = param.bound; id = param.binding })
          }
        in
        findInType extendedEnv (Array body)
      | Atom (Pi { parameters; body }) ->
        let extendedEnv =
          { env with
            sorts =
              List.fold parameters ~init:env.sorts ~f:(fun env param ->
                  Map.set
                    env
                    ~key:param.binding.name
                    ~data:{ e = param.bound; id = param.binding })
          }
        in
        findInType extendedEnv (Array body)
      | Atom (Sigma { parameters; body }) ->
        let extendedEnv =
          { env with
            sorts =
              List.fold parameters ~init:env.sorts ~f:(fun env param ->
                  Map.set
                    env
                    ~key:param.binding.name
                    ~data:{ e = param.bound; id = param.binding })
          }
        in
        findInType extendedEnv (Array body)
      | Atom (Tuple elements) -> List.bind elements ~f:(fun a -> findInType env (Atom a))
    in
    findInType env type'
  ;;

  let rec check (env : Environment.t) { elem = expr; source } =
    let module U = Ast.Expr in
    let module T = Core.Expr in
    match expr with
    | U.Ref name ->
      Map.find env.types name
      |> Option.map ~f:(fun entry -> T.Array (T.Ref { id = entry.id; type' = entry.e }))
      |> CheckerStateT.ofOption ~err:{ source; elem = UnboundVariable name }
    | U.Arr { dimensions; elements } ->
      let dimensions = dimensions.elem |> List.map ~f:(fun d -> d.elem) in
      let expectedElements = dimensions |> List.fold ~init:1 ~f:( * ) in
      let%bind () =
        CheckerStateT.require
          (expectedElements = NeList.length elements.elem)
          { source
          ; elem =
              WrongNumberOfElementsInArray
                { expected = expectedElements; actual = NeList.length elements.elem }
          }
      and elementsWithSource =
        elements.elem
        |> NeList.map ~f:(fun e ->
               checkAndExpectAtom env e >>| fun atom -> atom, e.source)
        |> CheckerStateT.allNE
      in
      let elements = elementsWithSource |> NeList.map ~f:(fun (e, _) -> e) in
      let ((firstElement, _) :: restElementsWithSource) = elementsWithSource in
      let%map () =
        restElementsWithSource
        |> List.map ~f:(fun (element, elementSource) ->
               requireType
                 ~expected:(T.type' (T.Atom firstElement))
                 ~actual:(T.type' (T.Atom element))
                 ~makeError:(fun { expected; actual } ->
                   { source = elementSource
                   ; elem =
                       ArrayTypeDisagreement
                         { firstElementType = expected; gotElementType = actual }
                   }))
        |> CheckerStateT.all_unit
      in
      let shape =
        List.map dimensions ~f:(fun n -> Core.Index.Add (Core.Index.dimensionConstant n))
      in
      T.Array
        (Arr
           { dimensions
           ; elements = NeList.to_list elements
           ; type' = { element = T.atomType firstElement; shape }
           })
    | U.EmptyArr { dimensions; elementType } ->
      let unwrappedDims = List.map dimensions.elem ~f:(fun n -> n.elem) in
      let hasZero = List.find unwrappedDims ~f:(( = ) 0) |> Option.is_some in
      let expectedElements = List.fold unwrappedDims ~init:1 ~f:( * ) in
      let%bind () =
        CheckerStateT.require
          hasZero
          { source = dimensions.source
          ; elem =
              WrongNumberOfElementsInArray { expected = expectedElements; actual = 0 }
          }
      in
      let%map elementType = KindChecker.checkAndExpectAtom env elementType in
      let shape =
        List.map unwrappedDims ~f:(fun n ->
            Core.Index.Add (Core.Index.dimensionConstant n))
      in
      T.Array
        (Arr
           { dimensions = unwrappedDims
           ; elements = []
           ; type' = { element = elementType; shape }
           })
    | U.Frame { dimensions; elements = arrays } ->
      let dimensions = dimensions.elem |> List.map ~f:(fun d -> d.elem) in
      let expectedArrays = dimensions |> List.fold ~init:1 ~f:( * ) in
      let%bind () =
        CheckerStateT.require
          (expectedArrays = NeList.length arrays.elem)
          { source
          ; elem =
              WrongNumberOfArraysInFrame
                { expected = expectedArrays; actual = NeList.length arrays.elem }
          }
      and arraysWithSource =
        arrays.elem
        |> NeList.map ~f:(fun e ->
               checkAndExpectArray env e >>| fun atom -> atom, e.source)
        |> CheckerStateT.allNE
      in
      let typedArrays = arraysWithSource |> NeList.map ~f:(fun (e, _) -> e) in
      let ((firstArray, _) :: restArraysWithSource) = arraysWithSource in
      let%bind firstArrayType =
        checkForArrType (NeList.hd arrays.elem).source (T.arrayType firstArray)
      in
      let%map () =
        restArraysWithSource
        |> List.map ~f:(fun (array, arraySource) ->
               requireType
                 ~expected:(T.type' (T.Array firstArray))
                 ~actual:(T.type' (T.Array array))
                 ~makeError:(fun { expected; actual } ->
                   { source = arraySource
                   ; elem =
                       FrameTypeDisagreement
                         { firstArrayType = expected; gotArrayType = actual }
                   }))
        |> CheckerStateT.all_unit
      in
      let shape =
        List.map dimensions ~f:(fun n -> Core.Index.Add (Core.Index.dimensionConstant n))
        @ firstArrayType.shape
      in
      (* if all arrays in the frame are array literals, gather the elements
         of the arrays *)
      let arrayLiterals =
        typedArrays
        |> NeList.map ~f:(function
               | T.Arr arr -> Some arr
               | _ -> None)
        |> NeList.all_options
      in
      T.Array
        (match arrayLiterals with
        | Some arrayElements ->
          (* all arrays are array literals, so we can flatten the frame into an array literal *)
          let elements =
            arrayElements |> NeList.to_list |> List.bind ~f:(fun arr -> arr.elements)
          in
          Arr
            { elements
            ; dimensions = dimensions @ (NeList.hd arrayElements).dimensions
            ; type' = { element = firstArrayType.element; shape }
            }
        | None ->
          Frame
            { dimensions
            ; arrays = NeList.to_list typedArrays
            ; type' = { element = firstArrayType.element; shape }
            })
    | U.EmptyFrame { dimensions; elementType = arrayType } ->
      let arrayTypeSource = arrayType.source in
      let unwrappedDims = List.map dimensions.elem ~f:(fun n -> n.elem) in
      let hasZero = List.find unwrappedDims ~f:(( = ) 0) |> Option.is_some in
      let expectedElements = List.fold unwrappedDims ~init:1 ~f:( * ) in
      let%map () =
        CheckerStateT.require
          hasZero
          { source = dimensions.source
          ; elem = WrongNumberOfArraysInFrame { expected = expectedElements; actual = 0 }
          }
      and arrayType =
        KindChecker.checkAndExpectArray env arrayType >>= checkForArrType arrayTypeSource
      in
      let shape =
        List.map unwrappedDims ~f:(fun n ->
            Core.Index.Add (Core.Index.dimensionConstant n))
        @ arrayType.shape
      in
      T.Array
        (T.Arr
           { dimensions = unwrappedDims
           ; elements = []
           ; type' = { element = arrayType.element; shape }
           })
    | U.TermApplication { func; args } ->
      let funcSource = func.source in
      let argsSource = args.source in
      let%bind func, paramTypes, returnType, funcShape =
        let%bind func = checkAndExpectArray env func in
        let%bind funcArrType = checkForArrType funcSource (T.arrayType func) in
        let%bind arrowType = checkForFuncType funcSource funcArrType.element in
        let%map paramTypes =
          arrowType.parameters
          |> List.map ~f:(checkForArrType funcSource)
          |> CheckerStateT.all
        and returnType = checkForArrType funcSource arrowType.return in
        func, paramTypes, returnType, funcArrType.shape
      and args =
        args.elem
        |> List.map ~f:(fun arg ->
               let argSource = arg.source in
               let%bind arg = checkAndExpectArray env arg in
               let%map argType = checkForArrType argSource (T.arrayType arg) in
               arg, argType, argSource)
        |> CheckerStateT.all
      in
      let%bind zippedArgs =
        zipLists ~expected:paramTypes ~actual:args ~makeError:(fun ea ->
            { source = argsSource; elem = WrongNumberOfArguments ea })
      in
      let eqShapeElement =
        Core.Index.(
          function
          | ShapeRef a, ShapeRef b -> Core.Identifier.equal a b
          | Add a, Add b -> a.const = b.const && Map.equal ( = ) a.refs b.refs
          | ShapeRef _, Add _ | Add _, ShapeRef _ -> false)
      in
      let%bind frames =
        zippedArgs
        |> List.map ~f:(fun (param, (_, argType, argSource)) ->
               (* Check the element type is correct *)
               let checkElementType =
                 requireType
                   ~expected:(Core.Type.Atom param.element)
                   ~actual:(Core.Type.Atom argType.element)
                   ~makeError:(fun ea ->
                     { source = argSource; elem = ArgumentTypeDisagreement ea })
               in
               (* Check that the cell shape is correct and get the frame *)
               let checkCellAndGetFrame =
                 let frame, argCell =
                   List.split_n
                     argType.shape
                     (List.length argType.shape - List.length param.shape)
                 in
                 let cellDisagreementErr =
                   { source = argSource
                   ; elem =
                       CellShapeDisagreement
                         { expected = param.shape; actual = argType.shape }
                   }
                 in
                 let%bind zippedCellElements =
                   zipLists ~expected:param.shape ~actual:argCell ~makeError:(fun _ ->
                       cellDisagreementErr)
                 in
                 let allShapeElementsAgree =
                   List.for_all zippedCellElements ~f:eqShapeElement
                 in
                 let%map () =
                   CheckerStateT.require allShapeElementsAgree cellDisagreementErr
                 in
                 frame
               in
               let%map () = checkElementType
               and frame = checkCellAndGetFrame in
               { elem = frame; source = argSource })
        |> CheckerStateT.all
      in
      let getPrincipalFrame
          (headFrame :: restFrames : ('s, Core.Index.shape) Source.annotate NeList.t)
        =
        (* Get the principal frame *)
        let principalFrame, _ =
          List.fold
            restFrames
            ~init:(headFrame.elem, List.length headFrame.elem)
            ~f:(fun (maxFrame, maxSize) { elem = curr; source = _ } ->
              let currSize = List.length curr in
              if currSize > maxSize then curr, currSize else maxFrame, maxSize)
        in
        (* Check that each frame conforms to the principal frame *)
        let%map () =
          headFrame :: restFrames
          |> List.map ~f:(fun { elem = frame; source = frameSource } ->
                 let rec zipAndDropRemainder a b =
                   match a, b with
                   | a :: aRest, b :: bRest -> (a, b) :: zipAndDropRemainder aRest bRest
                   | _, [] | [], _ -> []
                 in
                 let zippedShapeElements = zipAndDropRemainder principalFrame frame in
                 let allShapeElementsAgree =
                   List.for_all zippedShapeElements ~f:eqShapeElement
                 in
                 CheckerStateT.require
                   allShapeElementsAgree
                   { source = frameSource
                   ; elem =
                       PrincipalFrameDisagreement
                         { expected = principalFrame; actual = frame }
                   })
          |> CheckerStateT.all_unit
        in
        principalFrame
      in
      let%map principalFrame =
        getPrincipalFrame ({ elem = funcShape; source = funcSource } :: frames)
      in
      T.Array
        (T.TermApplication
           { func
           ; args = List.map args ~f:(fun (arg, _, _) -> arg)
           ; type' =
               { element = returnType.element; shape = principalFrame @ returnType.shape }
           })
    | U.TypeApplication { tFunc; args } ->
      let%bind tFuncTyped = checkAndExpectArray env tFunc in
      let%bind tFuncType = checkForArrType tFunc.source (T.arrayType tFuncTyped) in
      let%bind tFuncForall =
        match tFuncType.element with
        | Core.Type.Forall forall -> ok forall
        | _ as t ->
          CheckerStateT.err
            { source = tFunc.source; elem = ExpectedForall { actual = Core.Type.Atom t } }
      in
      let%bind bodyType = checkForArrType tFunc.source tFuncForall.body in
      let%bind zippedArgs =
        zipLists ~expected:args.elem ~actual:tFuncForall.parameters ~makeError:(fun ea ->
            { source = args.source; elem = WrongNumberOfArguments ea })
      in
      let%map substitutionsList =
        zippedArgs
        |> List.map ~f:(fun (arg, param) ->
               let%map typedArg = KindChecker.checkAndExpect param.bound env arg in
               param.binding, typedArg)
        |> CheckerStateT.all
      in
      let substitutions =
        Map.of_alist_reduce (module Core.Identifier) substitutionsList ~f:(fun a _ -> a)
      in
      let subbedElementType = subTypesIntoAtomType substitutions bodyType.element in
      let typedArgs = List.map substitutionsList ~f:(fun (_, arg) -> arg) in
      T.Array
        (T.TypeApplication
           { tFunc = tFuncTyped
           ; args = typedArgs
           ; type' =
               { element = subbedElementType; shape = tFuncType.shape @ bodyType.shape }
           })
    | U.IndexApplication { iFunc; args } ->
      let%bind iFuncTyped = checkAndExpectArray env iFunc in
      let%bind iFuncType = checkForArrType iFunc.source (T.arrayType iFuncTyped) in
      let%bind iFuncPi =
        match iFuncType.element with
        | Core.Type.Pi pi -> ok pi
        | _ as t ->
          CheckerStateT.err
            { source = iFunc.source; elem = ExpectedForall { actual = Core.Type.Atom t } }
      in
      let%bind bodyType = checkForArrType iFunc.source iFuncPi.body in
      let%bind zippedArgs =
        zipLists ~expected:iFuncPi.parameters ~actual:args.elem ~makeError:(fun ea ->
            { source = args.source; elem = WrongNumberOfArguments ea })
      in
      let%map substitutionsList =
        zippedArgs
        |> List.map ~f:(fun (param, arg) ->
               let%map typedArg = SortChecker.checkAndExpect param.bound env arg in
               param.binding, typedArg)
        |> CheckerStateT.all
      in
      let substitutions =
        Map.of_alist_reduce (module Core.Identifier) substitutionsList ~f:(fun a _ -> a)
      in
      let subbedElementType = subIndicesIntoAtomType substitutions bodyType.element in
      let subbedBodyShape = subIndicesIntoShapeIndex substitutions bodyType.shape in
      let typedArgs = List.map substitutionsList ~f:(fun (_, arg) -> arg) in
      T.Array
        (T.IndexApplication
           { iFunc = iFuncTyped
           ; args = typedArgs
           ; type' =
               { element = subbedElementType; shape = iFuncType.shape @ subbedBodyShape }
           })
    | U.Unbox { indexBindings; valueBinding; box; body } ->
      let boxSource = box.source in
      let%bind box = checkAndExpectArray env box in
      let%bind boxArrType = checkForArrType boxSource (T.arrayType box) in
      let%bind sigma = checkForSigmaType boxSource boxArrType.element in
      let%bind zippedIndexBindings =
        zipLists
          ~expected:sigma.parameters
          ~actual:indexBindings.elem
          ~makeError:(fun ea ->
            { source = indexBindings.source; elem = WrongNumberOfUnboxParameters ea })
      in
      let%bind indexBindingsRev, newSortEnvEntries, substitutions =
        List.fold
          zippedIndexBindings
          ~init:(ok ([], Map.empty (module String), Map.empty (module Core.Identifier)))
          ~f:(fun soFar (param, binding) ->
            let%bind bindingsSoFar, entriesSoFar, subsSoFar = soFar in
            let%bind () =
              match binding.elem.bound with
              | Some bound ->
                if Sort.equal bound.elem param.bound
                then ok ()
                else
                  CheckerStateT.err
                    { source = bound.source
                    ; elem =
                        UnexpectedSortBoundInUnbox
                          { expected = param.bound; actual = bound.elem }
                    }
              | None -> ok ()
            in
            let%bind id = CheckerStateT.createId binding.elem.binding.elem in
            let%map entries =
              match
                Map.add
                  entriesSoFar
                  ~key:binding.elem.binding.elem
                  ~data:Environment.{ e = param.bound; id }
              with
              | `Ok entries -> ok entries
              | `Duplicate ->
                CheckerStateT.err
                  { source = binding.elem.binding.source
                  ; elem = DuplicateParameterName binding.elem.binding.elem
                  }
            in
            let indexRef =
              match param.bound with
              | Sort.Dim -> Core.Index.Dimension (Core.Index.dimensionRef id)
              | Sort.Shape -> Core.Index.Shape [ Core.Index.ShapeRef id ]
            in
            let subs = Map.set subsSoFar ~key:param.binding ~data:indexRef in
            let bindings = id :: bindingsSoFar in
            bindings, entries, subs)
      in
      let%bind () =
        CheckerStateT.require
          (Option.is_none (Map.find newSortEnvEntries valueBinding.elem))
          { source = valueBinding.source
          ; elem = DuplicateParameterName valueBinding.elem
          }
      in
      let extendedSorts =
        Map.merge_skewed env.sorts newSortEnvEntries ~combine:(fun ~key:_ _ newEntry ->
            newEntry)
      in
      let%bind valueBindingTyped = CheckerStateT.createId valueBinding.elem in
      let extendedTypes =
        Map.set
          env.types
          ~key:valueBinding.elem
          ~data:
            { e = subIndicesIntoArrayType substitutions sigma.body
            ; id = valueBindingTyped
            }
      in
      let extendedEnv = { env with sorts = extendedSorts; types = extendedTypes } in
      let bodySource = body.source in
      let%bind body = checkAndExpectArray extendedEnv body in
      let%bind bodyType = checkForArrType bodySource (T.arrayType body) in
      let%map () =
        match findEscapingRefs env (Core.Type.Array (Core.Type.Arr bodyType)) with
        | [] -> ok ()
        | ref :: _ ->
          CheckerStateT.err { source = bodySource; elem = EscapingRef ref.name }
      in
      T.Array
        (T.Unbox
           { indexBindings = List.rev indexBindingsRev
           ; valueBinding = valueBindingTyped
           ; box
           ; body
           ; type' =
               { element = bodyType.element; shape = boxArrType.shape @ bodyType.shape }
           })
    | U.TermLambda { params; body } ->
      let%bind kindedParams =
        params.elem
        |> List.map ~f:(fun { elem = { binding; bound }; source } ->
               let%map bound = KindChecker.checkAndExpectArray env bound in
               let param : ('s, Core.Type.array) Ast.param = { binding; bound } in
               { elem = param; source })
        |> CheckerStateT.all
      in
      let%bind { typedParams; extendedEnv = extendedTypesEnv } =
        processParams
          env.types
          kindedParams
          ~makeError:(fun name -> DuplicateParameterName name)
          ~boundToEnvEntry:(fun b -> b)
      in
      let extendedEnv = { env with types = extendedTypesEnv } in
      let%map body = checkAndExpectArray extendedEnv body in
      let type' : Core.Type.func =
        { parameters = List.map typedParams ~f:(fun p -> p.bound)
        ; return = T.arrayType body
        }
      in
      T.Atom (T.TermLambda { params = typedParams; body = T.Array body; type' })
    | U.TypeLambda { params; body } ->
      let params =
        (* The parameter's have source-annotated bounds; remove the source annotations *)
        List.map params.elem ~f:(function
            | { elem = { binding; bound = { elem = bound; source = _ } }; source } ->
            { elem = ({ binding; bound } : ('s, Kind.t) Ast.param); source })
      in
      let%bind { typedParams; extendedEnv = extendedKindsEnv } =
        processParams
          env.kinds
          params
          ~makeError:(fun name -> DuplicateParameterName name)
          ~boundToEnvEntry:(fun b -> b)
      in
      let extendedEnv = { env with kinds = extendedKindsEnv } in
      let%bind () = requireValue body in
      let%map body = checkAndExpectArray extendedEnv body in
      let type' : Core.Type.forall =
        { parameters = typedParams; body = T.arrayType body }
      in
      T.Atom (T.TypeLambda { params = typedParams; body; type' })
    | U.IndexLambda { params; body } ->
      let params =
        (* The parameter's have source-annotated bounds; remove the source annotations *)
        List.map params.elem ~f:(function
            | { elem = { binding; bound = { elem = bound; source = _ } }; source } ->
            { elem = ({ binding; bound } : ('s, Sort.t) Ast.param); source })
      in
      let%bind { typedParams; extendedEnv = extendedSortsEnv } =
        processParams
          env.sorts
          params
          ~makeError:(fun name -> DuplicateParameterName name)
          ~boundToEnvEntry:(fun b -> b)
      in
      let extendedEnv = { env with sorts = extendedSortsEnv } in
      let%bind () = requireValue body in
      let%map body = checkAndExpectArray extendedEnv body in
      let type' : Core.Type.pi = { parameters = typedParams; body = T.arrayType body } in
      T.Atom (T.IndexLambda { params = typedParams; body; type' })
    | U.Boxes { params; elementType; dimensions; elements } ->
      let dimensions = List.map dimensions.elem ~f:(fun d -> d.elem) in
      let expectedElements = List.fold dimensions ~init:1 ~f:( * ) in
      let params =
        (* The parameter's have source-annotated bounds; remove the source annotations *)
        List.map params.elem ~f:(function
            | { elem = { binding; bound = { elem = bound; source = _ } }; source } ->
            { elem = Ast.{ binding; bound }; source })
      in
      let%bind { typedParams; extendedEnv = extendedSortsEnv } =
        processParams
          env.sorts
          params
          ~makeError:(fun name -> DuplicateIndexParameterName name)
          ~boundToEnvEntry:(fun b -> b)
      in
      let extendedEnv = { env with sorts = extendedSortsEnv } in
      let%bind elementType = KindChecker.checkAndExpectArray extendedEnv elementType in
      let sigmaType : Core.Type.sigma =
        { parameters = typedParams; body = elementType }
      in
      let%map checkedElements =
        elements.elem
        |> List.map ~f:(fun element ->
               let%bind body = checkAndExpectArray env element.elem.body in
               let%bind zippedIndices =
                 zipLists
                   ~expected:typedParams
                   ~actual:element.elem.indices.elem
                   ~makeError:(fun ea ->
                     { source = element.elem.indices.source
                     ; elem = WrongNumberOfArguments ea
                     })
               in
               let%bind zippedIndicesTyped =
                 zippedIndices
                 |> List.map ~f:(fun (param, index) ->
                        let%map checkedIndex =
                          SortChecker.checkAndExpect param.bound env index
                        in
                        param, checkedIndex)
                 |> CheckerStateT.all
               in
               let indices = List.map zippedIndicesTyped ~f:(fun (_, i) -> i) in
               let substitutions =
                 List.fold
                   zippedIndicesTyped
                   ~init:(Map.empty (module Core.Identifier))
                   ~f:(fun acc (param, index) ->
                     Map.set acc ~key:param.binding ~data:index)
               in
               let subbedType = subIndicesIntoArrayType substitutions elementType in
               let%map () =
                 requireType
                   ~expected:(Core.Type.Array subbedType)
                   ~actual:(T.type' (T.Array body))
                   ~makeError:(fun ea ->
                     { source = element.elem.body.source
                     ; elem = SigmaBodyTypeDisagreement ea
                     })
               in
               T.Box { indices; body; bodyType = elementType; type' = sigmaType })
        |> CheckerStateT.all
      and () =
        CheckerStateT.require
          (List.length elements.elem = expectedElements)
          { elem =
              WrongNumberOfElementsInBoxes
                { expected = expectedElements; actual = List.length elements.elem }
          ; source = elements.source
          }
      in
      let shape =
        List.map dimensions ~f:(fun d -> Core.Index.Add (Core.Index.dimensionConstant d))
      in
      T.Array
        (T.Arr
           { dimensions
           ; elements = checkedElements
           ; type' = { element = Core.Type.Sigma sigmaType; shape }
           })
    | U.Let { param; value; body } ->
      let binding = param.elem.binding in
      let bound = param.elem.bound in
      let%bind id = CheckerStateT.createId binding.elem in
      let%bind bound =
        bound
        |> Option.map ~f:(fun bound -> KindChecker.checkAndExpectArray env bound)
        |> CheckerStateT.traverseOpt
      in
      let checkBody valueType =
        let extendedTypesEnv =
          Map.set env.types ~key:binding.elem ~data:{ id; e = valueType }
        in
        let extendedEnv = { env with types = extendedTypesEnv } in
        checkAndExpectArray extendedEnv body
      in
      let checkValue =
        let%bind valueTyped = checkAndExpectArray env value in
        let%map () =
          match bound with
          | Some bound ->
            requireType
              ~expected:(Array bound)
              ~actual:(T.type' (Array valueTyped))
              ~makeError:(fun ea ->
                { source = value.source; elem = LetTypeDisagreement ea })
          | None -> ok ()
        in
        valueTyped
      in
      let%map bodyTyped, valueTyped =
        match bound with
        | Some bound -> CheckerStateT.both (checkBody bound) checkValue
        | None ->
          let%bind valueTyped = checkValue in
          let%map bodyTyped = checkBody (T.arrayType valueTyped) in
          bodyTyped, valueTyped
      in
      T.Array
        (T.Let
           { binding = id
           ; value = valueTyped
           ; body = bodyTyped
           ; type' = T.arrayType bodyTyped
           })
    | U.TupleLet { params; value; body } ->
      let%bind valueTyped = checkAndExpectArray env value in
      let%bind valueType = checkForArrType value.source (T.arrayType valueTyped) in
      let%bind valueElementType = checkForTupleType value.source valueType.element in
      let%bind zippedBindings =
        zipLists ~expected:valueElementType ~actual:params.elem ~makeError:(fun ea ->
            { source = params.source; elem = WrongNumberOfBindings ea })
      in
      let%bind newEnvEntries, revParamsTyped =
        List.fold
          zippedBindings
          ~init:(ok (Map.empty (module String), []))
          ~f:(fun acc (type', param) ->
            let%bind envEntriesSoFar, paramsSoFar = acc in
            let%bind id = CheckerStateT.createId param.elem.binding.elem in
            let%bind boundTyped =
              match param.elem.bound with
              | Some bound ->
                let%bind boundTyped = KindChecker.checkAndExpectAtom env bound in
                let%map () =
                  requireType
                    ~expected:(Atom type')
                    ~actual:(Atom boundTyped)
                    ~makeError:(fun ea ->
                      { source = bound.source; elem = TupleLetTypeDisagreement ea })
                in
                boundTyped
              | None -> ok type'
            in
            let bindingType =
              Core.Type.Arr { element = boundTyped; shape = valueType.shape }
            in
            let entry = Environment.{ id; e = bindingType } in
            let paramTyped = Core.{ binding = id; bound = boundTyped } in
            match Map.add envEntriesSoFar ~key:param.elem.binding.elem ~data:entry with
            | `Ok envEntries -> ok (envEntries, paramTyped :: paramsSoFar)
            | `Duplicate ->
              CheckerStateT.err
                { source = param.elem.binding.source
                ; elem = DuplicateTupleBindingName param.elem.binding.elem
                })
      in
      let extendedTypesEnv =
        Map.merge_skewed env.types newEnvEntries ~combine:(fun ~key:_ _ e -> e)
      in
      let extendedEnv = { env with types = extendedTypesEnv } in
      let%map bodyTyped = checkAndExpectArray extendedEnv body in
      T.Array
        (TupleLet
           { params = List.rev revParamsTyped
           ; value = valueTyped
           ; body = bodyTyped
           ; type' = T.arrayType bodyTyped
           })
    | U.Tuple elements ->
      let%map elements =
        elements.elem |> List.map ~f:(checkAndExpectAtom env) |> CheckerStateT.all
      in
      let elementTypes = List.map elements ~f:(fun e -> T.atomType e) in
      T.Atom (T.Tuple { elements; type' = elementTypes })
    | U.IntLiteral i ->
      let value = T.IntLiteral i in
      CheckerStateT.return (T.Atom (Literal { value; type' = env.literalType value }))
    | U.CharacterLiteral c ->
      let value = T.CharacterLiteral c in
      CheckerStateT.return (T.Atom (Literal { value; type' = env.literalType value }))

  and checkAndExpectArray env expr =
    let open Core.Expr in
    match%bind check env expr with
    | Array array -> ok array
    | Atom atom ->
      ok
        (Core.Expr.Arr
           { dimensions = []
           ; elements = [ atom ]
           ; type' = { element = Core.Expr.atomType atom; shape = [] }
           })

  and checkAndExpectAtom env expr =
    let open Core.Expr in
    match%bind check env expr with
    | Atom atom -> ok atom
    | Array _ as typed ->
      CheckerStateT.err
        { source = expr.source; elem = ExpectedAtomicExpr { actual = type' typed } }
  ;;
end

let baseEnv () =
  let module B = Environment.Base (CheckerStateT) in
  B.make ()
;;

let checkSort index =
  CheckerStateT.runA
    (let%bind env = baseEnv () in
     SortChecker.check env index)
    { idCounter = 0 }
;;

let checkKind type' =
  CheckerStateT.runA
    (let%bind env = baseEnv () in
     KindChecker.check env type')
    { idCounter = 0 }
;;

let checkType expr =
  CheckerStateT.runA
    (let%bind env = baseEnv () in
     TypeChecker.check env expr)
    { idCounter = 0 }
;;
