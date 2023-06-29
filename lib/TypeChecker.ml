open! Base
open Ast
open MResult
open MResult.Let_syntax

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
      { firstElementType : Typed.Type.t
      ; gotElementType : Typed.Type.t
      }
  | FrameTypeDisagreement of
      { firstArrayType : Typed.Type.t
      ; gotArrayType : Typed.Type.t
      }
  | SigmaBodyTypeDisagreement of Typed.Type.t expectedActual
  | ExpectedAtomicExpr of { actual : Typed.Type.t }
  | ExpectedArrType of { actual : Typed.Type.t }
  | ExpectedTupleType of { actual : Typed.Type.t }
  | ExpectedSigmaType of { actual : Typed.Type.t }
  | ExpectedFuncType of { actual : Typed.Type.t }
  | ExpectedValue
  | ExpectedForall of { actual : Typed.Type.t }
  | WrongNumberOfArguments of int expectedActual
  | WrongNumberOfBindings of int expectedActual
  | WrongNumberOfUnboxParameters of int expectedActual
  | DuplicateTupleBindingName of string
  | LetTypeDisagreement of Typed.Type.t expectedActual
  | UnexpectedSortBoundInUnbox of Sort.t expectedActual
  | EscapingRef of string
  | ArgumentTypeDisagreement of Typed.Type.t expectedActual
  | CellShapeDisagreement of Typed.Index.shape expectedActual
  | PrincipalFrameDisagreement of Typed.Index.shape expectedActual

module Show : sig
  val sort : Sort.t -> string
  val kind : Kind.t -> string
  val shape : Typed.Index.shape -> string
  val type' : Typed.Type.t -> string
end = struct
  let sort = function
    | Sort.Dim -> "Dim"
    | Sort.Shape -> "Shape"
  ;;

  let kind = function
    | Kind.Atom -> "Atom"
    | Kind.Array -> "Array"
  ;;

  let showDimension ({ const; refs } : Typed.Index.dimension) =
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
    | Typed.Index.Add dim -> showDimension dim
    | Typed.Index.ShapeRef ref -> ref.name
  ;;

  let showList ?(prependSpace = false) shower list =
    if prependSpace
    then list |> List.bind ~f:(fun e -> [ " "; shower e ]) |> String.concat
    else list |> List.map ~f:shower |> String.concat ~sep:" "
  ;;

  let shape elements = [%string "[%{showList showShapeElement elements}]"]
  let showParam (p : 't Typed.param) = p.binding.name

  let rec showArray =
    let open Typed.Type in
    function
    | ArrayRef ref -> ref.name
    | Arr { element; shape } ->
      if List.is_empty shape
      then showAtom element
      else (
        let shapeString = showList ~prependSpace:true showShapeElement shape in
        [%string "[%{showAtom element}%{shapeString}]"])

  and showAtom =
    let open Typed.Type in
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

  and type' : Typed.Type.t -> string = function
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
  | UnexpectedSortBoundInUnbox _
  | EscapingRef _
  | ArgumentTypeDisagreement _
  | CellShapeDisagreement _
  | PrincipalFrameDisagreement _ -> `Type
;;

type ('s, 't) checkResult = ('t, ('s, error) withSource) MResult.t

type 'v envEntry =
  { e : 'v
  ; id : Typed.Identifier.t
  }

type env =
  { sorts : Sort.t envEntry Map.M(String).t
  ; kinds : Kind.t envEntry Map.M(String).t
  ; types : Typed.Type.t envEntry Map.M(String).t
  }

let baseEnv =
  { sorts = Map.empty (module String)
  ; kinds = Map.empty (module String)
  ; types = Map.empty (module String)
  }
;;

let idCounter = ref 0

let createId () =
  let id = !idCounter in
  idCounter := !idCounter + 1;
  id
;;

type ('t, 'u) processedParams =
  { typedParams : 't Typed.param list
  ; extendedEnv : 'u envEntry Map.M(String).t
  }

type ('s, 't, 'u) paramsToMapResult =
  { typedParamsReversed : 't Typed.param list
  ; entries : 'u envEntry Map.M(String).t
  ; dups : 's Map.M(String).t
  }

(** Extend the given environment with the bindings specified by the parameters,
    and convert the untyped parameters into typed parameters. Any duplicate
    parameter names are detected and converted into erros via makeError *)
let processParams env params ~makeError ~boundToEnvEntry =
  (* Loops over the params, creating environment entries, converting into
      typed params, and detecting duplicates *)
  let rec collapseParams
      (params : ('s, 't) Untyped.paramList)
      (acc : ('s, 't, 'u) paramsToMapResult)
      : ('s, 't, 'u) paramsToMapResult
    =
    match params with
    | [] -> acc
    | { elem = { binding; bound }; source = _ } :: rest ->
      let name = binding.elem in
      let { typedParamsReversed = oldParams; entries = oldEntries; dups = oldDups } =
        acc
      in
      let typedParam : 't Typed.param = { binding = { name; id = createId () }; bound } in
      let newParams = typedParam :: oldParams in
      let entry = { e = boundToEnvEntry bound; id = typedParam.binding } in
      (match Map.add oldEntries ~key:name ~data:entry with
      | `Ok newEntries ->
        collapseParams
          rest
          { typedParamsReversed = newParams; entries = newEntries; dups = oldDups }
      | `Duplicate ->
        let newDups = Map.set oldDups ~key:name ~data:binding.source in
        collapseParams
          rest
          { typedParamsReversed = newParams; entries = oldEntries; dups = newDups })
  in
  let { typedParamsReversed; entries; dups } =
    collapseParams
      params
      { typedParamsReversed = []
      ; entries = Map.empty (module String)
      ; dups = Map.empty (module String)
      }
  in
  let typedParams = List.rev typedParamsReversed in
  match Non_empty_list.of_list (Map.to_alist dups) with
  | None ->
    let extendedEnv =
      Map.merge_skewed env entries ~combine:(fun ~key:_ _ newEntry -> newEntry)
    in
    MOk { typedParams; extendedEnv }
  | Some dups ->
    Errors
      (Non_empty_list.map dups ~f:(fun (name, source) ->
           { elem = makeError name; source }))
;;

let rec checkSort env { elem = index; source } =
  let module U = Untyped.Index in
  let module T = Typed.Index in
  match index with
  | U.Ref name ->
    Map.find env.sorts name
    |> Option.map ~f:(fun entry ->
           match entry.e with
           | Sort.Dim -> T.Dimension (T.dimensionRef entry.id)
           | Sort.Shape -> T.Shape [ T.ShapeRef entry.id ])
    |> MResult.ofOption ~err:{ source; elem = UnboundIndexVariable name }
  | U.Dimension dim -> MOk (T.Dimension (T.dimensionConstant dim))
  | U.Shape indices ->
    let%map indices = MResult.all (List.map indices ~f:(checkSortAndExpectDim env)) in
    T.Shape (List.map ~f:(fun d -> T.Add d) indices)
  | U.Add indices ->
    let%map indices =
      MResult.allNE (Non_empty_list.map indices ~f:(checkSortAndExpectDim env))
    in
    let flattenedDimension =
      indices
      |> Non_empty_list.fold_left
           ~init:({ const = 0; refs = Map.empty (module Typed.Identifier) } : T.dimension)
           ~f:(fun m1 m2 ->
             { const = m1.const + m2.const
             ; refs = Map.merge_skewed m1.refs m2.refs ~combine:(fun ~key:_ a b -> a + b)
             })
    in
    T.Dimension flattenedDimension
  | U.Append indices ->
    let%map nestedIndices =
      indices
      |> Non_empty_list.to_list
      |> List.map ~f:(checkSortAndExpectShape env)
      |> MResult.all
    in
    let indices = List.join nestedIndices in
    T.Shape indices

and checkSortAndExpectDim env index =
  match%bind checkSort env index with
  | Typed.Index.Dimension d -> MOk d
  | Typed.Index.Shape _ ->
    MResult.err
      { source = index.source
      ; elem = UnexpectedSort { expected = Sort.Dim; actual = Sort.Shape }
      }

and checkSortAndExpectShape env index =
  match%bind checkSort env index with
  | Typed.Index.Dimension _ ->
    MResult.err
      { source = index.source
      ; elem = UnexpectedSort { expected = Sort.Dim; actual = Sort.Shape }
      }
  | Typed.Index.Shape s -> MOk s
;;

let checkSortAndExpect sort env index =
  match sort with
  | Sort.Dim -> checkSortAndExpectDim env index >>| fun r -> Typed.Index.Dimension r
  | Sort.Shape -> checkSortAndExpectShape env index >>| fun r -> Typed.Index.Shape r
;;

let rec checkKind env { elem = type'; source } =
  let module U = Untyped.Type in
  let module T = Typed.Type in
  match type' with
  | U.Ref name ->
    Map.find env.kinds name
    |> Option.map ~f:(fun entry ->
           match entry.e with
           | Kind.Array -> T.Array (T.ArrayRef entry.id)
           | Kind.Atom -> T.Atom (T.AtomRef entry.id))
    |> MResult.ofOption ~err:{ source; elem = UnboundTypeVariable name }
  | U.Arr { element; shape } ->
    let%map element = checkKindAndExpectAtom env element
    and shape = checkSortAndExpectShape env shape in
    T.Array (T.Arr { element; shape })
  | U.Func { parameters; return } ->
    let%map parameters =
      parameters |> List.map ~f:(checkKindAndExpectArray env) |> MResult.all
    and return = checkKindAndExpectArray env return in
    T.Atom (T.Func { parameters; return })
  | U.Forall { parameters; body } ->
    let%bind { typedParams = parameters; extendedEnv = extendedKinds } =
      processParams
        env.kinds
        parameters
        ~makeError:(fun name -> DuplicateTypeParameterName name)
        ~boundToEnvEntry:(fun b -> b)
    in
    let extendedEnv = { env with kinds = extendedKinds } in
    let%map body = checkKindAndExpectArray extendedEnv body in
    T.Atom (T.Forall { parameters; body })
  | U.Pi { parameters; body } ->
    let%bind { typedParams = parameters; extendedEnv = extendeSorts } =
      processParams
        env.sorts
        parameters
        ~makeError:(fun name -> DuplicateIndexParameterName name)
        ~boundToEnvEntry:(fun b -> b)
    in
    let extendedEnv = { env with sorts = extendeSorts } in
    let%map body = checkKindAndExpectArray extendedEnv body in
    T.Atom (T.Pi { parameters; body })
  | U.Sigma { parameters; body } ->
    let%bind { typedParams = parameters; extendedEnv = extendeSorts } =
      processParams
        env.sorts
        parameters
        ~makeError:(fun name -> DuplicateIndexParameterName name)
        ~boundToEnvEntry:(fun b -> b)
    in
    let extendedEnv = { env with sorts = extendeSorts } in
    let%map body = checkKindAndExpectArray extendedEnv body in
    T.Atom (T.Sigma { parameters; body })
  | U.Tuple elements ->
    let%map kindedElements =
      elements |> List.map ~f:(checkKindAndExpectAtom env) |> MResult.all
    in
    T.Atom (T.Tuple kindedElements)

and checkKindAndExpectArray env type' =
  let open Typed.Type in
  match%bind checkKind env type' with
  | Array array -> MOk array
  | Atom atom -> MOk (Arr { element = atom; shape = [] })

and checkKindAndExpectAtom env type' =
  let open Typed.Type in
  match%bind checkKind env type' with
  | Atom atom -> MOk atom
  | Array _ ->
    MResult.err
      { source = type'.source
      ; elem = UnexpectedKind { expected = Kind.Atom; actual = Kind.Array }
      }
;;

let checkKindAndExpect kind env type' =
  match kind with
  | Kind.Array -> checkKindAndExpectArray env type' >>| fun r -> Typed.Type.Array r
  | Kind.Atom -> checkKindAndExpectAtom env type' >>| fun r -> Typed.Type.Atom r
;;

let zipLists ~expected ~actual ~makeError =
  match List.zip expected actual with
  | Ok zipped -> MOk zipped
  | Unequal_lengths ->
    MResult.err
      (makeError { expected = List.length expected; actual = List.length actual })
;;

(** Compare two types to check that they are equal*)
let eqType (a : Typed.Type.t) (b : Typed.Type.t) : bool =
  let open Typed in
  let universalIdCounter = ref 0 in
  let createUniversal () =
    let id = !universalIdCounter in
    universalIdCounter := !universalIdCounter + 1;
    id
  in
  let mapKeys cmp keyMap map =
    List.fold
      (Map.to_alist map)
      ~init:(Some (Map.empty cmp))
      ~f:(fun soFar (key, data) ->
        match soFar, Map.find keyMap key with
        | Some soFar, Some mappedKey -> Some (Map.set soFar ~key:mappedKey ~data)
        | _ -> None)
  in
  (* Booleans are represented as options in order to be able to use let syntax *)
  let boolToOpt = function
    | true -> Some ()
    | false -> None
  in
  let optToBool : unit option -> bool = Option.is_some in
  let open Option.Let_syntax in
  let rec compareIndices (a, aEnv) (b, bEnv) : unit option =
    let open Index in
    match a with
    | Dimension a ->
      let%bind b =
        match b with
        | Dimension b -> Some b
        | _ -> None
      in
      let%bind aRefs = mapKeys (module Int) aEnv a.refs in
      let%bind bRefs = mapKeys (module Int) bEnv b.refs in
      boolToOpt (Map.equal ( = ) aRefs bRefs)
    | Shape a ->
      let%bind b =
        match b with
        | Shape b -> Some b
        | _ -> None
      in
      let compareShapeElement (a, aEnv) (b, bEnv) : unit option =
        match a with
        | Add a ->
          let%bind b =
            match b with
            | Add b -> Some b
            | _ -> None
          in
          compareIndices (Dimension a, aEnv) (Dimension b, bEnv)
        | ShapeRef a ->
          let%bind b =
            match b with
            | ShapeRef b -> Some b
            | _ -> None
          in
          let%bind aRef = Map.find aEnv a in
          let%bind bRef = Map.find bEnv b in
          boolToOpt (aRef = bRef)
      in
      boolToOpt
        (List.equal
           (fun ae be -> optToBool (compareShapeElement (ae, aEnv) (be, bEnv)))
           a
           b)
  in
  (* Forall, Pi, and Sigma are all very similar, so compareTypeAbstractions
     pulls out this commonality*)
  let rec compareTypeAbstractions
            : 't.
              't Type.abstraction * int Map.M(Identifier).t
              -> 't Type.abstraction * int Map.M(Identifier).t
              -> ('t -> 't -> bool)
              -> unit option
    =
   fun (a, aEnv) (b, bEnv) boundEq ->
    let open Type in
    let%bind zippedParams =
      match List.zip a.parameters b.parameters with
      | Ok zp -> Some zp
      | Unequal_lengths -> None
    in
    let%bind aEnv, bEnv =
      List.fold
        zippedParams
        ~init:(Some (aEnv, bEnv))
        ~f:(fun envs (aParam, bParam) ->
          let%bind aEnv, bEnv = envs in
          let%bind () = boolToOpt (boundEq aParam.bound bParam.bound) in
          let universal = createUniversal () in
          Some
            ( Map.set aEnv ~key:aParam.binding ~data:universal
            , Map.set bEnv ~key:bParam.binding ~data:universal ))
    in
    compareTypes (Type.Array a.body, aEnv) (Type.Array b.body, bEnv)
  and compareTypes (a, aEnv) (b, bEnv) : unit option =
    let open Type in
    match a with
    | Array (ArrayRef a) ->
      let%bind b =
        match b with
        | Array (ArrayRef b) -> Some b
        | _ -> None
      in
      let%bind aRef = Map.find aEnv a in
      let%bind bRef = Map.find bEnv b in
      boolToOpt (aRef = bRef)
    | Array (Arr a) ->
      let%bind b =
        match b with
        | Array (Arr b) -> Some b
        | _ -> None
      in
      let%bind () = compareTypes (Atom a.element, aEnv) (Atom b.element, bEnv) in
      compareIndices (Index.Shape a.shape, aEnv) (Index.Shape b.shape, bEnv)
    | Atom (AtomRef a) ->
      let%bind b =
        match b with
        | Atom (AtomRef b) -> Some b
        | _ -> None
      in
      let%bind aRef = Map.find aEnv a in
      let%bind bRef = Map.find bEnv b in
      boolToOpt (aRef = bRef)
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
               optToBool (compareTypes (Array aParam, aEnv) (Array bParam, bEnv)))
             a.parameters
             b.parameters)
      in
      compareTypes (Array a.return, aEnv) (Array b.return, bEnv)
    | Atom (Forall a) ->
      let%bind b =
        match b with
        | Atom (Forall b) -> Some b
        | _ -> None
      in
      compareTypeAbstractions (a, aEnv) (b, bEnv) Kind.equal
    | Atom (Pi a) ->
      let%bind b =
        match b with
        | Atom (Pi b) -> Some b
        | _ -> None
      in
      compareTypeAbstractions (a, aEnv) (b, bEnv) Sort.equal
    | Atom (Sigma a) ->
      let%bind b =
        match b with
        | Atom (Sigma b) -> Some b
        | _ -> None
      in
      compareTypeAbstractions (a, aEnv) (b, bEnv) Sort.equal
    | Atom (Tuple a) ->
      let%bind b =
        match b with
        | Atom (Tuple b) -> Some b
        | _ -> None
      in
      boolToOpt
        (List.equal
           (fun aParam bParam ->
             optToBool (compareTypes (Atom aParam, aEnv) (Atom bParam, bEnv)))
           a
           b)
  in
  let result : unit option =
    compareTypes (a, Map.empty (module Identifier)) (b, Map.empty (module Identifier))
  in
  (* Convert from option back to boolean *)
  optToBool result
;;

let requireType ~expected ~actual ~makeError =
  if eqType expected actual then MOk () else MResult.err (makeError { expected; actual })
;;

let checkForArrType source type' =
  let open Typed.Type in
  match type' with
  | Arr arr -> MOk arr
  | ArrayRef _ -> MResult.err { source; elem = ExpectedArrType { actual = Array type' } }
;;

let checkForTupleType source type' =
  let open Typed.Type in
  match type' with
  | Tuple tup -> MOk tup
  | _ -> MResult.err { source; elem = ExpectedTupleType { actual = Atom type' } }
;;

let checkForSigmaType source type' =
  let open Typed.Type in
  match type' with
  | Sigma sigma -> MOk sigma
  | _ -> MResult.err { source; elem = ExpectedSigmaType { actual = Atom type' } }
;;

let checkForFuncType source type' =
  let open Typed.Type in
  match type' with
  | Func func -> MOk func
  | _ -> MResult.err { source; elem = ExpectedFuncType { actual = Atom type' } }
;;

(* Check if an expr is a value. This corresponds to Val and Atval in the
   formal syntax definition *)
let rec requireValue { elem = expr; source } =
  let open Untyped.Expr in
  let err = MResult.err { elem = ExpectedValue; source } in
  match expr with
  | Ref _ -> MOk ()
  | Arr arr ->
    arr.elements
    |> Non_empty_list.map ~f:requireValue
    |> MResult.allNE
    |> MResult.ignore_m
  | EmptyArr _ -> err
  | Frame _ -> err
  | EmptyFrame _ -> err
  | TermApplication _ -> err
  | TypeApplication _ -> err
  | IndexApplication _ -> err
  | Unbox _ -> err
  | TermLambda _ -> MOk ()
  | TypeLambda tl -> requireValue tl.body
  | IndexLambda il -> requireValue il.body
  | Boxes boxes ->
    boxes.elements.elem
    |> List.map ~f:(fun b -> requireValue b.elem.body)
    |> MResult.all_unit
  | Let _ -> err
  | TupleLet _ -> err
  | Tuple elements ->
    elements |> List.map ~f:requireValue |> MResult.all |> MResult.ignore_m
;;

let subIndicesIntoDimIndex indices ({ const; refs } : Typed.Index.dimension) =
  let open Typed.Index in
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
  let open Typed.Index in
  List.bind shape ~f:(function
      | Add dim -> [ Add (subIndicesIntoDimIndex indices dim) ]
      | ShapeRef id as ref ->
        (match Map.find indices id with
        | Some (Shape shape) -> shape
        | Some (Dimension _) -> [ ref ]
        | None -> [ ref ]))
;;

let rec subIndicesIntoArrayType indices =
  let open Typed.Type in
  function
  | ArrayRef _ as ref -> ref
  | Arr { element; shape } ->
    Arr
      { element = subIndicesIntoAtomType indices element
      ; shape = subIndicesIntoShapeIndex indices shape
      }

and subIndicesIntoAtomType indices =
  let open Typed.Type in
  function
  | AtomRef _ as ref -> ref
  | Func { parameters; return } ->
    Func { parameters; return = subIndicesIntoArrayType indices return }
  | Forall { parameters; body } ->
    Forall { parameters; body = subIndicesIntoArrayType indices body }
  | Pi { parameters; body } ->
    Pi { parameters; body = subIndicesIntoArrayType indices body }
  | Sigma { parameters; body } ->
    Sigma { parameters; body = subIndicesIntoArrayType indices body }
  | Tuple elements -> Tuple (List.map elements ~f:(subIndicesIntoAtomType indices))
;;

let rec subTypesIntoArrayType types =
  let open Typed.Type in
  function
  | ArrayRef id as ref ->
    (match Map.find types id with
    | Some (Array arrayType) -> arrayType
    | Some (Atom _) -> ref
    | None -> ref)
  | Arr { element; shape } -> Arr { element = subTypesIntoAtomType types element; shape }

and subTypesIntoAtomType types =
  let open Typed.Type in
  function
  | AtomRef id as ref ->
    (match Map.find types id with
    | Some (Atom atomType) -> atomType
    | Some (Array _) -> ref
    | None -> ref)
  | Func { parameters; return } ->
    Func { parameters; return = subTypesIntoArrayType types return }
  | Forall { parameters; body } ->
    Forall { parameters; body = subTypesIntoArrayType types body }
  | Pi { parameters; body } -> Pi { parameters; body = subTypesIntoArrayType types body }
  | Sigma { parameters; body } ->
    Sigma { parameters; body = subTypesIntoArrayType types body }
  | Tuple elements -> Tuple (List.map elements ~f:(subTypesIntoAtomType types))
;;

let findEscapingRefs env type' =
  let checkIfEscaping env (ref : Typed.Identifier.t) =
    if Map.mem env ref.name then [] else [ ref ]
  in
  let rec findInIndex env =
    let open Typed.Index in
    function
    | Shape shapeElements ->
      List.bind shapeElements ~f:(function
          | Add dim -> findInIndex env (Dimension dim)
          | ShapeRef ref -> checkIfEscaping env.sorts ref)
    | Dimension { const = _; refs } ->
      Map.keys refs |> List.bind ~f:(checkIfEscaping env.sorts)
  in
  let rec findInType env =
    let open Typed.Type in
    function
    | Array (ArrayRef ref) -> checkIfEscaping env.kinds ref
    | Array (Arr { element; shape }) ->
      findInType env (Atom element) @ findInIndex env (Typed.Index.Shape shape)
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

let rec checkType env { elem = expr; source } =
  let module U = Untyped.Expr in
  let module T = Typed.Expr in
  match expr with
  | U.Ref name ->
    Map.find env.types name
    |> Option.map ~f:(fun entry ->
           match entry.e with
           | Typed.Type.Array type' -> T.Array (T.ArrayRef { id = entry.id; type' })
           | Typed.Type.Atom type' -> T.Atom (T.AtomRef { id = entry.id; type' }))
    |> MResult.ofOption ~err:{ source; elem = UnboundVariable name }
  | U.Arr { dimensions; elements } ->
    let dimensions = dimensions.elem |> List.map ~f:(fun d -> d.elem) in
    let expectedElements = dimensions |> List.fold ~init:1 ~f:( * ) in
    let%bind () =
      MResult.require
        (expectedElements = Non_empty_list.length elements)
        { source
        ; elem =
            WrongNumberOfElementsInArray
              { expected = expectedElements; actual = Non_empty_list.length elements }
        }
    and elementsWithSource =
      elements
      |> Non_empty_list.map ~f:(fun e ->
             checkTypeAndExpectAtom env e >>| fun atom -> atom, e.source)
      |> MResult.allNE
    in
    let elements = elementsWithSource |> Non_empty_list.map ~f:(fun (e, _) -> e) in
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
      |> MResult.all_unit
    in
    let arrays =
      elements |> Non_empty_list.to_list |> List.map ~f:(fun element -> T.Scalar element)
    in
    let shape =
      List.map dimensions ~f:(fun n -> Typed.Index.Add (Typed.Index.dimensionConstant n))
    in
    T.Array
      (T.Frame
         { dimensions; arrays; type' = { element = T.atomType firstElement; shape } })
  | U.EmptyArr { dimensions; elementType } ->
    let unwrappedDims = List.map dimensions.elem ~f:(fun n -> n.elem) in
    let hasZero = List.find unwrappedDims ~f:(( = ) 0) |> Option.is_some in
    let expectedElements = List.fold unwrappedDims ~init:1 ~f:( * ) in
    let%bind () =
      MResult.require
        hasZero
        { source = dimensions.source
        ; elem = WrongNumberOfElementsInArray { expected = expectedElements; actual = 0 }
        }
    in
    let%map elementType = checkKindAndExpectAtom env elementType in
    let shape =
      List.map unwrappedDims ~f:(fun n ->
          Typed.Index.Add (Typed.Index.dimensionConstant n))
    in
    T.Array
      (T.Frame
         { dimensions = unwrappedDims
         ; arrays = []
         ; type' = { element = elementType; shape }
         })
  | U.Frame { dimensions; arrays } ->
    let dimensions = dimensions.elem |> List.map ~f:(fun d -> d.elem) in
    let expectedArrays = dimensions |> List.fold ~init:1 ~f:( * ) in
    let%bind () =
      MResult.require
        (expectedArrays = Non_empty_list.length arrays)
        { source
        ; elem =
            WrongNumberOfArraysInFrame
              { expected = expectedArrays; actual = Non_empty_list.length arrays }
        }
    and arraysWithSource =
      arrays
      |> Non_empty_list.map ~f:(fun e ->
             checkTypeAndExpectArray env e >>| fun atom -> atom, e.source)
      |> MResult.allNE
    in
    let typedArrays = arraysWithSource |> Non_empty_list.map ~f:(fun (e, _) -> e) in
    let ((firstArray, _) :: restArraysWithSource) = arraysWithSource in
    let%bind firstArrayType =
      checkForArrType (Non_empty_list.hd arrays).source (T.arrayType firstArray)
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
      |> MResult.all_unit
    in
    let shape =
      List.map dimensions ~f:(fun n -> Typed.Index.Add (Typed.Index.dimensionConstant n))
      @ firstArrayType.shape
    in
    T.Array
      (T.Frame
         { dimensions
         ; arrays = Non_empty_list.to_list typedArrays
         ; type' = { element = firstArrayType.element; shape }
         })
  | U.EmptyFrame { dimensions; arrayType } ->
    let arrayTypeSource = arrayType.source in
    let unwrappedDims = List.map dimensions.elem ~f:(fun n -> n.elem) in
    let hasZero = List.find unwrappedDims ~f:(( = ) 0) |> Option.is_some in
    let expectedElements = List.fold unwrappedDims ~init:1 ~f:( * ) in
    let%map () =
      MResult.require
        hasZero
        { source = dimensions.source
        ; elem = WrongNumberOfArraysInFrame { expected = expectedElements; actual = 0 }
        }
    and arrayType =
      checkKindAndExpectArray env arrayType >>= checkForArrType arrayTypeSource
    in
    let shape =
      List.map unwrappedDims ~f:(fun n ->
          Typed.Index.Add (Typed.Index.dimensionConstant n))
      @ arrayType.shape
    in
    T.Array
      (T.Frame
         { dimensions = unwrappedDims
         ; arrays = []
         ; type' = { element = arrayType.element; shape }
         })
  | U.TermApplication { func; args } ->
    let funcSource = func.source in
    let argsSource = args.source in
    let%bind func, paramTypes, returnType, funcShape =
      let%bind func = checkTypeAndExpectArray env func in
      let%bind funcArrType = checkForArrType funcSource (T.arrayType func) in
      let%bind arrowType = checkForFuncType funcSource funcArrType.element in
      let%map paramTypes =
        arrowType.parameters |> List.map ~f:(checkForArrType funcSource) |> MResult.all
      and returnType = checkForArrType funcSource arrowType.return in
      func, paramTypes, returnType, funcArrType.shape
    and args =
      args.elem
      |> List.map ~f:(fun arg ->
             let argSource = arg.source in
             let%bind arg = checkTypeAndExpectArray env arg in
             let%map argType = checkForArrType argSource (T.arrayType arg) in
             arg, argType, argSource)
      |> MResult.all
    in
    let%bind zippedArgs =
      zipLists ~expected:paramTypes ~actual:args ~makeError:(fun ea ->
          { source = argsSource; elem = WrongNumberOfArguments ea })
    in
    let eqShapeElement =
      Typed.Index.(
        function
        | ShapeRef a, ShapeRef b -> Typed.Identifier.equal a b
        | Add a, Add b -> a.const = b.const && Map.equal ( = ) a.refs b.refs
        | ShapeRef _, Add _ | Add _, ShapeRef _ -> false)
    in
    let%bind frames =
      zippedArgs
      |> List.map ~f:(fun (param, (_, argType, argSource)) ->
             (* Check the element type is correct *)
             let checkElementType =
               requireType
                 ~expected:(Typed.Type.Atom param.element)
                 ~actual:(Typed.Type.Atom argType.element)
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
               let%map () = MResult.require allShapeElementsAgree cellDisagreementErr in
               frame
             in
             let%map () = checkElementType
             and frame = checkCellAndGetFrame in
             { elem = frame; source = argSource })
      |> MResult.all
    in
    let getPrincipalFrame
        (headFrame :: restFrames : ('s, Typed.Index.shape) withSource Non_empty_list.t)
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
               MResult.require
                 allShapeElementsAgree
                 { source = frameSource
                 ; elem =
                     PrincipalFrameDisagreement
                       { expected = principalFrame; actual = frame }
                 })
        |> MResult.all_unit
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
    let%bind tFuncTyped = checkTypeAndExpectArray env tFunc in
    let%bind tFuncType = checkForArrType tFunc.source (T.arrayType tFuncTyped) in
    let%bind tFuncForall =
      match tFuncType.element with
      | Typed.Type.Forall forall -> MOk forall
      | _ as t ->
        MResult.err
          { source = tFunc.source; elem = ExpectedForall { actual = Typed.Type.Atom t } }
    in
    let%bind bodyType = checkForArrType tFunc.source tFuncForall.body in
    let%bind zippedArgs =
      zipLists ~expected:args.elem ~actual:tFuncForall.parameters ~makeError:(fun ea ->
          { source = args.source; elem = WrongNumberOfArguments ea })
    in
    let%map substitutionsList =
      zippedArgs
      |> List.map ~f:(fun (arg, param) ->
             let%map typedArg = checkKindAndExpect param.bound env arg in
             param.binding, typedArg)
      |> MResult.all
    in
    let substitutions =
      Map.of_alist_reduce (module Typed.Identifier) substitutionsList ~f:(fun a _ -> a)
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
    let%bind iFuncTyped = checkTypeAndExpectArray env iFunc in
    let%bind iFuncType = checkForArrType iFunc.source (T.arrayType iFuncTyped) in
    let%bind iFuncPi =
      match iFuncType.element with
      | Typed.Type.Pi pi -> MOk pi
      | _ as t ->
        MResult.err
          { source = iFunc.source; elem = ExpectedForall { actual = Typed.Type.Atom t } }
    in
    let%bind bodyType = checkForArrType iFunc.source iFuncPi.body in
    let%bind zippedArgs =
      zipLists ~expected:iFuncPi.parameters ~actual:args.elem ~makeError:(fun ea ->
          { source = args.source; elem = WrongNumberOfArguments ea })
    in
    let%map substitutionsList =
      zippedArgs
      |> List.map ~f:(fun (param, arg) ->
             let%map typedArg = checkSortAndExpect param.bound env arg in
             param.binding, typedArg)
      |> MResult.all
    in
    let substitutions =
      Map.of_alist_reduce (module Typed.Identifier) substitutionsList ~f:(fun a _ -> a)
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
    let%bind box = checkTypeAndExpectArray env box in
    let%bind boxArrType = checkForArrType boxSource (T.arrayType box) in
    let%bind sigma = checkForSigmaType boxSource boxArrType.element in
    let%bind zippedIndexBindings =
      zipLists ~expected:sigma.parameters ~actual:indexBindings.elem ~makeError:(fun ea ->
          { source = indexBindings.source; elem = WrongNumberOfUnboxParameters ea })
    in
    let%bind indexBindingsRev, newSortEnvEntries, substitutions =
      List.fold
        zippedIndexBindings
        ~init:(MOk ([], Map.empty (module String), Map.empty (module Typed.Identifier)))
        ~f:(fun soFar (param, binding) ->
          let%bind bindingsSoFar, entriesSoFar, subsSoFar = soFar in
          let%bind () =
            match binding.elem.bound with
            | Some bound ->
              if Sort.equal bound.elem param.bound
              then MOk ()
              else
                MResult.err
                  { source = bound.source
                  ; elem =
                      UnexpectedSortBoundInUnbox
                        { expected = param.bound; actual = bound.elem }
                  }
            | None -> MOk ()
          in
          let id : Typed.Identifier.t =
            { name = binding.elem.binding.elem; id = createId () }
          in
          let%map entries =
            match
              Map.add
                entriesSoFar
                ~key:binding.elem.binding.elem
                ~data:{ e = param.bound; id }
            with
            | `Ok entries -> MOk entries
            | `Duplicate ->
              MResult.err
                { source = binding.elem.binding.source
                ; elem = DuplicateParameterName binding.elem.binding.elem
                }
          in
          let indexRef =
            match param.bound with
            | Sort.Dim -> Typed.Index.Dimension (Typed.Index.dimensionRef id)
            | Sort.Shape -> Typed.Index.Shape [ Typed.Index.ShapeRef id ]
          in
          let subs = Map.set subsSoFar ~key:param.binding ~data:indexRef in
          let bindings = id :: bindingsSoFar in
          bindings, entries, subs)
    in
    let%bind () =
      MResult.require
        (Option.is_none (Map.find newSortEnvEntries valueBinding.elem))
        { source = valueBinding.source; elem = DuplicateParameterName valueBinding.elem }
    in
    let extendedSorts =
      Map.merge_skewed env.sorts newSortEnvEntries ~combine:(fun ~key:_ _ newEntry ->
          newEntry)
    in
    let valueBindingTyped : Typed.Identifier.t =
      { name = valueBinding.elem; id = createId () }
    in
    let extendedTypes =
      Map.set
        env.types
        ~key:valueBinding.elem
        ~data:
          { e = Typed.Type.Array (subIndicesIntoArrayType substitutions sigma.body)
          ; id = valueBindingTyped
          }
    in
    let extendedEnv = { env with sorts = extendedSorts; types = extendedTypes } in
    let bodySource = body.source in
    let%bind body = checkTypeAndExpectArray extendedEnv body in
    let%bind bodyType = checkForArrType bodySource (T.arrayType body) in
    let%map () =
      match findEscapingRefs env (Typed.Type.Array (Typed.Type.Arr bodyType)) with
      | [] -> MOk ()
      | ref :: _ -> MResult.err { source = bodySource; elem = EscapingRef ref.name }
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
      params
      |> List.map ~f:(fun { elem = { binding; bound }; source } ->
             let%map bound = checkKindAndExpectArray env bound in
             let param : ('s, Typed.Type.array) Untyped.param = { binding; bound } in
             { elem = param; source })
      |> MResult.all
    in
    let%bind { typedParams; extendedEnv = extendedTypesEnv } =
      processParams
        env.types
        kindedParams
        ~makeError:(fun name -> DuplicateParameterName name)
        ~boundToEnvEntry:(fun b -> Typed.Type.Array b)
    in
    let extendedEnv = { env with types = extendedTypesEnv } in
    let%map body = checkTypeAndExpectArray extendedEnv body in
    let type' : Typed.Type.func =
      { parameters = List.map typedParams ~f:(fun p -> p.bound)
      ; return = T.arrayType body
      }
    in
    T.Atom (T.TermLambda { params = typedParams; body = T.Array body; type' })
  | U.TypeLambda { params; body } ->
    let%bind { typedParams; extendedEnv = extendedKindsEnv } =
      processParams
        env.kinds
        params
        ~makeError:(fun name -> DuplicateParameterName name)
        ~boundToEnvEntry:(fun b -> b)
    in
    let extendedEnv = { env with kinds = extendedKindsEnv } in
    let%bind () = requireValue body in
    let%map body = checkTypeAndExpectArray extendedEnv body in
    let type' : Typed.Type.forall =
      { parameters = typedParams; body = T.arrayType body }
    in
    T.Atom (T.TypeLambda { params = typedParams; body; type' })
  | U.IndexLambda { params; body } ->
    let%bind { typedParams; extendedEnv = extendedSortsEnv } =
      processParams
        env.sorts
        params
        ~makeError:(fun name -> DuplicateParameterName name)
        ~boundToEnvEntry:(fun b -> b)
    in
    let extendedEnv = { env with sorts = extendedSortsEnv } in
    let%bind () = requireValue body in
    let%map body = checkTypeAndExpectArray extendedEnv body in
    let type' : Typed.Type.pi = { parameters = typedParams; body = T.arrayType body } in
    T.Atom (T.IndexLambda { params = typedParams; body; type' })
  | U.Boxes { params; elementType; dimensions; elements } ->
    let typedParams =
      List.map params ~f:(fun p ->
          let typedParam : Sort.t Typed.param =
            { binding = { name = p.elem.binding.elem; id = createId () }
            ; bound = p.elem.bound
            }
          in
          typedParam)
    in
    let dimensions = List.map dimensions.elem ~f:(fun d -> d.elem) in
    let expectedElements = List.fold dimensions ~init:1 ~f:( * ) in
    let%bind elementType = checkKindAndExpectArray env elementType
    and checkedElements =
      elements.elem
      |> List.map ~f:(fun e ->
             let%map indices =
               e.elem.indices.elem
               |> List.map ~f:(fun index ->
                      checkSort env index >>| fun i -> i, index.source)
               |> MResult.all
             and body = checkTypeAndExpectArray env e.elem.body in
             indices, body, e)
      |> MResult.all
    and _ =
      MResult.require
        (List.length elements.elem = expectedElements)
        { elem =
            WrongNumberOfElementsInBoxes
              { expected = expectedElements; actual = List.length elements.elem }
        ; source = elements.source
        }
    in
    let%map boxes =
      checkedElements
      |> List.map ~f:(fun (indices, body, element) ->
             let%bind zippedIndices =
               zipLists ~expected:typedParams ~actual:indices ~makeError:(fun ea ->
                   { source = element.elem.indices.source
                   ; elem = WrongNumberOfArguments ea
                   })
             in
             let%bind substitutions =
               List.fold
                 zippedIndices
                 ~init:(MOk (Map.empty (module Typed.Identifier)))
                 ~f:(fun acc _ -> acc)
             in
             let subbedType = subIndicesIntoArrayType substitutions elementType in
             let%map () =
               requireType
                 ~expected:(Typed.Type.Array subbedType)
                 ~actual:(T.type' (T.Array body))
                 ~makeError:(fun ea ->
                   { source = element.elem.body.source
                   ; elem = SigmaBodyTypeDisagreement ea
                   })
             in
             ({ indices = List.map indices ~f:(fun (i, _) -> i)
              ; body
              ; bodyType = elementType
              ; type' = { parameters = typedParams; body = elementType }
              }
               : T.box))
      |> MResult.all
    in
    let shape =
      List.map dimensions ~f:(fun d -> Typed.Index.Add (Typed.Index.dimensionConstant d))
    in
    let sigmaType : Typed.Type.sigma = { parameters = typedParams; body = elementType } in
    T.Array
      (T.Frame
         { dimensions
         ; arrays = List.map boxes ~f:(fun b -> T.Scalar (T.Box b))
         ; type' = { element = Typed.Type.Sigma sigmaType; shape }
         })
  | U.Let { binding; bound; value; body } ->
    let id : Typed.Identifier.t = { name = binding.elem; id = createId () } in
    let%bind bound =
      bound |> Option.map ~f:(fun bound -> checkKind env bound) |> MResult.traverseOpt
    in
    let checkBody valueType =
      let extendedTypesEnv =
        Map.set env.types ~key:binding.elem ~data:{ id; e = valueType }
      in
      let extendedEnv = { env with types = extendedTypesEnv } in
      checkType extendedEnv body
    in
    let checkValue =
      match bound with
      | Some bound ->
        let%bind valueTyped = checkTypeAndExpect (Typed.Type.kind bound) env value in
        let%map () =
          requireType ~expected:bound ~actual:(T.type' valueTyped) ~makeError:(fun ea ->
              { source = value.source; elem = LetTypeDisagreement ea })
        in
        valueTyped
      | None -> checkType env value
    in
    let%map bodyTyped, valueTyped =
      match bound with
      | Some bound -> MResult.both (checkBody bound) checkValue
      | None ->
        let%bind valueTyped = checkValue in
        let%map bodyTyped = checkBody (T.type' valueTyped) in
        bodyTyped, valueTyped
    in
    (match bodyTyped with
    | T.Array bodyTyped ->
      T.Array
        (T.ArrayLet
           { binding = id
           ; bound = T.type' valueTyped
           ; value = valueTyped
           ; body = bodyTyped
           ; type' = T.arrayType bodyTyped
           })
    | T.Atom bodyTyped ->
      T.Atom
        (T.AtomLet
           { binding = id
           ; bound = T.type' valueTyped
           ; value = valueTyped
           ; body = bodyTyped
           ; type' = T.atomType bodyTyped
           }))
  | U.TupleLet { bindings; bound; value; body } ->
    let%bind bound =
      bound
      |> Option.map ~f:(fun bound ->
             checkKindAndExpectAtom env bound >>= checkForTupleType bound.source)
      |> MResult.traverseOpt
    in
    let checkBodyAndGetBindings (valueType : Typed.Type.tuple) =
      let%bind zippedBindings =
        zipLists ~expected:valueType ~actual:bindings.elem ~makeError:(fun ea ->
            { source = bindings.source; elem = WrongNumberOfBindings ea })
      in
      let%bind newEnvEntries, revBindings =
        List.fold
          zippedBindings
          ~init:(MOk (Map.empty (module String), []))
          ~f:(fun acc (type', binding) ->
            let%bind envEntriesSoFar, bindingsSoFar = acc in
            let id : Typed.Identifier.t = { name = binding.elem; id = createId () } in
            let entry = { id; e = Typed.Type.Atom type' } in
            match Map.add envEntriesSoFar ~key:binding.elem ~data:entry with
            | `Ok envEntries -> MOk (envEntries, id :: bindingsSoFar)
            | `Duplicate ->
              MResult.err
                { source = binding.source; elem = DuplicateTupleBindingName binding.elem })
      in
      let extendedTypesEnv =
        Map.merge_skewed env.types newEnvEntries ~combine:(fun ~key:_ _ e -> e)
      in
      let extendedEnv = { env with types = extendedTypesEnv } in
      let%map body = checkTypeAndExpectAtom extendedEnv body in
      body, List.rev revBindings
    in
    let checkValue =
      let%bind valueTyped = checkTypeAndExpectAtom env value in
      let%bind valueType = checkForTupleType value.source (T.atomType valueTyped) in
      match bound with
      | Some bound ->
        let boundWidened = Typed.Type.Atom (Typed.Type.Tuple bound) in
        let%map () =
          requireType
            ~expected:boundWidened
            ~actual:(T.type' (T.Atom valueTyped))
            ~makeError:(fun ea ->
              { source = value.source; elem = LetTypeDisagreement ea })
        in
        valueTyped, valueType
      | None -> MOk (valueTyped, valueType)
    in
    (match bound with
    | Some bound ->
      let%map body, bindings = checkBodyAndGetBindings bound
      and value, valueType = checkValue in
      T.Atom
        (T.TupleLet { bindings; bound = valueType; value; body; type' = T.atomType body })
    | None ->
      let%bind value, valueType = checkValue in
      let%map body, bindings = checkBodyAndGetBindings valueType in
      T.Atom
        (T.TupleLet { bindings; bound = valueType; value; body; type' = T.atomType body }))
  | U.Tuple elements ->
    let%map elements =
      elements |> List.map ~f:(checkTypeAndExpectAtom env) |> MResult.all
    in
    let elementTypes = List.map elements ~f:(fun e -> T.atomType e) in
    T.Atom (T.Tuple { elements; type' = elementTypes })

and checkTypeAndExpectArray env expr =
  let open Typed.Expr in
  match%bind checkType env expr with
  | Array array -> MOk array
  | Atom atom -> MOk (Scalar atom)

and checkTypeAndExpectAtom env expr =
  let open Typed.Expr in
  match%bind checkType env expr with
  | Atom atom -> MOk atom
  | Array _ as typed ->
    MResult.err
      { source = expr.source; elem = ExpectedAtomicExpr { actual = type' typed } }

and checkTypeAndExpect kind env expr =
  match kind with
  | Kind.Array -> checkTypeAndExpectArray env expr >>| fun r -> Typed.Expr.Array r
  | Kind.Atom -> checkTypeAndExpectAtom env expr >>| fun r -> Typed.Expr.Atom r
;;
