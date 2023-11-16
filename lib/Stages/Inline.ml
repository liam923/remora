open! Base
module Index = Typed.Index
module Type = Typed.Type
module Canonical = Typed.Canonical

type application =
  | TypeApp of Type.t list
  | IndexApp of Index.t list
[@@deriving sexp_of]

type appStack = application list [@@deriving sexp_of]

module CanonicalAppStack = struct
  module T = struct
    type element =
      | TypeApp of Canonical.Type.t list
      | IndexApp of Canonical.Index.t list
    [@@deriving compare, sexp]

    type t = element list [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)

  let from =
    List.map ~f:(fun (app : application) ->
      match app with
      | TypeApp types -> TypeApp (List.map types ~f:Canonical.Type.from)
      | IndexApp indices -> IndexApp (List.map indices ~f:Canonical.Index.from))
  ;;
end

module Function = struct
  type t =
    | Lambda of
        { lambda : Explicit.Expr.termLambda
        ; id : Identifier.t
        ; captures : Set.M(Identifier).t
        }
    | Primitive of
        { func : Typed.Expr.primitiveFuncName
        ; type' : Explicit.Type.array
        ; appStack : appStack
        }
  [@@deriving sexp_of]

  let equal a b =
    match a, b with
    | Lambda a, Lambda b -> Identifier.equal a.id b.id
    | Primitive a, Primitive b -> Typed.Expr.equal_primitiveFuncName a.func b.func
    | Lambda _, Primitive _ | Primitive _, Lambda _ -> false
  ;;

  let toExplicit =
    let module E = Explicit.Expr in
    function
    | Lambda { lambda; id = _; captures = _ } ->
      E.Scalar
        { element = TermLambda lambda
        ; type' = { element = Func lambda.type'; shape = [] }
        }
    | Primitive { func; type'; appStack = _ } -> Primitive { name = Func func; type' }
  ;;
end

module FunctionSet = struct
  type t =
    | Multiple
    | One of Function.t
    | Empty
  [@@deriving sexp_of]

  let merge functionSets =
    let rec loop = function
      | acc, [] -> acc
      | _, Multiple :: _ -> Multiple
      | Multiple, _ -> Multiple
      | Empty, One func :: rest -> loop (One func, rest)
      | One a, One b :: rest ->
        if Function.equal a b then loop (One a, rest) else Multiple
      | Empty, Empty :: rest -> loop (Empty, rest)
      | (One _ as acc), Empty :: rest -> loop (acc, rest)
    in
    loop (Empty, functionSets)
  ;;
end

type cacheEntry =
  { binding : Identifier.t
  ; monoValue : Nucleus.Expr.array
  ; functions : FunctionSet.t
  }
[@@deriving sexp_of]

type envEntry =
  { polyValue : Explicit.Expr.array
  ; cache : cacheEntry Map.M(CanonicalAppStack).t
  }
[@@deriving sexp_of]

module InlineState = struct
  include StateT.Make2WithError (MResult)

  type state =
    { compilerState : CompilerState.state
    ; env : envEntry Map.M(Identifier).t
    }

  type 't u = (state, 't, string) t

  let getEnv () = get () >>| fun state -> state.env
  let setEnv env = get () >>= fun state -> set { state with env }

  let createId name =
    make ~f:(fun state ->
      State.run
        (Identifier.create
           name
           ~getCounter:(fun (s : state) -> s.compilerState.idCounter)
           ~setCounter:(fun (s : state) idCounter ->
             { s with compilerState = { s.compilerState with idCounter } }))
        state)
  ;;

  let err error = returnF (MResult.Errors (error :: []))
end

let rec inlineAtomTypeWithStack appStack : Typed.Type.atom -> Nucleus.Type.array
  = function
  | AtomRef _ ->
    raise (Unreachable.Error "There should be no type refs left after inlining")
  | Sigma sigma ->
    { element = Sigma (inlineSigmaTypeWithStack appStack sigma); shape = [] }
  | Literal CharacterLiteral -> { element = Literal CharacterLiteral; shape = [] }
  | Literal IntLiteral -> { element = Literal IntLiteral; shape = [] }
  | Literal BooleanLiteral -> { element = Literal BooleanLiteral; shape = [] }
  | Func _ -> { element = Tuple []; shape = [] }
  | Forall { parameters; body } ->
    (match appStack with
     | TypeApp types :: restStack ->
       let typeSubs =
         List.zip_exn parameters types
         |> List.fold
              ~init:(Map.empty (module Identifier))
              ~f:(fun subs (param, sub) -> Map.set subs ~key:param.binding ~data:sub)
       in
       let subbedBody = Explicit.Substitute.Type.subTypesIntoArray typeSubs body in
       inlineArrayTypeWithStack restStack subbedBody
     | [] ->
       (* Empty stack means the Forall's value is not passed to a type
          application, so we can replace it with a unit *)
       { element = Tuple []; shape = [] }
     | _ :: _ as stack ->
       raise
         (Unreachable.Error
            (String.concat_lines
               [ "Expected type application at head of stack or empty stack, got stack:"
               ; [%sexp_of: appStack] stack |> Sexp.to_string_hum
               ])))
  | Pi { parameters; body } ->
    (match appStack with
     | IndexApp types :: restStack ->
       let indexSubs =
         List.zip_exn parameters types
         |> List.fold
              ~init:(Map.empty (module Identifier))
              ~f:(fun subs (param, sub) -> Map.set subs ~key:param.binding ~data:sub)
       in
       let subbedBody = Explicit.Substitute.Type.subIndicesIntoArray indexSubs body in
       inlineArrayTypeWithStack restStack subbedBody
     | [] ->
       (* Empty stack means the Pi's value is not passed to an index
          application, so we can replace it with a unit *)
       { element = Tuple []; shape = [] }
     | _ :: _ as stack ->
       raise
         (Unreachable.Error
            (String.concat_lines
               [ "Expected index application at head of stack or empty stack, got stack:"
               ; [%sexp_of: appStack] stack |> Sexp.to_string_hum
               ])))

and inlineArrayTypeWithStack appStack : Typed.Type.array -> Nucleus.Type.array = function
  | ArrayRef _ ->
    raise (Unreachable.Error "There should be no type refs left after inlining")
  | Arr { element; shape = outerShape } ->
    let ({ element; shape = innerShape } : Nucleus.Type.array) =
      inlineAtomTypeWithStack appStack element
    in
    { element; shape = outerShape @ innerShape }

and inlineSigmaTypeWithStack appStack ({ parameters; body } : Typed.Type.sigma)
  : Nucleus.Type.sigma
  =
  { parameters; body = inlineArrayTypeWithStack appStack body }
;;

let assertValueRestriction value =
  let isPolymorphicType =
    let open Explicit.Type in
    let rec isPolymorphicArray = function
      | ArrayRef _ -> false
      | Arr arr -> isPolymorphicAtom arr.element
    and isPolymorphicAtom = function
      | AtomRef _ -> false
      | Func _ -> false
      | Forall _ -> true
      | Pi _ -> true
      | Sigma sigma -> isPolymorphicArray sigma.body
      | Literal IntLiteral -> false
      | Literal CharacterLiteral -> false
      | Literal BooleanLiteral -> false
    in
    isPolymorphicArray
  in
  let isValue =
    let open Explicit.Expr in
    let rec isValueArray = function
      | Ref _ -> true
      | Scalar scalar -> isValueAtom scalar.element
      | Frame frame -> List.for_all frame.elements ~f:isValueArray
      | TermApplication _ -> false
      | TypeApplication _ -> false
      | IndexApplication _ -> false
      | BoxValue _ -> false
      | IndexLet _ -> false
      | ReifyIndex _ -> true
      | Primitive _ -> true
      | Map _ -> false
    and isValueAtom = function
      | TermLambda _ -> true
      | TypeLambda _ -> true
      | IndexLambda _ -> true
      | Box box -> isValueArray box.body
      | Literal (IntLiteral _) -> true
      | Literal (CharacterLiteral _) -> true
      | Literal (BooleanLiteral _) -> true
    in
    isValueArray
  in
  if isPolymorphicType (Explicit.Expr.arrayType value) && not (isValue value)
  then
    InlineState.err
      (String.concat_lines
         [ "Polymorphic variables and function arguments must be a value type, got \
            not-value:"
         ; [%sexp_of: Explicit.Expr.array] value |> Sexp.to_string_hum
         ])
  else InlineState.return ()
;;

let scalar atom =
  Nucleus.Expr.(
    AtomAsArray { element = atom; type' = { element = atomType atom; shape = [] } })
;;

let rec inlineArray subs indexEnv (appStack : appStack) (array : Explicit.Expr.array)
  : (Nucleus.Expr.array * FunctionSet.t) InlineState.u
  =
  let module E = Explicit.Expr in
  let module I = Nucleus.Expr in
  let open InlineState.Let_syntax in
  match array with
  | Ref { id; type' } ->
    (* Substitute the id if it appears in subs *)
    let id = Map.find subs id |> Option.value ~default:id in
    (* Look up the cache entry corresponding to id and the current appStack. *)
    let%bind env = InlineState.getEnv () in
    let { polyValue; cache } = Map.find_exn env id in
    let canonicalStack = CanonicalAppStack.from appStack in
    (match Map.find cache canonicalStack with
     | Some { binding; monoValue = _; functions } ->
       (* The cache entry already exists *)
       return
         ( I.Ref { id = binding; type' = inlineArrayTypeWithStack appStack type' }
         , functions )
     | None ->
       (* No cache entry exists yet. Create a new monomorphization of the
          variable and put it in the cache. *)
       let%bind monoValue, functions = inlineArray subs indexEnv appStack polyValue
       and binding = InlineState.createId (Identifier.name id) in
       let cacheEntry = { binding; monoValue; functions } in
       (* env may have been updated when computing the monoValue, so
          need to re-fetch it *)
       let%bind env = InlineState.getEnv () in
       let newEnv =
         Map.set
           env
           ~key:id
           ~data:{ polyValue; cache = Map.set cache ~key:canonicalStack ~data:cacheEntry }
       in
       let%map () = InlineState.setEnv newEnv in
       I.Ref { id = binding; type' = inlineArrayTypeWithStack appStack type' }, functions)
  | Scalar { element; type' = _ } -> inlineAtom subs indexEnv appStack element
  | Frame { dimensions; elements; type' } ->
    let%map elements, functions =
      elements
      |> List.map ~f:(inlineArray subs indexEnv appStack)
      |> InlineState.all
      |> InlineState.unzip
    in
    let functions = FunctionSet.merge functions in
    ( I.Frame
        { dimensions; elements; type' = inlineArrayTypeWithStack appStack (Arr type') }
    , functions )
  | TermApplication termApplication ->
    inlineTermApplication subs indexEnv appStack termApplication
  | TypeApplication { tFunc; args; type' = _ } ->
    inlineArray subs indexEnv (TypeApp args :: appStack) tFunc
  | IndexApplication { iFunc; args; type' = _ } ->
    inlineArray subs indexEnv (IndexApp args :: appStack) iFunc
  | BoxValue { box; type' } ->
    let%map box, functions = inlineArray subs indexEnv appStack box in
    let type' = inlineArrayTypeWithStack appStack (Arr type') in
    I.BoxValue { box; type' }, functions
  | IndexLet { indexArgs; body; type' } ->
    let extendedIndexEnv =
      List.fold indexArgs ~init:indexEnv ~f:(fun env arg -> Set.add env arg.indexBinding)
    in
    let%map body, functions = inlineArray subs extendedIndexEnv appStack body
    and indexArgs =
      indexArgs
      |> List.map ~f:(fun { indexBinding; indexValue; sort } ->
        match indexValue with
        | Runtime value ->
          let%map value, _ = inlineArray subs indexEnv [] value in
          I.{ indexBinding; indexValue = Runtime value; sort }
        | FromBox { box; i } ->
          let%map box, _ = inlineArray subs indexEnv [] box in
          I.{ indexBinding; indexValue = FromBox { box; i }; sort })
      |> InlineState.all
    in
    let type' = inlineArrayTypeWithStack appStack type' in
    I.IndexLet { indexArgs; body; type' }, functions
  | ReifyIndex { index; type' } ->
    return
      ( I.ReifyIndex { index; type' = inlineArrayTypeWithStack appStack (Arr type') }
      , FunctionSet.Empty )
  | Primitive { name; type' } ->
    (match name with
     | Func func ->
       return
         ( scalar (I.Values { elements = []; type' = [] })
         , FunctionSet.One (Primitive { func; type'; appStack }) )
     | Val Iota ->
       (match appStack with
        | [ IndexApp [ Shape s ] ] ->
          let%map iota = InlineState.createId "iota" in
          ( I.ArrayPrimitive
              (Map
                 { frameShape = s
                 ; args = []
                 ; iotaVar = Some iota
                 ; body =
                     Ref
                       { id = iota; type' = { element = Literal IntLiteral; shape = [] } }
                 ; type' = inlineArrayTypeWithStack appStack type'
                 })
          , FunctionSet.Empty )
        | _ ->
          raise
            (Unreachable.Error
               (String.concat_lines
                  [ "iota expected a stack of [IndexApp], got"
                  ; [%sexp_of: appStack] appStack |> Sexp.to_string_hum
                  ]))))
  | Map { args; body; frameShape; type' } ->
    let args = List.map args ~f:(fun { binding; value } -> binding, value) in
    let%map body, args, functions =
      inlineBodyWithBindings subs indexEnv appStack body args
    in
    let args =
      args
      |> List.map ~f:(fun bindings ->
        bindings
        |> Map.to_alist
        |> List.map ~f:(fun (_, (binding, value)) : I.arg -> { binding; value }))
      |> List.join
    in
    ( I.ArrayPrimitive
        (Map
           { args
           ; body
           ; iotaVar = None
           ; frameShape
           ; type' = inlineArrayTypeWithStack appStack type'
           })
    , functions )

and inlineAtom subs indexEnv (appStack : appStack) (atom : Explicit.Expr.atom)
  : (Nucleus.Expr.array * FunctionSet.t) InlineState.u
  =
  let module E = Explicit.Expr in
  let module I = Nucleus.Expr in
  let open InlineState.Let_syntax in
  match atom with
  | TermLambda lambda ->
    (* Since all function calls get inlined, the value of the lambda is
       never used, so it can be safely replaced with unit *)
    let%map id = InlineState.createId "lambda" in
    let captures =
      let indexCaptures = function
        | Explicit.Index.Shape dims ->
          dims
          |> List.bind ~f:(function
            | ShapeRef ref -> [ ref ]
            | Add { const = _; refs } -> Map.keys refs)
          |> Set.of_list (module Identifier)
        | Explicit.Index.Dimension { const = _; refs } ->
          refs |> Map.keys |> Set.of_list (module Identifier)
      in
      let rec arrayCaptures = function
        | E.Ref { id; type' = _ } -> Set.singleton (module Identifier) id
        | E.Scalar { element; type' = _ } -> atomCaptures element
        | E.Frame { elements; dimensions = _; type' = _ } ->
          elements |> List.map ~f:arrayCaptures |> Set.union_list (module Identifier)
        | E.TermApplication { func; args; type' = _ } ->
          let funcCaptures = arrayCaptures func
          and argCaptures =
            args |> List.map ~f:(fun arg -> arg.id) |> Set.of_list (module Identifier)
          in
          Set.union funcCaptures argCaptures
        | E.TypeApplication { tFunc; args = _; type' = _ } -> arrayCaptures tFunc
        | E.IndexApplication { iFunc; args = _; type' = _ } -> arrayCaptures iFunc
        | E.BoxValue { box; type' = _ } -> arrayCaptures box
        | E.IndexLet { indexArgs; body; type' = _ } ->
          let indexValuesCaptures =
            List.map indexArgs ~f:(fun arg ->
              match arg.indexValue with
              | Runtime value -> arrayCaptures value
              | FromBox { box; i = _ } -> arrayCaptures box)
          and bodyCaptures = arrayCaptures body in
          let variablesUsed =
            Set.union_list (module Identifier) (bodyCaptures :: indexValuesCaptures)
          in
          let variablesDeclared =
            indexArgs
            |> List.map ~f:(fun arg -> arg.indexBinding)
            |> Set.of_list (module Identifier)
          in
          Set.diff variablesUsed variablesDeclared
        | E.Map { args; body; frameShape; type' = _ } ->
          let bodyCaptures = arrayCaptures body
          and argCaptures = List.map args ~f:(fun arg -> arrayCaptures arg.value)
          and frameCaptures = indexCaptures (Shape frameShape) in
          let variablesUsed =
            Set.union_list
              (module Identifier)
              (bodyCaptures :: frameCaptures :: argCaptures)
          in
          let variablesDeclared =
            args
            |> List.map ~f:(fun arg -> arg.binding)
            |> Set.of_list (module Identifier)
          in
          Set.diff variablesUsed variablesDeclared
        | E.ReifyIndex { index; type' = _ } -> indexCaptures index
        | E.Primitive { name = _; type' = _ } -> Set.empty (module Identifier)
      and atomCaptures = function
        | E.TermLambda { params; body; type' = _ } ->
          let variablesUsed = arrayCaptures body in
          let variablesDeclared =
            params
            |> List.map ~f:(fun param -> param.binding)
            |> Set.of_list (module Identifier)
          in
          Set.diff variablesUsed variablesDeclared
        | E.TypeLambda { params = _; body; type' = _ } -> arrayCaptures body
        | E.IndexLambda { params = _; body; type' = _ } -> arrayCaptures body
        | E.Box { indices; body; bodyType = _; type' = _ } ->
          let indexCaptures = List.map indices ~f:indexCaptures
          and bodyCaptures = arrayCaptures body in
          Set.union_list (module Identifier) (bodyCaptures :: indexCaptures)
        | E.Literal (IntLiteral _) -> Set.empty (module Identifier)
        | E.Literal (CharacterLiteral _) -> Set.empty (module Identifier)
        | E.Literal (BooleanLiteral _) -> Set.empty (module Identifier)
      in
      atomCaptures (E.TermLambda lambda)
    in
    ( scalar (I.Values { elements = []; type' = [] })
    , FunctionSet.One (Lambda { lambda; id; captures }) )
  | TypeLambda { params; body; type' = _ } ->
    (match appStack with
     | TypeApp types :: restStack ->
       let typeSubs =
         List.zip_exn params types
         |> List.fold
              ~init:(Map.empty (module Identifier))
              ~f:(fun subs (param, sub) -> Map.set subs ~key:param.binding ~data:sub)
       in
       let subbedBody = Explicit.Substitute.Expr.subTypesIntoArray typeSubs body in
       inlineArray subs indexEnv restStack subbedBody
     | [] ->
       (* Empty stack means the type lambda's value is not passed to a type
          application, so we can replace it with a unit *)
       return (scalar (I.Values { elements = []; type' = [] }), FunctionSet.Empty)
     | _ :: _ as stack ->
       raise
         (Unreachable.Error
            (String.concat_lines
               [ "Expected type application at head of stack or empty stack, got stack:"
               ; [%sexp_of: appStack] stack |> Sexp.to_string_hum
               ])))
  | IndexLambda { params; body; type' = _ } ->
    (match appStack with
     | IndexApp indices :: restStack ->
       let indexSubs =
         List.zip_exn params indices
         |> List.fold
              ~init:(Map.empty (module Identifier))
              ~f:(fun subs (param, sub) -> Map.set subs ~key:param.binding ~data:sub)
       in
       let subbedBody = Explicit.Substitute.Expr.subIndicesIntoArray indexSubs body in
       inlineArray subs indexEnv restStack subbedBody
     | [] ->
       (* Empty stack means the index lambda's value is not passed to an index
          application, so we can replace it with a unit *)
       return (scalar (I.Values { elements = []; type' = [] }), FunctionSet.Empty)
     | _ as stack ->
       raise
         (Unreachable.Error
            (String.concat_lines
               [ "Expected index application at head of stack, got stack:"
               ; [%sexp_of: appStack] stack |> Sexp.to_string_hum
               ])))
  | Box { indices; body; bodyType; type' } ->
    let%map body, functions = inlineArray subs indexEnv appStack body in
    ( scalar
        (I.Box
           { indices
           ; body
           ; bodyType = inlineArrayTypeWithStack appStack bodyType
           ; type' = inlineSigmaTypeWithStack appStack type'
           })
    , functions )
  | Literal (CharacterLiteral c) ->
    return (scalar (I.Literal (CharacterLiteral c)), FunctionSet.Empty)
  | Literal (IntLiteral i) -> return (scalar (I.Literal (IntLiteral i)), FunctionSet.Empty)
  | Literal (BooleanLiteral b) ->
    return (scalar (I.Literal (BooleanLiteral b)), FunctionSet.Empty)

and inlineTermApplication subs indexEnv appStack termApplication =
  let module E = Explicit.Expr in
  let module I = Nucleus.Expr in
  let open InlineState.Let_syntax in
  let ({ func; args; type' } : E.termApplication) = termApplication in
  let args =
    List.map args ~f:(fun ({ id; type' } : E.ref) ->
      let id = Map.find subs id |> Option.value ~default:id in
      ({ id; type' } : E.ref))
  in
  let%bind _, functions = inlineArray subs indexEnv [] func in
  let%bind func =
    match functions with
    | Empty -> raise (Unreachable.Error "func should've returned at least one function")
    | One f -> return f
    | Multiple ->
      InlineState.err
        (String.concat_lines
           [ "Could not determine what function is being called in function call:"
           ; [%sexp_of: E.termApplication] termApplication |> Sexp.to_string_hum
           ])
  in
  match func with
  | Lambda { lambda = { params; body; type' = _ }; captures; id = _ } ->
    (* Verify that all the variables captured by the function are declared.
       If not, throw an error (this should change in the future because it
       should be allowed) *)
    let%bind env = InlineState.getEnv () in
    let escapedVariables =
      captures
      |> Set.to_list
      |> List.filter ~f:(fun capturedVar ->
        Map.find env capturedVar |> Option.is_none && not (Set.mem indexEnv capturedVar))
    in
    let%bind () =
      match escapedVariables with
      | escapedVar :: _ ->
        InlineState.err
          [%string
            "Lambda captures variable %{Identifier.name escapedVar}, which escapes its \
             definition"]
      | [] -> return ()
    in
    (* Simply return the lambda's body inlined, with the parameters
       replaced with the args. The args are already guaranteed to be variables
       bound to a map due to how the Explicitize stage works, so we don't
       need to create bindings. (If this wasn't the case, we'd need to
       create bindings to guaranteed the params aren't computed twice.) *)
    let subs =
      List.zip_exn params args
      |> List.fold ~init:subs ~f:(fun subs (param, arg) ->
        (* Assert that the arg we're subbing conforms to the param's bound.
           This should have been already guaranteed by the type checker *)
        assert (
          Canonical.Type.equal
            (Canonical.Type.from (Array param.bound))
            (Canonical.Type.from (Array arg.type')));
        Map.set subs ~key:param.binding ~data:arg.id)
    in
    inlineArray subs indexEnv appStack body
  | Primitive primitive ->
    let scalarBinop op =
      assert (List.length args = 2);
      let%map args, _ =
        args
        |> List.map ~f:(fun arg -> inlineArray subs indexEnv [] (Ref arg))
        |> InlineState.all
        |> InlineState.unzip
      in
      let args =
        List.map args ~f:(fun arg ->
          I.ArrayAsAtom { array = arg; type' = (I.arrayType arg).element })
      in
      ( scalar
          (I.AtomicPrimitive
             { op; args; type' = (inlineArrayTypeWithStack appStack (Arr type')).element })
      , FunctionSet.Empty )
    in
    (match primitive.func with
     | Add -> scalarBinop Add
     | Sub -> scalarBinop Sub
     | Mul -> scalarBinop Mul
     | Div -> scalarBinop Div
     | Equal -> scalarBinop Equal
     | Append ->
       assert (List.length args = 2);
       (match primitive.appStack with
        | [ IndexApp [ Dimension d1; Dimension d2; Shape cellShape ]; TypeApp [ Atom _ ] ]
          ->
          let%map arg1, _ = inlineArray subs indexEnv [] (Ref (List.nth_exn args 0))
          and arg2, _ = inlineArray subs indexEnv [] (Ref (List.nth_exn args 1)) in
          ( I.ArrayPrimitive
              (Append
                 { arg1
                 ; arg2
                 ; d1
                 ; d2
                 ; cellShape
                 ; type' = inlineArrayTypeWithStack appStack (Arr type')
                 })
          , FunctionSet.Empty )
        | _ ->
          raise
            (Unreachable.Error
               (String.concat_lines
                  [ "append expected a stack of [IndexApp; TypeApp], got"
                  ; [%sexp_of: appStack] appStack |> Sexp.to_string_hum
                  ])))
     | Reduce { associative; explicitZero; character } ->
       (match primitive.appStack with
        | [ IndexApp [ Dimension d; Shape itemPad; Shape cellShape ]; TypeApp [ Atom t ] ]
          ->
          (* If there is no explicit zero, the index argument for d is
             actually d-1 *)
          let d = if explicitZero then d else { const = d.const + 1; refs = d.refs } in
          (* Extract the arguments to the function. Note that the arguments
             differ depending on explicitZero *)
          let f, zero, arrayArg =
            if explicitZero
            then (
              match args with
              | [ f; zero; arrayArg ] -> f, Some zero, arrayArg
              | _ ->
                raise
                  (Unreachable.Error "Reduce with explicit zero expected three arguments"))
            else (
              match args with
              | [ f; arrayArg ] -> f, None, arrayArg
              | _ ->
                raise
                  (Unreachable.Error "Reduce with explicit zero expected two arguments"))
          in
          let%bind _, functions = inlineArray subs indexEnv [] (Ref f) in
          let%bind func =
            match functions with
            | Empty ->
              raise
                (Unreachable.Error
                   "func of reduce should've returned at least one function")
            | One f -> return f
            | Multiple ->
              InlineState.err
                (String.concat_lines
                   [ "Could not determine what function is being passed to reduce:"
                   ; [%sexp_of: E.termApplication] termApplication |> Sexp.to_string_hum
                   ])
          in
          let%bind fArg1 = InlineState.createId "reduce-arg1"
          and fArg2 = InlineState.createId "reduce-arg2" in
          let body =
            E.TermApplication
              { func = Function.toExplicit func
              ; args =
                  [ { id = fArg1; type' = Arr { element = t; shape = cellShape } }
                  ; { id = fArg2; type' = Arr { element = t; shape = cellShape } }
                  ]
              ; type' = { element = t; shape = cellShape }
              }
          in
          let%bind () =
            (* Need to check the value restriction on the body.*)
            match func with
            | Lambda lambda ->
              (* Special case - just need to check to see if the body of the
                 lambda is a value. The term application around it isn't
                 technically a value, but it involves no computation since it
                 simply re-assigns variables *)
              assertValueRestriction lambda.lambda.body
            | Primitive _ -> assertValueRestriction body
          in
          let%bind body, bindings, functions =
            inlineBodyWithBindings
              subs
              indexEnv
              appStack
              body
              [ fArg1, Ref arrayArg; fArg2, Ref arrayArg ]
          in
          (* Since fArg1 and fArg2 both come from arrayArg, they need to have
             the same set of monomorphizations, so we need to merge together
             their cache entries *)
          let appStacksToMonoValue =
            bindings
            |> List.bind ~f:(fun map ->
              map |> Map.map ~f:(fun (_, monoValue) -> monoValue) |> Map.to_alist)
            |> Map.of_alist_reduce (module CanonicalAppStack) ~f:(fun a _ -> a)
          in
          (* If the reduce's type is not-polymorphic, then the app stack
             has to be empty. Thus, there is a unique app stack. If it is
             polymorphic, the body must be a value because of the value
             restriction. In this case, there cannot be any type applications
             in the body, so the only possible type stack origin is on the result
             of the reduce. Therefore, in both cases, the only app stack that
             can be used is the one applied to the result of the reduce. Thus,
             the length has to be less than or equal to 1. This is important
             because it ensures that the number of monomorphizations of the args
             is contained.
          *)
          assert (Map.length appStacksToMonoValue <= 1);
          let canonicalAppStack = CanonicalAppStack.from appStack in
          let%bind arrayArgMono =
            match Map.find appStacksToMonoValue canonicalAppStack with
            | Some monoValue -> return monoValue
            | None ->
              let%map monoValue, _ = inlineArray subs indexEnv appStack (Ref arrayArg) in
              monoValue
          in
          assert (Map.is_empty (Map.remove appStacksToMonoValue canonicalAppStack));
          let getBindingFrom argBindings argName =
            match Map.find argBindings canonicalAppStack with
            | Some (binding, _) -> return binding
            | None -> InlineState.createId argName
          in
          let%bind firstBinding, secondBinding =
            match bindings with
            | [ bindings1; bindings2 ] ->
              let%map firstBinding = getBindingFrom bindings1 "reduce-arg1"
              and secondBinding = getBindingFrom bindings2 "reduce-arg2" in
              firstBinding, secondBinding
            | _ -> raise (Unreachable.Error "bindings should have len 2")
          in
          let arg = I.{ firstBinding; secondBinding; value = arrayArgMono } in
          let type' = inlineArrayTypeWithStack appStack (Arr type') in
          let%map zero =
            match zero with
            | Some zero ->
              let%map zero, _ = inlineArray subs indexEnv [] (Ref zero) in
              Some zero
            | None -> return None
          in
          ( I.ArrayPrimitive
              (Reduce
                 { arg; body; zero; d; itemPad; cellShape; associative; character; type' })
          , functions )
        | _ ->
          raise
            (Unreachable.Error
               (String.concat_lines
                  [ "reduce expected a stack of [IndexApp; TypeApp], got"
                  ; [%sexp_of: appStack] appStack |> Sexp.to_string_hum
                  ])))
     | Fold { character } ->
       (match primitive.appStack with
        | [ IndexApp [ Dimension d; Shape itemPad; Shape cellShape ]
          ; TypeApp [ Atom t; Array u ]
          ] ->
          (* Extract the arguments to the function. Note that the arguments
             differ depending on explicitZero *)
          let f, zero, arrayArg =
            match args with
            | [ f; zero; arrayArg ] -> f, zero, arrayArg
            | _ -> raise (Unreachable.Error "Fold expected three arguments")
          in
          let%bind _, functions = inlineArray subs indexEnv [] (Ref f) in
          let%bind func =
            match functions with
            | Empty ->
              raise
                (Unreachable.Error "func of fold should've returned at least one function")
            | One f -> return f
            | Multiple ->
              InlineState.err
                (String.concat_lines
                   [ "Could not determine what function is being passed to fold:"
                   ; [%sexp_of: E.termApplication] termApplication |> Sexp.to_string_hum
                   ])
          in
          let%bind fZeroArg = InlineState.createId "fold-zero-arg"
          and fArrayArg = InlineState.createId "fold-array-arg" in
          let body =
            E.TermApplication
              { func = Function.toExplicit func
              ; args =
                  [ { id = fZeroArg; type' = u }
                  ; { id = fArrayArg; type' = Arr { element = t; shape = cellShape } }
                  ]
              ; type' = { element = t; shape = cellShape }
              }
          in
          let%bind () =
            (* Need to check the value restriction on the body.*)
            match func with
            | Lambda lambda ->
              (* Special case - just need to check to see if the body of the
                 lambda is a value. The term application around it isn't
                 technically a value, but it involves no computation since it
                 simply re-assigns variables *)
              assertValueRestriction lambda.lambda.body
            | Primitive _ -> assertValueRestriction body
          in
          let%bind body, bindings, functions =
            inlineBodyWithBindings
              subs
              indexEnv
              appStack
              body
              [ fZeroArg, Ref zero; fArrayArg, Ref arrayArg ]
          in
          let zeroBindings, arrayBindings =
            match bindings with
            | [ zeroBindings; arrayBindings ] -> zeroBindings, arrayBindings
            | [] | [ _ ] | _ :: _ :: _ :: _ ->
              raise (Unreachable.Error "bindings should have len 2")
          in
          (* If the fold's type is not-polymorphic, then the app stack
             has to be empty. Thus, there is a unique app stack. If it is
             polymorphic, the body must be a value because of the value
             restriction. In this case, there cannot be any type applications
             in the body, so the only possible type stack origin is on the result
             of the fold. Therefore, in both cases, the only app stack that
             can be used is the one applied to the result of the fold. Thus,
             the length has to be less than or equal to 1. This is important
             because it ensures that the number of monomorphizations of the args
             is contained.
          *)
          assert (Map.length zeroBindings <= 1);
          let canonicalAppStack = CanonicalAppStack.from appStack in
          let%map zeroArgBinding, zeroArgValue =
            match Map.find zeroBindings canonicalAppStack with
            | Some (binding, monoValue) -> return (binding, monoValue)
            | None ->
              let%map monoValue, _ = inlineArray subs indexEnv appStack (Ref zero)
              and binding = InlineState.createId "fold-zero-arg" in
              binding, monoValue
          in
          assert (Map.is_empty (Map.remove zeroBindings canonicalAppStack));
          let arrayArgs =
            arrayBindings
            |> Map.data
            |> List.map ~f:(fun (binding, value) : I.arg -> { binding; value })
          in
          let type' = inlineArrayTypeWithStack appStack (Arr type') in
          ( I.ArrayPrimitive
              (Fold
                 { zeroArg = { binding = zeroArgBinding; value = zeroArgValue }
                 ; arrayArgs
                 ; body
                 ; d
                 ; itemPad
                 ; cellShape
                 ; character
                 ; type'
                 })
          , functions )
        | _ ->
          raise
            (Unreachable.Error
               (String.concat_lines
                  [ "reduce expected a stack of [IndexApp; TypeApp], got"
                  ; [%sexp_of: appStack] appStack |> Sexp.to_string_hum
                  ])))
     | Index ->
       assert (List.length args = 2);
       (match primitive.appStack with
        | [ IndexApp [ Shape s; Shape cellShape; Dimension l ]; TypeApp [ Atom _ ] ] ->
          let%map arrayArg, _ = inlineArray subs indexEnv [] (Ref (List.nth_exn args 0))
          and indexArg, _ = inlineArray subs indexEnv [] (Ref (List.nth_exn args 1)) in
          ( I.ArrayPrimitive
              (Index
                 { arrayArg
                 ; indexArg
                 ; s
                 ; cellShape
                 ; l
                 ; type' = inlineArrayTypeWithStack appStack (Arr type')
                 })
          , FunctionSet.Empty )
        | _ ->
          raise
            (Unreachable.Error
               (String.concat_lines
                  [ "index expected a stack of [IndexApp; TypeApp], got"
                  ; [%sexp_of: appStack] appStack |> Sexp.to_string_hum
                  ])))
     | Scatter ->
       assert (List.length args = 2);
       (match primitive.appStack with
        | [ IndexApp [ Dimension dIn; Dimension dOut; Shape cellShape ]
          ; TypeApp [ Atom _ ]
          ] ->
          let%map valuesArg, _ = inlineArray subs indexEnv [] (Ref (List.nth_exn args 0))
          and indicesArg, _ = inlineArray subs indexEnv [] (Ref (List.nth_exn args 1)) in
          ( I.ArrayPrimitive
              (Scatter
                 { valuesArg
                 ; indicesArg
                 ; dIn
                 ; dOut
                 ; cellShape
                 ; type' = inlineArrayTypeWithStack appStack (Arr type')
                 })
          , FunctionSet.Empty )
        | _ ->
          raise
            (Unreachable.Error
               (String.concat_lines
                  [ "scatter expected a stack of [IndexApp; TypeApp], got"
                  ; [%sexp_of: appStack] appStack |> Sexp.to_string_hum
                  ])))
     | Replicate ->
       assert (List.length args = 1);
       (match primitive.appStack with
        | [ IndexApp [ Shape s; Shape cellShape ]; TypeApp [ Atom _ ] ] ->
          let%map valueArg, functions =
            inlineArray subs indexEnv [] (Ref (List.nth_exn args 0))
          and valueBinding = InlineState.createId "replicated-value" in
          let valueType = I.arrayType valueArg in
          let resultType =
            Nucleus.Type.{ element = valueType.element; shape = s @ cellShape }
          in
          ( I.ArrayPrimitive
              (Map
                 { args = [ { binding = valueBinding; value = valueArg } ]
                 ; iotaVar = None
                 ; frameShape = []
                 ; body =
                     ArrayPrimitive
                       (Map
                          { args = []
                          ; iotaVar = None
                          ; frameShape = s
                          ; body = Ref { id = valueBinding; type' = valueType }
                          ; type' = resultType
                          })
                 ; type' = resultType
                 })
          , functions )
        | _ ->
          raise
            (Unreachable.Error
               (String.concat_lines
                  [ "replicate expected a stack of [IndexApp; TypeApp], got"
                  ; [%sexp_of: appStack] appStack |> Sexp.to_string_hum
                  ]))))

(* Handle cases where there are values bound to variables and then used in some
   body of code *)
and inlineBodyWithBindings subs indexEnv appStack body bindings =
  let open InlineState.Let_syntax in
  (* Need to assert the value restriction in order to avoid duplicating
     computations. Note that in Remora, the value restriction also applies
     to function arguments. (SML doesn't have higher-rank polymorphism,
     so the case never arises in it.) *)
  let%bind () =
    bindings
    |> List.map ~f:(fun (_, value) -> assertValueRestriction value)
    |> InlineState.all_unit
  in
  (* Extend the environment by adding the variables to it with empty caches *)
  let%bind env = InlineState.getEnv () in
  let%bind () =
    InlineState.setEnv
      (List.fold bindings ~init:env ~f:(fun env (binding, value) ->
         Map.set
           env
           ~key:binding
           ~data:{ polyValue = value; cache = Map.empty (module CanonicalAppStack) }))
  in
  (* Inline the body using the extended env *)
  let%bind body, functions = inlineArray subs indexEnv appStack body in
  (* Inspect the cache entries for each variable. For each application stack
     used on a variable, create a new variable. *)
  let%bind env = InlineState.getEnv () in
  let inlinedBindings =
    List.map bindings ~f:(fun (binding, _) ->
      let cache = (Map.find_exn env binding).cache in
      let monoBindings =
        cache
        |> Map.map ~f:(fun { binding; monoValue; functions = _ } -> binding, monoValue)
      in
      monoBindings)
  in
  (* Remove the added bindings from the env *)
  let%map () =
    InlineState.setEnv
      (List.fold bindings ~init:env ~f:(fun env (binding, _) -> Map.remove env binding))
  in
  body, inlinedBindings, functions
;;

let inline (prog : Explicit.t) : (CompilerState.state, Nucleus.t, string) CompilerState.t =
  CompilerState.makeF ~f:(fun compilerState ->
    let%map.MResult state, (result, _) =
      InlineState.run
        (inlineArray
           (Map.empty (module Identifier))
           (Set.empty (module Identifier))
           []
           prog)
        { compilerState; env = Map.empty (module Identifier) }
    in
    state.compilerState, result)
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Explicit.t
  type output = Nucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "Inline and Monomorphize"

  let run input =
    CompilerPipeline.S.makeF ~f:(fun state ->
      match CompilerState.run (inline input) state with
      | MOk _ as expr -> expr
      | Errors errs ->
        Errors (NeList.map errs ~f:(fun err -> Source.{ elem = err; source = None })))
  ;;
end
