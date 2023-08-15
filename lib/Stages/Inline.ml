open! Base
module Index = Nucleus.Index
module Type = Nucleus.Type
module Canonical = Nucleus.Canonical

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
        { lambda : ExplicitNucleus.Expr.termLambda
        ; id : Identifier.t
        }
    | Primitive of
        { func : ExplicitNucleus.Expr.primitive
        ; appStack : appStack
        }
  [@@deriving sexp_of]

  let equal a b =
    match a, b with
    | Lambda a, Lambda b -> Identifier.equal a.id b.id
    | Primitive a, Primitive b -> ExplicitNucleus.Expr.equal_primitive a.func b.func
    | Lambda _, Primitive _ | Primitive _, Lambda _ -> false
  ;;

  let toExplicit =
    let module E = ExplicitNucleus.Expr in
    function
    | Lambda { lambda; id = _ } ->
      E.Scalar
        { element = TermLambda lambda
        ; type' = { element = Func lambda.type'; shape = [] }
        }
    | Primitive { func; appStack = _ } -> Primitive func
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
  ; monoValue : InlineNucleus.Expr.array
  ; functions : FunctionSet.t
  }
[@@deriving sexp_of]

type envEntry =
  { polyValue : ExplicitNucleus.Expr.array
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
             { s with compilerState = CompilerState.{ idCounter } }))
        state)
  ;;

  let err error = returnF (MResult.Errors (error :: []))
end

let rec inlineAtomType : Nucleus.Type.atom -> InlineNucleus.Type.atom = function
  | AtomRef _ ->
    raise (Unreachable.Error "There should be no type refs left after inlining")
  | Sigma { parameters; body } -> Sigma { parameters; body = inlineArrayType body }
  | Tuple _ -> raise (Unimplemented.Error "Tuples are not supported")
  | Literal CharacterLiteral -> Literal CharacterLiteral
  | Literal IntLiteral -> Literal IntLiteral
  | Literal BooleanLiteral -> Literal BooleanLiteral
  | Func _ -> Literal UnitLiteral
  | Forall _ -> Literal UnitLiteral
  | Pi _ -> Literal UnitLiteral

and inlineArrayType : Nucleus.Type.array -> InlineNucleus.Type.array = function
  | ArrayRef _ ->
    raise (Unreachable.Error "There should be no type refs left after inlining")
  | Arr { element; shape } -> { element = inlineAtomType element; shape }
;;

let rec inlineAtomTypeWithStack appStack : Nucleus.Type.atom -> InlineNucleus.Type.array
  = function
  | AtomRef _ ->
    raise (Unreachable.Error "There should be no type refs left after inlining")
  | Sigma sigma ->
    { element = Sigma (inlineSigmaTypeWithStack appStack sigma); shape = [] }
  | Tuple _ -> raise (Unimplemented.Error "Tuples are not supported")
  | Literal CharacterLiteral -> { element = Literal CharacterLiteral; shape = [] }
  | Literal IntLiteral -> { element = Literal IntLiteral; shape = [] }
  | Literal BooleanLiteral -> { element = Literal BooleanLiteral; shape = [] }
  | Func _ -> { element = Literal UnitLiteral; shape = [] }
  | Forall { parameters; body } ->
    (match appStack with
     | TypeApp types :: restStack ->
       let typeSubs =
         List.zip_exn parameters types
         |> List.fold
              ~init:(Map.empty (module Identifier))
              ~f:(fun subs (param, sub) -> Map.set subs ~key:param.binding ~data:sub)
       in
       let subbedBody = ExplicitNucleus.Substitute.Type.subTypesIntoArray typeSubs body in
       inlineArrayTypeWithStack restStack subbedBody
     | [] ->
       (* Empty stack means the Forall's value is not passed to a type
          application, so we can replace it with a unit *)
       { element = Literal UnitLiteral; shape = [] }
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
       let subbedBody =
         ExplicitNucleus.Substitute.Type.subIndicesIntoArray indexSubs body
       in
       inlineArrayTypeWithStack restStack subbedBody
     | [] ->
       (* Empty stack means the Pi's value is not passed to an index
          application, so we can replace it with a unit *)
       { element = Literal UnitLiteral; shape = [] }
     | _ :: _ as stack ->
       raise
         (Unreachable.Error
            (String.concat_lines
               [ "Expected index application at head of stack or empty stack, got stack:"
               ; [%sexp_of: appStack] stack |> Sexp.to_string_hum
               ])))

and inlineArrayTypeWithStack appStack : Nucleus.Type.array -> InlineNucleus.Type.array
  = function
  | ArrayRef _ ->
    raise (Unreachable.Error "There should be no type refs left after inlining")
  | Arr { element; shape = outerShape } ->
    let ({ element; shape = innerShape } : InlineNucleus.Type.array) =
      inlineAtomTypeWithStack appStack element
    in
    { element; shape = outerShape @ innerShape }

and inlineSigmaTypeWithStack appStack ({ parameters; body } : Nucleus.Type.sigma)
  : InlineNucleus.Type.sigma
  =
  { parameters; body = inlineArrayTypeWithStack appStack body }
;;

let assertValueRestriction value =
  let isPolymorphicType =
    let open ExplicitNucleus.Type in
    let rec isPolymorphicArray = function
      | ArrayRef _ -> false
      | Arr arr -> isPolymorphicAtom arr.element
    and isPolymorphicAtom = function
      | AtomRef _ -> false
      | Func _ -> false
      | Forall _ -> true
      | Pi _ -> true
      | Sigma sigma -> isPolymorphicArray sigma.body
      | Tuple elements -> List.exists elements ~f:isPolymorphicAtom
      | Literal IntLiteral -> false
      | Literal CharacterLiteral -> false
      | Literal BooleanLiteral -> false
    in
    isPolymorphicArray
  in
  let isValue =
    let open ExplicitNucleus.Expr in
    let rec isValueArray = function
      | Ref _ -> true
      | Scalar scalar -> isValueAtom scalar.element
      | Frame frame -> List.for_all frame.elements ~f:isValueArray
      | TermApplication _ -> false
      | TypeApplication _ -> false
      | IndexApplication _ -> false
      | Unbox _ -> false
      | TupleLet _ -> false
      | Primitive _ -> true
      | Map _ -> false
    and isValueAtom = function
      | TermLambda _ -> true
      | TypeLambda _ -> true
      | IndexLambda _ -> true
      | Box box -> isValueArray box.body
      | Tuple tuple -> List.for_all tuple.elements ~f:isValueAtom
      | Literal (IntLiteral _) -> true
      | Literal (CharacterLiteral _) -> true
      | Literal (BooleanLiteral _) -> true
    in
    isValueArray
  in
  if isPolymorphicType (ExplicitNucleus.Expr.arrayType value) && not (isValue value)
  then
    InlineState.err
      (String.concat_lines
         [ "Polymorphic variables and function arguments must be a value type, got \
            not-value:"
         ; [%sexp_of: ExplicitNucleus.Expr.array] value |> Sexp.to_string_hum
         ])
  else InlineState.return ()
;;

let scalar atom =
  InlineNucleus.Expr.(
    Scalar { element = atom; type' = { element = atomType atom; shape = [] } })
;;

let rec inlineArray subs (appStack : appStack) (array : ExplicitNucleus.Expr.array)
  : (InlineNucleus.Expr.array * FunctionSet.t) InlineState.u
  =
  let module E = ExplicitNucleus.Expr in
  let module I = InlineNucleus.Expr in
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
       let%bind monoValue, functions = inlineArray subs appStack polyValue
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
  | Scalar { element; type' = _ } -> inlineAtom subs appStack element
  | Frame { dimensions; elements; type' } ->
    let%map elements, functions =
      elements
      |> List.map ~f:(inlineArray subs appStack)
      |> InlineState.all
      |> InlineState.unzip
    in
    let functions = FunctionSet.merge functions in
    ( I.Frame
        { dimensions; elements; type' = inlineArrayTypeWithStack appStack (Arr type') }
    , functions )
  | TermApplication termApplication -> inlineTermApplication subs appStack termApplication
  | TypeApplication { tFunc; args; type' = _ } ->
    inlineArray subs (TypeApp args :: appStack) tFunc
  | IndexApplication { iFunc; args; type' = _ } ->
    inlineArray subs (IndexApp args :: appStack) iFunc
  | Unbox { indexBindings; valueBinding; box; body; type' } ->
    let args = [ valueBinding, box ] in
    let%map body, args, functions = inlineBodyWithBindings subs appStack body args in
    let boxBindings =
      args
      |> List.map ~f:(fun bindings ->
        Map.to_alist bindings
        |> List.map ~f:(fun (_, (binding, box)) : I.unboxBinding -> { binding; box }))
      |> List.join
    in
    ( I.Unbox
        { indexBindings
        ; boxBindings
        ; body
        ; type' = inlineArrayTypeWithStack appStack (Arr type')
        }
    , functions )
  | TupleLet _ -> raise (Unimplemented.Error "Tuples are not supported")
  | Primitive primitive ->
    return
      ( scalar (I.Literal UnitLiteral)
      , FunctionSet.One (Primitive { func = primitive; appStack }) )
  | Map { args; body; frameShape; type' } ->
    let args = List.map args ~f:(fun { binding; value } -> binding, value) in
    let%map body, args, functions = inlineBodyWithBindings subs appStack body args in
    let args =
      args
      |> List.map ~f:(fun bindings ->
        bindings
        |> Map.to_alist
        |> List.map ~f:(fun (_, (binding, value)) : I.mapArg -> { binding; value }))
      |> List.join
    in
    ( I.IntrinsicCall
        (Map { args; body; frameShape; type' = inlineArrayTypeWithStack appStack type' })
    , functions )

and inlineAtom subs (appStack : appStack) (atom : ExplicitNucleus.Expr.atom)
  : (InlineNucleus.Expr.array * FunctionSet.t) InlineState.u
  =
  let module E = ExplicitNucleus.Expr in
  let module I = InlineNucleus.Expr in
  let open InlineState.Let_syntax in
  match atom with
  | TermLambda lambda ->
    (* Since all function calls gget inlined, the value of the lambda is
       never used, so it can be safely replaced with unit *)
    let%map id = InlineState.createId "lambda" in
    scalar (I.Literal UnitLiteral), FunctionSet.One (Lambda { lambda; id })
  | TypeLambda { params; body; type' = _ } ->
    (match appStack with
     | TypeApp types :: restStack ->
       let typeSubs =
         List.zip_exn params types
         |> List.fold
              ~init:(Map.empty (module Identifier))
              ~f:(fun subs (param, sub) -> Map.set subs ~key:param.binding ~data:sub)
       in
       let subbedBody = ExplicitNucleus.Substitute.Expr.subTypesIntoArray typeSubs body in
       inlineArray subs restStack subbedBody
     | [] ->
       (* Empty stack means the type lambda's value is not passed to a type
          application, so we can replace it with a unit *)
       return (scalar (I.Literal UnitLiteral), FunctionSet.Empty)
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
       let subbedBody =
         ExplicitNucleus.Substitute.Expr.subIndicesIntoArray indexSubs body
       in
       inlineArray subs restStack subbedBody
     | [] ->
       (* Empty stack means the index lambda's value is not passed to an index
          application, so we can replace it with a unit *)
       return (scalar (I.Literal UnitLiteral), FunctionSet.Empty)
     | _ as stack ->
       raise
         (Unreachable.Error
            (String.concat_lines
               [ "Expected index application at head of stack, got stack:"
               ; [%sexp_of: appStack] stack |> Sexp.to_string_hum
               ])))
  | Box { indices; body; bodyType; type' } ->
    let%map body, functions = inlineArray subs appStack body in
    ( scalar
        (I.Box
           { indices
           ; body
           ; bodyType = inlineArrayTypeWithStack appStack bodyType
           ; type' = inlineSigmaTypeWithStack appStack type'
           })
    , functions )
  | Tuple _ -> raise (Unimplemented.Error "Tuples are not supported")
  | Literal (CharacterLiteral c) ->
    return (scalar (I.Literal (CharacterLiteral c)), FunctionSet.Empty)
  | Literal (IntLiteral i) -> return (scalar (I.Literal (IntLiteral i)), FunctionSet.Empty)
  | Literal (BooleanLiteral b) ->
    return (scalar (I.Literal (BooleanLiteral b)), FunctionSet.Empty)

and inlineTermApplication subs appStack termApplication =
  let module E = ExplicitNucleus.Expr in
  let module I = InlineNucleus.Expr in
  let open InlineState.Let_syntax in
  let ({ func; args; type' } : E.termApplication) = termApplication in
  let args =
    List.map args ~f:(fun ({ id; type' } : E.ref) ->
      let id = Map.find subs id |> Option.value ~default:id in
      ({ id; type' } : E.ref))
  in
  let%bind _, functions = inlineArray subs [] func in
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
  | Lambda { lambda = { params; body; type' = _ }; id = _ } ->
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
    inlineArray subs appStack body
  | Primitive primitive ->
    let binop op =
      assert (List.length args = 2);
      let%map args, _ =
        args
        |> List.map ~f:(fun arg -> inlineArray subs [] (Ref arg))
        |> InlineState.all
        |> InlineState.unzip
      in
      ( I.PrimitiveCall { op; args; type' = inlineArrayTypeWithStack appStack (Arr type') }
      , FunctionSet.Empty )
    in
    (match primitive.func.func with
     | Add -> binop Add
     | Sub -> binop Sub
     | Mul -> binop Mul
     | Div -> binop Div
     | Length ->
       assert (List.length args = 1);
       (match primitive.appStack with
        | [ IndexApp [ Dimension d; Shape cellShape ]; TypeApp [ Atom t ] ] ->
          let%map arg, _ = inlineArray subs [] (Ref (List.hd_exn args)) in
          ( I.IntrinsicCall
              (Length
                 { arg
                 ; t = inlineAtomType t
                 ; d
                 ; cellShape
                 ; type' = inlineArrayTypeWithStack appStack (Arr type')
                 })
          , FunctionSet.Empty )
        | _ ->
          raise
            (Unreachable.Error
               (String.concat_lines
                  [ "length expected a stack of [IndexApp; TypeApp], got"
                  ; [%sexp_of: appStack] appStack |> Sexp.to_string_hum
                  ])))
     | Append ->
       assert (List.length args = 2);
       (match primitive.appStack with
        | [ IndexApp [ Dimension d1; Dimension d2; Shape cellShape ]; TypeApp [ Atom t ] ]
          ->
          let%map arg1, _ = inlineArray subs [] (Ref (List.nth_exn args 0))
          and arg2, _ = inlineArray subs [] (Ref (List.nth_exn args 1)) in
          ( I.IntrinsicCall
              (Append
                 { arg1
                 ; arg2
                 ; t = inlineAtomType t
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
     | Filter ->
       assert (List.length args = 2);
       (match primitive.appStack with
        | [ IndexApp [ Dimension d; Shape cellShape ]; TypeApp [ Atom t ] ] ->
          let%map array, _ = inlineArray subs [] (Ref (List.nth_exn args 0))
          and flags, _ = inlineArray subs [] (Ref (List.nth_exn args 1)) in
          ( I.IntrinsicCall
              (Filter
                 { array
                 ; flags
                 ; t = inlineAtomType t
                 ; d
                 ; cellShape
                 ; type' = inlineArrayTypeWithStack appStack (Arr type')
                 })
          , FunctionSet.Empty )
        | _ ->
          raise
            (Unreachable.Error
               (String.concat_lines
                  [ "filter expected a stack of [IndexApp; TypeApp], got"
                  ; [%sexp_of: appStack] appStack |> Sexp.to_string_hum
                  ])))
     | Reduce ->
       assert (List.length args = 2);
       (match primitive.appStack with
        | [ IndexApp [ Dimension dSub1; Shape itemPad; Shape cellShape ]
          ; TypeApp [ Atom t ]
          ] ->
          (match args with
           | [ f; arrayArg ] ->
             let%bind _, functions = inlineArray subs [] (Ref f) in
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
                      ; [%sexp_of: E.termApplication] termApplication
                        |> Sexp.to_string_hum
                      ])
             in
             let%bind fArg1 = InlineState.createId "reduceArg1"
             and fArg2 = InlineState.createId "reduceArg2" in
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
             let%bind body, bindings, functions =
               inlineBodyWithBindings
                 subs
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
             let bindings1, bindings2 =
               match bindings with
               | [ bindings1; bindings2 ] -> bindings1, bindings2
               | _ -> raise (Unreachable.Error "bindings should have len 2")
             in
             let%map args =
               appStacksToMonoValue
               |> Map.to_alist
               |> List.map ~f:(fun (appStack, monoValue) ->
                 let getBindingFrom argBindings argName =
                   match Map.find argBindings appStack with
                   | Some (binding, _) -> return binding
                   | None -> InlineState.createId argName
                 in
                 let%map firstBinding = getBindingFrom bindings1 "reduce-arg1"
                 and secondBinding = getBindingFrom bindings2 "reduce-arg2" in
                 I.{ firstBinding; secondBinding; value = monoValue })
               |> InlineState.all
             in
             let type' = inlineArrayTypeWithStack appStack (Arr type') in
             ( I.IntrinsicCall
                 (Reduce
                    { args; body; t = type'.element; dSub1; itemPad; cellShape; type' })
             , functions )
           | _ -> raise (Unreachable.Error "Reduce expected two arguments"))
        | _ ->
          raise
            (Unreachable.Error
               (String.concat_lines
                  [ "reduce expected a stack of [IndexApp; TypeApp], got"
                  ; [%sexp_of: appStack] appStack |> Sexp.to_string_hum
                  ])))
     | Scan ->
       assert (List.length args = 2);
       (match primitive.appStack with
        | [ IndexApp [ Dimension dSub1; Shape itemPad; Shape cellShape ]
          ; TypeApp [ Atom t ]
          ] ->
          (match args with
           | [ f; arrayArg ] ->
             let%bind _, functions = inlineArray subs [] (Ref f) in
             let%bind func =
               match functions with
               | Empty ->
                 raise
                   (Unreachable.Error
                      "func of scan should've returned at least one function")
               | One f -> return f
               | Multiple ->
                 InlineState.err
                   (String.concat_lines
                      [ "Could not determine what function is being passed to scan:"
                      ; [%sexp_of: E.termApplication] termApplication
                        |> Sexp.to_string_hum
                      ])
             in
             let%bind fArg1 = InlineState.createId "scan-arg1"
             and fArg2 = InlineState.createId "scan-arg2" in
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
             let%bind body, bindings, functions =
               inlineBodyWithBindings
                 subs
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
             let bindings1, bindings2 =
               match bindings with
               | [ bindings1; bindings2 ] -> bindings1, bindings2
               | _ -> raise (Unreachable.Error "bindings should have len 2")
             in
             let%map args =
               appStacksToMonoValue
               |> Map.to_alist
               |> List.map ~f:(fun (appStack, monoValue) ->
                 let getBindingFrom argBindings argName =
                   match Map.find argBindings appStack with
                   | Some (binding, _) -> return binding
                   | None -> InlineState.createId argName
                 in
                 let%map firstBinding = getBindingFrom bindings1 "scan-arg1"
                 and secondBinding = getBindingFrom bindings2 "scan-arg2" in
                 I.{ firstBinding; secondBinding; value = monoValue })
               |> InlineState.all
             in
             let type' = inlineArrayTypeWithStack appStack (Arr type') in
             ( I.IntrinsicCall
                 (Scan { args; body; t = type'.element; dSub1; itemPad; cellShape; type' })
             , functions )
           | _ -> raise (Unreachable.Error "Scan expected two arguments"))
        | _ ->
          raise
            (Unreachable.Error
               (String.concat_lines
                  [ "scan expected a stack of [IndexApp; TypeApp], got"
                  ; [%sexp_of: appStack] appStack |> Sexp.to_string_hum
                  ]))))

(* Handle cases where there values bound to variables and then used in some
   body of code *)
and inlineBodyWithBindings subs appStack body bindings =
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
  let%bind body, functions = inlineArray subs appStack body in
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

let inline (prog : ExplicitNucleus.t)
  : (CompilerState.state, InlineNucleus.t, string) CompilerState.t
  =
  CompilerState.makeF ~f:(fun compilerState ->
    let%map.MResult state, (result, _) =
      InlineState.run
        (inlineArray (Map.empty (module Identifier)) [] prog)
        { compilerState; env = Map.empty (module Identifier) }
    in
    state.compilerState, result)
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = ExplicitNucleus.t
  type output = InlineNucleus.t
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
