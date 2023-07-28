(* open! Base

module Function = struct
  module T = struct
    open Nucleus

    type t =
      | BuiltIn of
          { funcName : Expr.builtInFunctionName
          ; arrayEnv : Type.array Map.M(String).t
          ; atomEnv : Type.atom Map.M(String).t
          ; shapeEnv : Index.shape Map.M(String).t
          ; dimensionEnv : Index.dimension Map.M(String).t
          ; type' : Type.array
          }
      | Lambda of Expr.termLambda
      | TypeLambda of
          { params : Kind.t param list
          ; body : t
          }
      | IndexLambda of
          { params : Sort.t param list
          ; body : t
          }
    [@@deriving sexp_of, compare, equal]

    let rec subTypes types = function
      | BuiltIn _ as builtIn -> builtIn
      | Lambda lambda -> Lambda (Substitute.subTypesIntoTermLambda types lambda)
      | TypeLambda { params; body } -> TypeLambda { params; body = subTypes types body }
      | IndexLambda { params; body } -> IndexLambda { params; body = subTypes types body }
    ;;

    let rec subIndices indices = function
      | BuiltIn _ as builtIn -> builtIn
      | Lambda lambda -> Lambda (Substitute.subIndicesIntoTermLambda indices lambda)
      | TypeLambda { params; body } ->
        TypeLambda { params; body = subIndices indices body }
      | IndexLambda { params; body } ->
        IndexLambda { params; body = subIndices indices body }
    ;;
  end

  include T
  include Comparator.Make (T)
end

module FunctionSet = struct
  type t =
    | Multiple
    | One of Function.t
    | Empty

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

  let map s ~f =
    match s with
    | Multiple -> Multiple
    | One s -> One (f s)
    | Empty -> Empty
  ;;
end

type env = FunctionSet.t Map.M(Identifier).t

type 't return =
  { value : 't
  ; functions : FunctionSet.t
  }

module InlineState = struct
  include CompilerState

  type 't u = (state, 't, string) t

  let createId name =
    make ~f:(fun state ->
      State.run
        (Identifier.create
           name
           ~getCounter:(fun (s : state) -> s.idCounter)
           ~setCounter:(fun _ idCounter -> CompilerState.{ idCounter }))
        state)
  ;;
end

let rec explicitizeArray (env : env) subs (array : Nucleus.Expr.array)
  : InlineNucleus.Expr.array return InlineState.u
  =
  let open CompilerState.Let_syntax in
  let module N = Nucleus.Expr in
  let module I = InlineNucleus.Expr in
  match array with
  | N.Ref { id; type' } ->
    let id = Map.find subs id |> Option.value ~default:id in
    let functions = Map.find env id |> Option.value ~default:Multiple in
    InlineState.return { value = I.Ref { id; type' }; functions }
  | N.Scalar { element; type' } ->
    let%map { value = element; functions } = explicitizeAtom env subs element in
    { value = I.Scalar { element; type' }; functions }
  | N.Frame { dimensions; elements; type' } ->
    let%map elementsWtihFunctions =
      elements |> List.map ~f:(explicitizeArray env subs) |> InlineState.all
    in
    let elements = List.map elementsWtihFunctions ~f:(fun e -> e.value) in
    let functionSets = List.map elementsWtihFunctions ~f:(fun e -> e.functions) in
    let functions = FunctionSet.merge functionSets in
    { value = I.Frame { dimensions; elements; type' }; functions }
  | N.TermApplication ({ func = funcExpr; args; type' } as call) ->
    (* Need to make maps explicit and inline functions *)
    let funcArrType =
      match Nucleus.Expr.arrayType funcExpr with
      | Arr arr -> arr
      | _ -> Unreachable.raiseStr "Expected an Arr type in function call position"
    in
    let funcType =
      match funcArrType.element with
      | Func funcType -> funcType
      | _ -> Unreachable.raiseStr "Expected a function type in function call position"
    in
    let%bind { value = _; functions } = explicitizeArray env subs funcExpr in
    let callStr () = [%sexp_of: N.termApplication] call |> Sexp.to_string_hum in
    let%bind func =
      match functions with
      | Multiple ->
        InlineState.returnF
          (MResult.err
             [%string
               "At function call %{callStr ()}, unable to determine what function is \
                being called"])
      | One func -> InlineState.return func
      | Empty ->
        Error.raise
          (Error.of_string
             "At least one function should be available, but got an empty set. There's \
              either a bug in Inline or a type error in the program")
    in
    (* Define type to contain info about each arg, as well as the array in the function call position *)
    let module CallComponent = struct
      type arg =
        { arg : InlineNucleus.Expr.array (* The argument's value *)
        ; argType : InlineNucleus.Type.arr (* The type of the argument *)
        ; frame : InlineNucleus.Index.shape (* The frame of the argument *)
        ; cellShape : InlineNucleus.Index.shape (* The cell shape *)
        ; name : string
            (* A name for the argument. If the function is a lambda, it is the parameter names. If not, it is generated *)
        ; index : int (* Whether the argument is first, second, third, etc. *)
        ; functions : FunctionSet.t
        }

      type t =
        | FunctionCall of { frame : InlineNucleus.Index.shape }
        | Arg of arg

      type boundArg =
        { binding : Identifier.t
        ; ref : I.array
        ; functions : FunctionSet.t
        ; index : int
        }

      let frame = function
        | FunctionCall fc -> fc.frame
        | Arg arg -> arg.frame
      ;;

      let makeComponents args =
        let paramNames =
          match func with
          | Lambda lambda ->
            List.map lambda.params ~f:(fun param -> Identifier.name param.binding)
          | _ -> List.init (List.length args) ~f:(fun i -> [%string "arg%{i#Int}"])
        in
        let%map args =
          List.zip_exn funcType.parameters args
          |> List.zip_exn paramNames
          |> List.zip_exn (List.init (List.length args) ~f:(fun i -> i))
          |> List.map ~f:(fun (index, (name, (param, arg))) ->
            let%map { value = arg; functions } = explicitizeArray env subs arg in
            let cellShape =
              match param with
              | Arr { element = _; shape } -> shape
              | ArrayRef _ -> Unreachable.raiseStr "Expected an Arr for a function param"
            in
            let argType =
              match I.arrayType arg with
              | Arr arr -> arr
              | ArrayRef _ ->
                Unreachable.raiseStr "Expected an Arr for a function argument"
            in
            let argShape = argType.shape in
            let getFrame ~argShape ~cellShape =
              List.rev argShape
              |> (fun l -> List.drop l (List.length cellShape))
              |> List.rev
            in
            let frame = getFrame ~argShape ~cellShape in
            Arg { arg; argType; frame; cellShape; name; index; functions })
          |> InlineState.all
        in
        FunctionCall { frame = funcArrType.shape } :: args
      ;;
    end
    in
    (* Group callComponents by their frame size, with the shortest frames coming first *)
    let%bind components =
      CallComponent.makeComponents args
      |> InlineState.map
           ~f:
             (List.sort_and_group ~compare:(fun a b ->
                Int.compare
                  (List.length (CallComponent.frame a))
                  (List.length (CallComponent.frame b))))
    in
    (* Recur over the grouped args, inserting the implicit maps, and inlining the function call *)
    let rec explicitizeMapsAndThenInline
      (components : CallComponent.t list list)
      (boundArgs : CallComponent.boundArg list)
      (type' : Nucleus.Type.arr)
      =
      let dropFrameFromArrType frame (arrType : Nucleus.Type.arr) =
        Nucleus.Type.
          { element = arrType.element
          ; shape = List.drop arrType.shape (List.length frame)
          }
      in
      let dropFrameFromFrame frameToDrop fullFrame =
        List.drop fullFrame (List.length frameToDrop)
      in
      match components with
      | [] ->
        (* There are no more implicit maps left to deal with, so now we
           inline the function call *)
        let boundArgs =
          List.sort boundArgs ~compare:(fun a b -> Int.compare a.index b.index)
        in
        (match func with
         | Lambda lambda ->
           let subs =
             List.zip_exn lambda.params boundArgs
             |> List.fold ~init:subs ~f:(fun subs (param, arg) ->
               Map.set subs ~key:param.binding ~data:arg.binding)
           in
           let env =
             List.fold boundArgs ~init:env ~f:(fun env arg ->
               Map.set env ~key:arg.binding ~data:arg.functions)
           in
           explicitizeArray env subs lambda.body
         | BuiltIn { funcName; arrayEnv; atomEnv; shapeEnv; dimensionEnv; type' } ->
           let makeBinop op =
             InlineState.return
               { value =
                   I.BuiltInFunctionCall
                     (Binop
                        { op
                        ; arg1 = (List.nth_exn boundArgs 0).ref
                        ; arg2 = (List.nth_exn boundArgs 0).ref
                        ; type'
                        })
               ; functions = Empty
               }
           in
           (match funcName with
            | Add -> makeBinop Add
            | Sub -> makeBinop Sub
            | Mul -> makeBinop Mul
            | Div -> makeBinop Div
            | Length ->
              let arg = List.nth_exn boundArgs 0 in
              let t = Map.find_exn atomEnv "t" in
              let d = Map.find_exn dimensionEnv "d" in
              let cellShape = Map.find_exn shapeEnv "@cell-shape" in
              InlineState.return
                { value =
                    I.BuiltInFunctionCall
                      (Length { arg = arg.ref; t; d; cellShape; type' })
                ; functions = Empty
                }
            | Reduce ->
              let arg = List.nth_exn boundArgs 0 in
              let t = Map.find_exn atomEnv "t" in
              let dSub1 = Map.find_exn dimensionEnv "d-1" in
              let itemPad = Map.find_exn shapeEnv "@item-pad" in
              let cellShape = Map.find_exn shapeEnv "@cell-shape" in
              InlineState.return
                { value =
                    I.BuiltInFunctionCall
                      (Reduce
                         { body = ()
                         ; args = [ arg.ref ]
                         ; t
                         ; dSub1
                         ; itemPad
                         ; cellShape
                         ; type'
                         })
                ; functions = Empty
                })
         | TypeLambda _ -> Unreachable.raise ()
         | IndexLambda _ -> Unreachable.raise ())
      | minFrameComponents :: restComponents ->
        (* minFrameArgs are the arguments whose frame is the shortest of all the arguments.
           The shape of the minFrameArgs can be safely mapped over across all
           arguments, and then the minFrameArgs will have been reduced to their cells. *)
        let minFrame = CallComponent.frame (List.hd_exn minFrameComponents) in
        let processComponent (component : CallComponent.t)
          : (I.mapArg option * CallComponent.t * CallComponent.boundArg option)
          InlineState.u
          =
          match component with
          | Arg arg ->
            let%map binding = InlineState.createId arg.name in
            let newArgType = dropFrameFromArrType minFrame arg.argType in
            let newArg =
              CallComponent.
                { arg = Ref { id = binding; type' = Arr newArgType }
                ; argType = newArgType
                ; frame = dropFrameFromFrame minFrame arg.frame
                ; cellShape = arg.cellShape
                ; name = arg.name
                ; index = arg.index
                ; functions = arg.functions
                }
            in
            let boundArg =
              CallComponent.
                { binding
                ; ref = I.Ref { id = binding; type' = Arr newArgType }
                ; index = arg.index
                ; functions = arg.functions
                }
            in
            let mapArg : I.mapArg = { binding; value = arg.arg } in
            Some mapArg, CallComponent.Arg newArg, Some boundArg
          | FunctionCall { frame } ->
            InlineState.return
              ( None
              , CallComponent.FunctionCall { frame = dropFrameFromFrame minFrame frame }
              , None )
        in
        let%bind minFrameComponentsProcessed =
          List.map minFrameComponents ~f:processComponent |> InlineState.all
        in
        let%bind restComponentsProcessed =
          List.map restComponents ~f:(fun cs ->
            cs |> List.map ~f:processComponent |> InlineState.all)
          |> InlineState.all
        in
        let mapArgs =
          List.map minFrameComponentsProcessed ~f:(fun (mapArg, _, _) -> mapArg)
          @ List.bind
              restComponentsProcessed
              ~f:(List.map ~f:(fun (mapArg, _, _) -> mapArg))
          |> List.filter_opt
        in
        let boundMinArgs =
          List.map minFrameComponentsProcessed ~f:(fun (_, _, boundArg) -> boundArg)
          |> List.filter_opt
        in
        let mapType = dropFrameFromArrType minFrame type' in
        let%map { value = mapBody; functions } =
          explicitizeMapsAndThenInline restComponents (boundMinArgs @ boundArgs) mapType
        in
        { value =
            I.BuiltInFunctionCall
              (I.Map
                 { body = mapBody
                 ; args = mapArgs
                 ; frameShape = minFrame
                 ; type' = Arr type'
                 })
        ; functions
        }
    in
    explicitizeMapsAndThenInline components [] type'
  | N.TypeApplication { tFunc; args; type' } ->
    let%map { value = tFunc; functions = rawFunctions } =
      explicitizeArray env subs tFunc
    in
    let functions =
      FunctionSet.map rawFunctions ~f:(function
        | TypeLambda { params; body } ->
          let subs =
            List.zip_exn params args
            |> List.fold
                 ~init:(Map.empty (module Identifier))
                 ~f:(fun acc (param, arg) -> Map.set acc ~key:param.binding ~data:arg)
          in
          Function.subTypes subs body
        | BuiltIn { funcName; arrayEnv; atomEnv; shapeEnv; dimensionEnv; type' } ->
          (match type' with
           | Arr { element = Forall { parameters; body }; shape = _ } ->
             let subs, arrayEnv, atomEnv =
               List.zip_exn parameters args
               |> List.fold
                    ~init:(Map.empty (module Identifier), arrayEnv, atomEnv)
                    ~f:(fun (subs, arrayEnv, atomEnv) (param, arg) ->
                      let subs = Map.set subs ~key:param.binding ~data:arg in
                      match arg with
                      | Array arg ->
                        let arrayEnv =
                          Map.set arrayEnv ~key:(Identifier.name param.binding) ~data:arg
                        in
                        subs, arrayEnv, atomEnv
                      | Atom arg ->
                        let atomEnv =
                          Map.set atomEnv ~key:(Identifier.name param.binding) ~data:arg
                        in
                        subs, arrayEnv, atomEnv)
             in
             let type' = Substitute.subTypesIntoArrayType subs body in
             BuiltIn { funcName; arrayEnv; atomEnv; shapeEnv; dimensionEnv; type' }
           | _ -> Unreachable.raiseStr "TypeLambda's body should have Forall type")
        | IndexLambda _ | Lambda _ -> Unreachable.raise ())
    in
    { value = I.TypeApplication { tFunc; args; type' }; functions }
  | N.IndexApplication { iFunc; args; type' } ->
    let%map { value = iFunc; functions = rawFunctions } =
      explicitizeArray env subs iFunc
    in
    let functions =
      FunctionSet.map rawFunctions ~f:(function
        | IndexLambda { params; body } ->
          let subs =
            List.zip_exn params args
            |> List.fold
                 ~init:(Map.empty (module Identifier))
                 ~f:(fun acc (param, arg) -> Map.set acc ~key:param.binding ~data:arg)
          in
          Function.subIndices subs body
        | BuiltIn { funcName; arrayEnv; atomEnv; shapeEnv; dimensionEnv; type' } ->
          (match type' with
           | Arr { element = Pi { parameters; body }; shape = _ } ->
             let subs, shapeEnv, dimensionEnv =
               List.zip_exn parameters args
               |> List.fold
                    ~init:(Map.empty (module Identifier), shapeEnv, dimensionEnv)
                    ~f:(fun (subs, shapeEnv, dimensionEnv) (param, arg) ->
                      let subs = Map.set subs ~key:param.binding ~data:arg in
                      match arg with
                      | Shape arg ->
                        let shapeEnv =
                          Map.set shapeEnv ~key:(Identifier.name param.binding) ~data:arg
                        in
                        subs, shapeEnv, dimensionEnv
                      | Dimension arg ->
                        let dimensionEnv =
                          Map.set
                            dimensionEnv
                            ~key:(Identifier.name param.binding)
                            ~data:arg
                        in
                        subs, shapeEnv, dimensionEnv)
             in
             let type' = Substitute.subIndicesIntoArrayType subs body in
             BuiltIn { funcName; arrayEnv; atomEnv; shapeEnv; dimensionEnv; type' }
           | _ -> Unreachable.raiseStr "TypeLambda's body should have Forall type")
        | TypeLambda _ | Lambda _ -> Unreachable.raise ())
    in
    { value = I.IndexApplication { iFunc; args; type' }; functions }
  | N.Unbox { indexBindings; valueBinding; box; body; type' } ->
    let%bind { value = box; functions = boxFunctions } = explicitizeArray env subs box in
    let env = Map.set env ~key:valueBinding ~data:boxFunctions in
    let%map { value = body; functions } = explicitizeArray env subs body in
    { value = I.Unbox { indexBindings; valueBinding; box; body; type' }; functions }
  | N.Let { binding; value; body; type' } ->
    let%bind { value; functions = valueFunctions } = explicitizeArray env subs value in
    let env = Map.set env ~key:binding ~data:valueFunctions in
    let%map { value = body; functions } = explicitizeArray env subs body in
    { value =
        I.BuiltInFunctionCall
          (Map { body; args = [ { binding; value } ]; frameShape = []; type' })
    ; functions
    }
  | N.TupleLet { params; value; body; type' } ->
    let%map { value; functions = _ } = explicitizeArray env subs value
    (* Copy propogation is not followed through tuple lets *)
    and { value = body; functions } = explicitizeArray env subs body in
    { value = I.TupleLet { params; value; body; type' }; functions }
  | N.BuiltInFunction { func; type' } ->
    return
      { value = I.BuiltInFunction { func; type' }
      ; functions =
          One
            (Function.BuiltIn
               { funcName = func
               ; arrayEnv = Map.empty (module String)
               ; atomEnv = Map.empty (module String)
               ; shapeEnv = Map.empty (module String)
               ; dimensionEnv = Map.empty (module String)
               ; type'
               })
      }

and explicitizeAtom (env : env) subs atom : InlineNucleus.Expr.atom return InlineState.u =
  let open CompilerState.Let_syntax in
  let module N = Nucleus.Expr in
  let module I = InlineNucleus.Expr in
  match atom with
  | N.TermLambda lambda ->
    return
      { value = I.TermLambda { type' = lambda.type' }; functions = One (Lambda lambda) }
  | N.TypeLambda { params; body; type' } ->
    let%map { value = body; functions = rawFunctions } = explicitizeArray env subs body in
    let functions =
      FunctionSet.map rawFunctions ~f:(fun body -> TypeLambda { params; body })
    in
    { value = I.TypeLambda { params; body; type' }; functions }
  | N.IndexLambda { params; body; type' } ->
    let%map { value = body; functions = rawFunctions } = explicitizeArray env subs body in
    let functions =
      FunctionSet.map rawFunctions ~f:(fun body -> IndexLambda { params; body })
    in
    { value = I.IndexLambda { params; body; type' }; functions }
  | N.Box _ -> Unimplemented.raise ()
  | N.Tuple { elements; type' } ->
    let%map elementsWithFunctions =
      elements |> List.map ~f:(explicitizeAtom env subs) |> InlineState.all
    in
    let elements = List.map elementsWithFunctions ~f:(fun e -> e.value) in
    { value = I.Tuple { elements; type' }; functions = Multiple }
  | N.Literal (IntLiteral i) ->
    return { value = I.Literal (IntLiteral i); functions = Empty }
  | N.Literal (CharacterLiteral c) ->
    return { value = I.Literal (CharacterLiteral c); functions = Empty }
;;

let explicitize (prog : Nucleus.t)
  : (CompilerState.state, InlineNucleus.t, string) CompilerState.t
  =
  let open InlineState.Let_syntax in
  let%map { value = prog; functions = _ } =
    explicitizeArray (Map.empty (module Identifier)) (Map.empty (module Identifier)) prog
  in
  prog
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nucleus.t
  type output = InlineNucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "Inline"

  let run input =
    CompilerPipeline.S.makeF ~f:(fun state ->
      match CompilerState.run (explicitize input) state with
      | MOk _ as expr -> expr
      | Errors errs ->
        Errors (NeList.map errs ~f:(fun err -> Source.{ elem = err; source = None })))
  ;;
end *)
