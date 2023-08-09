open! Base

module ExplicitState = struct
  include State

  type state = CompilerState.state

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

(* Given an expression, if it is a function, return the parameter names.
   This function doesn't have to be perfect because this is simply to make
   variable names more readable when debugging *)
let rec funcParamNamesArray env : Nucleus.Expr.array -> string list option = function
  | Ref ref -> Map.find env ref.id |> Option.join
  | Scalar s -> funcParamNamesAtom env s.element
  | Frame f -> List.hd f.elements |> Option.bind ~f:(funcParamNamesArray env)
  | TermApplication _ -> None
  | TypeApplication app -> funcParamNamesArray env app.tFunc
  | IndexApplication app -> funcParamNamesArray env app.iFunc
  | Unbox unbox -> funcParamNamesArray env unbox.body
  | Let l ->
    let env = Map.set env ~key:l.binding ~data:(funcParamNamesArray env l.value) in
    funcParamNamesArray env l.body
  | TupleLet l -> funcParamNamesArray env l.body
  | Primitive p ->
    Some
      (match p.func with
       | Reduce -> [ "reduceArg1"; "reduceArg2" ]
       | Add -> [ "+arg1"; "+arg2" ]
       | Sub -> [ "-arg1"; "-arg2" ]
       | Mul -> [ "*arg1"; "*arg2" ]
       | Div -> [ "/arg1"; "/arg2" ]
       | Length -> [ "lengthArg" ])

and funcParamNamesAtom env : Nucleus.Expr.atom -> string list option = function
  | TermLambda lambda ->
    Some (List.map lambda.params ~f:(fun p -> Identifier.name p.binding))
  | TypeLambda lambda -> funcParamNamesArray env lambda.body
  | IndexLambda lambda -> funcParamNamesArray env lambda.body
  | Box box -> funcParamNamesArray env box.body
  | Tuple _ -> None
  | Literal _ -> None
;;

let rec explicitizeArray paramNamesEnv array
  : (CompilerState.state, ExplicitNucleus.Expr.array, _) ExplicitState.t
  =
  let open ExplicitState.Let_syntax in
  let module N = Nucleus.Expr in
  let module E = ExplicitNucleus.Expr in
  match array with
  | N.Ref { id; type' } -> ExplicitState.return (E.Ref { id; type' })
  | N.Scalar { element; type' } ->
    let%map element = explicitizeAtom paramNamesEnv element in
    E.Scalar { element; type' }
  | N.Frame { dimensions; elements; type' } ->
    let%map elements =
      elements |> List.map ~f:(explicitizeArray paramNamesEnv) |> ExplicitState.all
    in
    E.Frame { dimensions; elements; type' }
  | N.TermApplication termApplication ->
    explicitizeTermApplication paramNamesEnv termApplication
  | N.TypeApplication { tFunc; args; type' } ->
    let%map tFunc = explicitizeArray paramNamesEnv tFunc in
    E.TypeApplication { tFunc; args; type' }
  | N.IndexApplication { iFunc; args; type' } ->
    let%map iFunc = explicitizeArray paramNamesEnv iFunc in
    E.IndexApplication { iFunc; args; type' }
  | N.Unbox { indexBindings; valueBinding; box; body; type' } ->
    let%map box = explicitizeArray paramNamesEnv box
    and body = explicitizeArray paramNamesEnv body in
    E.Unbox { indexBindings; valueBinding; box; body; type' }
  | N.Let { binding; value; body; type' } ->
    let extendedParamNamesEnv =
      Map.set paramNamesEnv ~key:binding ~data:(funcParamNamesArray paramNamesEnv value)
    in
    let%bind value = explicitizeArray paramNamesEnv value in
    let%map body = explicitizeArray extendedParamNamesEnv body in
    E.Map { body; args = [ { binding; value } ]; frameShape = []; type' }
  | N.TupleLet { params; value; body; type' } ->
    let%map value = explicitizeArray paramNamesEnv value
    (* Copy propogation is not followed through tuple lets *)
    and body = explicitizeArray paramNamesEnv body in
    E.TupleLet { params; value; body; type' }
  | N.Primitive { func; type' } -> return (E.Primitive { func; type' })

and explicitizeAtom paramNamesEnv atom
  : (CompilerState.state, ExplicitNucleus.Expr.atom, _) ExplicitState.t
  =
  let open ExplicitState.Let_syntax in
  let module N = Nucleus.Expr in
  let module E = ExplicitNucleus.Expr in
  match atom with
  | N.TermLambda { params; body; type' } ->
    let%map body = explicitizeArray paramNamesEnv body in
    E.TermLambda { params; body; type' }
  | N.TypeLambda { params; body; type' } ->
    let%map body = explicitizeArray paramNamesEnv body in
    E.TypeLambda { params; body; type' }
  | N.IndexLambda { params; body; type' } ->
    let%map body = explicitizeArray paramNamesEnv body in
    E.IndexLambda { params; body; type' }
  | N.Box { indices; body; bodyType; type' } ->
    let%map body = explicitizeArray paramNamesEnv body in
    E.Box { indices; body; bodyType; type' }
  | N.Tuple { elements; type' } ->
    let%map elements =
      elements |> List.map ~f:(explicitizeAtom paramNamesEnv) |> ExplicitState.all
    in
    E.Tuple { elements; type' }
  | N.Literal literal -> return (E.Literal literal)

and explicitizeTermApplication
  paramNamesEnv
  ({ func; args; type' } : Nucleus.Expr.termApplication)
  : (CompilerState.state, ExplicitNucleus.Expr.array, _) ExplicitState.t
  =
  let open ExplicitState.Let_syntax in
  let module N = Nucleus.Expr in
  let module E = ExplicitNucleus.Expr in
  let funcArrType =
    match Nucleus.Expr.arrayType func with
    | Arr arr -> arr
    | _ -> raise (Unreachable.Error "Expected an Arr type in function call position")
  in
  let funcType =
    match funcArrType.element with
    | Func funcType -> funcType
    | _ -> raise (Unreachable.Error "Expected a function type in function call position")
  in
  let paramNames =
    Option.value_or_thunk (funcParamNamesArray paramNamesEnv func) ~default:(fun () ->
      List.mapi funcType.parameters ~f:(fun i _ -> [%string "arg%{i#Int}"]))
  in
  (* Define type to contain info about each arg, as well as the array in the function call position *)
  let module CallComponent = struct
    type t =
      { value : ExplicitNucleus.Expr.ref (* The argument's value *)
      ; type' : ExplicitNucleus.Type.arr (* The type of the argument *)
      ; frame : ExplicitNucleus.Index.shape (* The frame of the argument *)
      ; index : int
          (* Whether the argument is first, second, third, etc. -1 Correspondsto the function call position *)
      ; name : string
      }
  end
  in
  (* Recur over the grouped args, inserting the implicit maps *)
  let rec explicitizeMaps
    (componentsToMap : CallComponent.t list list)
    (mappedComponents : CallComponent.t list)
    (type' : Nucleus.Type.arr)
    =
    let dropFrameFromArrType frame (arrType : Nucleus.Type.arr) =
      Nucleus.Type.
        { element = arrType.element; shape = List.drop arrType.shape (List.length frame) }
    in
    let dropFrameFromFrame frameToDrop fullFrame =
      List.drop fullFrame (List.length frameToDrop)
    in
    match componentsToMap with
    | [] ->
      (* There are no more implicit maps left to deal with, so now we
         inline the function call *)
      let components =
        List.sort mappedComponents ~compare:(fun a b -> Int.compare a.index b.index)
      in
      (match components with
       | func :: args ->
         assert (func.index = -1);
         assert (List.is_sorted args ~compare:(fun a b -> Int.compare a.index b.index));
         assert (List.length args = List.length funcType.parameters);
         let func = func.value in
         let args = List.map args ~f:(fun arg -> arg.value) in
         return (E.TermApplication { func = Ref func; args; type' })
       | [] -> raise Unreachable.default)
    | minFrameComponents :: restComponents ->
      (* minFrameArgs are the arguments whose frame is the shortest of all the arguments.
         The shape of the minFrameArgs can be safely mapped over across all
         arguments, and then the minFrameArgs will have been reduced to their cells. *)
      let minFrame = (List.hd_exn minFrameComponents).frame in
      let processComponentAndGetMapArg (component : CallComponent.t) =
        let%map binding = ExplicitState.createId component.name in
        let newComponentType = dropFrameFromArrType minFrame component.type' in
        let newComponent =
          CallComponent.
            { value = { id = binding; type' = Arr newComponentType }
            ; type' = newComponentType
            ; frame = dropFrameFromFrame minFrame component.frame
            ; name = component.name
            ; index = component.index
            }
        in
        let mapArg : E.mapArg = { binding; value = Ref component.value } in
        mapArg, newComponent
      in
      let%bind minFrameComponentsProcessedWithMapArgs =
        List.map minFrameComponents ~f:processComponentAndGetMapArg |> ExplicitState.all
      and restComponentsProcessedWithMapArgs =
        List.map restComponents ~f:(fun cs ->
          cs |> List.map ~f:processComponentAndGetMapArg |> ExplicitState.all)
        |> ExplicitState.all
      in
      let mapArgs =
        List.map minFrameComponentsProcessedWithMapArgs ~f:(fun (mapArg, _) -> mapArg)
        @ List.bind
            restComponentsProcessedWithMapArgs
            ~f:(List.map ~f:(fun (mapArg, _) -> mapArg))
      in
      let minFrameComponentsProcessed =
        List.map minFrameComponentsProcessedWithMapArgs ~f:(fun (_, c) -> c)
      in
      let restComponentsProcessed =
        List.map restComponentsProcessedWithMapArgs ~f:(List.map ~f:(fun (_, c) -> c))
      in
      let mapType = dropFrameFromArrType minFrame type' in
      let%map mapBody =
        explicitizeMaps
          restComponentsProcessed
          (minFrameComponentsProcessed @ mappedComponents)
          mapType
      in
      E.Map { body = mapBody; args = mapArgs; frameShape = minFrame; type' = Arr type' }
  in
  (* Before calling explicitizeMaps, construct an outermost map that binds
     all the arguments to a varaible. *)
  let%bind argComponentsWithMapArgs =
    List.zip_exn funcType.parameters args
    |> List.zip_exn paramNames
    |> List.mapi ~f:(fun index (name, (param, arg)) ->
      let type' = Nucleus.Expr.arrayType arg in
      let%map arg = explicitizeArray paramNamesEnv arg
      and binding = ExplicitState.createId name in
      let ref : E.ref = { id = binding; type' } in
      let cellShape =
        match param with
        | Arr { element = _; shape } -> shape
        | ArrayRef _ -> raise (Unreachable.Error "Expected an Arr for a function param")
      in
      let argType =
        match E.arrayType arg with
        | Arr arr -> arr
        | ArrayRef _ ->
          raise (Unreachable.Error "Expected an Arr for a function argument")
      in
      let argShape = argType.shape in
      let frame =
        List.rev argShape |> (fun l -> List.drop l (List.length cellShape)) |> List.rev
      in
      let mapArg : E.mapArg = { binding; value = arg } in
      let comp = CallComponent.{ value = ref; type' = argType; frame; index; name } in
      mapArg, comp)
    |> ExplicitState.all
  and func = explicitizeArray paramNamesEnv func
  and funcBinding = ExplicitState.createId "f" in
  let functionComponent =
    CallComponent.
      { value = { id = funcBinding; type' = Arr funcArrType }
      ; type' = funcArrType
      ; frame = funcArrType.shape
      ; index = -1
      ; name = "f"
      }
  in
  let argComponents = List.map argComponentsWithMapArgs ~f:(fun (_, c) -> c) in
  let components = functionComponent :: argComponents in
  let mappedComponents = List.filter components ~f:(fun c -> List.is_empty c.frame) in
  let componentsToMap =
    List.filter components ~f:(fun c -> not (List.is_empty c.frame))
    |> List.sort_and_group ~compare:(fun (a : CallComponent.t) b ->
      Int.compare (List.length a.frame) (List.length b.frame))
  in
  let mapArgs : E.mapArg list =
    { binding = funcBinding; value = func }
    :: List.map argComponentsWithMapArgs ~f:(fun (ma, _) -> ma)
  in
  let%map mapBody = explicitizeMaps componentsToMap mappedComponents type' in
  E.Map { args = mapArgs; body = mapBody; frameShape = []; type' = Arr type' }
;;

let explicitize (prog : Nucleus.t) : (CompilerState.state, ExplicitNucleus.t, _) State.t =
  let open ExplicitState.Let_syntax in
  let%map prog = explicitizeArray (Map.empty (module Identifier)) prog in
  prog
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nucleus.t
  type output = ExplicitNucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "Explicitize"

  let run input =
    CompilerPipeline.S.make ~f:(fun state -> State.run (explicitize input) state)
  ;;
end
