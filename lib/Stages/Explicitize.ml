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
           ~setCounter:(fun s idCounter -> { s with idCounter }))
        state)
  ;;
end

(* Given an expression, if it is a function, return the parameter names.
   This function doesn't have to be perfect because this is simply to make
   variable names more readable when debugging *)
let rec funcParamNamesArray env : Typed.Expr.array -> string list option = function
  | Ref ref -> Map.find env ref.id |> Option.join
  | Scalar s -> funcParamNamesAtom env s.element
  | Frame f -> List.hd f.elements |> Option.bind ~f:(funcParamNamesArray env)
  | TermApplication _ -> None
  | TypeApplication app -> funcParamNamesArray env app.tFunc
  | IndexApplication app -> funcParamNamesArray env app.iFunc
  | Unbox unbox -> funcParamNamesArray env unbox.body
  | Lift lift -> funcParamNamesArray env lift.body
  | ReifyIndex _ -> None
  | Let l ->
    let env = Map.set env ~key:l.binding ~data:(funcParamNamesArray env l.value) in
    funcParamNamesArray env l.body
  | Primitive p ->
    (match p.name with
     | Func func ->
       Some
         (match func with
          | Add -> [ "+arg1"; "+arg2" ]
          | Sub -> [ "-arg1"; "-arg2" ]
          | Mul -> [ "*arg1"; "*arg2" ]
          | Div -> [ "/arg1"; "/arg2" ]
          | Equal -> [ "=arg1"; "=arg2" ]
          | Reduce { associative = _; explicitZero = true; character = _ } ->
            [ "reduce-f-arg"; "reduce-zero-arg"; "reduce-array-arg" ]
          | Reduce { associative = _; explicitZero = false; character = _ } ->
            [ "reduce-f-arg"; "reduce-array-arg" ]
          | Fold _ -> [ "fold-f-arg"; "fold-zero-arg"; "fold-array-arg" ]
          | Append -> [ "append-arg1"; "append-arg2" ]
          | Index -> [ "index-array"; "index-index" ]
          | Scatter -> [ "scatter-values"; "scatter-indices" ]
          | Replicate -> [ "replicate-value" ])
     | Val _ -> None)

and funcParamNamesAtom env : Typed.Expr.atom -> string list option = function
  | TermLambda lambda ->
    Some (List.map lambda.params ~f:(fun p -> Identifier.name p.binding))
  | TypeLambda lambda -> funcParamNamesArray env lambda.body
  | IndexLambda lambda -> funcParamNamesArray env lambda.body
  | Box box -> funcParamNamesArray env box.body
  | Literal _ -> None
;;

let rec explicitizeArray paramNamesEnv array
  : (CompilerState.state, Explicit.Expr.array, _) ExplicitState.t
  =
  let open ExplicitState.Let_syntax in
  let module T = Typed.Expr in
  let module E = Explicit.Expr in
  match array with
  | T.Ref { id; type' } -> ExplicitState.return (E.Ref { id; type' })
  | T.Scalar { element; type' } ->
    let%map element = explicitizeAtom paramNamesEnv element in
    E.Scalar { element; type' }
  | T.Frame { dimensions; elements; type' } ->
    let%map elements =
      elements |> List.map ~f:(explicitizeArray paramNamesEnv) |> ExplicitState.all
    in
    E.Frame { dimensions; elements; type' }
  | T.TermApplication termApplication ->
    explicitizeTermApplication paramNamesEnv termApplication
  | T.TypeApplication { tFunc; args; type' } ->
    let%map tFunc = explicitizeArray paramNamesEnv tFunc in
    E.TypeApplication { tFunc; args; type' }
  | T.IndexApplication { iFunc; args; type' } ->
    let%map iFunc = explicitizeArray paramNamesEnv iFunc in
    E.IndexApplication { iFunc; args; type' }
  | T.Unbox { indexBindings; valueBinding; box; body; type' } ->
    let%map boxes = explicitizeArray paramNamesEnv box
    and body = explicitizeArray paramNamesEnv body
    and boxBinding = ExplicitState.createId "box" in
    let getArrType exp =
      match E.arrayType exp with
      | Arr arr -> arr
      | ArrayRef _ -> raise Unreachable.default
    in
    let boxesType = getArrType boxes in
    let boxBindingRef =
      E.Ref { id = boxBinding; type' = Arr { element = boxesType.element; shape = [] } }
    in
    E.Map
      { args = [ { binding = boxBinding; value = boxes } ]
      ; frameShape = boxesType.shape
      ; body =
          IndexLet
            { indexArgs =
                List.mapi indexBindings ~f:(fun i (indexBinding, sort) ->
                  E.
                    { indexBinding
                    ; indexValue = E.FromBox { box = boxBindingRef; i }
                    ; sort
                    })
            ; body =
                Map
                  { args =
                      [ { binding = valueBinding
                        ; value =
                            BoxValue
                              { box = boxBindingRef
                              ; type' =
                                  (match E.arrayType boxBindingRef with
                                   | Arr
                                       { element =
                                           Sigma { parameters = _; body = Arr body }
                                       ; shape = _
                                       } -> body
                                   | _ -> raise Unreachable.default)
                              }
                        }
                      ]
                  ; frameShape = []
                  ; body
                  ; type' = E.arrayType body
                  }
            ; type' = E.arrayType body
            }
      ; type' = Arr type'
      }
  | T.Lift { indexBinding; indexValue; sort; frameShape; body; type' } ->
    let%map indexValue = explicitizeArray paramNamesEnv indexValue
    and body = explicitizeArray paramNamesEnv body
    and indexValueBinding = ExplicitState.createId "index-value" in
    let getArrType exp =
      match E.arrayType exp with
      | Arr arr -> arr
      | ArrayRef _ -> raise Unreachable.default
    in
    let box =
      E.Box
        { indices =
            [ (match sort with
               | Shape -> Shape [ ShapeRef indexBinding ]
               | Dim -> Dimension (Explicit.Index.dimensionRef indexBinding))
            ]
        ; bodyType = E.arrayType body
        ; body
        ; type' =
            { parameters = [ { binding = indexBinding; bound = sort } ]
            ; body = E.arrayType body
            }
        }
    in
    E.Map
      { args = [ { binding = indexValueBinding; value = indexValue } ]
      ; frameShape
      ; body =
          IndexLet
            { indexArgs =
                [ { indexBinding
                  ; indexValue =
                      Runtime
                        (Ref
                           { id = indexValueBinding
                           ; type' =
                               Arr
                                 { element = (getArrType indexValue).element; shape = [] }
                           })
                  ; sort
                  }
                ]
            ; body =
                Scalar { element = box; type' = { element = E.atomType box; shape = [] } }
            ; type' = Arr { element = E.atomType box; shape = [] }
            }
      ; type'
      }
  | T.ReifyIndex { index; type' } -> ExplicitState.return (E.ReifyIndex { index; type' })
  | T.Let { binding; value; body; type' } ->
    let extendedParamNamesEnv =
      Map.set paramNamesEnv ~key:binding ~data:(funcParamNamesArray paramNamesEnv value)
    in
    let%bind value = explicitizeArray paramNamesEnv value in
    let%map body = explicitizeArray extendedParamNamesEnv body in
    E.Map { body; args = [ { binding; value } ]; frameShape = []; type' }
  | T.Primitive { name; type' } -> return (E.Primitive { name; type' })

and explicitizeAtom paramNamesEnv atom
  : (CompilerState.state, Explicit.Expr.atom, _) ExplicitState.t
  =
  let open ExplicitState.Let_syntax in
  let module T = Typed.Expr in
  let module E = Explicit.Expr in
  match atom with
  | T.TermLambda { params; body; type' } ->
    let%map body = explicitizeArray paramNamesEnv body in
    E.TermLambda { params; body; type' }
  | T.TypeLambda { params; body; type' } ->
    let%map body = explicitizeArray paramNamesEnv body in
    E.TypeLambda { params; body; type' }
  | T.IndexLambda { params; body; type' } ->
    let%map body = explicitizeArray paramNamesEnv body in
    E.IndexLambda { params; body; type' }
  | T.Box { indices; body; bodyType; type' } ->
    let%map body = explicitizeArray paramNamesEnv body in
    E.Box { indices; body; bodyType; type' }
  | T.Literal literal -> return (E.Literal literal)

and explicitizeTermApplication
  paramNamesEnv
  ({ func; args; type' } : Typed.Expr.termApplication)
  : (CompilerState.state, Explicit.Expr.array, _) ExplicitState.t
  =
  let open ExplicitState.Let_syntax in
  let module T = Typed.Expr in
  let module E = Explicit.Expr in
  let funcArrType =
    match Typed.Expr.arrayType func with
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
      { value : Explicit.Expr.ref (* The argument's value *)
      ; type' : Explicit.Type.arr (* The type of the argument *)
      ; frame : Explicit.Index.shape (* The frame of the argument *)
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
    (type' : Typed.Type.arr)
    =
    let dropFrameFromArrType frame (arrType : Typed.Type.arr) =
      Typed.Type.
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
      let type' = Typed.Expr.arrayType arg in
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

let explicitize (prog : Typed.t) : (CompilerState.state, Explicit.t, _) State.t =
  let open ExplicitState.Let_syntax in
  let%map prog = explicitizeArray (Map.empty (module Identifier)) prog in
  prog
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Typed.t
  type output = Explicit.t
  type error = (SB.source option, string) Source.annotate

  let name = "Explicitize"

  let run input =
    CompilerPipeline.S.make ~f:(fun state -> State.run (explicitize input) state)
  ;;
end
