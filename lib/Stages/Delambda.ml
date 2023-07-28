open! Base

type state =
  { functions : NolamNucleus.func Map.M(Identifier).t
  ; functionCounter : int
  ; compilerState : CompilerState.state
  }

let delambda (expr : Nucleus.t) =
  let module NN = NolamNucleus.Expr in
  let module LN = Nucleus.Expr in
  let open State.Let_syntax in
  let rec defunctionalizeArray = function
    | LN.Ref { id; type' } -> return (NN.Ref { id; type' })
    | LN.Scalar { element; type' } ->
      let%map element = defunctionalizeAtom element in
      NN.Scalar { element; type' }
    | LN.Frame { dimensions; elements; type' } ->
      let%map elements = elements |> List.map ~f:defunctionalizeArray |> State.all in
      NN.Frame { dimensions; elements; type' }
    | LN.TermApplication { func; args; type' } ->
      let%map func = defunctionalizeArray func
      and args = args |> List.map ~f:defunctionalizeArray |> State.all in
      NN.TermApplication { func; args; type' }
    | LN.TypeApplication { tFunc; args; type' } ->
      let%map tFunc = defunctionalizeArray tFunc in
      NN.TypeApplication { tFunc; args; type' }
    | LN.IndexApplication { iFunc; args; type' } ->
      let%map iFunc = defunctionalizeArray iFunc in
      NN.IndexApplication { iFunc; args; type' }
    | LN.Unbox { indexBindings; valueBinding; box; body; type' } ->
      let%map box = defunctionalizeArray box
      and body = defunctionalizeArray body in
      NN.Unbox { indexBindings; valueBinding; box; body; type' }
    | LN.Let { binding; value; body; type' } ->
      let%map value = defunctionalizeArray value
      and body = defunctionalizeArray body in
      NN.Let { binding; value; body; type' }
    | LN.TupleLet { params; value; body; type' } ->
      let%map value = defunctionalizeArray value
      and body = defunctionalizeArray body in
      NN.TupleLet { params; value; body; type' }
    | LN.BuiltInFunction builtIn -> return (NN.BuiltInFunction builtIn)
  and defunctionalizeAtom = function
    | LN.TermLambda { params; body; type' } ->
      let%bind body = defunctionalizeArray body in
      let%bind { functions; functionCounter; compilerState = _ } = State.get () in
      let name = [%string "fn_%{functionCounter#Int}"] in
      let%bind id =
        Identifier.create
          name
          ~getCounter:(fun s -> s.compilerState.idCounter)
          ~setCounter:(fun s idCounter -> { s with compilerState = { idCounter } })
      in
      let functions = Map.set functions ~key:id ~data:{ params; body; type' } in
      let functionCounter = functionCounter + 1 in
      let%map () =
        State.modify ~f:(fun state -> { state with functions; functionCounter })
      in
      NN.FunctionRef { id; type' }
    | LN.TypeLambda { params; body; type' } ->
      let%map body = defunctionalizeArray body in
      NN.TypeLambda { params; body; type' }
    | LN.IndexLambda { params; body; type' } ->
      let%map body = defunctionalizeArray body in
      NN.IndexLambda { params; body; type' }
    | LN.Box { indices; body; bodyType; type' } ->
      let%map body = defunctionalizeArray body in
      NN.Box { indices; body; bodyType; type' }
    | LN.Tuple { elements; type' } ->
      let%map elements = elements |> List.map ~f:defunctionalizeAtom |> State.all in
      NN.Tuple { elements; type' }
    | LN.Literal literal -> return (NN.Literal literal)
  in
  State.make ~f:(fun compilerState ->
    let { functions; functionCounter = _; compilerState }, body =
      State.run
        (defunctionalizeArray expr)
        { functions = Map.empty (module Identifier); functionCounter = 0; compilerState }
    in
    compilerState, NolamNucleus.{ functions; body })
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nucleus.t
  type output = NolamNucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "De-lambda"

  let run input =
    CompilerPipeline.S.make ~f:(fun compilerState ->
      State.run (delambda input) compilerState)
  ;;
end
