open! Base

type state =
  { functions : NolamNucleus.func Map.M(Identifier).t
  ; functionCounter : int
  ; compilerState : CompilerState.state
  }

let delambda (expr : Nucleus.t) =
  let module DN = NolamNucleus.Expr in
  let module MN = Nucleus.Expr in
  let open State.Let_syntax in
  let rec defunctionalizeExpr = function
    | MN.Array array ->
      let%map array = defunctionalizeArray array in
      DN.Array array
    | MN.Atom atom ->
      let%map atom = defunctionalizeAtom atom in
      DN.Atom atom
  and defunctionalizeArray = function
    | MN.Ref { id; type' } -> return (DN.Ref { id; type' })
    | MN.Scalar { element; type' } ->
      let%map element = defunctionalizeAtom element in
      DN.Scalar { element; type' }
    | MN.Frame { dimensions; elements; type' } ->
      let%map elements = elements |> List.map ~f:defunctionalizeArray |> State.all in
      DN.Frame { dimensions; elements; type' }
    | MN.TermApplication { func; args; type' } ->
      let%map func = defunctionalizeArray func
      and args = args |> List.map ~f:defunctionalizeArray |> State.all in
      DN.TermApplication { func; args; type' }
    | MN.TypeApplication { tFunc; args; type' } ->
      let%map tFunc = defunctionalizeArray tFunc in
      DN.TypeApplication { tFunc; args; type' }
    | MN.IndexApplication { iFunc; args; type' } ->
      let%map iFunc = defunctionalizeArray iFunc in
      DN.IndexApplication { iFunc; args; type' }
    | MN.Unbox { indexBindings; valueBinding; box; body; type' } ->
      let%map box = defunctionalizeArray box
      and body = defunctionalizeArray body in
      DN.Unbox { indexBindings; valueBinding; box; body; type' }
    | MN.Let { binding; value; body; type' } ->
      let%map value = defunctionalizeArray value
      and body = defunctionalizeArray body in
      DN.Let { binding; value; body; type' }
    | MN.TupleLet { params; value; body; type' } ->
      let%map value = defunctionalizeArray value
      and body = defunctionalizeArray body in
      DN.TupleLet { params; value; body; type' }
  and defunctionalizeAtom = function
    | MN.TermLambda { params; body; type' } ->
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
      DN.FunctionRef { id; type' }
    | MN.TypeLambda { params; body; type' } ->
      let%map body = defunctionalizeArray body in
      DN.TypeLambda { params; body; type' }
    | MN.IndexLambda { params; body; type' } ->
      let%map body = defunctionalizeArray body in
      DN.IndexLambda { params; body; type' }
    | MN.Box { indices; body; bodyType; type' } ->
      let%map body = defunctionalizeArray body in
      DN.Box { indices; body; bodyType; type' }
    | MN.Tuple { elements; type' } ->
      let%map elements = elements |> List.map ~f:defunctionalizeAtom |> State.all in
      DN.Tuple { elements; type' }
    | MN.Literal literal -> return (DN.Literal literal)
    | MN.BuiltInFunction builtIn -> return (DN.BuiltInFunction builtIn)
  in
  State.make ~f:(fun compilerState ->
    let { functions; functionCounter = _; compilerState }, body =
      State.run
        (defunctionalizeExpr expr)
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
