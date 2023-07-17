(* open! Base

type state =
  { functions : DefunNucleus.func Map.M(Identifier).t
  ; functionCounter : int
  ; compilerState : CompilerState.state
  }

let defunctionalize (expr : MonoNucleus.t) =
  let module DN = DefunNucleus.Expr in
  let module MN = MonoNucleus.Expr in
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
    | MN.Arr { dimensions; elements; type' } ->
      let%map elements = elements |> List.map ~f:defunctionalizeAtom |> State.all in
      DN.Arr { dimensions; elements; type' }
    | MN.Frame { dimensions; arrays; type' } ->
      let%map arrays = arrays |> List.map ~f:defunctionalizeArray |> State.all in
      DN.Frame { dimensions; arrays; type' }
    | MN.TermApplication { func; args; type' } ->
      let%map func = defunctionalizeArray func
      and args = args |> List.map ~f:defunctionalizeArray |> State.all in
      DN.TermApplication { func; args; type' }
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
    | MN.Box { indices; body; bodyType; type' } ->
      let%map body = defunctionalizeArray body in
      DN.Box { indices; body; bodyType; type' }
    | MN.Tuple { elements; type' } ->
      let%map elements = elements |> List.map ~f:defunctionalizeAtom |> State.all in
      DN.Tuple { elements; type' }
    | MN.Literal literal -> return (DN.Literal literal)
  in
  State.make ~f:(fun compilerState ->
      let { functions; functionCounter = _; compilerState }, body =
        State.run
          (defunctionalizeExpr expr)
          { functions = Map.empty (module Identifier)
          ; functionCounter = 0
          ; compilerState
          }
      in
      compilerState, DefunNucleus.{ functions; body })
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = MonoNucleus.t
  type output = DefunNucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "Defunctionalize"

  let run input =
    CompilerPipeline.S.make ~f:(fun compilerState ->
        State.run (defunctionalize input) compilerState)
  ;;
end *)
