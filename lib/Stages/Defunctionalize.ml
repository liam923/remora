open! Base

type state =
  { functions : DefunNucleus.func Map.M(Identifier).t
  ; idCounter : int
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
      let module IdCreator = Identifier.Creator (State) in
      let%bind body = defunctionalizeArray body in
      let%bind { functions; idCounter } = State.get () in
      let name = [%string "fn_%{idCounter#Int}"] in
      let%bind id =
        IdCreator.create name ~updateCounter:(fun state ->
            { state with idCounter = state.idCounter + 1 }, state.idCounter)
      in
      let functions = Map.set functions ~key:id ~data:{ params; body; type' } in
      let%map () = State.modify ~f:(fun state -> { state with functions }) in
      DN.FunctionRef { id; type' }
    | MN.Box { indices; body; bodyType; type' } ->
      let%map body = defunctionalizeArray body in
      DN.Box { indices; body; bodyType; type' }
    | MN.Tuple { elements; type' } ->
      let%map elements = elements |> List.map ~f:defunctionalizeAtom |> State.all in
      DN.Tuple { elements; type' }
    | MN.Literal literal -> return (DN.Literal literal)
  in
  let { functions; idCounter = _ }, body =
    State.run
      (defunctionalizeExpr expr)
      { functions = Map.empty (module Identifier); idCounter = 1000000 }
    (* TODO!!!!!! *)
  in
  DefunNucleus.{ functions; body }
;;

module Stage (SB : Source.BuilderT) = struct
  type input = MonoNucleus.t
  type output = DefunNucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "Defunctionalize"
  let run input = MResult.MOk (defunctionalize input)
end
