open! Base

let rec fuseAndSimplify (prog : Nested.t) : (CompilerState.state, Nested.t, _) State.t =
  let open State.Let_syntax in
  let%bind simplified = SimplifyNested.simplify prog in
  let%bind fusionResult = Fuse.fuse simplified in
  if fusionResult.fusedAny then fuseAndSimplify fusionResult.result else return simplified
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nested.t
  type output = Nested.t
  type error = (SB.source option, string) Source.annotate

  let name = "Fuse and Simplify"

  let run input =
    CompilerPipeline.S.make ~f:(fun inState -> State.run (fuseAndSimplify input) inState)
  ;;
end
