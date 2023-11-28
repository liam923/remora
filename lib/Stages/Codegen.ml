open! Base

let codegen (_ : Acorn.withCaptures) : (CompilerState.state, C.t, _) State.t =
  raise Unimplemented.default
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Acorn.withCaptures
  type output = C.t
  type error = (SB.source option, string) Source.annotate

  let name = "Code Generation"

  let run input =
    CompilerPipeline.S.make ~f:(fun inState -> State.run (codegen input) inState)
  ;;
end
