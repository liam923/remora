(* open! Base

val defunctionalize : MonoNucleus.t -> (CompilerState.state, DefunNucleus.t, _) State.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = MonoNucleus.t
    with type output = DefunNucleus.t
    with type error = (SB.source option, string) Source.annotate *)
