open! Base

val explicitize : Nucleus.t -> (CompilerState.state, ExplicitNucleus.t, _) State.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Nucleus.t
    with type output = ExplicitNucleus.t
    with type error = (SB.source option, string) Source.annotate
