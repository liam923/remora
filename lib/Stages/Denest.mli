open! Base

val denest : Nucleus.t -> (CompilerState.state, Fused.t, _) State.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Nucleus.t
    with type output = Fused.t
    with type error = (SB.source option, string) Source.annotate
