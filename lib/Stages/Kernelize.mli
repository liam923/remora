open! Base

val kernelize : Nested.t -> (CompilerState.state, Corn.t, _) State.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Nested.t
    with type output = Corn.t
    with type error = (SB.source option, string) Source.annotate
