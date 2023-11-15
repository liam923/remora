open! Base

val fuseAndSimplify : Nested.t -> (CompilerState.state, Nested.t, _) State.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Nested.t
    with type output = Nested.t
    with type error = (SB.source option, string) Source.annotate
