open! Base

val alloc : Corn.t -> (McCorn.t, _) CompilerState.u

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Corn.t
    with type output = McCorn.t
    with type error = (SB.source option, string) Source.annotate
