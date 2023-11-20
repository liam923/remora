open! Base

val alloc : Corn.t -> (Acorn.t, ('s option, string) Source.annotate) CompilerState.u

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Corn.t
    with type output = Acorn.t
    with type error = (SB.source option, string) Source.annotate
