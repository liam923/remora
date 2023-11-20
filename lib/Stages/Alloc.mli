open! Base

val alloc
  :  Corn.t
  -> (Acorn.sansCaptures, ('s option, string) Source.annotate) CompilerState.u

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Corn.t
    with type output = Acorn.sansCaptures
    with type error = (SB.source option, string) Source.annotate
