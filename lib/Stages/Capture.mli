open! Base

val annotateCaptures : Acorn.sansCaptures -> Acorn.withCaptures

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Acorn.sansCaptures
    with type output = Acorn.withCaptures
    with type error = (SB.source option, string) Source.annotate
