open! Base

val showExpr : C.expr -> string
val printC : C.t -> string

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = C.t
    with type output = string
    with type error = (SB.source option, string) Source.annotate
