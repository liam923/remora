open! Base

val codegen : DeviceInfo.t -> Acorn.withCaptures -> (CompilerState.state, C.t, _) State.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Acorn.withCaptures
    with type output = C.t
    with type error = (SB.source option, string) Source.annotate
