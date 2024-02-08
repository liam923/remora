open! Base

type 'a fusionResult =
  { result : 'a
  ; fusedAny : bool
  }

val fuse : Nested.t -> (CompilerState.state, Nested.t fusionResult, _) State.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Nested.t
    with type output = Nested.t
    with type error = (SB.source option, string) Source.annotate
