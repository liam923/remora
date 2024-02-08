open! Base

type 't reduceTupleResult =
  { res : 't
  ; droppedAny : bool
  }

val reduceTuples
  :  Nested.t
  -> (CompilerState.state, Nested.t reduceTupleResult, _) State.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Nested.t
    with type output = Nested.t
    with type error = (SB.source option, string) Source.annotate
