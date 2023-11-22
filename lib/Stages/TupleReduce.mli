open! Base

type 't reduceTupleResult =
  { res : 't
  ; droppedAny : bool
  }

val reduceTuples
  :  Nested.t
  -> (CompilerState.state, Nested.t reduceTupleResult, _) State.t
