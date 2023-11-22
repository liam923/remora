open! Base

type 'a fusionResult =
  { result : 'a
  ; fusedAny : bool
  }

val fuse : Nested.t -> (CompilerState.state, Nested.t fusionResult, _) State.t
