open! Base

(** This compiler stage makes all the maps that are implicitly
    included in function calls explicit. *)

val explicitize : Typed.t -> (CompilerState.state, Explicit.t, _) State.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Typed.t
    with type output = Explicit.t
    with type error = (SB.source option, string) Source.annotate
