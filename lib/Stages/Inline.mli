open! Base

(** This compiler stage monomorphizes the program and makes all function calls
    (besides primitive ones) inline *)

val inline : Explicit.t -> (CompilerState.state, Nucleus.t, string) CompilerState.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Explicit.t
    with type output = Nucleus.t
    with type error = (SB.source option, string) Source.annotate
