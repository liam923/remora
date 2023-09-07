open! Base

(** This compiler stage does the following optimizations:
    - Copy propogation
    - Delete unused variables
    - Inline variables only used once
    - Inline variables with constant value
    - Constant folding
    - Remove redundant maps
    - Hoist variables *)

val simplify : Nucleus.t -> (CompilerState.state, Nucleus.t, _) State.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Nucleus.t
    with type output = Nucleus.t
    with type error = (SB.source option, string) Source.annotate
