open! Base

(** This compiler stage does the following optimizations:
    - Copy propogation
    - Delete unused variables
    - Inline variables only used once
    - Inline variables with constant value
    - Constant folding
    - Remove redundant maps
    - Hoist variables *)

val simplify : InlineNucleus.t -> (CompilerState.state, InlineNucleus.t, _) State.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = InlineNucleus.t
    with type output = InlineNucleus.t
    with type error = (SB.source option, string) Source.annotate
