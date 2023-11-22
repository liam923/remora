open! Base

(** This compiler stage does the following optimizations:
    - Copy propogation
    - Delete unused variables
    - Inline variables only used once
    - Inline variables with constant value
    - Constant folding
    - Remove redundant maps
    - Hoist variables *)

val simplify : Nested.t -> (CompilerState.state, Nested.t, _) State.t
