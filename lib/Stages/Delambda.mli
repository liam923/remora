open! Base

(** The Delambda compiler stage rips all lambdas out to the top level.
    As implemented, it does not deal with escaping references, because
    the Inline stage will disallow higher-order function passing,
    so references to not need to be captured by functions*)

val delambda : Nucleus.t -> (CompilerState.state, NolamNucleus.t, _) State.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Nucleus.t
    with type output = NolamNucleus.t
    with type error = (SB.source option, string) Source.annotate
