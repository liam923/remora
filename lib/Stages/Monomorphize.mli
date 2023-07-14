open! Base

val monomorphize : Nucleus.t -> (MonoNucleus.t, string) MResult.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Nucleus.t
    with type output = MonoNucleus.t
    with type error = (SB.source option, string) Source.annotate
