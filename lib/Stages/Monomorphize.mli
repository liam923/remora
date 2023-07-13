open! Base

val monomorphize : Nucleus.t -> (MonoNucleus.t, string) MResult.t

module Stage (SB : Source.BuilderT) :
  Pipeline.Stage
    with type input = Nucleus.t
    with type output = MonoNucleus.t
    with type error = (SB.source option, string) Source.annotate
