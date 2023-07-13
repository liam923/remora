open! Base

val defunctionalize : MonoNucleus.t -> DefunNucleus.t

module Stage (SB : Source.BuilderT) :
  Pipeline.Stage
    with type input = MonoNucleus.t
    with type output = DefunNucleus.t
    with type error = (SB.source option, string) Source.annotate
