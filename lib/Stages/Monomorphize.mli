(* open! Base

val monomorphize
  :  Nucleus.t
  -> (CompilerState.state, MonoNucleus.t, string) CompilerState.t

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = Nucleus.t
    with type output = MonoNucleus.t
    with type error = (SB.source option, string) Source.annotate *)
