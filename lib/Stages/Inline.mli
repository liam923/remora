(* open! Base

   val explicitize
   :  Nucleus.t
   -> (CompilerState.state, InlineNucleus.t, string) CompilerState.t

   module Stage (SB : Source.BuilderT) :
   CompilerPipeline.Stage
   with type state = CompilerState.state
   with type input = Nucleus.t
   with type output = InlineNucleus.t
   with type error = (SB.source option, string) Source.annotate *)
