open! Base

module type S = sig
  type source
  type error = (source option, string) Source.annotate

  val compiler : (CompilerState.state, string, string, error) CompilerPipeline.t
  val compileStringToString : string -> (string, error) MResult.t
end

module Make (SB : Source.BuilderT) = struct
  type source = SB.source
  type error = (source option, string) Source.annotate

  let compiler =
    CompilerPipeline.(
      (module Parse.Stage (SB))
      @> (module TypeCheckStage.M (SB))
      @> (module Explicitize.Stage (SB))
      @> (module Inline.Stage (SB))
      @> (module Simplify.Stage (SB))
      @> (module Nest.Stage (SB))
      @> (module Fuse.Stage (SB))
      @> (module SimplifyNested.Stage (SB))
      @> (module Kernelize.Stage (SB))
      @> (module Show.Stage (Corn) (SB))
      @> empty)
  ;;

  let compileStringToString input =
    CompilerPipeline.S.runA (CompilerPipeline.make compiler input) CompilerState.initial
  ;;
end

module Default = Make (Source.Builder)
module Unit = Make (Source.UnitBuilder)
