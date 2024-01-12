open! Base

module type S = sig
  type source
  type error = (source option, string) Source.annotate

  val compiler : (CompilerState.state, string, string, error) CompilerPipeline.t
  val compileStringToString : string -> (string, error) MResult.t
  val showError : error -> string
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
      @> (module Nest.Stage (SB))
      @> (module FuseAndSimplify.Stage (SB))
      @> (module Kernelize.Stage (SB))
      @> (module Alloc.Stage (SB))
      (* @> (module Show.Passthrough.Stage (Acorn.SansCaptures) (SB)) *)
      @> (module Capture.Stage (SB))
      (* @> (module Show.Passthrough.Stage (Acorn.WithCaptures) (SB)) *)
      @> (module Codegen.Stage (SB))
      @> (module PrintC.Stage (SB))
      @> empty)
  ;;

  let compileStringToString input =
    CompilerPipeline.S.runA (CompilerPipeline.make compiler input) CompilerState.initial
  ;;

  let showError ({ elem = error; source } : error) =
    match Option.map source ~f:SB.show with
    | None | Some "" -> [%string "Error: %{error}"]
    | Some source -> [%string "Error (%{source}): %{error}"]
  ;;
end

module Default = Make (Source.Builder)
module Unit = Make (Source.UnitBuilder)
