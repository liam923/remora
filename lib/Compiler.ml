open! Base

module type S = sig
  type source
  type error = (source option, string) Source.annotate

  val compiler : (string, string, error) Pipeline.t
  val compileStringToString : string -> (string, error) MResult.t
end

module Make (SB : Source.BuilderT) = struct
  type source = SB.source
  type error = (source option, string) Source.annotate

  module type CompilerStage = Pipeline.Stage with type error = error

  let compiler =
    Pipeline.(
      (module Parser.Stage (SB))
      @> (module TypeChecker.Stage (SB))
      @> (module Monomorphize.Stage (SB))
      @> (module MonoNucleus.ShowStage (SB))
      @> empty)
  ;;

  let compileStringToString input = Pipeline.exec compiler input
end

module Default = Make (Source.Builder)

module Unit = Make (struct
  type source = unit

  let make ~start:_ ~finish:_ = ()
  let merge () () = ()
  let between () () = ()
end)
