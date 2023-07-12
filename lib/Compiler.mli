open! Base

module type S = sig
  type source
  type error = (source, string) Source.annotate

  val compiler : (string, string, error) Pipeline.t
  val compileStringToString : string -> (string, error) MResult.t
end

module Make (SourceBuilder : Source.BuilderT) : S with type source = SourceBuilder.source
module Default : S with type source = Source.t
module Unit : S with type source = unit
