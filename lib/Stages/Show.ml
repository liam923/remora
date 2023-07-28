open! Base

module type Basic = sig
  type t [@@deriving sexp_of]
end

module Stage (B : Basic) (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type error = (SB.source option, string) Source.annotate
  type input = B.t
  type output = string

  let name = "Show"
  let run input = CompilerPipeline.S.return (Sexp.to_string_hum ([%sexp_of: B.t] input))
end

module type CustomBasic = sig
  type t

  val to_string : t -> string
end

module CustomStage (B : CustomBasic) (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type error = (SB.source option, string) Source.annotate
  type input = B.t
  type output = string

  let name = "Show"
  let run input = CompilerPipeline.S.return (B.to_string input)
end
