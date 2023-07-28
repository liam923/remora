open! Base

module type Basic = sig
  type t [@@deriving sexp_of]
end

module Stage (B : Basic) (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = B.t
    with type output = string
    with type error = (SB.source option, string) Source.annotate

module type CustomBasic = sig
  type t

  val to_string : t -> string
end

module CustomStage (B : CustomBasic) (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = B.t
    with type output = string
    with type error = (SB.source option, string) Source.annotate
