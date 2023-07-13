open! Base

module type S = sig
  type source
  type error = string * source
  type 't result = ('t, error) MResult.t

  module type Parser = sig
    type t

    val parseTexps : source Texp.t NeList.t -> t result
    val parseString : string -> t result
    val parseFile : string -> t result
  end

  module IndexParser : Parser with type t = source Ast.Index.t
  module TypeParser : Parser with type t = source Ast.Type.t
  module ExprParser : Parser with type t = source Ast.Expr.t
  include module type of ExprParser
end

module Make (SourceBuilder : Source.BuilderT) : S with type source = SourceBuilder.source
module Default : S with type source = Source.t
module Unit : S with type source = unit

module Stage (SB : Source.BuilderT) :
  Pipeline.Stage
    with type input = string
    with type output = SB.source Ast.t
    with type error = (SB.source option, string) Source.annotate
