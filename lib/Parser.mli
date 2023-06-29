open! Base

module type S = sig
  type source
  type error = string * source
  type result = (source Ast.Untyped.Expr.t, error) MResult.t

  val parseTexps : source Texp.t Non_empty_list.t -> result
  val parseString : string -> result
  val parseFile : string -> result
end

module Make (SourceBuilder : Source.BuilderT) : S with type source = SourceBuilder.source
module Default : S with type source = Source.t
module Unit : S with type source = unit
