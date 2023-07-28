open! Base

type ('s, 't) annotate =
  { elem : 't
  ; source : 's
  }
[@@deriving sexp_of]

val map : ('s, 't) annotate -> f:('t -> 'u) -> ('s, 'u) annotate
val unzip : ('s, 'a * 'b) annotate -> ('s, 'a) annotate * ('s, 'b) annotate

type t =
  { start : Lexing.position
  ; finish : Lexing.position
  }

module type BuilderT = sig
  type source

  val make : start:Lexing.position -> finish:Lexing.position -> source
  val merge : source -> source -> source
  val between : source -> source -> source
end

module Builder : BuilderT with type source = t
module UnitBuilder : BuilderT with type source = unit
