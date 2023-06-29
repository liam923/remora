open! Base

type ('s, 't) annotate =
  { elem : 't
  ; source : 's
  }

type t =
  { start : Lexing.position
  ; finish : Lexing.position
  }

module type BuilderT = sig
  type source

  val make : start:Lexing.position -> finish:Lexing.position -> source
end

module Builder = struct
  type source = t

  let make ~start ~finish : source = { start; finish }
end
