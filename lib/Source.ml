open! Base

type ('s, 't) annotate =
  { elem : 't
  ; source : 's
  }

type t =
  { start : Lexing.position
  ; finish : Lexing.position
  }

let map { elem; source } ~f = { elem = f elem; source }
let unzip { elem = a, b; source } = { elem = a; source }, { elem = b; source }

module type BuilderT = sig
  type source

  val make : start:Lexing.position -> finish:Lexing.position -> source
  val merge : source -> source -> source
  val between : source -> source -> source
end

module Builder = struct
  type source = t

  let make ~start ~finish : source = { start; finish }
  let merge { start; finish = _ } { start = _; finish } = { start; finish }

  let between { start = _; finish = start } { start = finish; finish = _ } =
    { start; finish }
  ;;
end
