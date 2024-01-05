open! Base

type ('s, 't) annotate =
  { elem : 't
  ; source : 's
  }

let sexp_of_annotate sexpOfSource sexpOfElem { elem; source } =
  if Sexp.equal (sexpOfSource source) (List [])
  then sexpOfElem elem
  else
    Sexp.List
      [ List [ Atom "elem"; sexpOfElem elem ]
      ; List [ Atom "source"; sexpOfSource source ]
      ]
;;

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
  val show : source -> string
end

module Builder = struct
  type source = t

  let make ~start ~finish : source = { start; finish }
  let merge { start; finish = _ } { start = _; finish } = { start; finish }

  let between { start = _; finish = start } { start = finish; finish = _ } =
    { start; finish }
  ;;

  let show { start; finish } =
    let showPos (pos : Lexing.position) =
      [%string "%{pos.pos_lnum#Int}:%{(pos.pos_cnum - pos.pos_bol)#Int}"]
    in
    [%string "%{showPos start} - %{showPos finish}"]
  ;;
end

module UnitBuilder = struct
  type source = unit

  let make ~start:_ ~finish:_ = ()
  let merge () () = ()
  let between () () = ()
  let show () = ""
end
