open! Base

module T = struct
  type t =
    { name : string
    ; id : int
    }
  [@@deriving compare, sexp, equal]
end

include T
include Comparator.Make (T)

let name { name; id = _ } = name

module Creator (S : StateT.S2) = struct
  open S.Let_syntax

  let create name ~updateCounter =
    let%bind state = S.get () in
    let state, id = updateCounter state in
    let%map () = S.set state in
    { name; id }
  ;;
end
