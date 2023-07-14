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

let create name ~getCounter ~setCounter =
  let open State.Let_syntax in
  let%bind state = State.get () in
  let id = getCounter state in
  let state = setCounter state (id + 1) in
  let%map () = State.set state in
  { name; id }
;;
