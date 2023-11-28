open! Base

module type S = sig
  type t [@@deriving compare, sexp, equal]
  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
  val name : t -> string
  val uniqueNum : t -> int
  val show : t -> string

  val create
    :  string
    -> getCounter:('s -> int)
    -> setCounter:('s -> int -> 's)
    -> ('s, t, _) State.t
end

module Make () = struct
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
  let uniqueNum { name = _; id } = id
  let show { name; id } = [%string "%{name}.%{id#Int}"]

  let create name ~getCounter ~setCounter =
    let open State.Let_syntax in
    let%bind state = State.get () in
    let id = getCounter state in
    let state = setCounter state (id + 1) in
    let%map () = State.set state in
    { name; id }
  ;;
end

include Make ()
