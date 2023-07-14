open! Base

type t [@@deriving compare, sexp, equal]
type comparator_witness

val comparator : (t, comparator_witness) Comparator.t
val name : t -> string

val create
  :  string
  -> getCounter:('s -> int)
  -> setCounter:('s -> int -> 's)
  -> ('s, t, _) State.t
