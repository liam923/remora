open! Base

type t [@@deriving compare, sexp, equal]
type comparator_witness

val comparator : (t, comparator_witness) Comparator.t
val name : t -> string

module Creator (S : StateT.S2) : sig
  val create : string -> updateCounter:('s -> 's * int) -> ('s, t, _) S.t
end
