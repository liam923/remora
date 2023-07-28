open! Base

(** Inspired by Cats' StateT (from Scala).
    A monad that holds a state and a value, where the value is also a monad *)

module type S2 = sig
  (** m is the monad being wrapped *)
  type ('a, 'e) m

  (** t is the state monad *)
  type ('s, 'a, 'e) t

  include Monad.S3 with type ('a, 's, 'e) t := ('s, 'a, 'e) t

  (** Run the stateful computation *)
  val run : ('s, 'a, 'e) t -> 's -> ('s * 'a, 'e) m

  val runA : ('s, 'a, 'e) t -> 's -> ('a, 'e) m

  (** Like map, but allows the state to be modified *)
  val transform : ('s, 'a, 'e) t -> f:('s -> 'a -> 's * 'b) -> ('s, 'b, 'e) t

  val transformF : ('s, 'a, 'e) t -> f:('s -> 'a -> ('s * 'b, 'e) m) -> ('s, 'b, 'e) t

  (** Inspect the state to get a value, without modifying the state *)
  val inspect : f:('s -> 'c) -> ('s, 'c, 'e) t

  val inspectF : f:('s -> ('c, 'e) m) -> ('s, 'c, 'e) t

  (** Perform a map on the state *)
  val modify : f:('s -> 's) -> ('s, unit, 'e) t

  val modifyF : f:('s -> ('s, 'e) m) -> ('s, unit, 'e) t

  (* Get the state *)
  val get : unit -> ('s, 's, 'e) t

  (** Set the state *)
  val set : 's -> ('s, unit, 'e) t

  val setF : ('s, 'e) m -> ('s, unit, 'e) t
  val returnF : ('a, 'e) m -> ('s, 'a, 'e) t
  val make : f:('s -> 's * 'a) -> ('s, 'a, 'e) t
  val makeF : f:('s -> ('s * 'a, 'e) m) -> ('s, 'a, 'e) t
  val all_map : ('k, ('s, 'ok, 'err) t, 'cmp) Map.t -> ('s, ('k, 'ok, 'cmp) Map.t, 'err) t
end

module Make2 (M : Monad.S2) : S2 with type ('a, 'e) m = ('a, 'e) M.t

module type S2WithError = sig
  include S2

  val both : ('s, 'a, 'e) t -> ('s, 'b, 'e) t -> ('s, 'a * 'b, 'e) t
  val allNE : ('s, 'ok, 'err) t NeList.t -> ('s, 'ok NeList.t, 'err) t
end

module Make2WithError (M : MonadWithError.S2) :
  S2WithError with type ('a, 'e) m = ('a, 'e) M.t
