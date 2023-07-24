open! Base

(** MonadWithError is similar to Monad. However, it provides an additional
    bindWithError function. When performing a flatmap, this allows for
    creating errors from the given function even if the value being mapped on
    is an error *)

module type S2 = sig
  type 'e error

  include Monad.S2

  (** Perform a flatmap. If the given value is an error,
      join the error together with the results of the error funciton *)
  val bindWithError
    :  ('a, 'e) t
    -> f:('a -> ('b, 'e) t)
    -> error:('e error -> (unit, 'e) t)
    -> ('b, 'e) t
end

module type Basic2 = sig
  type 'e error

  include Monad.Basic2

  val bindWithError
    :  ('a, 'e) t
    -> f:('a -> ('b, 'e) t)
    -> error:('e error -> (unit, 'e) t)
    -> ('b, 'e) t
end

module Make2 (B : Basic2) :
  S2 with type ('a, 'e) t := ('a, 'e) B.t with type 'e error := 'e B.error
