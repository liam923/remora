open! Base

module type S2 = sig
  type 'e error

  include Monad.S2

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

module Make2 (B : Basic2) = struct
  include Monad.Make2 (B)

  let bindWithError = B.bindWithError
end
