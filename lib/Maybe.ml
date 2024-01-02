open! Base

type doesExist = DoesExist [@@deriving sexp_of]
type doesNotExist = DoesNotExist [@@deriving sexp_of]

type ('a, 'e) t =
  | Just : 'a -> ('a, doesExist) t
  | Nothing : (_, doesNotExist) t
[@@deriving sexp_of]

let map : type a b e. (a, e) t -> f:(a -> b) -> (b, e) t =
  fun m ~f ->
  match m with
  | Just v -> Just (f v)
  | Nothing -> Nothing
;;
