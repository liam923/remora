open! Base

type t =
  | Atom
  | Array
[@@deriving eq, sexp]
