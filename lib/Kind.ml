open! Base

type t =
  | Atom
  | Array
[@@deriving equal, sexp, compare]
