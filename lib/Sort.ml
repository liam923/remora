open! Base

type t =
  | Dim
  | Shape
[@@deriving equal, sexp, compare]
