open! Base

module T = struct
  type t =
    { name : string
    ; id : int
    }
  [@@deriving compare, sexp, equal]
end

include T
include Comparator.Make (T)
