open! Base
open Nucleus

type t =
  { sorts : Index.t Map.M(String).t
  ; kinds : Type.t Map.M(String).t
  ; types : Expr.array Map.M(String).t
  }
