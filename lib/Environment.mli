open! Base
open Nucleus

type t =
  { sorts : Index.t Map.M(String).t
  ; kinds : Type.t Map.M(String).t
  ; types : Expr.array Map.M(String).t
  }

module type IdentifierGenerator = sig
  include StateT.S2

  type state

  val createId : string -> (state, Identifier.t, 'e) t
end

module Base (Gen : IdentifierGenerator) : sig
  val make : unit -> (Gen.state, t, _) Gen.t
end
