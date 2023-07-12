open! Base
open Core

type 'v entry =
  { e : 'v
  ; id : Identifier.t
  }

type t =
  { sorts : Sort.t entry Map.M(String).t
  ; kinds : Kind.t entry Map.M(String).t
  ; types : Type.array entry Map.M(String).t
  ; literalType : Expr.literalValue -> Type.atom
  }

module type IdentifierGenerator = sig
  include StateT.S2

  type state

  val createId : string -> (state, Identifier.t, 'e) t
end

module Base (Gen : IdentifierGenerator) : sig
  val make : unit -> (Gen.state, t, _) Gen.t
end
