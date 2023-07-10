open! Base
open Ast

type 'v entry =
  { e : 'v
  ; id : Typed.Identifier.t
  }

type t =
  { sorts : Sort.t entry Map.M(String).t
  ; kinds : Kind.t entry Map.M(String).t
  ; types : Typed.Type.array entry Map.M(String).t
  }

module type IdentifierGenerator = sig
  include StateT.S2

  val createId : string -> ('s, Typed.Identifier.t, 'e) t
end

module Base (Gen : IdentifierGenerator) : sig
  val make : unit -> (_, t, _) Gen.t
end
