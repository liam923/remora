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

  type state

  val createId : string -> (state, Typed.Identifier.t, 'e) t
end

module Base (Gen : IdentifierGenerator) = struct
  let make () =
    let open Gen.Let_syntax in
    let intName = "int"
    and charName = "char" in
    let%map intId = Gen.createId intName
    and charId = Gen.createId charName in
    { sorts = Map.empty (module String)
    ; kinds =
        Map.of_alist_exn
          (module String)
          [ intName, { id = intId; e = Kind.Atom }
          ; charName, { id = charId; e = Kind.Atom }
          ]
    ; types = Map.empty (module String)
    }
  ;;
end
