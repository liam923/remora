open! Base

type state =
  { idCounter : int
  ; deviceInfo : DeviceInfo.t
  }

include StateT.S2WithError with type ('a, 'e) m = ('a, 'e) MResult.t

type ('t, 'e) u = (state, 't, 'e) t

val initial : state
val createId : string -> (Identifier.t, _) u
val error : ?source:'s option -> string -> (_, ('s option, string) Source.annotate) u
