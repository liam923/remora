open! Base

type state = { idCounter : int }

include StateT.S2WithError with type ('a, 'e) m = ('a, 'e) MResult.t

val initial : state
