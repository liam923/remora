open! Base

type state = { idCounter : int }

include StateT.S2WithError with type ('a, 'e) m = ('a, 'e) MResult.t

type ('t, 'e) u = (state, 't, 'e) t

val initial : state
