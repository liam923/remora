open! Base

type state = { idCounter : int }

include StateT.Make2WithError (MResult)

type ('t, 'e) u = (state, 't, 'e) t

let initial = { idCounter = 0 }
