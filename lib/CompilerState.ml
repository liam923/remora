open! Base

type state = { idCounter : int }

include StateT.Make2WithError (MResult)

let initial = { idCounter = 0 }
