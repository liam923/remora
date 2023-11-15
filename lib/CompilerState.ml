open! Base

type state =
  { idCounter : int
  ; deviceInfo : DeviceInfo.t
  }

include StateT.Make2WithError (MResult)

type ('t, 'e) u = (state, 't, 'e) t

let initial = { idCounter = 0; deviceInfo = DeviceInfo.grid_m10_8q }
