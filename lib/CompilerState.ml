open! Base

type state =
  { idCounter : int
  ; deviceInfo : DeviceInfo.t
  }

include StateT.Make2WithError (MResult)

type ('t, 'e) u = (state, 't, 'e) t

let initial = { idCounter = 0; deviceInfo = DeviceInfo.grid_m10_8q }

let createId name =
  make ~f:(fun state ->
    State.run
      (Identifier.create
         name
         ~getCounter:(fun (s : state) -> s.idCounter)
         ~setCounter:(fun s idCounter -> { s with idCounter }))
      state)
;;

let error ?(source = None) msg =
  makeF ~f:(fun _ -> MResult.err Source.{ elem = msg; source })
;;
