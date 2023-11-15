open! Base

type t =
  { maxThreadsPerBlock : int
  ; maxRegistersPerBlock : int
  ; maxThreadsPerMultiprocessor : int
  ; multiprocessors : int
  }

let maxThreads device = device.maxThreadsPerMultiprocessor * device.multiprocessors

let grid_m10_8q =
  { maxThreadsPerBlock = 1024
  ; maxRegistersPerBlock = 65536
  ; maxThreadsPerMultiprocessor = 2048
  ; multiprocessors = 5
  }
;;
