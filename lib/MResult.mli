open! Base

type ('ok, 'err) t =
  | MOk of 'ok
  | Errors of 'err NeList.t
[@@deriving sexp, eq, ord]

include
  MonadWithError.S2
    with type ('ok, 'err) t := ('ok, 'err) t
    with type 'err error = 'err NeList.t

val ofOption : 'ok option -> err:'err -> ('ok, 'err) t
val err : 'err -> ('ok, 'err) t
val require : bool -> 'err -> (unit, 'err) t
val allNE : ('ok, 'err) t NeList.t -> ('ok NeList.t, 'err) t
val traverseOpt : ('ok, 'err) t option -> ('ok option, 'err) t
val both : ('a, 'err) t -> ('b, 'err) t -> ('a * 'b, 'err) t
