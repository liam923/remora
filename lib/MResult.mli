open! Base

type ('ok, 'err) t =
  | MOk of 'ok
  | Errors of 'err Non_empty_list.t

include Monad.S2 with type ('ok, 'err) t := ('ok, 'err) t

val ofOption : 'ok option -> err:'err -> ('ok, 'err) t
val err : 'err -> ('ok, 'err) t
val require : bool -> 'err -> (unit, 'err) t
val allNE : ('ok, 'err) t Non_empty_list.t -> ('ok Non_empty_list.t, 'err) t
val traverseOpt : ('ok, 'err) t option -> ('ok option, 'err) t
val both : ('a, 'err) t -> ('b, 'err) t -> ('a * 'b, 'err) t
