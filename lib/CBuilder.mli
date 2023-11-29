open! Base

type ('s, 'a, 'e) t

include Monad.S3 with type ('a, 's, 'e) t := ('s, 'a, 'e) t

type ('a, 'e) u = (CompilerState.state, 'a, 'e) t

type name =
  | NameOfStr of
      { str : string
      ; needsUniquifying : bool
      }
  | NameOfId of Identifier.t

val createName : name -> (C.name, _) u
val defineFunL : name -> f:(C.name -> ('a * C.fun', 'e) u) -> ('a * C.name, 'e) u
val defineFun : name -> f:(C.name -> (C.fun', 'e) u) -> (C.name, 'e) u
val defineStructL : name -> f:(C.name -> ('a * C.struct', 'e) u) -> ('a * C.name, 'e) u
val defineStruct : name -> f:(C.name -> (C.struct', 'e) u) -> (C.name, 'e) u
val include' : string -> (unit, _) u

val build
  :  ?prelude:string list
  -> (C.block option, _) u
  -> (CompilerState.state, C.program, _) State.t
