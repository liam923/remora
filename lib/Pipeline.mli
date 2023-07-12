open! Base

module type Stage = sig
  type input
  type output
  type error

  val name : string
  val run : input -> (output, error) MResult.t
end

type ('a, 'b, 'e) stage =
  (module Stage with type input = 'a and type output = 'b and type error = 'e)

type ('a, 'b, 'e) pipeline
type ('a, 'b, 'e) t = ('a, 'b, 'e) pipeline

val ( @> ) : ('a, 'b, 'e) stage -> ('b, 'c, 'e) pipeline -> ('a, 'c, 'e) pipeline

type ('a, 'b, 'e) compositePipeline =
  { name : string
  ; pipeline : ('a, 'b, 'e) pipeline
  }

val ( @@> )
  :  ('a, 'b, 'e) compositePipeline
  -> ('b, 'c, 'e) pipeline
  -> ('a, 'c, 'e) pipeline

val empty : ('a, 'a, 'e) pipeline
val exec : ('a, 'b, 'e) pipeline -> 'a -> ('b, 'e) MResult.t

module type Plugin = sig
  type acc
  type error

  val initialAcc : acc
  val executeStage : acc -> 'a -> ('a, 'b, error) stage -> acc * ('b, error) MResult.t
end

val execWithPlugin
  :  ('a, 'b, 'e) pipeline
  -> 'a
  -> (module Plugin with type acc = 'acc and type error = 'e)
  -> 'acc * ('b, 'e) MResult.t

module type NestingPlugin = sig
  type acc
  type error

  val initialAcc : acc
  val increaseNesting : name:string -> acc -> acc
  val decreaseNesting : acc -> acc
  val executeStage : acc -> 'a -> ('a, 'b, error) stage -> acc * ('b, error) MResult.t
end

val execWithNestingPlugin
  :  ('a, 'b, 'e) pipeline
  -> 'a
  -> (module NestingPlugin with type acc = 'acc and type error = 'e)
  -> 'acc * ('b, 'e) MResult.t
