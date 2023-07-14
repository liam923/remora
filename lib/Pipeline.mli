open! Base

module type S2 = sig
  type ('a, 'e) m

  module S : StateT.S2 with type ('a, 'e) m = ('a, 'e) m

  module type Stage = sig
    type state
    type input
    type output
    type error

    val name : string
    val run : input -> (state, output, error) S.t
  end

  type ('s, 'a, 'b, 'e) stage =
    (module Stage
       with type state = 's
        and type input = 'a
        and type output = 'b
        and type error = 'e)

  type ('s, 'a, 'b, 'e) pipeline
  type ('s, 'a, 'b, 'e) t = ('s, 'a, 'b, 'e) pipeline

  val ( @> )
    :  ('s, 'a, 'b, 'e) stage
    -> ('s, 'b, 'c, 'e) pipeline
    -> ('s, 'a, 'c, 'e) pipeline

  type ('s, 'a, 'b, 'e) compositePipeline =
    { name : string
    ; pipeline : ('s, 'a, 'b, 'e) pipeline
    }

  val ( @@> )
    :  ('s, 'a, 'b, 'e) compositePipeline
    -> ('s, 'b, 'c, 'e) pipeline
    -> ('s, 'a, 'c, 'e) pipeline

  val empty : ('s, 'a, 'a, 'e) pipeline
  val make : ('s, 'a, 'b, 'e) pipeline -> 'a -> ('s, 'b, 'e) S.t

  module type Plugin = sig
    type acc
    type error

    val initialAcc : acc
    val makeStage : acc -> 'a -> ('s, 'a, 'b, error) stage -> ('s, acc * 'b, error) S.t
  end

  val makeWithPlugin
    :  ('s, 'a, 'b, 'e) pipeline
    -> 'a
    -> (module Plugin with type acc = 'acc and type error = 'e)
    -> ('s, 'acc * 'b, 'e) S.t

  module type NestingPlugin = sig
    type acc
    type error

    val initialAcc : acc
    val increaseNesting : name:string -> acc -> acc
    val decreaseNesting : acc -> acc
    val makeStage : acc -> 'a -> ('s, 'a, 'b, error) stage -> ('s, acc * 'b, error) S.t
  end

  val makeWithNestingPlugin
    :  ('s, 'a, 'b, 'e) pipeline
    -> 'a
    -> (module NestingPlugin with type acc = 'acc and type error = 'e)
    -> ('s, 'acc * 'b, 'e) S.t
end

module Make2 (M : MonadWithError.S2) : S2 with type ('a, 'e) m = ('a, 'e) M.t
