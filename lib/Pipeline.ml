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

module Make2 (M : MonadWithError.S2) = struct
  type ('a, 'e) m = ('a, 'e) M.t

  module S = StateT.Make2WithError (M)

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

  type (_, _, _, _) pipeline =
    | Step :
        ('s, 'a, 'b, 'e) stage * ('s, 'b, 'c, 'e) pipeline
        -> ('s, 'a, 'c, 'e) pipeline
    | CompositeStep :
        ('s, 'a, 'b, 'e) compositePipeline * ('s, 'b, 'c, 'e) pipeline
        -> ('s, 'a, 'c, 'e) pipeline
    | Empty : ('s, 'a, 'a, 'e) pipeline

  and ('s, 'a, 'b, 'e) compositePipeline =
    { name : string
    ; pipeline : ('s, 'a, 'b, 'e) pipeline
    }

  type ('s, 'a, 'b, 'e) t = ('s, 'a, 'b, 'e) pipeline

  let ( @> ) step rest = Step (step, rest)
  let empty = Empty
  let ( @@> ) step rest = CompositeStep (step, rest)

  module type NestingPlugin = sig
    type acc
    type error

    val initialAcc : acc
    val increaseNesting : name:string -> acc -> acc
    val decreaseNesting : acc -> acc
    val makeStage : acc -> 'a -> ('s, 'a, 'b, error) stage -> ('s, acc * 'b, error) S.t
  end

  let makeWithNestingPlugin
    (type s a b e acc)
    (pipeline : (s, a, b, e) pipeline)
    (init : a)
    (plugin : (module NestingPlugin with type acc = acc and type error = e))
    : (s, acc * b, e) S.t
    =
    let module P = (val plugin : NestingPlugin with type acc = acc and type error = e) in
    let rec loop : type c d. acc -> c -> (s, c, d, e) pipeline -> (s, acc * d, e) S.t =
      fun acc curr pipeline ->
      match pipeline with
      | Step (stage, rest) ->
        let%bind.S acc, next = P.makeStage acc curr stage in
        loop acc next rest
      | CompositeStep ({ name; pipeline }, rest) ->
        let acc = P.increaseNesting ~name acc in
        let%bind.S acc, next = loop acc curr pipeline in
        let acc = P.decreaseNesting acc in
        loop acc next rest
      | Empty -> S.return (acc, curr)
    in
    loop P.initialAcc init pipeline
  ;;

  module type Plugin = sig
    type acc
    type error

    val initialAcc : acc
    val makeStage : acc -> 'a -> ('s, 'a, 'b, error) stage -> ('s, acc * 'b, error) S.t
  end

  let makeWithPlugin
    (type s a b e acc)
    (pipeline : (s, a, b, e) pipeline)
    (init : a)
    (plugin : (module Plugin with type acc = acc and type error = e))
    : (s, acc * b, e) S.t
    =
    let module P = struct
      include (val plugin : Plugin with type acc = acc and type error = e)

      let increaseNesting ~name:_ acc = acc
      let decreaseNesting acc = acc
    end
    in
    makeWithNestingPlugin pipeline init (module P)
  ;;

  let make (type s a b e) (pipeline : (s, a, b, e) pipeline) (init : a) =
    (* Error.raise (Error.of_string "")
       ;; *)
    let module P = struct
      type acc = unit
      type error = e

      let initialAcc = ()

      let makeStage (type s c d) () curr stage =
        let module Stage =
          (val stage
              : Stage
              with type state = s
               and type input = c
               and type output = d
               and type error = e)
        in
        let%map.S next = Stage.run curr in
        (), next
      ;;
    end
    in
    let%map.S (), result = makeWithPlugin pipeline init (module P) in
    result
  ;;
end
