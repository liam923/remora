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

type (_, _, _) pipeline =
  | Step : ('a, 'b, 'e) stage * ('b, 'c, 'e) pipeline -> ('a, 'c, 'e) pipeline
  | CompositeStep :
      ('a, 'b, 'e) compositePipeline * ('b, 'c, 'e) pipeline
      -> ('a, 'c, 'e) pipeline
  | Empty : ('a, 'a, 'e) pipeline

and ('a, 'b, 'e) compositePipeline =
  { name : string
  ; pipeline : ('a, 'b, 'e) pipeline
  }

type ('a, 'b, 'e) t = ('a, 'b, 'e) pipeline

let ( @> ) step rest = Step (step, rest)
let empty = Empty
let ( @@> ) step rest = CompositeStep (step, rest)

module type NestingPlugin = sig
  type acc
  type error

  val initialAcc : acc
  val increaseNesting : name:string -> acc -> acc
  val decreaseNesting : acc -> acc
  val executeStage : acc -> 'a -> ('a, 'b, error) stage -> acc * ('b, error) MResult.t
end

let execWithNestingPlugin
    (type a b e acc)
    (pipeline : (a, b, e) pipeline)
    (init : a)
    (plugin : (module NestingPlugin with type acc = acc and type error = e))
    : acc * (b, e) MResult.t
  =
  let module P = (val plugin : NestingPlugin with type acc = acc and type error = e) in
  let rec loop : type c d. acc -> c -> (c, d, e) pipeline -> acc * (d, e) MResult.t =
   fun acc curr pipeline ->
    match pipeline with
    | Step (stage, rest) ->
      let acc, next = P.executeStage acc curr stage in
      (match next with
      | MOk next -> loop acc next rest
      | Errors _ as errs -> acc, errs)
    | CompositeStep ({ name; pipeline }, rest) ->
      let acc = P.increaseNesting ~name acc in
      let acc, next = loop acc curr pipeline in
      (match next with
      | MOk next ->
        let acc = P.decreaseNesting acc in
        loop acc next rest
      | Errors _ as errs -> acc, errs)
    | Empty -> acc, MOk curr
  in
  loop P.initialAcc init pipeline
;;

module type Plugin = sig
  type acc
  type error

  val initialAcc : acc
  val executeStage : acc -> 'a -> ('a, 'b, error) stage -> acc * ('b, error) MResult.t
end

let execWithPlugin
    (type a b e acc)
    (pipeline : (a, b, e) pipeline)
    (init : a)
    (plugin : (module Plugin with type acc = acc and type error = e))
    : acc * (b, e) MResult.t
  =
  let module P = struct
    include (val plugin : Plugin with type acc = acc and type error = e)

    let increaseNesting ~name:_ acc = acc
    let decreaseNesting acc = acc
  end
  in
  execWithNestingPlugin pipeline init (module P)
;;

let exec (type a b e) (pipeline : (a, b, e) pipeline) (init : a) =
  let module P = struct
    type acc = unit
    type error = e

    let initialAcc = ()

    let executeStage (type c d) () curr stage =
      let module S = (val stage : Stage
                        with type input = c
                         and type output = d
                         and type error = e)
      in
      (), S.run curr
    ;;
  end
  in
  let (), result = execWithPlugin pipeline init (module P) in
  result
;;
