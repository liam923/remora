open! Base
module Index = Corn.Index
module Type = Corn.Type

type host = Corn.Expr.host [@@deriving sexp_of]
type device = Corn.Expr.device [@@deriving sexp_of]

module KernelizeState = struct
  include State

  type state = CompilerState.state
  type ('a, 'e) u = (state, 'a, 'e) t

  let getDeviceInfo () = get () >>| fun (s : state) -> s.deviceInfo
end

module ParallelismShape = struct
  (** Describes the shape of parallelism in an expression. *)
  type t =
    | Known of int
    | ParallelAcrossDim of
        { dim : Index.shapeElement
        ; rest : t
        ; parallelismFloor : int
        }
    | MaxParallelism of
        { maxAcross : t list
        ; parallelismFloor : int
        }
  [@@deriving sexp_of]

  let parallelismFloor = function
    | Known n -> n
    | ParallelAcrossDim p -> p.parallelismFloor
    | MaxParallelism p -> p.parallelismFloor
  ;;

  let known = function
    | Known n -> Some n
    | ParallelAcrossDim _ -> None
    | MaxParallelism _ -> None
  ;;

  let nestParallelism (shapeElement : Index.shapeElement) restParallelism =
    match shapeElement, known restParallelism with
    | Add { const; refs }, Some rest when Map.is_empty refs -> Known (const * rest)
    | _ ->
      let minParallelismOfTopDim =
        match shapeElement with
        | Add { const; refs = _ } -> const
        | ShapeRef _ -> 0
      in
      ParallelAcrossDim
        { dim = shapeElement
        ; rest = restParallelism
        ; parallelismFloor = minParallelismOfTopDim * parallelismFloor restParallelism
        }
  ;;

  let singleDimensionParallelism shapeElement = nestParallelism shapeElement (Known 1)
  let empty = Known 1

  let max pars =
    let rec flatten l =
      List.bind l ~f:(function
        | MaxParallelism p -> flatten p.maxAcross
        | p -> [ p ])
    in
    let pars = flatten pars in
    let maxKnown =
      pars |> List.map ~f:known |> List.filter_opt |> List.min_elt ~compare:Int.compare
    in
    let pars =
      match maxKnown with
      | Some maxKnown ->
        Known maxKnown
        :: List.filter_map pars ~f:(function
          | Known _ -> None
          | p -> Some p)
      | None -> pars
    in
    match flatten pars with
    | [] -> Known 0
    | [ par ] -> par
    | maxAcrossHead :: maxAcrossRest ->
      let parallelismFloor =
        maxAcrossHead :: maxAcrossRest
        |> NeList.map ~f:parallelismFloor
        |> NeList.max_elt ~compare:Int.compare
      in
      let maxAcross = maxAcrossHead :: maxAcrossRest in
      MaxParallelism { maxAcross; parallelismFloor }
  ;;

  let rec toCorn : t -> Corn.Expr.parallelism = function
    | Known n -> KnownParallelism n
    | ParallelAcrossDim p -> Parallelism { shape = p.dim; rest = toCorn p.rest }
    | MaxParallelism p -> MaxParallelism (List.map p.maxAcross ~f:toCorn)
  ;;
end

(** For a Nested expression, the best host and device Corn expressions
    that it can be compiled into. *)
type compilationOptions =
  { hostExpr : host Corn.Expr.t
  ; deviceExpr : device Corn.Expr.t
  ; hostParShape : ParallelismShape.t
      (** The expression if it were to be the body of a map kernel,
          enabling flattening *)
  ; flattenedMapBody : Corn.Expr.mapBody
  ; flattenedMapBodyParShape : ParallelismShape.t
  }

let compilationOptions ~hostExpr ~deviceExpr ~hostParShape =
  { hostExpr
  ; deviceExpr
  ; hostParShape
  ; flattenedMapBody = MapBodyExpr deviceExpr
  ; flattenedMapBodyParShape = Known 1
  }
;;

let hostExpr { hostExpr; _ } = hostExpr
let deviceExpr { deviceExpr; _ } = deviceExpr
let hostParShape { hostParShape; _ } = hostParShape

module ParallelismWorthwhileness = struct
  type t =
    | NotWorthwhile of { bound : int option }
    | Worthwhile of { bound : int option }
    | Saturating
  [@@deriving sexp_of]

  let saturatationCutoff device = DeviceInfo.maxThreads device

  let worthwhileParallelismCutoff (_ : DeviceInfo.t) =
    (* Arbitrary heuristic I came up with with no testing.
       A good heuristic should factor in both host and device info. *)
    128
  ;;

  let get deviceInfo p =
    if ParallelismShape.parallelismFloor p >= saturatationCutoff deviceInfo
    then Saturating
    else if ParallelismShape.parallelismFloor p >= worthwhileParallelismCutoff deviceInfo
    then Worthwhile { bound = ParallelismShape.known p }
    else NotWorthwhile { bound = ParallelismShape.known p }
  ;;
end

let getThreads (_ : DeviceInfo.t) = 128

let getBlocks ~threads:threadsPerBlock ~parShape deviceInfo =
  let blocksToReachThreads threads = (threads + threadsPerBlock - 1) / threadsPerBlock in
  let maxBlocks = blocksToReachThreads (DeviceInfo.maxThreads deviceInfo) in
  match ParallelismShape.known parShape with
  | Some parShape -> Int.min (blocksToReachThreads parShape) maxBlocks
  | None -> maxBlocks
;;

let decideParallelism ~par:(parExpr, parParShape) ~seq:(seqExpr, seqParShape) =
  let open KernelizeState.Let_syntax in
  let%map deviceInfo = KernelizeState.getDeviceInfo () in
  let type' = Corn.Expr.type' parExpr in
  match
    ( ParallelismWorthwhileness.get deviceInfo parParShape
    , ParallelismWorthwhileness.get deviceInfo seqParShape )
  with
  | Saturating, _
  | Worthwhile { bound = _ }, NotWorthwhile { bound = Some _ }
  | Worthwhile { bound = Some _ }, NotWorthwhile { bound = None } -> parExpr, parParShape
  | NotWorthwhile { bound = Some _ }, _ | _, Saturating -> seqExpr, seqParShape
  | Worthwhile { bound = Some parBound }, Worthwhile { bound = Some seqBound } ->
    if parBound >= seqBound then parExpr, parParShape else seqExpr, seqParShape
  | NotWorthwhile { bound = None }, NotWorthwhile { bound = Some _ } ->
    ( Corn.Expr.IfParallelismHitsCutoff
        { parallelism = ParallelismShape.toCorn parParShape
        ; cutoff = ParallelismWorthwhileness.worthwhileParallelismCutoff deviceInfo
        ; then' = parExpr
        ; else' = seqExpr
        ; type'
        }
    , parParShape )
  | Worthwhile { bound = Some parBound }, Worthwhile { bound = None } ->
    if parBound >= ParallelismShape.parallelismFloor seqParShape
    then parExpr, parParShape
    else seqExpr, seqParShape
  | ( (Worthwhile { bound = None } | NotWorthwhile { bound = None })
    , (Worthwhile { bound = _ } | NotWorthwhile { bound = None }) ) ->
    ( Corn.Expr.IfParallelismHitsCutoff
        { parallelism = ParallelismShape.toCorn parParShape
        ; cutoff = ParallelismShape.parallelismFloor seqParShape
        ; then' = parExpr
        ; else' = seqExpr
        ; type'
        }
    , ParallelismShape.max [ parParShape; seqParShape ] )
;;

let rec getOpts (expr : Nested.t) : (compilationOptions, _) KernelizeState.u =
  let open KernelizeState.Let_syntax in
  let open Corn in
  match expr with
  | Ref ref ->
    return
    @@ compilationOptions
         ~hostExpr:(Ref ref)
         ~deviceExpr:(Ref ref)
         ~hostParShape:(Known 1)
  | Frame { elements; dimension; type' } ->
    let%map hostElements, deviceElements, hostParShape, _, _ = getOptsList elements in
    compilationOptions
      ~hostExpr:(Frame { elements = hostElements; dimension; type' })
      ~deviceExpr:(Frame { elements = deviceElements; dimension; type' })
      ~hostParShape
  | BoxValue { box; type' } ->
    let%map boxOpts = getOpts box in
    compilationOptions
      ~hostExpr:(BoxValue { box = boxOpts.hostExpr; type' })
      ~deviceExpr:(BoxValue { box = boxOpts.deviceExpr; type' })
      ~hostParShape:boxOpts.hostParShape
  | IndexLet { indexArgs; body; type' } ->
    let%map indexArgsHostExprs, indexArgsDeviceExprs, indexArgsHostParShapes =
      indexArgs
      |> List.map ~f:(fun { indexBinding; indexValue; sort } ->
        let%map indexValueHostExpr, indexValueDeviceExpr, indexValueHostParShape =
          match indexValue with
          | Runtime value ->
            let%map valueOpts = getOpts value in
            ( Expr.Runtime valueOpts.hostExpr
            , Expr.Runtime valueOpts.deviceExpr
            , valueOpts.hostParShape )
          | FromBox { box; i } ->
            let%map boxOpts = getOpts box in
            ( Expr.FromBox { box = boxOpts.hostExpr; i }
            , Expr.FromBox { box = boxOpts.deviceExpr; i }
            , boxOpts.hostParShape )
        in
        ( Expr.{ indexBinding; indexValue = indexValueHostExpr; sort }
        , Expr.{ indexBinding; indexValue = indexValueDeviceExpr; sort }
        , indexValueHostParShape ))
      |> KernelizeState.all
      >>| List.unzip3
    and bodyOpts = getOpts body in
    let indexArgsHostParShape = ParallelismShape.max indexArgsHostParShapes in
    compilationOptions
      ~hostExpr:
        (IndexLet { indexArgs = indexArgsHostExprs; body = bodyOpts.hostExpr; type' })
      ~deviceExpr:
        (IndexLet { indexArgs = indexArgsDeviceExprs; body = bodyOpts.deviceExpr; type' })
      ~hostParShape:
        (ParallelismShape.max [ indexArgsHostParShape; bodyOpts.hostParShape ])
  | ReifyIndex reifyIndex ->
    return
    @@ compilationOptions
         ~hostExpr:(ReifyIndex reifyIndex)
         ~deviceExpr:(ReifyIndex reifyIndex)
         ~hostParShape:ParallelismShape.empty
  | Let { args; body; type' } ->
    let%map argsHost, argsDevice, argsHostParShapes =
      args
      |> List.map ~f:(fun { binding; value } ->
        let%map valueOpts = getOpts value in
        ( Expr.{ binding; value = valueOpts.hostExpr }
        , Expr.{ binding; value = valueOpts.deviceExpr }
        , valueOpts.hostParShape ))
      |> KernelizeState.all
      >>| List.unzip3
    and bodyOpts = getOpts body in
    let argsHostParShape = ParallelismShape.max argsHostParShapes in
    compilationOptions
      ~hostExpr:(Let { args = argsHost; body = bodyOpts.hostExpr; type' })
      ~deviceExpr:(Let { args = argsDevice; body = bodyOpts.deviceExpr; type' })
      ~hostParShape:(ParallelismShape.max [ argsHostParShape; bodyOpts.hostParShape ])
  | LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; consumer = None
      ; type'
      } ->
    let%bind mapBodyOpts = getOpts mapBody in
    let mapAsKernel : Expr.mapKernel =
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody = mapBodyOpts.flattenedMapBody
      ; mapBodyMatcher
      ; mapResults
      ; type' = List.nth_exn type' 0
      }
    in
    let mapAsKernelParShape =
      ParallelismShape.nestParallelism frameShape mapBodyOpts.flattenedMapBodyParShape
    in
    let mapAsHostExpr =
      Expr.LoopBlock
        { frameShape
        ; mapArgs
        ; mapIotas
        ; mapBody = mapBodyOpts.hostExpr
        ; mapBodyMatcher
        ; mapResults
        ; consumer = None
        ; type'
        }
    in
    let%bind deviceInfo = KernelizeState.getDeviceInfo () in
    let threads = getThreads deviceInfo in
    let blocks = getBlocks ~threads ~parShape:mapAsKernelParShape deviceInfo in
    let%map hostExpr, hostParShape =
      decideParallelism
        ~par:
          ( Expr.values
              [ Expr.MapKernel { kernel = mapAsKernel; blocks; threads }; Expr.values [] ]
          , mapAsKernelParShape )
        ~seq:(mapAsHostExpr, mapBodyOpts.hostParShape)
    in
    { hostExpr
    ; deviceExpr =
        LoopBlock
          { frameShape
          ; mapArgs
          ; mapIotas
          ; mapBody = mapBodyOpts.deviceExpr
          ; mapBodyMatcher
          ; mapResults
          ; consumer = None
          ; type'
          }
    ; hostParShape
    ; flattenedMapBody = MapBodyValues [ MapBodyMap mapAsKernel; MapBodyValues [] ]
    ; flattenedMapBodyParShape = mapAsKernelParShape
    }
  | LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; consumer = Some consumer
      ; type'
      } ->
    let%bind consumerAsHostExpr, consumerAsDeviceExpr, consumerAsHostParShape, parConsumer
      =
      match consumer with
      | Reduce { arg; zero; body; d; itemPad; associative; character; type' } ->
        let%map zeroOpts = zero |> Option.map ~f:getOpts |> KernelizeState.all_opt
        and bodyOpts = getOpts body in
        let zeroOptsHostParShapeAsList =
          zeroOpts |> Option.map ~f:hostParShape |> Option.to_list
        in
        ( Expr.ReduceSeq
            { arg
            ; zero = Option.map zeroOpts ~f:hostExpr
            ; body = bodyOpts.hostExpr
            ; d
            ; itemPad
            ; character
            ; type'
            }
        , Expr.ReduceSeq
            { arg
            ; zero = Option.map zeroOpts ~f:deviceExpr
            ; body = bodyOpts.deviceExpr
            ; d
            ; itemPad
            ; character
            ; type'
            }
        , ParallelismShape.max (bodyOpts.hostParShape :: zeroOptsHostParShapeAsList)
        , if associative
          then
            Some
              ( Expr.ReducePar
                  { reduce =
                      { arg
                      ; zero = Option.map zeroOpts ~f:hostExpr
                      ; body = bodyOpts.deviceExpr
                      ; d
                      ; itemPad
                      ; character
                      ; type'
                      }
                  ; outerBody = bodyOpts.hostExpr
                  }
              , ParallelismShape.max
                  (ParallelismShape.singleDimensionParallelism (Add d)
                   :: zeroOptsHostParShapeAsList) )
          else None )
      | Fold { zeroArg; arrayArgs; body; d; itemPad; character; type' } ->
        let%map bodyOpts = getOpts body
        and zeroArgValueOpts = getOpts zeroArg.zeroValue in
        ( Expr.Fold
            { zeroArg =
                { zeroBinding = zeroArg.zeroBinding
                ; zeroValue = zeroArgValueOpts.hostExpr
                }
            ; arrayArgs
            ; body = bodyOpts.hostExpr
            ; d
            ; itemPad
            ; character
            ; type'
            }
        , Expr.Fold
            { zeroArg =
                { zeroBinding = zeroArg.zeroBinding
                ; zeroValue = zeroArgValueOpts.deviceExpr
                }
            ; arrayArgs
            ; body = bodyOpts.deviceExpr
            ; d
            ; itemPad
            ; character
            ; type'
            }
        , ParallelismShape.max [ bodyOpts.hostParShape; zeroArgValueOpts.hostParShape ]
        , None )
      | Scatter { valuesArg; indicesArg; dIn; dOut; type' } ->
        let scatter = Expr.Scatter { valuesArg; indicesArg; dIn; dOut; type' } in
        return
          ( scatter
          , scatter
          , ParallelismShape.empty
          , Some (scatter, ParallelismShape.singleDimensionParallelism (Add dIn)) )
    and mapBodyOpts = getOpts mapBody in
    let sequentialBlockParShape =
      ParallelismShape.max [ mapBodyOpts.hostParShape; consumerAsHostParShape ]
    in
    let%map hostExpr, hostParShape =
      match parConsumer with
      | Some (parConsumerExpr, parConsumerParShape) ->
        let%bind deviceInfo = KernelizeState.getDeviceInfo () in
        let threads = getThreads deviceInfo in
        let blocks = getBlocks ~threads ~parShape:parConsumerParShape deviceInfo in
        decideParallelism
          ~par:
            ( Expr.LoopKernel
                { kernel =
                    { frameShape
                    ; mapArgs
                    ; mapIotas
                    ; mapBody = mapBodyOpts.deviceExpr
                    ; mapBodyMatcher
                    ; mapResults
                    ; consumer = Some parConsumerExpr
                    ; type'
                    }
                ; blocks
                ; threads
                }
            , parConsumerParShape )
          ~seq:
            ( Expr.LoopBlock
                { frameShape
                ; mapArgs
                ; mapIotas
                ; mapBody = mapBodyOpts.hostExpr
                ; mapBodyMatcher
                ; mapResults
                ; consumer = Some consumerAsHostExpr
                ; type'
                }
            , sequentialBlockParShape )
      | None ->
        (* TODO: might be a good idea to unfuse and parallelize just the map *)
        return
          ( Expr.LoopBlock
              { frameShape
              ; mapArgs
              ; mapIotas
              ; mapBody = mapBodyOpts.hostExpr
              ; mapBodyMatcher
              ; mapResults
              ; consumer = Some consumerAsHostExpr
              ; type'
              }
          , sequentialBlockParShape )
    in
    compilationOptions
      ~hostExpr
      ~deviceExpr:
        (LoopBlock
           { frameShape
           ; mapArgs
           ; mapIotas
           ; mapBody = mapBodyOpts.deviceExpr
           ; mapBodyMatcher
           ; mapResults
           ; consumer = Some consumerAsDeviceExpr
           ; type'
           })
      ~hostParShape
  | Box { indices; body; bodyType; type' } ->
    let%map bodyOpts = getOpts body in
    compilationOptions
      ~hostExpr:(Box { indices; body = bodyOpts.hostExpr; bodyType; type' })
      ~deviceExpr:(Box { indices; body = bodyOpts.deviceExpr; bodyType; type' })
      ~hostParShape:bodyOpts.hostParShape
  | Literal literal ->
    return
    @@ compilationOptions
         ~hostExpr:(Literal literal)
         ~deviceExpr:(Literal literal)
         ~hostParShape:ParallelismShape.empty
  | Values { elements; type' } ->
    let%map ( elementsHostExprs
            , elementsDeviceExprs
            , elementsHostParShape
            , elementsFlattenedMapBodies
            , elementsFlattenedMapBodiesParShape )
      =
      getOptsList elements
    in
    { hostExpr = Values { elements = elementsHostExprs; type' }
    ; deviceExpr = Values { elements = elementsDeviceExprs; type' }
    ; hostParShape = elementsHostParShape
    ; flattenedMapBody = Expr.MapBodyValues elementsFlattenedMapBodies
    ; flattenedMapBodyParShape = elementsFlattenedMapBodiesParShape
    }
  | ScalarPrimitive { op; args; type' } ->
    let%map argsHostExprs, argsDeviceExprs, argsHostParShape, _, _ = getOptsList args in
    compilationOptions
      ~hostExpr:(ScalarPrimitive { op; args = argsHostExprs; type' })
      ~deviceExpr:(ScalarPrimitive { op; args = argsDeviceExprs; type' })
      ~hostParShape:argsHostParShape
  | TupleDeref { tuple; index; type' } ->
    let%map tupleOpts = getOpts tuple in
    let hostExpr =
      match tupleOpts.hostExpr with
      | Values { elements; type' = _ } -> List.nth_exn elements index
      | tuple -> TupleDeref { tuple; index; type' }
    in
    let deviceExpr =
      match tupleOpts.deviceExpr with
      | Values { elements; type' = _ } -> List.nth_exn elements index
      | tuple -> TupleDeref { tuple; index; type' }
    in
    let flattenedMapBody =
      match tupleOpts.flattenedMapBody with
      | MapBodyValues elements -> List.nth_exn elements index
      | _ -> MapBodyDeref { tuple = tupleOpts.flattenedMapBody; index }
    in
    { hostExpr
    ; deviceExpr
    ; hostParShape = tupleOpts.hostParShape
    ; flattenedMapBody
    ; flattenedMapBodyParShape = tupleOpts.flattenedMapBodyParShape
    }
  | SubArray { arrayArg; indexArg; type' } ->
    let%map arrayArgOpts = getOpts arrayArg
    and indexArgOpts = getOpts indexArg in
    compilationOptions
      ~hostExpr:
        (SubArray
           { arrayArg = arrayArgOpts.hostExpr; indexArg = indexArgOpts.hostExpr; type' })
      ~deviceExpr:
        (SubArray
           { arrayArg = arrayArgOpts.deviceExpr
           ; indexArg = indexArgOpts.deviceExpr
           ; type'
           })
      ~hostParShape:
        (ParallelismShape.max [ arrayArgOpts.hostParShape; indexArgOpts.hostParShape ])
  | Append { args; type' } ->
    let%map argsHostExprs, argsDeviceExprs, argsHostParShape, _, _ = getOptsList args in
    compilationOptions
      ~hostExpr:(Append { args = argsHostExprs; type' })
      ~deviceExpr:(Append { args = argsDeviceExprs; type' })
      ~hostParShape:argsHostParShape
  | Zip { zipArg; nestCount; type' } ->
    let%map zipArgOpts = getOpts zipArg in
    compilationOptions
      ~hostExpr:(Zip { zipArg = zipArgOpts.hostExpr; nestCount; type' })
      ~deviceExpr:(Zip { zipArg = zipArgOpts.deviceExpr; nestCount; type' })
      ~hostParShape:zipArgOpts.hostParShape
  | Unzip { unzipArg; type' } ->
    let%map unzipArgOpts = getOpts unzipArg in
    compilationOptions
      ~hostExpr:(Unzip { unzipArg = unzipArgOpts.hostExpr; type' })
      ~deviceExpr:(Unzip { unzipArg = unzipArgOpts.deviceExpr; type' })
      ~hostParShape:unzipArgOpts.hostParShape

and getOptsList exprs =
  let open KernelizeState.Let_syntax in
  let%map optss = exprs |> List.map ~f:getOpts |> KernelizeState.all in
  let hostExprs = List.map optss ~f:hostExpr in
  let deviceExprs = List.map optss ~f:deviceExpr in
  let hostParShapes = List.map optss ~f:hostParShape in
  let flattenedMapBodies = List.map optss ~f:(fun o -> o.flattenedMapBody) in
  let flattenedMapBodiesParShape =
    List.map optss ~f:(fun o -> o.flattenedMapBodyParShape)
  in
  ( hostExprs
  , deviceExprs
  , ParallelismShape.max hostParShapes
  , flattenedMapBodies
  , ParallelismShape.max flattenedMapBodiesParShape )
;;

let kernelize (expr : Nested.t) : (CompilerState.state, Corn.t, _) State.t =
  let open KernelizeState.Let_syntax in
  let%map opts = getOpts expr in
  opts.hostExpr
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nested.t
  type output = Corn.t
  type error = (SB.source option, string) Source.annotate

  let name = "Kernelize"

  let run input =
    CompilerPipeline.S.make ~f:(fun state -> State.run (kernelize input) state)
  ;;
end
