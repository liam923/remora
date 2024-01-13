open! Base
open Nested

module TupleRequest = struct
  module T = struct
    type collectionType =
      | Array of Type.array
      | Sigma of Type.sigma
    [@@deriving compare, sexp_of, eq]

    type deref =
      { i : int
      ; rest : t
      }

    and t =
      | Element of deref
      | Elements of deref list
      | Whole
      | Collection of
          { subRequest : t
          ; collectionType : collectionType
          }
    [@@deriving compare, sexp_of]
  end

  include Comparator.Make (T)
  include T

  let isWhole = function
    | Whole -> true
    | _ -> false
  ;;

  let unexpected ~actual ~expected =
    let actualStr = actual |> sexp_of_t |> Sexp.to_string in
    Unreachable.Error [%string "Expected %{expected} request type, got %{actualStr}"]
  ;;
end

type 't reduceTupleResult =
  { res : 't
  ; droppedAny : bool
  }

module ReduceTupleState = struct
  include State

  type cache =
    | TupleCache of
        { bindings : Identifier.t list
        ; subCaches : cache Map.M(Int).t
        }
    | CollectionCache of
        { subCache : cache
        ; collectionType : TupleRequest.collectionType
        }
  [@@deriving sexp_of]

  type state =
    { compilerState : CompilerState.state
    ; caches : cache Map.M(Identifier).t
    ; droppedAny : bool
    }

  type ('t, 'e) u = (state, 't, 'e) t

  let getCaches () = get () >>| fun state -> state.caches
  let setCaches caches = get () >>= fun state -> set { state with caches }
  let updateCaches ~f = getCaches () >>= fun caches -> setCaches (f caches)
  let markDrop () = get () >>= fun state -> set { state with droppedAny = true }

  let createId name =
    make ~f:(fun state ->
      State.run
        (Identifier.create
           name
           ~getCounter:(fun (s : state) -> s.compilerState.idCounter)
           ~setCounter:(fun (s : state) idCounter ->
             { s with compilerState = { s.compilerState with idCounter } }))
        state)
  ;;

  let toCompilerState (s : ('t, _) u)
    : (CompilerState.state, 't reduceTupleResult, _) State.t
    =
    State.make ~f:(fun compilerState ->
      let state =
        { compilerState; caches = Map.empty (module Identifier); droppedAny = false }
      in
      let state, res = run s state in
      state.compilerState, { res; droppedAny = state.droppedAny })
  ;;
end

let rec reduceTuplesType (request : TupleRequest.t) : Type.t -> Type.t = function
  | Array array -> Array (reduceTuplesArrayType request array)
  | Tuple elements as t ->
    (match request with
     | Whole -> t
     | Element { i; rest } ->
       let element = List.nth_exn elements i in
       reduceTuplesType rest element
     | Elements elementRequests ->
       Tuple
         (List.map elementRequests ~f:(fun { i; rest } ->
            let element = List.nth_exn elements i in
            reduceTuplesType rest element))
     | Collection _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"tuple"))
  | Literal _ as lit ->
    assert (TupleRequest.isWhole request);
    lit
  | Sigma sigma -> Sigma (reduceTuplesSigmaType request sigma)

and reduceTuplesArrayType (request : TupleRequest.t) ({ element; size } as t : Type.array)
  : Type.array
  =
  match request with
  | Collection { subRequest; collectionType = Array { element = _; size = requestSize } }
    ->
    assert ([%equal: Index.shapeElement] size requestSize);
    { element = reduceTuplesType subRequest element; size }
  | Whole -> t
  | Element _ | Elements _ | Collection { subRequest = _; collectionType = Sigma _ } ->
    raise (TupleRequest.unexpected ~actual:request ~expected:"array")

and reduceTuplesSigmaType (request : TupleRequest.t) ({ parameters; body } : Type.sigma)
  : Type.sigma
  =
  match request with
  | Collection { subRequest; collectionType = Sigma requestSigma } ->
    assert ([%equal: Type.sigma] { parameters; body } requestSigma);
    let body = reduceTuplesType subRequest body in
    { parameters; body }
  | Whole -> { parameters; body }
  | Element _ | Elements _ | Collection { subRequest = _; collectionType = Array _ } ->
    raise (TupleRequest.unexpected ~actual:request ~expected:"sigma")
;;

let rec createUnpackersFromCache
  (cache : ReduceTupleState.cache)
  (derefStack : [ `IntDeref of int | `CollectionUnzip ] list)
  ~(insideWhole : bool)
  : (Expr.t -> Expr.letArg) list
  =
  match cache with
  | TupleCache { bindings; subCaches } ->
    let insideWhole = insideWhole || not (List.is_empty bindings) in
    let refUnpacker =
      List.map bindings ~f:(fun id ->
        let rec deref value = function
          | `IntDeref index :: rest ->
            Expr.tupleDeref ~tuple:(Expr.unzip (deref value rest)) ~index
          | `CollectionUnzip :: rest -> deref value rest
          | [] -> value
        in
        fun masterRef -> Expr.{ binding = id; value = deref masterRef derefStack })
    in
    let subCacheList =
      subCaches
      |> Map.to_alist
      |> List.sort ~compare:(fun (ai, _) (bi, _) -> Int.compare ai bi)
    in
    let subUnpackers =
      if insideWhole
      then
        List.bind subCacheList ~f:(fun (index, subCache) ->
          createUnpackersFromCache subCache (`IntDeref index :: derefStack) ~insideWhole)
      else (
        match subCacheList with
        | [ (_, subCache) ] when not insideWhole ->
          (* if not inside a whole and there's only one element,
             the tuple is un-nested *)
          createUnpackersFromCache subCache derefStack ~insideWhole
        | subCaches ->
          subCaches
          |> List.mapi ~f:(fun index (_, subCache) ->
            createUnpackersFromCache subCache (`IntDeref index :: derefStack) ~insideWhole)
          |> List.concat)
    in
    refUnpacker @ subUnpackers
  | CollectionCache { subCache; collectionType = _ } ->
    createUnpackersFromCache subCache (`CollectionUnzip :: derefStack) ~insideWhole
;;

let rec createRequestFromCache (cache : ReduceTupleState.cache) : TupleRequest.t =
  match cache with
  | TupleCache { bindings = _ :: _; subCaches = _ } -> Whole
  | TupleCache { bindings = []; subCaches } ->
    let subCacheList =
      subCaches
      |> Map.to_alist
      |> List.sort ~compare:(fun (ai, _) (bi, _) -> Int.compare ai bi)
    in
    (match subCacheList with
     | [] -> Elements []
     | [ (i, subCache) ] ->
       let subRequest = createRequestFromCache subCache in
       Element { i; rest = subRequest }
     | _ :: _ :: _ as subCaches ->
       let elementRequests =
         List.map subCaches ~f:(fun (sourceI, subCache) ->
           let subRequest = createRequestFromCache subCache in
           TupleRequest.{ i = sourceI; rest = subRequest })
       in
       Elements elementRequests)
  | CollectionCache { subCache; collectionType } ->
    Collection { subRequest = createRequestFromCache subCache; collectionType }
;;

let rec reduceTuplesInExpr (request : TupleRequest.t) expr =
  let open ReduceTupleState.Let_syntax in
  match expr with
  | Expr.Ref { id = masterId; type' } ->
    let%bind caches = ReduceTupleState.getCaches () in
    let cache = Map.find caches masterId in
    let%bind value, cache =
      let rec resolveRequest
        (cache : ReduceTupleState.cache option)
        (request : TupleRequest.t)
        (elementType : Type.t)
        (typeCollectionWrapper : Type.t -> Type.t)
        (zipDepth : int)
        derefStack
        : (Expr.t * ReduceTupleState.cache, _) ReduceTupleState.u
        =
        let createId () =
          let derefString =
            derefStack
            |> List.rev
            |> List.map ~f:(fun i -> [%string "[%{i#Int}]"])
            |> String.concat
          in
          let varName = [%string "%{Identifier.name masterId}%{derefString}"] in
          ReduceTupleState.createId varName
        in
        match cache with
        | Some (TupleCache cache) ->
          (match request with
           | Whole ->
             let%map id = createId () in
             ( Expr.Ref { id; type' = typeCollectionWrapper elementType }
             , ReduceTupleState.TupleCache { cache with bindings = id :: cache.bindings }
             )
           | Element { i; rest = subRequest } ->
             let subCache = Map.find cache.subCaches i in
             let subType = reduceTuplesType (Element { i; rest = Whole }) elementType in
             let%map value, subCache =
               resolveRequest
                 subCache
                 subRequest
                 subType
                 typeCollectionWrapper
                 zipDepth
                 (i :: derefStack)
             in
             ( value
             , ReduceTupleState.TupleCache
                 { cache with subCaches = Map.set cache.subCaches ~key:i ~data:subCache }
             )
           | Elements elementRequests ->
             let%map elementsRev, cache =
               List.fold
                 elementRequests
                 ~init:(return ([], ReduceTupleState.TupleCache cache))
                 ~f:(fun curr nextRequest ->
                   let%bind currElementArrays, prevCache = curr in
                   let%map nextElementArray, nextCache =
                     resolveRequest
                       (Some prevCache)
                       (Element nextRequest)
                       elementType
                       typeCollectionWrapper
                       zipDepth
                       derefStack
                   in
                   nextElementArray :: currElementArrays, nextCache)
             in
             let elements = List.rev elementsRev in
             let tupleType =
               match elementType with
               | Tuple tuple -> tuple
               | _ -> raise (Unreachable.Error "Expected tuple type")
             in
             let stripCollections n type' =
               if n = 0
               then type'
               else (
                 match type' with
                 | Type.Array { element; size = _ } -> element
                 | Type.Sigma { parameters = _; body } -> body
                 | _ -> raise @@ Unimplemented.Error "Expected collection type")
             in
             let tuple = Expr.Values { elements; type' = tupleType } in
             let zippedTuple =
               Expr.Zip
                 { zipArg = tuple
                 ; nestCount = zipDepth
                 ; type' =
                     tupleType
                     |> List.map ~f:(stripCollections zipDepth)
                     |> Type.Tuple
                     |> typeCollectionWrapper
                 }
             in
             zippedTuple, cache
           | _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"tuple"))
        | Some (CollectionCache cache) ->
          let restRequest =
            match request with
            | Collection { subRequest; collectionType } ->
              assert (
                [%equal: TupleRequest.collectionType] collectionType cache.collectionType);
              subRequest
            | Whole -> Whole
            | _ -> raise @@ TupleRequest.unexpected ~actual:request ~expected:"collection"
          in
          let restType, newTypeCollectionWrapper =
            match elementType with
            | Array { element; size } ->
              element, fun t -> typeCollectionWrapper @@ Array { element = t; size }
            | Sigma { parameters; body } ->
              body, fun t -> typeCollectionWrapper @@ Sigma { parameters; body = t }
            | _ -> raise @@ Unreachable.Error "Expected collection type"
          in
          let%map ref, subCache =
            resolveRequest
              (Some cache.subCache)
              restRequest
              restType
              newTypeCollectionWrapper
              (zipDepth + 1)
              derefStack
          in
          ref, ReduceTupleState.CollectionCache { cache with subCache }
        | None ->
          let rec makeEmptyCache (type' : Type.t) =
            match type' with
            | Array { element; size } ->
              ReduceTupleState.CollectionCache
                { subCache = makeEmptyCache element
                ; collectionType = Array { element; size }
                }
            | Sigma { parameters; body } ->
              ReduceTupleState.CollectionCache
                { subCache = makeEmptyCache body
                ; collectionType = Sigma { parameters; body }
                }
            | Literal _ | Tuple _ ->
              ReduceTupleState.TupleCache
                { bindings = []; subCaches = Map.empty (module Int) }
          in
          let cache = makeEmptyCache elementType in
          resolveRequest
            (Some cache)
            request
            elementType
            typeCollectionWrapper
            0
            derefStack
      in
      resolveRequest cache request type' (fun t -> t) 0 []
    in
    let%map () = ReduceTupleState.setCaches (Map.set caches ~key:masterId ~data:cache) in
    value
  | Frame { elements; dimension; type' } ->
    let elementRequest =
      match request with
      | Collection { subRequest; collectionType = Array _ } -> subRequest
      | Whole -> Whole
      | _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"array")
    in
    let%map elements =
      elements |> List.map ~f:(reduceTuplesInExpr elementRequest) |> ReduceTupleState.all
    in
    let type' = reduceTuplesType request type' in
    Expr.Frame { elements; dimension; type' }
  | ReifyIndex _ as reifyIndex ->
    assert (TupleRequest.isWhole request);
    return reifyIndex
  | Append { args; type' } ->
    let%map args =
      args |> List.map ~f:(reduceTuplesInExpr request) |> ReduceTupleState.all
    in
    let type' = reduceTuplesType request type' in
    Expr.Append { args; type' }
  | Box { indices; body; bodyType; type' } ->
    let bodyRequest =
      match request with
      | Collection { subRequest; collectionType = Sigma _ } -> subRequest
      | Whole -> Whole
      | _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"sigma")
    in
    let%map body = reduceTuplesInExpr bodyRequest body in
    let type' = reduceTuplesSigmaType request type' in
    Expr.Box { indices; body; bodyType; type' }
  | Literal _ as literal ->
    assert (TupleRequest.isWhole request);
    return literal
  | TupleDeref { tuple; index; type' = _ } ->
    reduceTuplesInExpr (Element { i = index; rest = request }) tuple
  | ScalarPrimitive { op; args; type' } ->
    let%map args =
      args |> List.map ~f:(reduceTuplesInExpr Whole) |> ReduceTupleState.all
    in
    let type' = reduceTuplesType Whole type' in
    Expr.ScalarPrimitive { op; args; type' }
  | Values { elements; type' } ->
    (match request with
     | Whole ->
       let%map elements =
         elements |> List.map ~f:(reduceTuplesInExpr Whole) |> ReduceTupleState.all
       in
       Expr.Values { elements; type' }
     | Element { i; rest } ->
       (* DISCARD!!! *)
       let value = List.nth_exn elements i in
       let%bind () =
         if List.length elements > 1 then ReduceTupleState.markDrop () else return ()
       in
       reduceTuplesInExpr rest value
     | Elements elementRequests ->
       (* DISCARD!!! *)
       let oldElementCount = List.length elements in
       let%bind elements =
         elementRequests
         |> List.map ~f:(fun { i; rest } ->
           let value = List.nth_exn elements i in
           reduceTuplesInExpr rest value)
         |> ReduceTupleState.all
       in
       let type' = List.map elements ~f:Expr.type' in
       let%map () =
         if List.length elementRequests < oldElementCount
         then ReduceTupleState.markDrop ()
         else return ()
       in
       Expr.Values { elements; type' }
     | Collection _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"tuple"))
  | BoxValue { box; type' } ->
    let boxType =
      match Expr.type' box with
      | Sigma sigma -> sigma
      | _ -> raise (Unreachable.Error "expected sigma type")
    in
    let rec makeRequestOfType type' request =
      match request with
      | TupleRequest.Whole -> TupleRequest.Whole
      | TupleRequest.Element { i; rest } ->
        let derefedType = reduceTuplesType (Element { i; rest = Whole }) type' in
        TupleRequest.Element { i; rest = makeRequestOfType derefedType rest }
      | TupleRequest.Elements elems ->
        TupleRequest.Elements
          (List.map elems ~f:(fun { i; rest } ->
             let derefedType = reduceTuplesType (Element { i; rest = Whole }) type' in
             TupleRequest.{ i; rest = makeRequestOfType derefedType rest }))
      | TupleRequest.Collection { subRequest; collectionType = Array _ } ->
        (match type' with
         | Type.Array { element; size } ->
           TupleRequest.Collection
             { subRequest = makeRequestOfType element subRequest
             ; collectionType = Array { element; size }
             }
         | Type.Sigma _ | Type.Tuple _ | Type.Literal _ ->
           raise @@ Unreachable.Error "expected array type")
      | TupleRequest.Collection { subRequest; collectionType = Sigma _ } ->
        (match type' with
         | Type.Sigma { parameters; body } ->
           TupleRequest.Collection
             { subRequest = makeRequestOfType body subRequest
             ; collectionType = Sigma { parameters; body }
             }
         | Type.Array _ | Type.Tuple _ | Type.Literal _ ->
           raise @@ Unreachable.Error "expected sigma type")
    in
    let%map box =
      reduceTuplesInExpr
        (Collection
           { subRequest = makeRequestOfType boxType.body request
           ; collectionType = Sigma boxType
           })
        box
    in
    let type' = reduceTuplesType request type' in
    Expr.BoxValue { box; type' }
  | SubArray { arrayArg; indexArg; type' } ->
    let argType =
      match Expr.type' arrayArg with
      | Array array -> array
      | _ -> raise (Unreachable.Error "expected array type")
    in
    let%map arrayArg =
      reduceTuplesInExpr
        (Collection { subRequest = request; collectionType = Array argType })
        arrayArg
    and indexArg = reduceTuplesInExpr Whole indexArg in
    let type' = reduceTuplesType request type' in
    Expr.SubArray { arrayArg; indexArg; type' }
  | Zip { zipArg; nestCount; type' } ->
    let rec interchangeCollections nestCount wrapper request =
      if nestCount = 0
      then (
        match request with
        | TupleRequest.Whole -> TupleRequest.Whole
        | TupleRequest.Element { i; rest } ->
          TupleRequest.Element { i; rest = wrapper rest }
        | TupleRequest.Elements derefs ->
          TupleRequest.Elements
            (List.map derefs ~f:(fun { i; rest } ->
               TupleRequest.{ i; rest = wrapper rest }))
        | _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"tuple"))
      else (
        match request with
        | TupleRequest.Whole -> TupleRequest.Whole
        | TupleRequest.Collection { subRequest; collectionType } ->
          interchangeCollections
            (nestCount - 1)
            (fun r -> wrapper @@ Collection { subRequest = r; collectionType })
            subRequest
        | TupleRequest.Element _ | TupleRequest.Elements _ ->
          raise (TupleRequest.unexpected ~actual:request ~expected:"collection"))
    in
    let%map zipArg =
      reduceTuplesInExpr (interchangeCollections nestCount (fun r -> r) request) zipArg
    in
    Expr.Zip { zipArg; nestCount; type' }
  | Unzip { unzipArg; type' } ->
    let nestCount, collectionRequestWrapper =
      let rec unzipType = function
        | Type.Tuple _ -> 0, fun r -> r
        | Type.Array { element; size } ->
          let subCount, subWrapper = unzipType element in
          ( subCount + 1
          , fun r ->
              TupleRequest.Collection
                { subRequest = subWrapper r; collectionType = Array { element; size } } )
        | Type.Sigma { parameters; body } ->
          let subCount, subWrapper = unzipType body in
          ( subCount + 1
          , fun r ->
              TupleRequest.Collection
                { subRequest = subWrapper r; collectionType = Sigma { parameters; body } }
          )
        | Type.Literal _ -> raise (Unreachable.Error "Unexpected literal type")
      in
      unzipType (Expr.type' unzipArg)
    in
    let rec stripCollections nestCount request =
      if nestCount = 0
      then request
      else (
        match request with
        | TupleRequest.Collection { subRequest; collectionType = _ } ->
          stripCollections (nestCount - 1) subRequest
        | TupleRequest.Whole -> Whole
        | _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"collection"))
    in
    (match request with
     | Whole ->
       let%map unzipArg = reduceTuplesInExpr Whole unzipArg in
       Expr.Unzip { unzipArg; type' }
     | Element { i; rest } ->
       let request =
         collectionRequestWrapper (Element { i; rest = stripCollections nestCount rest })
       in
       reduceTuplesInExpr request unzipArg
     | Elements elementRequests ->
       let unzipArgRequest =
         collectionRequestWrapper
           (Elements
              (List.map elementRequests ~f:(fun { i; rest } ->
                 TupleRequest.{ i; rest = stripCollections nestCount rest })))
       in
       let%map unzipArg = reduceTuplesInExpr unzipArgRequest unzipArg in
       let type' =
         match reduceTuplesType request (Tuple type') with
         | Tuple tuple -> tuple
         | _ -> raise (Unreachable.Error "expected tuple type")
       in
       Expr.Unzip { unzipArg; type' }
     | Collection _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"tuple"))
  | IndexLet { indexArgs; body; type' } ->
    let%map body = reduceTuplesInExpr request body in
    Expr.IndexLet { indexArgs; body; type' }
  | Let { args; body; type' } ->
    let%bind body = reduceTuplesInExpr request body in
    let type' = reduceTuplesType request type' in
    let%bind caches = ReduceTupleState.getCaches () in
    let%bind args, unpackerss =
      args
      |> List.map ~f:(fun { binding; value } ->
        Map.find caches binding
        |> Option.map ~f:(fun cache ->
          let request = createRequestFromCache cache in
          let unpackersRaw = createUnpackersFromCache cache [] ~insideWhole:false in
          let%map value = reduceTuplesInExpr request value in
          let ref = Expr.Ref { id = binding; type' = Expr.type' value } in
          let unpackers = List.map unpackersRaw ~f:(fun unpacker -> unpacker ref) in
          Expr.{ binding; value }, unpackers))
      |> List.filter_opt
      |> ReduceTupleState.all
      >>| List.unzip
    in
    let%map () =
      ReduceTupleState.updateCaches ~f:(fun caches ->
        args
        |> List.map ~f:(fun arg -> arg.binding)
        |> List.fold ~init:caches ~f:Map.remove)
    in
    let unpackers = List.bind unpackerss ~f:(fun e -> e) in
    Expr.Let { args; body = Let { args = unpackers; body; type' }; type' }
  | LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; consumer
      ; type' = _
      } ->
    let mapRequest, consumerRequest, wrapResult =
      let wrapperForPair block ~mapWrapper ~consumerWrapper =
        let%map blockBinding = ReduceTupleState.createId "loop-block-result" in
        let blockRef = Expr.Ref { id = blockBinding; type' = Expr.type' block } in
        Expr.let'
          ~args:[ { binding = blockBinding; value = block } ]
          ~body:
            (Expr.values
               [ mapWrapper @@ Expr.tupleDeref ~tuple:blockRef ~index:0
               ; consumerWrapper @@ Expr.tupleDeref ~tuple:blockRef ~index:1
               ])
      in
      let wrapperForMapOnly block ~mapWrapper ~consumerWrapper:_ =
        let mapValue = Expr.tupleDeref ~tuple:block ~index:0 in
        let%map mapBinding = ReduceTupleState.createId "map-result" in
        let mapRef = Expr.Ref { id = mapBinding; type' = Expr.type' mapValue } in
        Expr.let'
          ~args:[ { binding = mapBinding; value = mapValue } ]
          ~body:(mapWrapper mapRef)
      in
      let wrapperForConsumerOnly block ~mapWrapper:_ ~consumerWrapper =
        let consumerValue = Expr.tupleDeref ~tuple:block ~index:1 in
        let%map consumerBinding = ReduceTupleState.createId "consumer-result" in
        let consumerRef =
          Expr.Ref { id = consumerBinding; type' = Expr.type' consumerValue }
        in
        Expr.let'
          ~args:[ { binding = consumerBinding; value = consumerValue } ]
          ~body:(consumerWrapper consumerRef)
      in
      let wrapperForUnit _ ~mapWrapper:_ ~consumerWrapper:_ = return @@ Expr.values [] in
      match request with
      | Whole -> TupleRequest.Whole, TupleRequest.Whole, wrapperForPair
      | Element { i = 0; rest = mapRequest } ->
        mapRequest, TupleRequest.Elements [], wrapperForMapOnly
      | Element { i = 1; rest = consumerRequest } ->
        TupleRequest.Elements [], consumerRequest, wrapperForConsumerOnly
      | Element { i = _; rest = _ } ->
        raise (Unreachable.Error "invalid tuple index of loop block")
      | Elements [] -> TupleRequest.Elements [], TupleRequest.Elements [], wrapperForUnit
      | Elements [ { i = 0; rest = mapRequest } ] ->
        mapRequest, TupleRequest.Elements [], wrapperForMapOnly
      | Elements [ { i = 1; rest = consumerRequest } ] ->
        TupleRequest.Elements [], consumerRequest, wrapperForConsumerOnly
      | Elements [ { i = 0; rest = mapRequest }; { i = 1; rest = consumerRequest } ] ->
        mapRequest, consumerRequest, wrapperForPair
      | Elements _ -> raise (Unreachable.Error "invalid tuple indices of loop block")
      | Collection _ -> raise (TupleRequest.unexpected ~actual:request ~expected:"tuple")
    in
    let%bind consumerUsages, consumer =
      match consumer, consumerRequest with
      | _, Elements [] | None, _ -> return (Set.empty (module Identifier), None)
      | Some (Reduce { arg; zero; d; body; associative; character; type' }), _ ->
        let%bind zero =
          zero |> Option.map ~f:(reduceTuplesInExpr Whole) |> ReduceTupleState.all_opt
        in
        let rec getUsagesInProductionTuple = function
          | Expr.ProductionTuple t ->
            t.elements
            |> List.map ~f:getUsagesInProductionTuple
            |> Set.union_list (module Identifier)
          | Expr.ProductionTupleAtom p -> Set.singleton (module Identifier) p.productionId
        in
        let usages = getUsagesInProductionTuple arg.production in
        let%bind body = reduceTuplesInExpr Whole body in
        let%bind caches = ReduceTupleState.getCaches () in
        let unpackers =
          [ arg.firstBinding; arg.secondBinding ]
          |> List.map ~f:(fun binding ->
            Map.find caches binding
            |> Option.map ~f:(fun cache ->
              let unpackersRaw = createUnpackersFromCache cache [] ~insideWhole:true in
              let ref =
                Expr.Ref { id = binding; type' = Expr.productionTupleType arg.production }
              in
              let unpackers = List.map unpackersRaw ~f:(fun unpacker -> unpacker ref) in
              unpackers))
          |> List.filter_opt
          |> List.concat
        in
        let%map () =
          ReduceTupleState.setCaches
            (List.fold [ arg.firstBinding; arg.secondBinding ] ~init:caches ~f:Map.remove)
        in
        let reduce =
          Expr.Reduce
            { arg
            ; zero
            ; body = Expr.let' ~args:unpackers ~body
            ; d
            ; associative
            ; character
            ; type'
            }
        in
        usages, Some reduce
      | Some (Fold { zeroArg; arrayArgs; body; d; character; type' }), _ ->
        let%bind body = reduceTuplesInExpr Whole body in
        let%bind caches = ReduceTupleState.getCaches () in
        let bindings =
          (zeroArg.zeroBinding, Expr.type' zeroArg.zeroValue)
          :: List.map arrayArgs ~f:(fun { binding; production = _ } ->
            let argType =
              match raise Unimplemented.default (* production.type' *) with
              | Type.Array { element; size = _ } -> element
              | _ -> raise @@ Unreachable.Error "expected array type"
            in
            binding, argType)
        in
        let unpackers =
          bindings
          |> List.map ~f:(fun (binding, type') ->
            Map.find caches binding
            |> Option.map ~f:(fun cache ->
              let unpackersRaw = createUnpackersFromCache cache [] ~insideWhole:true in
              let ref = Expr.Ref { id = binding; type' } in
              let unpackers = List.map unpackersRaw ~f:(fun unpacker -> unpacker ref) in
              unpackers))
          |> List.filter_opt
          |> List.concat
        in
        let%map () =
          ReduceTupleState.updateCaches ~f:(fun caches ->
            bindings
            |> List.map ~f:(fun (binding, _) -> binding)
            |> List.fold ~init:caches ~f:Map.remove)
        in
        let fold =
          Expr.Fold
            { zeroArg
            ; arrayArgs
            ; body = Expr.let' ~args:unpackers ~body
            ; d
            ; character
            ; type'
            }
        in
        let usages =
          arrayArgs
          |> List.map ~f:(fun arg -> arg.production.productionId)
          |> Set.of_list (module Identifier)
        in
        usages, Some fold
      | ( (Some (Scatter { valuesArg; indicesArg; dIn = _; dOut = _; type' = _ }) as
           scatter)
        , _ ) ->
        let usages =
          Set.of_list
            (module Identifier)
            [ valuesArg.productionId; indicesArg.productionId ]
        in
        return (usages, scatter)
    in
    let consumerWrapper e = e in
    let mapBodyRequest, mapWrapper, mapBodyMatcher, mapResults =
      let resultIdsAndRequests, mapWrapper =
        let rec extractFromConstExpr nestCount typeWrapper request constExpr =
          match request with
          | TupleRequest.Whole -> constExpr
          | TupleRequest.Element { i; rest } ->
            Expr.tupleDeref
              ~tuple:(Expr.unzip (extractFromConstExpr 0 (fun t -> t) rest constExpr))
              ~index:i
          | TupleRequest.Elements derefs ->
            let values =
              List.map derefs ~f:(fun { i; rest } ->
                Expr.tupleDeref
                  ~tuple:(Expr.unzip (extractFromConstExpr 0 (fun t -> t) rest constExpr))
                  ~index:i)
            in
            let types =
              List.map values ~f:(fun value ->
                let rec getType n type' =
                  if n = 0
                  then type'
                  else (
                    match type' with
                    | Type.Array array -> getType (n - 1) array.element
                    | Type.Sigma sigma -> getType (n - 1) sigma.body
                    | _ -> raise @@ Unreachable.Error "expected collection type")
                in
                getType nestCount @@ Expr.type' value)
            in
            Expr.Zip
              { zipArg = Expr.values values
              ; nestCount
              ; type' = typeWrapper @@ Type.Tuple types
              }
          | TupleRequest.Collection { subRequest; collectionType } ->
            extractFromConstExpr
              (nestCount + 1)
              (fun t ->
                let innerType =
                  match collectionType with
                  | Array arr -> Type.Array { element = t; size = arr.size }
                  | Sigma sigma -> Type.Sigma { parameters = sigma.parameters; body = t }
                in
                typeWrapper innerType)
              subRequest
              constExpr
        in
        let getForDeref TupleRequest.{ i; rest } =
          let subRequest =
            match rest with
            | Whole -> TupleRequest.Whole
            | Collection { subRequest; collectionType = _ } -> subRequest
            | _ -> raise @@ TupleRequest.unexpected ~actual:rest ~expected:"collection"
          in
          let resultId = List.nth_exn mapResults i in
          if Set.mem consumerUsages resultId
          then (resultId, TupleRequest.Whole), extractFromConstExpr 0 (fun t -> t) rest
          else (resultId, subRequest), fun e -> e
        in
        match mapRequest with
        | Whole ->
          ( List.map mapResults ~f:(fun resultId -> resultId, TupleRequest.Whole)
          , fun e -> e )
        | Element deref ->
          let resultIdAndRequest, extractor = getForDeref deref in
          let mapWrapper mapResult =
            extractor @@ Expr.tupleDeref ~tuple:mapResult ~index:0
          in
          [ resultIdAndRequest ], mapWrapper
        | Elements derefs ->
          let resultIdsAndRequest, extractors =
            derefs |> List.map ~f:getForDeref |> List.unzip
          in
          let mapWrapper mapResult =
            extractors
            |> List.mapi ~f:(fun i extractor ->
              extractor @@ Expr.tupleDeref ~tuple:mapResult ~index:i)
            |> Expr.values
          in
          resultIdsAndRequest, mapWrapper
        | Collection _ ->
          raise @@ TupleRequest.unexpected ~actual:mapRequest ~expected:"tuple"
      in
      let mapResults, _ = List.unzip resultIdsAndRequests in
      let resultRequestsFromMap =
        List.map resultIdsAndRequests ~f:(fun (resultId, request) -> resultId, request)
      in
      let resultRequestsFromConsumer =
        consumerUsages
        |> Set.to_list
        |> List.map ~f:(fun resultId -> resultId, TupleRequest.Whole)
      in
      let resultRequests =
        Map.of_alist_reduce
          (module Identifier)
          (resultRequestsFromMap @ resultRequestsFromConsumer)
          ~f:(fun a _ -> a)
      in
      let rec makeMapBodyRequestAndMatcher (oldMatcher : Expr.tupleMatch)
        : TupleRequest.t * Expr.tupleMatch * bool
        =
        match oldMatcher with
        | Binding resultId ->
          (match Map.find resultRequests resultId with
           | Some request -> request, Binding resultId, true
           | None -> Whole, Binding resultId, false)
        | Unpack matchers ->
          let requestsAndMatchers =
            List.filter_mapi matchers ~f:(fun i matcher ->
              let request, subMatcher, used = makeMapBodyRequestAndMatcher matcher in
              if used then Some (i, request, subMatcher) else None)
          in
          (match requestsAndMatchers with
           | [] -> Elements [], Unpack [], false
           | [ (i, request, matcher) ] -> Element { i; rest = request }, matcher, true
           | _ ->
             let elementsRequests, matchers =
               requestsAndMatchers
               |> List.map ~f:(fun (i, request, matcher) ->
                 TupleRequest.{ i; rest = request }, matcher)
               |> List.unzip
             in
             Elements elementsRequests, Unpack matchers, true)
      in
      let mapBodyRequest, mapBodyMatcher, _ =
        makeMapBodyRequestAndMatcher mapBodyMatcher
      in
      mapBodyRequest, mapWrapper, mapBodyMatcher, mapResults
    in
    let%bind mapArgs, mapBody, blockUnpackers =
      let%bind body = reduceTuplesInExpr mapBodyRequest mapBody in
      let%bind caches = ReduceTupleState.getCaches () in
      let%bind args, argUnpackerss, blockUnpackers =
        mapArgs
        |> List.map ~f:(fun { binding; ref } ->
          Map.find caches binding
          |> Option.map ~f:(fun cache ->
            let argArrayType =
              match ref.type' with
              | Array array -> array
              | _ -> raise @@ Unreachable.Error "expected array type"
            in
            let unpackersRaw = createUnpackersFromCache cache [] ~insideWhole:false in
            let argRef = Expr.Ref { id = binding; type' = argArrayType.element } in
            let unpackers = List.map unpackersRaw ~f:(fun unpacker -> unpacker argRef) in
            let argRequest =
              TupleRequest.Collection
                { subRequest = createRequestFromCache cache
                ; collectionType = Array argArrayType
                }
            in
            let%map value = reduceTuplesInExpr argRequest (Expr.Ref ref)
            and valueBinding = ReduceTupleState.createId (Identifier.name ref.id) in
            let valueRef : Expr.ref = { id = valueBinding; type' = Expr.type' value } in
            let valueUnpacker : Expr.letArg = { binding = valueBinding; value } in
            Expr.{ binding; ref = valueRef }, unpackers, valueUnpacker))
        |> List.filter_opt
        |> ReduceTupleState.all
        >>| List.unzip3
      in
      let argUnpackers = List.concat argUnpackerss in
      let iotaUnpackers =
        mapIotas
        |> List.map ~f:(fun { iota; nestIn = _ } ->
          Map.find caches iota
          |> Option.map ~f:(fun cache ->
            createUnpackersFromCache cache [] ~insideWhole:false
            |> List.map ~f:(fun unpacker ->
              unpacker (Ref { id = iota; type' = Literal IntLiteral }))))
        |> List.filter_opt
        |> List.concat
      in
      let unpackers = argUnpackers @ iotaUnpackers in
      (* Remove the caches for variables bound only in the map *)
      let%map () =
        ReduceTupleState.updateCaches ~f:(fun caches ->
          List.map mapArgs ~f:(fun arg -> arg.binding)
          @ List.map mapIotas ~f:(fun i -> i.iota)
          |> List.fold ~init:caches ~f:Map.remove)
      in
      args, Expr.let' ~args:unpackers ~body, blockUnpackers
    in
    let rec extractTypesFromTupleType (matcher : Expr.tupleMatch) type' =
      match matcher with
      | Binding id -> [ id, type' ]
      | Unpack matchers ->
        (match type' with
         | Type.Tuple types ->
           List.zip_exn matchers types
           |> List.bind ~f:(fun (matcher, type') ->
             extractTypesFromTupleType matcher type')
         | _ -> raise @@ Unreachable.Error "expected tuple type")
    in
    let resultElementTypes =
      extractTypesFromTupleType mapBodyMatcher (Expr.type' mapBody)
    in
    let resultTypes =
      resultElementTypes
      |> List.map ~f:(fun (id, element) -> id, Type.Array { element; size = frameShape })
      |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
    in
    let mapResultType =
      Type.Tuple
        (List.map mapResults ~f:(fun resultId -> Map.find_exn resultTypes resultId))
    in
    let block =
      Expr.let'
        ~args:blockUnpackers
        ~body:
          (Expr.LoopBlock
             { frameShape
             ; mapArgs
             ; mapIotas
             ; mapBody
             ; mapBodyMatcher
             ; mapResults
             ; consumer
             ; type' =
                 [ mapResultType
                 ; consumer
                   |> Option.map ~f:Expr.consumerOpType
                   |> Option.value ~default:(Type.Tuple [])
                 ]
             })
    in
    wrapResult block ~mapWrapper ~consumerWrapper
;;

let reduceTuples expr = reduceTuplesInExpr Whole expr |> ReduceTupleState.toCompilerState
