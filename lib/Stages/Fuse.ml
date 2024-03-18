open! Base
open Nested

type 'a fusionResult =
  { result : 'a
  ; fusedAny : bool
  }

module FuseState = struct
  include State

  type state =
    { compilerState : CompilerState.state
    ; fusedAny : bool
    }

  type ('t, 'e) u = (state, 't, 'e) t

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

  let markFusion = State.modify ~f:(fun state -> { state with fusedAny = true })

  let asCompilerState program =
    State.make ~f:(fun compilerState ->
      let inState = { compilerState; fusedAny = false } in
      let outState, result = run program inState in
      outState.compilerState, { result; fusedAny = outState.fusedAny })
  ;;
end

module ConsumerCompatibility = struct
  type structure =
    | Reduce of { character : Expr.reduceCharacter }
    | Fold of
        { character : Expr.foldCharacter
        ; reverse : bool
        }
  [@@deriving equal]

  type t =
    | Incompatible
    | SometimesCompatible of structure

  let of_op : Expr.consumerOp -> t = function
    | Reduce { arg = _; zero = _; body = _; d = _; character; type' = _ } ->
      SometimesCompatible (Reduce { character })
    | Fold { zeroArg = _; arrayArgs = _; body = _; reverse; d = _; character; type' = _ }
      -> SometimesCompatible (Fold { character; reverse })
    | Scatter { valuesArg = _; indicesArg = _; dIn = _; dOut = _; type' = _ } ->
      Incompatible
  ;;

  let isCompatible a b =
    match a, b with
    | SometimesCompatible a, SometimesCompatible b -> [%equal: structure] a b
    | Incompatible, _ | _, Incompatible -> false
  ;;
end

let rec getUsesInIndex : Index.t -> Set.M(Identifier).t = function
  | Shape elements ->
    elements
    |> List.map ~f:(function
      | Add dim -> getUsesInIndex (Dimension dim)
      | ShapeRef ref -> Set.singleton (module Identifier) ref)
    |> Set.union_list (module Identifier)
  | Dimension { const = _; refs } -> Map.keys refs |> Set.of_list (module Identifier)
;;

let rec getUsesInType : Type.t -> Set.M(Identifier).t = function
  | Array { element; size } ->
    Set.union (getUsesInType element) (getUsesInIndex (Shape [ size ]))
  | Sigma sigma -> getUsesInSigma sigma
  | Tuple tuple -> getUsesInTuple tuple
  | Literal _ -> Set.empty (module Identifier)

and getUsesInSigma ({ parameters = _; body } : Type.sigma) = getUsesInType body

and getUsesInTuple elements =
  elements |> List.map ~f:getUsesInType |> Set.union_list (module Identifier)
;;

let rec getUsesInExpr : Expr.t -> Set.M(Identifier).t = function
  | Ref { id; type' } -> Set.add (getUsesInType type') id
  | Frame { dimension = _; elements; type' } ->
    Set.union
      (elements |> List.map ~f:getUsesInExpr |> Set.union_list (module Identifier))
      (getUsesInType type')
  | BoxValue { box; type' } -> Set.union (getUsesInExpr box) (getUsesInType type')
  | IndexLet { indexArgs; body; type' } ->
    let argsUsages =
      List.map indexArgs ~f:(fun arg ->
        match arg.indexValue with
        | Runtime value -> getUsesInExpr value
        | FromBox { box; i = _ } -> getUsesInExpr box)
    in
    let bindings =
      indexArgs
      |> List.map ~f:(fun arg -> arg.indexBinding)
      |> Set.of_list (module Identifier)
    in
    let bodyUsages = Set.diff (getUsesInExpr body) bindings in
    let typeUsages = getUsesInType type' in
    Set.union_list (module Identifier) (typeUsages :: bodyUsages :: argsUsages)
  | ReifyIndex { index; type' } -> Set.union (getUsesInIndex index) (getUsesInType type')
  | ShapeProd shape -> getUsesInIndex @@ Shape shape
  | Let { args; body; type' } ->
    let argsUsages = List.map args ~f:(fun arg -> getUsesInExpr arg.value) in
    let bindings =
      args |> List.map ~f:(fun arg -> arg.binding) |> Set.of_list (module Identifier)
    in
    let bodyUsages = Set.diff (getUsesInExpr body) bindings in
    let typeUsages = getUsesInType type' in
    Set.union_list (module Identifier) (typeUsages :: bodyUsages :: argsUsages)
  | LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher = _
      ; mapResults = _
      ; consumer
      ; type'
      } ->
    let frameShapeUsages = getUsesInIndex (Shape [ frameShape ]) in
    let argsUsages =
      mapArgs
      |> List.map ~f:(fun arg -> getUsesInExpr (Ref arg.ref))
      |> Set.union_list (module Identifier)
    in
    let argBindings =
      mapArgs |> List.map ~f:(fun arg -> arg.binding) |> Set.of_list (module Identifier)
    in
    let iotaBindings = Set.of_list (module Identifier) mapIotas in
    let bodyUsages =
      Set.diff (getUsesInExpr mapBody) (Set.union argBindings iotaBindings)
    in
    let consumerUsages = getUsesInConsumer consumer in
    let typeUsages = getUsesInTuple type' in
    Set.union_list
      (module Identifier)
      [ frameShapeUsages; argsUsages; bodyUsages; consumerUsages; typeUsages ]
  | Box { indices; body; bodyType; type' } ->
    let indicesUsages = List.map indices ~f:getUsesInIndex in
    let bodyUsages = getUsesInExpr body in
    let bodyTypeUsages = getUsesInType bodyType in
    let typeUsages = getUsesInSigma type' in
    Set.union_list
      (module Identifier)
      (bodyUsages :: bodyTypeUsages :: typeUsages :: indicesUsages)
  | Literal _ -> Set.empty (module Identifier)
  | Values { elements; type' } ->
    Set.union
      (elements |> List.map ~f:getUsesInExpr |> Set.union_list (module Identifier))
      (getUsesInTuple type')
  | TupleDeref { tuple; index = _; type' } ->
    Set.union (getUsesInExpr tuple) (getUsesInType type')
  | ScalarPrimitive { op = _; args; type' } ->
    Set.union
      (args |> List.map ~f:getUsesInExpr |> Set.union_list (module Identifier))
      (getUsesInType type')
  | ContiguousSubArray { arrayArg; indexArg; originalShape; resultShape; type' } ->
    Set.union_list
      (module Identifier)
      [ getUsesInExpr arrayArg
      ; getUsesInExpr indexArg
      ; getUsesInType type'
      ; getUsesInIndex (Shape originalShape)
      ; getUsesInIndex (Shape resultShape)
      ]
  | Append { args; type' } ->
    Set.union
      (args |> List.map ~f:getUsesInExpr |> Set.union_list (module Identifier))
      (getUsesInType type')
  | Zip { zipArg; nestCount = _; type' } ->
    Set.union (getUsesInExpr zipArg) (getUsesInType type')
  | Unzip { unzipArg; type' } ->
    Set.union (getUsesInExpr unzipArg) (getUsesInType (Tuple type'))

and getUsesInConsumer consumer =
  match consumer with
  | Some (Reduce { arg; zero; body; d; character = _; type' }) ->
    let argBindings =
      Set.of_list (module Identifier) [ arg.firstBinding; arg.secondBinding ]
    in
    Set.union_list
      (module Identifier)
      [ getUsesInExpr zero
      ; Set.diff (getUsesInExpr body) argBindings
      ; getUsesInIndex (Dimension d)
      ; getUsesInType type'
      ]
  | Some (Fold { zeroArg; arrayArgs; body; reverse = _; d; character = _; type' }) ->
    let arrayBindings =
      arrayArgs |> List.map ~f:(fun arg -> arg.binding) |> Set.of_list (module Identifier)
    in
    Set.union_list
      (module Identifier)
      [ Set.diff (getUsesInExpr body) (Set.add arrayBindings zeroArg.zeroBinding)
      ; getUsesInIndex (Dimension d)
      ; getUsesInType type'
      ]
  | Some (Scatter { valuesArg = _; indicesArg = _; dIn; dOut; type' }) ->
    Set.union_list
      (module Identifier)
      [ getUsesInIndex (Dimension dIn)
      ; getUsesInIndex (Dimension dOut)
      ; getUsesInType type'
      ]
  | None -> Set.empty (module Identifier)
;;

(** Represents a value that contains the result of a map.
    Value

    Represents a location of a value inside nested tuples, which itself can
    be tuple derefed.

    Ex:
    (?, (?, (x[2][0], ?)), ?, x[1][5]) =>
    Tuple [None; Tuple [None; Tuple [Value [0; 2]; None]]; None; Value [5; 1]] *)
type mapValueLocation =
  | Value of int list
      (** represents a result of a map, with possible tuple dereferencing *)
  | Tuple of mapValueLocation option list
      (** Represents a tuple, which may contain mapValues in its elements *)
[@@deriving sexp_of]

let rec getMapValue mapRefs =
  let open Expr in
  function
  | Ref { id; type' = _ } -> Map.find mapRefs id
  | Frame _ -> None
  | BoxValue _ -> None
  | IndexLet { indexArgs = _; body; type' = _ } -> getMapValue mapRefs body
  | ReifyIndex _ -> None
  | ShapeProd _ -> None
  | Let { args; body; type' = _ } ->
    let mapRefs =
      List.fold args ~init:mapRefs ~f:(fun acc { binding; value } ->
        match getMapValue mapRefs value with
        | Some mapValue -> Map.set acc ~key:binding ~data:mapValue
        | None -> acc)
    in
    getMapValue mapRefs body
  | LoopBlock _ -> None
  | Box _ -> None
  | Literal _ -> None
  | Values { elements; type' = _ } ->
    Some (Tuple (List.map elements ~f:(getMapValue mapRefs)))
  | TupleDeref { tuple; index; type' = _ } ->
    (match getMapValue mapRefs tuple with
     | Some (Tuple mapValues) -> List.nth mapValues index |> Option.bind ~f:(fun v -> v)
     | Some (Value derefs) -> Some (Value (index :: derefs))
     | None -> None)
  | ScalarPrimitive _ | ContiguousSubArray _ | Append _ | Zip _ | Unzip _ -> None
;;

type consumerExtraction =
  { op : Expr.consumerOp
  ; consumerBinding : Identifier.t
  }

type extraction =
  { captures : Set.M(Identifier).t
  ; addLifts : Expr.t -> Expr.t
  ; liftedArgs : Expr.mapArg list
  ; liftedIotas : Identifier.t list
  ; liftedBody : Expr.t
  ; liftedBodyMatcher : Expr.tupleMatch
  ; liftedResults : Identifier.t list
  ; archerMapResultBinding : Identifier.t
  ; targetMapResultElementBinding : Identifier.t
  ; consumer : consumerExtraction option
  }

let rec liftFrom
  (target : Expr.loopBlock)
  (targetConsumer : ConsumerCompatibility.t option)
  (capturables : Set.M(Identifier).t)
  (mapRefs : mapValueLocation Map.M(Identifier).t)
  : Expr.t -> (extraction option * Expr.t, _) FuseState.u
  =
  let open FuseState.Let_syntax in
  let open Expr in
  let liftFromList list ~f =
    let%map extraction, revList =
      List.fold
        list
        ~init:(return (None, []))
        ~f:(fun acc e ->
          let%bind extraction, prev = acc in
          match extraction with
          | Some _ as extraction -> return (extraction, e :: prev)
          | None ->
            let%map extraction, e = f e in
            extraction, e :: prev)
    in
    extraction, List.rev revList
  in
  function
  | Frame { dimension; elements; type' } ->
    let%map extraction, elements =
      liftFromList elements ~f:(liftFrom target targetConsumer capturables mapRefs)
    in
    extraction, Frame { dimension; elements; type' }
  | BoxValue { box; type' } ->
    let%map extraction, box = liftFrom target targetConsumer capturables mapRefs box in
    extraction, BoxValue { box; type' }
  | IndexLet { indexArgs; body; type' } ->
    (* Try lifting from the argument values *)
    let%bind extraction, indexArgs =
      liftFromList indexArgs ~f:(fun { indexBinding; indexValue; sort } ->
        match indexValue with
        | Runtime value ->
          let%map extraction, value =
            liftFrom target targetConsumer capturables mapRefs value
          in
          extraction, { indexBinding; indexValue = Runtime value; sort }
        | FromBox { box; i } ->
          let%map extraction, box =
            liftFrom target targetConsumer capturables mapRefs box
          in
          extraction, { indexBinding; indexValue = FromBox { box; i }; sort })
    in
    (match extraction with
     | Some _ as extraction -> return (extraction, IndexLet { indexArgs; body; type' })
     | None ->
       (* Any args that are dependent only on capturable variables are also capturable *)
       let capturableBindings =
         indexArgs
         |> List.filter ~f:(fun arg ->
           let uses =
             match arg.indexValue with
             | Runtime value -> getUsesInExpr value
             | FromBox { box; i = _ } -> getUsesInExpr box
           in
           Set.is_subset uses ~of_:capturables)
         |> List.map ~f:(fun arg -> arg.indexBinding)
         |> Set.of_list (module Identifier)
       in
       let capturables = Set.union capturables capturableBindings in
       (* Lift from body using the extended capturables *)
       let%map extraction, body =
         liftFrom target targetConsumer capturables mapRefs body
       in
       (match extraction with
        | Some extraction ->
          (* The body had an extraction. Any args that the extraction requires
             lifting need to be lifted. *)
          let lifts = Set.inter capturableBindings extraction.captures in
          let liftedIndexArgs, indexArgs =
            List.partition_tf indexArgs ~f:(fun arg -> Set.mem lifts arg.indexBinding)
          in
          let captures = Set.diff extraction.captures lifts in
          let addLifts body =
            let body = extraction.addLifts body in
            IndexLet { indexArgs = liftedIndexArgs; body; type' = Expr.type' body }
          in
          Some { extraction with captures; addLifts }, IndexLet { indexArgs; body; type' }
        | None -> None, IndexLet { indexArgs; body; type' }))
  | Let { args; body; type' } ->
    (* Try lifting from the argument values *)
    let%bind extraction, args =
      liftFromList args ~f:(fun { binding; value } ->
        let%map extraction, value =
          liftFrom target targetConsumer capturables mapRefs value
        in
        extraction, { binding; value })
    in
    (match extraction with
     | Some _ as extraction -> return (extraction, Let { args; body; type' })
     | None ->
       (* If there was nothing to lift from the arg values, try lifting from the body *)
       (* Any args that are dependent only on capturable variables are also capturable *)
       let capturableBindings =
         args
         |> List.filter ~f:(fun { binding = _; value } ->
           Set.is_subset (getUsesInExpr value) ~of_:capturables)
         |> List.map ~f:(fun arg -> arg.binding)
         |> Set.of_list (module Identifier)
       in
       let capturables = Set.union capturables capturableBindings in
       (* Extend map refs with any map values that are bound in this let *)
       let extendedMapRefs =
         List.fold args ~init:mapRefs ~f:(fun acc { binding; value } ->
           match getMapValue mapRefs value with
           | Some mapValue -> Map.set acc ~key:binding ~data:mapValue
           | None -> acc)
       in
       (* Lift from body using the extended capturables *)
       let%map extraction, body =
         liftFrom target targetConsumer capturables extendedMapRefs body
       in
       (match extraction with
        | Some extraction ->
          (* The body had an extraction. Any args that the extraction captures
             need to be lifted. *)
          let lifts = Set.inter capturableBindings extraction.captures in
          let liftedArgs, args =
            List.partition_tf args ~f:(fun arg -> Set.mem lifts arg.binding)
          in
          let captures = Set.diff extraction.captures lifts in
          let addLifts body =
            let body = extraction.addLifts body in
            Let { args = liftedArgs; body; type' = Expr.type' body }
          in
          Some { extraction with captures; addLifts }, Let { args; body; type' }
        | None -> None, Let { args; body; type' }))
  | LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; consumer
      ; type'
      } as loopBlock ->
    (* To be able to lift:
       - frameShape needs to match
       - each mapArg needs to refer to only the map and capturables
       - each iota parent needs to be available? not necessary - will not
         propogate lifting to inside a map body, so the same parents are always in
         scope
       - body and consumer can only use capturables (any uses should be put in captures)
       - consumers need to be compatible *)
    let frameShapesMatch = Index.equal_shapeElement target.frameShape frameShape in
    let argsToLift, argsToRemove, badArgs =
      List.partition3_map mapArgs ~f:(fun arg ->
        if Set.mem capturables arg.ref.id
        then `Fst arg
        else (
          match Map.find mapRefs arg.ref.id with
          | Some (Value derefs) -> `Snd (arg, derefs)
          | Some (Tuple _) | None -> `Trd ()))
    in
    let bindings = List.map mapArgs ~f:(fun arg -> arg.binding) @ mapIotas in
    let bodyCaptures =
      Set.diff (getUsesInExpr mapBody) (Set.of_list (module Identifier) bindings)
    in
    let consumerCaptures = getUsesInConsumer consumer in
    let bodyAndConsumerOk =
      Set.is_subset (Set.union bodyCaptures consumerCaptures) ~of_:capturables
    in
    let consumersAreCompatible =
      match Option.both targetConsumer consumer with
      | Some (targetConsumer, consumer) ->
        ConsumerCompatibility.isCompatible
          targetConsumer
          (ConsumerCompatibility.of_op consumer)
      | None -> true
    in
    if frameShapesMatch
       && List.is_empty badArgs
       && bodyAndConsumerOk
       && consumersAreCompatible
    then (
      let%bind archerMapResultBinding = FuseState.createId "fusion-archer-map-result"
      and targetMapResultElementBinding = FuseState.createId "fusion-target-map-result" in
      let bodyToLift =
        Let
          { args =
              List.map argsToRemove ~f:(fun ({ binding; ref = _ }, derefs) ->
                let rec derefValue derefs value =
                  match derefs with
                  | index :: restDerefs ->
                    Expr.tupleDeref ~tuple:(derefValue restDerefs value) ~index
                  | [] -> value
                in
                { binding
                ; value =
                    derefValue
                      derefs
                      (Ref
                         { id = targetMapResultElementBinding
                         ; type' = Expr.type' target.mapBody
                         })
                })
          ; body = mapBody
          ; type' = Expr.type' mapBody
          }
      in
      let argCaptures =
        argsToLift
        |> List.map ~f:(fun arg -> arg.ref.id)
        |> Set.of_list (module Identifier)
      in
      let captures = Set.union bodyCaptures argCaptures in
      let%map consumerExtraction =
        match consumer with
        | Some op ->
          let%map consumerBinding = FuseState.createId "fusion-archer-consumer-result" in
          Some { op; consumerBinding }
        | None -> return None
      in
      ( Some
          { captures
          ; addLifts = (fun body -> body)
          ; liftedArgs = argsToLift
          ; liftedIotas = mapIotas
          ; liftedBody = bodyToLift
          ; liftedBodyMatcher = mapBodyMatcher
          ; liftedResults = mapResults
          ; archerMapResultBinding
          ; targetMapResultElementBinding
          ; consumer = consumerExtraction
          }
      , Values
          { elements =
              [ Ref { id = archerMapResultBinding; type' = Expr.type' bodyToLift }
              ; (match consumerExtraction with
                 | Some { op; consumerBinding } ->
                   Ref
                     { id = consumerBinding
                     ; type' =
                         (match op with
                          | Reduce reduce -> reduce.type'
                          | Fold fold -> fold.type'
                          | Scatter scatter -> scatter.type')
                     }
                 | None -> Expr.values [])
              ]
          ; type'
          } ))
    else return (None, loopBlock)
  | Box { indices; body; bodyType; type' } ->
    let%map extraction, body = liftFrom target targetConsumer capturables mapRefs body in
    extraction, Box { indices; body; bodyType; type' }
  | Values { elements; type' } ->
    let%map extraction, elements =
      liftFromList elements ~f:(liftFrom target targetConsumer capturables mapRefs)
    in
    extraction, Values { elements; type' }
  | TupleDeref { tuple; index; type' } ->
    let%map extraction, tuple =
      liftFrom target targetConsumer capturables mapRefs tuple
    in
    extraction, TupleDeref { tuple; index; type' }
  | ScalarPrimitive { op; args; type' } ->
    let%map extraction, args =
      liftFromList args ~f:(liftFrom target targetConsumer capturables mapRefs)
    in
    extraction, ScalarPrimitive { op; args; type' }
  | ContiguousSubArray { arrayArg; indexArg; originalShape; resultShape; type' } ->
    let%bind extraction, arrayArg =
      liftFrom target targetConsumer capturables mapRefs arrayArg
    in
    (match extraction with
     | Some _ as extraction ->
       return
         ( extraction
         , ContiguousSubArray { arrayArg; indexArg; originalShape; resultShape; type' } )
     | None ->
       let%map extraction, indexArg =
         liftFrom target targetConsumer capturables mapRefs indexArg
       in
       ( extraction
       , ContiguousSubArray { arrayArg; indexArg; originalShape; resultShape; type' } ))
  | Append { args; type' } ->
    let%map extraction, args =
      liftFromList args ~f:(liftFrom target targetConsumer capturables mapRefs)
    in
    extraction, Append { args; type' }
  | Zip { zipArg; nestCount; type' } ->
    let%map extraction, zipArg =
      liftFrom target targetConsumer capturables mapRefs zipArg
    in
    extraction, Zip { zipArg; nestCount; type' }
  | Unzip { unzipArg; type' } ->
    let%map extraction, unzipArg =
      liftFrom target targetConsumer capturables mapRefs unzipArg
    in
    extraction, Unzip { unzipArg; type' }
  | (Ref _ | ReifyIndex { index = _; type' = _ } | ShapeProd _ | Literal _) as expr ->
    return (None, expr)
;;

type mergedConsumer =
  { mergedOp : Expr.consumerOp option
  ; getTargetConsumerResultFromBlockResult : Expr.t -> Expr.t
  ; getArcherConsumerResultFromBlockResult : Expr.t -> Expr.t
  }

let mergeConsumers ~(target : Expr.consumerOp option) ~(archer : Expr.consumerOp option)
  : (mergedConsumer, _) FuseState.u
  =
  let open FuseState.Let_syntax in
  match target, archer with
  | None, None ->
    return
      { mergedOp = None
      ; getTargetConsumerResultFromBlockResult = (fun _ -> Expr.values [])
      ; getArcherConsumerResultFromBlockResult = (fun _ -> Expr.values [])
      }
  | None, Some consumerOp ->
    return
      { mergedOp = Some consumerOp
      ; getTargetConsumerResultFromBlockResult = (fun _ -> Expr.values [])
      ; getArcherConsumerResultFromBlockResult =
          (fun blockResult -> Expr.tupleDeref ~tuple:blockResult ~index:1)
      }
  | Some consumerOp, None ->
    return
      { mergedOp = Some consumerOp
      ; getTargetConsumerResultFromBlockResult =
          (fun blockResult -> Expr.tupleDeref ~tuple:blockResult ~index:1)
      ; getArcherConsumerResultFromBlockResult = (fun _ -> Expr.values [])
      }
  | Some (Reduce target), Some archer ->
    (match archer with
     | Reduce archer ->
       let%map firstBinding = FuseState.createId "fused-reduce-arg1"
       and secondBinding = FuseState.createId "fused-reduce-arg2" in
       let argType =
         Type.Tuple
           [ Expr.productionTupleType target.arg.production
           ; Expr.productionTupleType archer.arg.production
           ]
       in
       let firstBindingRef = Expr.Ref { id = firstBinding; type' = argType }
       and secondBindingRef = Expr.Ref { id = secondBinding; type' = argType } in
       { mergedOp =
           Some
             (Reduce
                { arg =
                    { firstBinding
                    ; secondBinding
                    ; production =
                        ProductionTuple
                          { elements = [ target.arg.production; archer.arg.production ]
                          ; type' = argType
                          }
                    }
                ; zero = Expr.values [ target.zero; archer.zero ]
                ; body =
                    Expr.let'
                      ~args:
                        [ { binding = target.arg.firstBinding
                          ; value = Expr.tupleDeref ~tuple:firstBindingRef ~index:0
                          }
                        ; { binding = target.arg.secondBinding
                          ; value = Expr.tupleDeref ~tuple:secondBindingRef ~index:0
                          }
                        ; { binding = archer.arg.firstBinding
                          ; value = Expr.tupleDeref ~tuple:firstBindingRef ~index:1
                          }
                        ; { binding = archer.arg.secondBinding
                          ; value = Expr.tupleDeref ~tuple:secondBindingRef ~index:1
                          }
                        ]
                      ~body:(Expr.values [ target.body; archer.body ])
                ; d =
                    (assert (Index.equal_dimension target.d archer.d);
                     target.d)
                ; character =
                    (assert (Expr.equal_reduceCharacter target.character archer.character);
                     target.character)
                ; type' = Tuple [ target.type'; archer.type' ]
                })
       ; getTargetConsumerResultFromBlockResult =
           (fun blockResult ->
             Expr.tupleDeref ~tuple:(Expr.tupleDeref ~tuple:blockResult ~index:1) ~index:0)
       ; getArcherConsumerResultFromBlockResult =
           (fun blockResult ->
             Expr.tupleDeref ~tuple:(Expr.tupleDeref ~tuple:blockResult ~index:1) ~index:1)
       }
     | _ -> raise (Unreachable.Error "Tried to fuse incompatible consumers"))
  | Some (Fold target), Some archer ->
    (match archer with
     | Fold archer ->
       let%map zeroBinding = FuseState.createId "fused-fold-zero" in
       let zeroBindingRef =
         Expr.Ref
           { id = zeroBinding
           ; type' =
               Tuple
                 [ Expr.type' target.zeroArg.zeroValue
                 ; Expr.type' archer.zeroArg.zeroValue
                 ]
           }
       in
       { mergedOp =
           Some
             (Expr.Fold
                { arrayArgs = target.arrayArgs @ archer.arrayArgs
                ; zeroArg =
                    { zeroBinding
                    ; zeroValue =
                        Expr.values [ target.zeroArg.zeroValue; archer.zeroArg.zeroValue ]
                    }
                ; body =
                    Expr.let'
                      ~args:
                        [ { binding = target.zeroArg.zeroBinding
                          ; value = Expr.tupleDeref ~tuple:zeroBindingRef ~index:0
                          }
                        ; { binding = archer.zeroArg.zeroBinding
                          ; value = Expr.tupleDeref ~tuple:zeroBindingRef ~index:1
                          }
                        ]
                      ~body:(Expr.values [ target.body; archer.body ])
                ; reverse = target.reverse
                ; d =
                    (assert (Index.equal_dimension target.d archer.d);
                     target.d)
                ; character =
                    (assert (Expr.equal_foldCharacter target.character archer.character);
                     target.character)
                ; type' = Tuple [ target.type'; archer.type' ]
                })
       ; getTargetConsumerResultFromBlockResult =
           (fun blockResult ->
             Expr.tupleDeref ~tuple:(Expr.tupleDeref ~tuple:blockResult ~index:1) ~index:0)
       ; getArcherConsumerResultFromBlockResult =
           (fun blockResult ->
             Expr.tupleDeref ~tuple:(Expr.tupleDeref ~tuple:blockResult ~index:1) ~index:1)
       }
     | _ -> raise (Unreachable.Error "Tried to fuse incompatible consumers"))
  | Some (Scatter _), Some _ ->
    raise (Unreachable.Error "Tried to fuse incompatible consumers")
;;

type fusionOpportunity =
  { argBinding : Identifier.t
  ; addLifts : Expr.t -> Expr.t
  ; subForLoopBlockInArgValue : Expr.t -> Expr.t
  ; loopBlock : Expr.loopBlock
  ; mapValueLocationBuilder : mapValueLocation -> mapValueLocation option
  }

let rec fuseLoops (scope : Set.M(Identifier).t)
  : Nested.Expr.t -> (Nested.Expr.t, _) FuseState.u
  =
  let open FuseState.Let_syntax in
  let open Expr in
  function
  | Ref { id = _; type' = _ } as expr -> return expr
  | Frame { dimension; elements; type' } ->
    let%map elements = elements |> List.map ~f:(fuseLoops scope) |> FuseState.all in
    Frame { dimension; elements; type' }
  | BoxValue { box; type' } ->
    let%map box = fuseLoops scope box in
    BoxValue { box; type' }
  | IndexLet { indexArgs; body; type' } ->
    let%bind indexArgs =
      indexArgs
      |> List.map ~f:(fun { indexBinding; indexValue; sort } ->
        let%map indexValue =
          match indexValue with
          | Runtime value ->
            let%map value = fuseLoops scope value in
            Runtime value
          | FromBox { box; i } ->
            let%map box = fuseLoops scope box in
            FromBox { box; i }
        in
        { indexBinding; indexValue; sort })
      |> FuseState.all
    in
    let bindings =
      indexArgs
      |> List.map ~f:(fun arg -> arg.indexBinding)
      |> Set.of_list (module Identifier)
    in
    let extendedScope = Set.union scope bindings in
    let%map body = fuseLoops extendedScope body in
    IndexLet { indexArgs; body; type' }
  | ReifyIndex { index = _; type' = _ } as expr -> return expr
  | ShapeProd _ as expr -> return expr
  | Let { args; body; type' } ->
    let allBindings =
      args |> List.map ~f:(fun arg -> arg.binding) |> Set.of_list (module Identifier)
    in
    (* Start by performing fusion within each arg and within the body *)
    let bodyScope = Set.union scope allBindings in
    let%bind args =
      args
      |> List.map ~f:(fun { binding; value } ->
        let%map value = fuseLoops scope value in
        { binding; value })
      |> FuseState.all
    and body = fuseLoops bodyScope body in
    (* Then find all the possible fusion opportunities *)
    let opportunities =
      List.bind args ~f:(fun { binding = argBinding; value } ->
        let rec opportunitiesInExpr env subBuilder mapValueLocationBuilder expr =
          match expr with
          | Ref { id; type' = _ } ->
            Map.find env id
            |> Option.value ~default:[]
            |> List.map ~f:(fun opp ->
              { opp with
                mapValueLocationBuilder =
                  (fun l ->
                    let%bind.Option oppLocation = opp.mapValueLocationBuilder l in
                    mapValueLocationBuilder oppLocation)
              })
          | IndexLet { indexArgs; body; type' = _ } ->
            opportunitiesInExpr env subBuilder mapValueLocationBuilder body
            |> List.map ~f:(fun opp ->
              let addLifts body =
                let body = opp.addLifts body in
                IndexLet { indexArgs; body; type' = Expr.type' body }
              in
              { opp with addLifts })
          | Let { args; body; type' } ->
            (* Fold over the args, but for each arg, also know the args that
               come before and after it *)
            let rec extendEnv seenArgsRev args extendedEnv =
              match args with
              | [] -> extendedEnv
              | { binding; value } :: restArgs ->
                let opportunities =
                  opportunitiesInExpr
                    env
                    (fun v ->
                      subBuilder
                        (Let
                           { args =
                               List.rev seenArgsRev
                               @ [ { binding; value = v } ]
                               @ restArgs
                           ; body
                           ; type'
                           }))
                    (fun l -> Some l)
                    value
                in
                extendEnv
                  ({ binding; value } :: seenArgsRev)
                  restArgs
                  (Map.set extendedEnv ~key:binding ~data:opportunities)
            in
            opportunitiesInExpr
              (extendEnv [] args env)
              subBuilder
              mapValueLocationBuilder
              body
            |> List.map ~f:(fun opp ->
              let addLifts body =
                let body = opp.addLifts body in
                Let { args; body; type' = Expr.type' body }
              in
              { opp with addLifts })
          | LoopBlock loopBlock ->
            [ { argBinding
              ; addLifts = (fun v -> v)
              ; subForLoopBlockInArgValue = subBuilder
              ; loopBlock
              ; mapValueLocationBuilder =
                  (fun l -> mapValueLocationBuilder (Tuple [ Some l; None ]))
              }
            ]
          | Values { elements; type' } ->
            let tupleSize = List.length elements in
            elements
            |> List.mapi ~f:(fun i e -> i, e)
            |> List.bind ~f:(fun (location, e) ->
              opportunitiesInExpr
                env
                (fun v ->
                  subBuilder
                    (Values
                       { elements =
                           List.mapi elements ~f:(fun i e ->
                             if i = location then v else e)
                       ; type'
                       }))
                (fun l ->
                  mapValueLocationBuilder
                    (Tuple
                       (List.init tupleSize ~f:(fun i ->
                          if i = location then Some l else None))))
                e)
          | TupleDeref { tuple; index; type' } ->
            opportunitiesInExpr
              env
              (fun v -> subBuilder (TupleDeref { tuple = v; index; type' }))
              (function
               | Value derefs -> mapValueLocationBuilder (Value (index :: derefs))
               | Tuple elements -> List.nth_exn elements index)
              tuple
          | Frame _
          | BoxValue _
          | ReifyIndex _
          | ShapeProd _
          | Box _
          | Literal _
          | ScalarPrimitive _
          | ContiguousSubArray _
          | Append _
          | Zip _
          | Unzip _ -> []
        in
        opportunitiesInExpr
          (Map.empty (module Identifier))
          (fun v -> v)
          (fun l -> Some l)
          value)
    in
    (* Try fusing at each fusion opportunity until one succeeds *)
    (* tryFusing attempts a fusion opportunity *)
    let tryFusing
      { argBinding
      ; addLifts = addTargetLifts
      ; subForLoopBlockInArgValue
      ; loopBlock
      ; mapValueLocationBuilder
      }
      =
      match mapValueLocationBuilder (Value []) with
      | None -> return None
      | Some mapValueLocation ->
        let%bind extraction, body =
          liftFrom
            loopBlock
            (Option.map loopBlock.consumer ~f:ConsumerCompatibility.of_op)
            (Set.remove bodyScope argBinding)
            (Map.singleton (module Identifier) argBinding mapValueLocation)
            body
        in
        (match extraction with
         | None -> return None
         | Some
             { captures = _
             ; addLifts = addArcherLifts
             ; liftedArgs
             ; liftedIotas
             ; liftedBody
             ; liftedBodyMatcher
             ; liftedResults
             ; archerMapResultBinding
             ; targetMapResultElementBinding
             ; consumer = liftedConsumer
             } ->
           let argsMinusTarget, targetArg =
             let rec loop seenArgsRev args =
               match args with
               | [] -> raise (Unreachable.Error "argBinding should be in args")
               | (arg : Expr.letArg) :: rest ->
                 if Identifier.equal argBinding arg.binding
                 then List.rev seenArgsRev @ rest, arg
                 else loop (arg :: seenArgsRev) rest
             in
             loop [] args
           in
           let%bind blockResultBinding = FuseState.createId "fused-block-result"
           and targetMapResultBinding = FuseState.createId "fusion-target-map-result" in
           let rec extractTypesFromTuple (matcher : Expr.tupleMatch) (tupleType : Type.t)
             : Type.t Map.M(Identifier).t
             =
             match matcher with
             | Binding id ->
               Map.singleton
                 (module Identifier)
                 id
                 (Type.Array { element = tupleType; size = loopBlock.frameShape })
             | Unpack matchers ->
               (match tupleType with
                | Tuple tupleTypes ->
                  List.zip_exn matchers tupleTypes
                  |> List.fold
                       ~init:(Map.empty (module Identifier))
                       ~f:(fun acc (matcher, tupleType) ->
                         Map.merge_skewed
                           acc
                           (extractTypesFromTuple matcher tupleType)
                           ~combine:(fun ~key:_ a _ -> a))
                | _ -> raise (Unreachable.Error "expected tuple type"))
           in
           let mapBodyMatcher = Unpack [ loopBlock.mapBodyMatcher; liftedBodyMatcher ] in
           let mergedBodyType : Type.tuple =
             [ Expr.type' loopBlock.mapBody; Expr.type' liftedBody ]
           in
           let mergedResults = loopBlock.mapResults @ liftedResults in
           let resultBindingTypes =
             extractTypesFromTuple mapBodyMatcher (Tuple mergedBodyType)
           in
           let%map mergedConsumer =
             mergeConsumers
               ~target:loopBlock.consumer
               ~archer:(Option.map liftedConsumer ~f:(fun c -> c.op))
           in
           let blockType : Type.tuple =
             [ Tuple (List.map mergedResults ~f:(Map.find_exn resultBindingTypes))
             ; (match mergedConsumer.mergedOp with
                | None -> Tuple []
                | Some consumer -> Expr.consumerOpType consumer)
             ]
           in
           (*
              1. declare variables captured
              2. declare things from addLifts
              3. run the loop block
              4. declare variables that hold the results
              5. run the body (the one returned by liftFrom)
           *)
           Some
             (* Declare all args that don't contain the loop block first to
                guarantee that captured variables are available *)
             (Expr.let'
                ~args:argsMinusTarget
                ~body:
                  ((* lift any variables that need to be declared *)
                   addTargetLifts
                     (addArcherLifts
                        (* Bind the result of the merged loop blocks to a variable *)
                        (Expr.let'
                           ~args:
                             [ { binding = blockResultBinding
                               ; value =
                                   (* Create the merged loop block *)
                                   LoopBlock
                                     { frameShape = loopBlock.frameShape
                                     ; mapArgs = loopBlock.mapArgs @ liftedArgs
                                     ; mapIotas = loopBlock.mapIotas @ liftedIotas
                                     ; mapBody =
                                         Expr.let'
                                           ~args:
                                             [ { binding = targetMapResultElementBinding
                                               ; value = loopBlock.mapBody
                                               }
                                             ]
                                           ~body:
                                             (Expr.values
                                                [ Ref
                                                    { id = targetMapResultElementBinding
                                                    ; type' = Expr.type' loopBlock.mapBody
                                                    }
                                                ; liftedBody
                                                ])
                                     ; mapBodyMatcher
                                     ; mapResults = mergedResults
                                     ; consumer = mergedConsumer.mergedOp
                                     ; type' = blockType
                                     }
                               }
                             ]
                           ~body:
                             ((* Move the results of the loop blocks into the right
                                 places, and then proceed with the regular body *)
                              let blockResult =
                                Ref { id = blockResultBinding; type' = Tuple blockType }
                              in
                              Expr.let'
                                ~args:
                                  ([ { binding = targetMapResultBinding
                                     ; value =
                                         Expr.values
                                           (List.mapi
                                              loopBlock.mapResults
                                              ~f:(fun index _ ->
                                                Expr.tupleDeref
                                                  ~tuple:
                                                    (Expr.tupleDeref
                                                       ~tuple:blockResult
                                                       ~index:0)
                                                  ~index))
                                     }
                                   ; { binding = archerMapResultBinding
                                     ; value =
                                         (let indexOffset =
                                            List.length loopBlock.mapResults
                                          in
                                          Expr.values
                                            (List.mapi liftedResults ~f:(fun index _ ->
                                               Expr.tupleDeref
                                                 ~tuple:
                                                   (Expr.tupleDeref
                                                      ~tuple:blockResult
                                                      ~index:0)
                                                 ~index:Int.(index + indexOffset))))
                                     }
                                   ]
                                   @ (liftedConsumer
                                      |> Option.map ~f:(fun { op = _; consumerBinding } ->
                                        { binding = consumerBinding
                                        ; value =
                                            mergedConsumer
                                              .getArcherConsumerResultFromBlockResult
                                              blockResult
                                        })
                                      |> Option.to_list))
                                ~body:
                                  (Expr.let'
                                     ~args:
                                       [ { binding = targetArg.binding
                                         ; value =
                                             subForLoopBlockInArgValue
                                               (Expr.values
                                                  [ Ref
                                                      { id = targetMapResultBinding
                                                      ; type' =
                                                          Tuple
                                                            (List.map
                                                               loopBlock.mapResults
                                                               ~f:
                                                                 (Map.find_exn
                                                                    resultBindingTypes))
                                                      }
                                                  ; mergedConsumer
                                                      .getTargetConsumerResultFromBlockResult
                                                      blockResult
                                                  ])
                                         }
                                       ]
                                     ~body)))))))
    in
    (* tryFusingList attempts fusion opportunities until one succeeds
       and gives up if none do *)
    let rec tryFusingList opportunities =
      match opportunities with
      | opportunity :: rest ->
        (match%bind tryFusing opportunity with
         | Some result ->
           let%bind () = FuseState.markFusion in
           return result
           (* fuseLoops scope result *)
         | None -> tryFusingList rest)
      | [] -> return (Let { args; body; type' })
    in
    tryFusingList opportunities
  | LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; consumer
      ; type'
      } ->
    let mapBindings =
      mapArgs |> List.map ~f:(fun arg -> arg.binding) |> Set.of_list (module Identifier)
    in
    let mapExtendedScope = Set.union scope mapBindings in
    let%map mapBody = fuseLoops mapExtendedScope mapBody
    and consumer =
      match consumer with
      | None -> return None
      | Some (Reduce { arg; zero; body; d; character; type' }) ->
        let reduceBindings =
          [ arg.firstBinding; arg.secondBinding ] |> Set.of_list (module Identifier)
        in
        let reduceExtendedScope = Set.union scope reduceBindings in
        let%map zero = fuseLoops scope zero
        and body = fuseLoops reduceExtendedScope body in
        Some (Reduce { arg; zero; body; d; character; type' })
      | Some (Fold { zeroArg; arrayArgs; body; reverse; d; character; type' }) ->
        let foldBindings =
          zeroArg.zeroBinding :: List.map arrayArgs ~f:(fun arg -> arg.binding)
          |> Set.of_list (module Identifier)
        in
        let foldExtendedScope = Set.union scope foldBindings in
        let%map body = fuseLoops foldExtendedScope body in
        Some (Fold { zeroArg; arrayArgs; body; reverse; d; character; type' })
      | Some (Scatter { valuesArg = _; indicesArg = _; dIn = _; dOut = _; type' = _ }) as
        v -> return v
    in
    LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; consumer
      ; type'
      }
  | Box { indices; body; bodyType; type' } ->
    let%map body = fuseLoops scope body in
    Box { indices; body; bodyType; type' }
  | Literal _ as expr -> return expr
  | Values { elements; type' } ->
    let%map elements = elements |> List.map ~f:(fuseLoops scope) |> FuseState.all in
    Values { elements; type' }
  | TupleDeref { tuple; index; type' } ->
    let%map tuple = fuseLoops scope tuple in
    TupleDeref { tuple; index; type' }
  | ScalarPrimitive { op; args; type' } ->
    let%map args = args |> List.map ~f:(fuseLoops scope) |> FuseState.all in
    ScalarPrimitive { op; args; type' }
  | ContiguousSubArray { arrayArg; indexArg; originalShape; resultShape; type' } ->
    let%map arrayArg = fuseLoops scope arrayArg
    and indexArg = fuseLoops scope indexArg in
    ContiguousSubArray { arrayArg; indexArg; originalShape; resultShape; type' }
  | Append { args; type' } ->
    let%map args = args |> List.map ~f:(fuseLoops scope) |> FuseState.all in
    Append { args; type' }
  | Zip { zipArg; nestCount; type' } ->
    let%map zipArg = fuseLoops scope zipArg in
    Zip { zipArg; nestCount; type' }
  | Unzip { unzipArg; type' } ->
    let%map unzipArg = fuseLoops scope unzipArg in
    Unzip { unzipArg; type' }
;;

let fuse (prog : Nested.t) : (CompilerState.state, Nested.t fusionResult, _) State.t =
  FuseState.asCompilerState (fuseLoops (Set.empty (module Identifier)) prog)
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nested.t
  type output = Nested.t
  type error = (SB.source option, string) Source.annotate

  let name = "Fuse"

  let run input =
    CompilerPipeline.S.make ~f:(fun state ->
      State.run (State.map (fuse input) ~f:(fun r -> r.result)) state)
  ;;
end
