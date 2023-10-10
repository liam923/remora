open! Base

module DenestState = struct
  include State

  type state = CompilerState.state
  type ('t, 'e) u = (state, 't, 'e) t

  let createId name =
    make ~f:(fun state ->
      State.run
        (Identifier.create
           name
           ~getCounter:(fun (s : state) -> s.idCounter)
           ~setCounter:(fun _ idCounter -> CompilerState.{ idCounter }))
        state)
  ;;
end

let rec denestTypeArray ({ element; shape } : Nucleus.Type.array) : Fused.Type.t =
  match shape with
  | [] -> denestTypeAtom element
  | shapeElement :: restShape ->
    Array
      { element = denestTypeArray { element; shape = restShape }; size = shapeElement }

and denestTypeAtom : Nucleus.Type.atom -> Fused.Type.t = function
  | Sigma sigma -> Sigma (denestTypeSigma sigma)
  | Tuple elements -> Tuple (denestTypeTuple elements)
  | Literal literal -> Literal literal

and denestTypeSigma ({ parameters; body } : Nucleus.Type.sigma) : Fused.Type.sigma =
  { parameters; body = denestTypeArray body }

and denestTypeTuple = List.map ~f:denestTypeAtom

let rec denestArray : Nucleus.Expr.array -> (Fused.t, _) DenestState.u =
  let open DenestState.Let_syntax in
  let open Fused.Expr in
  function
  | Ref { id; type' } -> return (Ref { id; type' = denestTypeArray type' })
  | Frame { dimensions; elements; type' } ->
    let%map elements = elements |> List.map ~f:denestArray |> DenestState.all in
    Frame { dimensions; elements; type' = denestTypeArray type' }
  | AtomAsArray { element; type' = _ } -> denestAtom element
  | BoxValue { box; type' } ->
    let%map box = denestArray box in
    let type' = denestTypeArray type' in
    BoxValue { box; type' }
  | IndexLet { indexArgs; body; type' } ->
    let%map indexArgs =
      indexArgs
      |> List.map ~f:(fun { indexBinding; indexValue; sort } ->
        let%map indexValue =
          match indexValue with
          | Runtime array ->
            let%map array = denestArray array in
            Runtime array
          | FromBox { box; i } ->
            let%map box = denestArray box in
            FromBox { box; i }
        in
        { indexBinding; indexValue; sort })
      |> DenestState.all
    and body = denestArray body in
    let type' = denestTypeArray type' in
    IndexLet { indexArgs; body; type' }
  | ReifyIndex { index; type' } ->
    let type' = denestTypeArray type' in
    return (ReifyIndex { index; type' })
  | ArrayPrimitive (Map { args; iotaVar; frameShape; body; type' }) ->
    let rec decompose args iotaVar frameShape =
      match frameShape with
      | [] ->
        let%map body = denestArray body in
        let iotaArg =
          Option.map iotaVar ~f:(fun iotaVar ->
            { binding = iotaVar; value = Literal (IntLiteral 0) })
        in
        Let { args = Option.to_list iotaArg @ args; body; type' = Fused.Expr.type' body }
      | shapeElement :: restFrameShape ->
        let%bind readsAndLocalRefs =
          args
          |> List.map ~f:(fun { binding; value } ->
            let%map localBinding = DenestState.createId (Identifier.name binding)
            and consumerBinding = DenestState.createId (Identifier.name binding) in
            ( { op = Read value
              ; localBinding
              ; consumerBinding
              ; bodyBinding = None
              ; type' = Fused.Expr.type' value
              }
            , { binding; value = Ref { id = localBinding; type' = poop } } ))
          |> DenestState.all
        in
        let reads, localRefs = List.unzip readsAndLocalRefs in
        let%map resultBinding = DenestState.createId "map-result" in
        
        raise Unimplemented.default
    in
    let%bind args =
      args
      |> List.map ~f:(fun { binding; value } ->
        let%map value = denestArray value in
        { binding; value })
      |> DenestState.all
    in
    decompose args iotaVar frameShape
  | ArrayPrimitive
      (Reduce { args; zero; body; d; itemPad; cellShape; associative; character; type' })
    ->
    let type' = denestTypeArray type' in
    let%bind producersAndArgs =
      args
      |> List.map ~f:(fun { firstBinding; secondBinding; value } ->
        let%map production, ref = makeProductionWithRef "reduce-arg" value in
        production, { firstBinding; secondBinding; production = ref })
      |> DenestState.all
    and body = denestArray body
    and zero =
      match zero with
      | Some zero -> denestArray zero |> DenestState.map ~f:(fun z -> Some z)
      | None -> return None
    in
    let producers, args = List.unzip producersAndArgs in
    let%map resultBinding = DenestState.createId "reduce-result" in
    let consumer =
      { binding = resultBinding
      ; op =
          Reduce
            { args; zero; body; d; itemPad; cellShape; associative; character; type' }
      }
    in
    ConsumerBlock
      { frameShape = Add d
      ; producers
      ; consumer = Some consumer
      ; body = Ref { id = resultBinding; type' }
      ; type'
      }
  | ArrayPrimitive
      (Fold { zeroArgs; arrayArgs; body; d; itemPad; cellShape; character; type' }) ->
    let type' = denestTypeArray type' in
    let%bind zeroProducersAndArgs =
      zeroArgs
      |> List.map ~f:(fun { binding; value } ->
        let%map production, ref = makeProductionWithRef (Identifier.name binding) value in
        production, { binding; production = ref })
      |> DenestState.all
    and arrayProducersAndArgs =
      arrayArgs
      |> List.map ~f:(fun { binding; value } ->
        let%map production, ref = makeProductionWithRef (Identifier.name binding) value in
        production, { binding; production = ref })
      |> DenestState.all
    and body = denestArray body in
    let zeroProducers, zeroArgs = List.unzip zeroProducersAndArgs in
    let arrayProducers, arrayArgs = List.unzip arrayProducersAndArgs in
    let%map resultBinding = DenestState.createId "fold-result" in
    let consumer =
      { binding = resultBinding
      ; op = Fold { zeroArgs; arrayArgs; body; d; itemPad; cellShape; character; type' }
      }
    in
    ConsumerBlock
      { frameShape = Add d
      ; producers = zeroProducers @ arrayProducers
      ; consumer = Some consumer
      ; body = Ref { id = resultBinding; type' }
      ; type'
      }
  | ArrayPrimitive (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' }) ->
    let type' = denestTypeArray type' in
    let%bind valuesProducer, valuesArg =
      makeProductionWithRef "scatter-values-arg" valuesArg
    and indicesProducer, indicesArg =
      makeProductionWithRef "scatter-indices-arg" indicesArg
    in
    let%map resultBinding = DenestState.createId "scatter-result" in
    let consumer =
      { binding = resultBinding
      ; op = Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' }
      }
    in
    ConsumerBlock
      { frameShape = Add dIn
      ; producers = [ valuesProducer; indicesProducer ]
      ; consumer = Some consumer
      ; body = Ref { id = resultBinding; type' }
      ; type'
      }
  | ArrayPrimitive (Append { arg1; arg2; d1 = _; d2 = _; cellShape = _; type' }) ->
    let%map arg1 = denestArray arg1
    and arg2 = denestArray arg2 in
    let type' = denestTypeArray type' in
    Append { args = [ arg1; arg2 ]; type' }
  | ArrayPrimitive (Index { arrayArg; indexArg; s = _; cellShape = _; l = _; type' }) ->
    let%map arrayArg = denestArray arrayArg
    and indexArg = denestArray indexArg in
    let type' = denestTypeArray type' in
    SubArray { arrayArg; indexArg; type' }

and makeProductionWithRef refName value =
  let open DenestState.Let_syntax in
  let open Fused.Expr in
  let%bind value = denestArray value in
  let op = Read value in
  let%map localBinding = DenestState.createId refName
  and consumerBinding = DenestState.createId refName in
  let production =
    { op
    ; localBinding
    ; consumerBinding
    ; bodyBinding = None
    ; type' = Fused.Expr.type' value
    }
  in
  production, { id = consumerBinding; type' = production.type' }

and denestAtom : Nucleus.Expr.atom -> (Fused.t, _) DenestState.u =
  let open DenestState.Let_syntax in
  let open Fused.Expr in
  function
  | ArrayAsAtom { array; type' = _ } -> denestArray array
  | Box { indices; body; bodyType; type' } ->
    let%map body = denestArray body in
    let bodyType = denestTypeArray bodyType in
    let type' = denestTypeSigma type' in
    Box { indices; body; bodyType; type' }
  | Literal lit -> return (Literal lit)
  | Values { elements; type' } ->
    let%map elements = elements |> List.map ~f:denestAtom |> DenestState.all in
    let type' = denestTypeTuple type' in
    Values { elements; type' }
  | AtomicPrimitive { op; args; type' } ->
    let%map args = args |> List.map ~f:denestAtom |> DenestState.all in
    let type' = denestTypeAtom type' in
    ScalarPrimitive { op; args; type' }
;;

let denest (prog : Nucleus.t) : (CompilerState.state, Fused.t, _) State.t =
  let open DenestState.Let_syntax in
  let%map prog = denestArray prog in
  prog
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nucleus.t
  type output = Fused.t
  type error = (SB.source option, string) Source.annotate

  let name = "Denest"
  let run input = CompilerPipeline.S.make ~f:(fun state -> State.run (denest input) state)
end
