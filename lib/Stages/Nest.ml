open! Base

module NestState = struct
  include State

  type state = CompilerState.state
  type ('t, 'e) u = (state, 't, 'e) t

  let createId name =
    make ~f:(fun state ->
      State.run
        (Identifier.create
           name
           ~getCounter:(fun (s : state) -> s.idCounter)
           ~setCounter:(fun s idCounter -> { s with idCounter }))
        state)
  ;;
end

let rec nestTypeArray ({ element; shape } : Nucleus.Type.array) : Nested.Type.t =
  match shape with
  | [] -> nestTypeAtom element
  | shapeElement :: restShape ->
    Array { element = nestTypeArray { element; shape = restShape }; size = shapeElement }

and nestTypeAtom : Nucleus.Type.atom -> Nested.Type.t = function
  | Sigma sigma -> Sigma (nestTypeSigma sigma)
  | Tuple elements -> Tuple (nestTypeTuple elements)
  | Literal literal -> Literal literal

and nestTypeSigma ({ parameters; body } : Nucleus.Type.sigma) : Nested.Type.sigma =
  { parameters; body = nestTypeArray body }

and nestTypeTuple = List.map ~f:nestTypeAtom

let elementOfArrayType frameShape = function
  | Nested.Type.Array { element; size } ->
    assert (Nested.Index.equal_shapeElement size frameShape);
    element
  | _ -> raise (Unreachable.Error "Expected an array type")
;;

let rec nestArray : Nucleus.Expr.array -> (Nested.t, _) NestState.u =
  let open NestState.Let_syntax in
  let open Nested.Expr in
  function
  | Ref { id; type' } -> return (Ref { id; type' = nestTypeArray type' })
  | Frame { dimensions; elements; type' } ->
    let%map elements = elements |> List.map ~f:nestArray |> NestState.all in
    let rec nest type' dimensions elements =
      match dimensions with
      | 0 :: _ -> Frame { dimension = 0; elements = []; type' }
      | dimension :: restDims ->
        let groupSize = List.length elements / dimension in
        let groups =
          if List.length elements > 0
          then List.groupi elements ~break:(fun i _ _ -> i % groupSize = 0)
          else List.init dimension ~f:(fun _ -> [])
        in
        let innerType =
          elementOfArrayType (Add (Nested.Index.dimensionConstant dimension)) type'
        in
        Frame
          { dimension; elements = List.map groups ~f:(nest innerType restDims); type' }
      | [] ->
        (match elements with
         | [ element ] -> element
         | _ -> raise (Unreachable.Error "expected single element"))
    in
    nest (nestTypeArray type') dimensions elements
  | AtomAsArray { element; type' = _ } -> nestAtom element
  | BoxValue { box; type' } ->
    let%map box = nestArray box in
    let type' = nestTypeArray type' in
    BoxValue { box; type' }
  | IndexLet { indexArgs; body; type' } ->
    let%map indexArgs =
      indexArgs
      |> List.map ~f:(fun { indexBinding; indexValue; sort } ->
        let%map indexValue =
          match indexValue with
          | Runtime array ->
            let%map array = nestArray array in
            Runtime array
          | FromBox { box; i } ->
            let%map box = nestArray box in
            FromBox { box; i }
        in
        { indexBinding; indexValue; sort })
      |> NestState.all
    and body = nestArray body in
    let type' = nestTypeArray type' in
    IndexLet { indexArgs; body; type' }
  | ReifyIndex { index; type' } ->
    let type' = nestTypeArray type' in
    return (ReifyIndex { index; type' })
  | ArrayPrimitive (Map { args; iotaVar; frameShape; body; type' = _ }) ->
    let rec decompose args parentIota frameShape =
      match frameShape with
      | [] ->
        let%map body = nestArray body in
        let iotaArg =
          Option.map iotaVar ~f:(fun iotaVar ->
            { binding = iotaVar
            ; value =
                (match parentIota with
                 | None -> Literal (IntLiteral 0)
                 | Some parentIota -> Ref { id = parentIota; type' = Literal IntLiteral })
            })
        in
        let letArgs =
          List.map args ~f:(fun { binding; ref } -> { binding; value = Ref ref })
        in
        Let
          { args = Option.to_list iotaArg @ letArgs
          ; body
          ; type' = Nested.Expr.type' body
          }
      | shapeElement :: restFrameShape ->
        let%bind reusltBinding = NestState.createId "map-result"
        and mapArgsAndNextArgs =
          args
          |> List.map ~f:(fun { binding; ref } ->
            let%map newBinding = NestState.createId (Identifier.name binding) in
            let valueElementType = elementOfArrayType shapeElement ref.type' in
            ( { binding = newBinding; ref }
            , { binding; ref = { id = newBinding; type' = valueElementType } } ))
          |> NestState.all
        and mapIota =
          match iotaVar with
          | Some iotaVar ->
            let%map newIota = NestState.createId (Identifier.name iotaVar) in
            Some { iota = newIota; nestIn = parentIota }
          | None -> return None
        in
        let mapArgs, nextArgs = List.unzip mapArgsAndNextArgs in
        let%map mapBody =
          decompose
            nextArgs
            (Option.map mapIota ~f:(fun iota -> iota.iota))
            restFrameShape
        in
        Nested.Expr.tupleDeref
          ~tuple:
            (Nested.Expr.tupleDeref
               ~tuple:
                 (LoopBlock
                    { frameShape = shapeElement
                    ; mapArgs
                    ; mapIotas = Option.to_list mapIota
                    ; mapBody
                    ; mapBodyMatcher = Binding reusltBinding
                    ; mapResults = [ reusltBinding ]
                    ; consumer = None
                    ; type' =
                        [ Tuple
                            [ Array
                                { element = Nested.Expr.type' mapBody
                                ; size = shapeElement
                                }
                            ]
                        ; Tuple []
                        ]
                    })
               ~index:0)
          ~index:0
    in
    let%bind letAndMapArgs =
      args
      |> List.map ~f:(fun { binding = mapBinding; value } ->
        let%map value = nestArray value
        and letBinding = NestState.createId (Identifier.name mapBinding) in
        ( { binding = letBinding; value }
        , { binding = mapBinding
          ; ref = { id = letBinding; type' = Nested.Expr.type' value }
          } ))
      |> NestState.all
    in
    let letArgs, mapArgs = List.unzip letAndMapArgs in
    let%map body = decompose mapArgs None frameShape in
    Let { args = letArgs; body; type' = Nested.Expr.type' body }
  | ArrayPrimitive
      (Reduce { arg; zero; body; d; cellShape = _; associative; character; type' }) ->
    let type' = nestTypeArray type' in
    let%bind productionBinding = NestState.createId "reduce-arg"
    and argValue = nestArray arg.value in
    let arg =
      { firstBinding = arg.firstBinding
      ; secondBinding = arg.secondBinding
      ; production =
          ProductionTupleAtom
            { productionId = productionBinding
            ; type' = elementOfArrayType (Add d) @@ Nested.Expr.type' argValue
            }
      }
    in
    let%bind zero =
      match zero with
      | Some zero -> nestArray zero |> NestState.map ~f:(fun z -> Some z)
      | None -> return None
    and body = nestArray body in
    let consumer = Reduce { arg; zero; body; d; associative; character; type' } in
    let%map letArgs, mapArgs, mapBody, mapBodyMatcher =
      makeMap (Nested.Index.Add d) [ productionBinding, argValue ]
    in
    Let
      { args = letArgs
      ; body =
          TupleDeref
            { tuple =
                LoopBlock
                  { frameShape = Add d
                  ; mapArgs
                  ; mapIotas = []
                  ; mapBody
                  ; mapBodyMatcher
                  ; mapResults = []
                  ; consumer = Some consumer
                  ; type' = [ Tuple []; type' ]
                  }
            ; index = 1
            ; type'
            }
      ; type'
      }
  | ArrayPrimitive (Fold { zeroArg; arrayArgs; body; d; cellShape = _; character; type' })
    ->
    let type' = nestTypeArray type' in
    let%bind zeroArgValue = nestArray zeroArg.value in
    let zeroArg = { zeroBinding = zeroArg.binding; zeroValue = zeroArgValue } in
    let%bind arrays =
      arrayArgs
      |> List.map ~f:(fun { binding; value } ->
        let%map productionBinding = NestState.createId (Identifier.name binding)
        and value = nestArray value in
        ( productionBinding
        , value
        , { binding
          ; production =
              { productionId = productionBinding
              ; type' = elementOfArrayType (Add d) @@ Nested.Expr.type' value
              }
          } ))
      |> NestState.all
    and body = nestArray body in
    let arrayBindings, arrayValues, arrayArgs = List.unzip3 arrays in
    let consumer = Fold { zeroArg; arrayArgs; body; d; character; type' } in
    let%map letArgs, mapArgs, mapBody, mapBodyMatcher =
      makeMap (Nested.Index.Add d) (List.zip_exn arrayBindings arrayValues)
    in
    Let
      { args = letArgs
      ; body =
          TupleDeref
            { tuple =
                LoopBlock
                  { frameShape = Add d
                  ; mapArgs
                  ; mapIotas = []
                  ; mapBody
                  ; mapBodyMatcher
                  ; mapResults = []
                  ; consumer = Some consumer
                  ; type' = [ Tuple []; type' ]
                  }
            ; index = 1
            ; type'
            }
      ; type'
      }
  | ArrayPrimitive (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape = _; type' }) ->
    let type' = nestTypeArray type' in
    let%bind valuesBinding = NestState.createId "scatter-values-arg"
    and valuesArg = nestArray valuesArg
    and indicesBinding = NestState.createId "scatter-indices-arg"
    and indicesArg = nestArray indicesArg in
    let consumer =
      Scatter
        { valuesArg =
            { productionId = valuesBinding
            ; type' = elementOfArrayType (Add dIn) @@ Nested.Expr.type' valuesArg
            }
        ; indicesArg =
            { productionId = indicesBinding
            ; type' = elementOfArrayType (Add dIn) @@ Nested.Expr.type' indicesArg
            }
        ; dIn
        ; dOut
        ; type'
        }
    in
    let%map letArgs, mapArgs, mapBody, mapBodyMatcher =
      makeMap
        (Typed.Index.Add dIn)
        [ valuesBinding, valuesArg; indicesBinding, indicesArg ]
    in
    Let
      { args = letArgs
      ; body =
          TupleDeref
            { tuple =
                LoopBlock
                  { frameShape = Add dIn
                  ; mapArgs
                  ; mapIotas = []
                  ; mapBody
                  ; mapBodyMatcher
                  ; mapResults = []
                  ; consumer = Some consumer
                  ; type' = [ Tuple []; type' ]
                  }
            ; index = 1
            ; type'
            }
      ; type'
      }
  | ArrayPrimitive (Append { arg1; arg2; d1 = _; d2 = _; cellShape = _; type' }) ->
    let%map arg1 = nestArray arg1
    and arg2 = nestArray arg2 in
    let type' = nestTypeArray type' in
    Append { args = [ arg1; arg2 ]; type' }
  | ArrayPrimitive (Index { arrayArg; indexArg; s = _; cellShape = _; l = _; type' }) ->
    let%map arrayArg = nestArray arrayArg
    and indexArg = nestArray indexArg in
    let type' = nestTypeArray type' in
    SubArray { arrayArg; indexArg; type' }

and makeMap frameShape args =
  let open NestState.Let_syntax in
  let open Nested.Expr in
  let%bind letArgs =
    args
    |> List.map ~f:(fun (resultId, arg) ->
      let%map argId = NestState.createId (Identifier.name resultId) in
      { binding = argId; value = arg })
    |> NestState.all
  in
  let%map mapArgs =
    letArgs
    |> List.map ~f:(fun { binding = letId; value } ->
      let%map localId = NestState.createId (Identifier.name letId) in
      { binding = localId; ref = { id = letId; type' = Nested.Expr.type' value } })
    |> NestState.all
  in
  let elements =
    List.map mapArgs ~f:(fun { binding; ref } ->
      Ref { id = binding; type' = elementOfArrayType frameShape ref.type' })
  in
  let body = Values { elements; type' = List.map elements ~f:Nested.Expr.type' } in
  let matcher = Unpack (List.map args ~f:(fun (id, _) -> Binding id)) in
  letArgs, mapArgs, body, matcher

and nestAtom : Nucleus.Expr.atom -> (Nested.t, _) NestState.u =
  let open NestState.Let_syntax in
  let open Nested.Expr in
  function
  | ArrayAsAtom { array; type' = _ } -> nestArray array
  | Box { indices; body; bodyType; type' } ->
    let%map body = nestArray body in
    let bodyType = nestTypeArray bodyType in
    let type' = nestTypeSigma type' in
    Box { indices; body; bodyType; type' }
  | Literal lit -> return (Literal lit)
  | Values { elements; type' } ->
    let%map elements = elements |> List.map ~f:nestAtom |> NestState.all in
    let type' = nestTypeTuple type' in
    Values { elements; type' }
  | AtomicPrimitive { op; args; type' } ->
    let%map args = args |> List.map ~f:nestAtom |> NestState.all in
    let type' = nestTypeAtom type' in
    ScalarPrimitive { op; args; type' }
  | TupleDeref { tuple; index; type' } ->
    let%map tuple = nestAtom tuple in
    let type' = nestTypeAtom type' in
    TupleDeref { tuple; index; type' }
;;

let nest (prog : Nucleus.t) : (CompilerState.state, Nested.t, _) State.t =
  let open NestState.Let_syntax in
  let%map prog = nestArray prog in
  prog
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nucleus.t
  type output = Nested.t
  type error = (SB.source option, string) Source.annotate

  let name = "Nest"
  let run input = CompilerPipeline.S.make ~f:(fun state -> State.run (nest input) state)
end
