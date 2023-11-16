open! Base
module Index = McCorn.Index
module Type = McCorn.Type

type host = Corn.Expr.host [@@deriving sexp_of]
type device = Corn.Expr.device [@@deriving sexp_of]

module AllocState = struct
  include CompilerState
end

let typeAsArray = function
  | Type.Array array -> array
  | _ -> raise @@ Unreachable.Error "expected array type"
;;

let getUsesInDimension Index.{ const = _; refs } =
  refs |> Map.keys |> Set.of_list (module Identifier)
;;

let rec getUsesInShapeElement = function
  | Index.Add dim -> getUsesInDimension dim
  | Index.ShapeRef ref -> Set.singleton (module Identifier) ref
;;

let getUsesInShape shapeElements =
  shapeElements |> List.map ~f:getUsesInShapeElement |> Set.union_list (module Identifier)
;;

let rec getUsesInIndex = function
  | Index.Dimension dim -> getUsesInDimension dim
  | Index.Shape shape -> getUsesInShape shape
;;

let rec getUsesInType = function
  | Type.Literal _ -> Set.empty (module Identifier)
  | Type.Array { element; size } ->
    Set.union (getUsesInType element) (getUsesInShapeElement size)
  | Type.Sigma { parameters; body } ->
    let params =
      parameters |> List.map ~f:(fun p -> p.binding) |> Set.of_list (module Identifier)
    in
    Set.diff (getUsesInType body) params
  | Type.Tuple elements ->
    elements |> List.map ~f:getUsesInType |> Set.union_list (module Identifier)
;;

let rec flattenTupleMatch (matcher : Corn.Expr.tupleMatch) =
  match matcher with
  | Binding id -> Set.singleton (module Identifier) id
  | Unpack matchers ->
    matchers |> List.map ~f:flattenTupleMatch |> Set.union_list (module Identifier)
;;

let rec getUses (expr : device Corn.Expr.t) : Set.M(Identifier).t =
  match expr with
  | Ref ref -> Set.singleton (module Identifier) ref.id
  | Frame { elements; dimension = _; type' = _ } ->
    elements |> List.map ~f:getUses |> Set.union_list (module Identifier)
  | BoxValue { box; type' = _ } -> getUses box
  | IndexLet { indexArgs; body; type' } ->
    let argBindings, argUses =
      indexArgs
      |> List.map ~f:(fun arg ->
        ( arg.indexBinding
        , match arg.indexValue with
          | Runtime value -> getUses value
          | FromBox { box; i = _ } -> getUses box ))
      |> List.unzip
    in
    let bodyUses =
      Set.diff (getUses body) (Set.of_list (module Identifier) argBindings)
    in
    Set.union_list (module Identifier) (bodyUses :: getUsesInType type' :: argUses)
  | ReifyIndex { index; type' } -> Set.union (getUsesInIndex index) (getUsesInType type')
  | Let { args; body; type' } ->
    let argBindings, argUses =
      args |> List.map ~f:(fun { binding; value } -> binding, getUses value) |> List.unzip
    in
    let bodyUses =
      Set.diff (getUses body) (Set.of_list (module Identifier) argBindings)
    in
    Set.union_list (module Identifier) (bodyUses :: getUsesInType type' :: argUses)
  | LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults = _
      ; consumer
      ; type'
      } ->
    let frameShapeUses = getUsesInShapeElement frameShape in
    let argUsesList, argBindings =
      mapArgs |> List.map ~f:(fun { binding; ref } -> ref.id, binding) |> List.unzip
    in
    let argUses = Set.of_list (module Identifier) argUsesList in
    let iotaUsesNested, iotaBindings =
      mapIotas
      |> List.map ~f:(fun { iota; nestIn } -> Option.to_list nestIn, iota)
      |> List.unzip
    in
    let iotaUses = iotaUsesNested |> List.concat |> Set.of_list (module Identifier) in
    let bindings = Set.of_list (module Identifier) (argBindings @ iotaBindings) in
    let bodyUses = Set.diff (getUses mapBody) bindings in
    let typeUses = getUsesInType (Tuple type') in
    let consumerUsesWithMapValues =
      match consumer with
      | None -> Set.empty (module Identifier)
      | Some (ReduceSeq { arg; zero; body; d; itemPad; character = _; type' }) ->
        let argBindings =
          Set.of_list (module Identifier) [ arg.secondBinding; arg.secondBinding ]
        in
        let zeroUses =
          zero
          |> Option.map ~f:getUses
          |> Option.value ~default:(Set.empty (module Identifier))
        in
        let bodyUses = Set.diff (getUses body) argBindings in
        Set.union_list
          (module Identifier)
          [ zeroUses
          ; bodyUses
          ; getUsesInDimension d
          ; getUsesInShape itemPad
          ; getUsesInType type'
          ]
      | Some (Fold { zeroArg; arrayArgs; body; d; itemPad; character = _; type' }) ->
        let zeroUses = getUses zeroArg.zeroValue in
        let argBindings =
          Set.of_list
            (module Identifier)
            (zeroArg.zeroBinding :: List.map arrayArgs ~f:(fun arg -> arg.binding))
        in
        let bodyUses = Set.diff (getUses body) argBindings in
        Set.union_list
          (module Identifier)
          [ zeroUses
          ; bodyUses
          ; getUsesInDimension d
          ; getUsesInShape itemPad
          ; getUsesInType type'
          ]
      | Some (Scatter { valuesArg = _; indicesArg = _; dIn; dOut; type' }) ->
        Set.union_list
          (module Identifier)
          [ getUsesInDimension dIn; getUsesInDimension dOut; getUsesInType type' ]
    in
    let mapValues = flattenTupleMatch mapBodyMatcher in
    let consumerUses = Set.diff consumerUsesWithMapValues mapValues in
    Set.union_list
      (module Identifier)
      [ frameShapeUses; argUses; iotaUses; bodyUses; consumerUses; typeUses ]
  | Literal _ -> Set.empty (module Identifier)
  | Box { indices; body; bodyType = _; type' } ->
    Set.union_list
      (module Identifier)
      (getUses body :: getUsesInType (Sigma type') :: List.map indices ~f:getUsesInIndex)
  | Values { elements; type' } ->
    Set.union_list
      (module Identifier)
      (getUsesInType (Tuple type') :: List.map elements ~f:getUses)
  | ScalarPrimitive { op = _; args; type' } ->
    Set.union_list (module Identifier) (getUsesInType type' :: List.map args ~f:getUses)
  | TupleDeref { tuple; index = _; type' } ->
    Set.union (getUses tuple) (getUsesInType type')
  | SubArray { arrayArg; indexArg; type' } ->
    Set.union_list
      (module Identifier)
      [ getUses arrayArg; getUses indexArg; getUsesInType type' ]
  | Append { args; type' } ->
    Set.union_list (module Identifier) (getUsesInType type' :: List.map args ~f:getUses)
  | Zip { zipArg; nestCount = _; type' } ->
    Set.union (getUses zipArg) (getUsesInType type')
  | Unzip { unzipArg; type' } ->
    Set.union (getUses unzipArg) (getUsesInType (Tuple type'))
;;

module AllocAcc = struct
  type allocation =
    { binding : Identifier.t
    ; type' : Type.array
    }

  type ('a, 'e) t = ('a * allocation list, 'e) AllocState.u

  include Monad.Make2 (struct
      type nonrec ('a, 'e) t = ('a, 'e) t

      let return v = AllocState.return (v, [])

      let bind v ~f =
        let open AllocState.Let_syntax in
        let%bind a, aAcc = v in
        let%map b, bAcc = f a in
        b, bAcc @ aAcc
      ;;

      let map =
        `Custom
          (fun v ~f ->
            let open AllocState.Let_syntax in
            let%map a, acc = v in
            f a, acc)
      ;;
    end)

  let mallocDevice type' name =
    let open AllocState.Let_syntax in
    let%map binding = AllocState.createId name in
    let acc = { binding; type' } in
    let ref : McCorn.Expr.ref = { id = binding; type' = Array type' } in
    ref, [ acc ]
  ;;

  let avoidCaptures capturesToAvoid prog =
    let open AllocState in
    let open Let_syntax in
    let%bind result, acc = prog in
    let usesInAcc =
      acc
      |> List.map ~f:(fun alloc -> getUsesInType @@ Array alloc.type')
      |> Set.union_list (module Identifier)
    in
    let intersection = Set.inter usesInAcc capturesToAvoid in
    match Set.to_list intersection with
    | [] -> return (result, acc)
    | captured :: _ ->
      let capturedStr = Identifier.show captured in
      CompilerState.error
        [%string
          "Cannot allocate array of unknown size inside kernel; allocation relies on \
           %{capturedStr}"]
  ;;

  let extractAllocations prog =
    let open AllocState in
    let open Let_syntax in
    let%map result, acc = prog in
    (result, acc), []
  ;;

  let all_opt prog =
    let open AllocState in
    let open Let_syntax in
    match prog with
    | Some prog ->
      let%map result, acc = prog in
      Some result, acc
    | None -> return (None, [])
  ;;
end

let rec allocInDevice (expr : device Corn.Expr.t) : (device McCorn.Expr.t, _) AllocAcc.t =
  let open AllocAcc in
  let open Let_syntax in
  let open McCorn.Expr in
  match expr with
  | Ref ref -> return @@ Ref ref
  | Frame { elements; dimension; type' } ->
    let%map elements = elements |> List.map ~f:allocInDevice |> all
    and memRef = mallocDevice (typeAsArray type') "frame-array" in
    Frame { elements; dimension; mem = Ref memRef; type' }
  | BoxValue { box; type' } ->
    let%map box = allocInDevice box in
    BoxValue { box; type' }
  | IndexLet { indexArgs; body; type' } ->
    let argBindings =
      indexArgs
      |> List.map ~f:(fun arg -> arg.indexBinding)
      |> Set.of_list (module Identifier)
    in
    let%map indexArgs =
      indexArgs
      |> List.map ~f:(fun { indexBinding; indexValue; sort } ->
        let%map indexValue =
          match indexValue with
          | Runtime value ->
            let%map value = allocInDevice value in
            Runtime value
          | FromBox { box; i } ->
            let%map box = allocInDevice box in
            FromBox { box; i }
        in
        { indexBinding; indexValue; sort })
      |> all
    and body = avoidCaptures argBindings @@ allocInDevice body in
    IndexLet { indexArgs; body; type' }
  | ReifyIndex reifyIndex -> return @@ ReifyIndex reifyIndex
  | Let { args; body; type' } ->
    let argBindings =
      args |> List.map ~f:(fun arg -> arg.binding) |> Set.of_list (module Identifier)
    in
    let%map args =
      args
      |> List.map ~f:(fun { binding; value } ->
        let%map value = allocInDevice value in
        { binding; value })
      |> all
    and body = avoidCaptures argBindings @@ allocInDevice body in
    Let { args; body; type' }
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
    let bindingsForMapBody =
      List.map mapArgs ~f:(fun arg -> arg.binding)
      @ List.map mapIotas ~f:(fun iota -> iota.iota)
      |> Set.of_list (module Identifier)
    in
    let%bind mapBody, mapBodyAllocations =
      allocInDevice mapBody |> avoidCaptures bindingsForMapBody |> extractAllocations
    in
    let%bind additionalMapArgs =
      mapBodyAllocations
      |> List.map ~f:(fun { binding; type' } ->
        let%map ref =
          mallocDevice
            { element = Array type'; size = frameShape }
            (Identifier.name binding)
        in
        ({ binding; ref } : McCorn.Expr.mapArg))
      |> all
    in
    let mapArgs = mapArgs @ additionalMapArgs in
    let%bind consumer =
      match consumer with
      | None -> return None
      | Some (ReduceSeq { arg; zero; body; d; itemPad; character; type' }) ->
        let%bind body, bodyAllocations = extractAllocations @@ allocInDevice body
        and zero = zero |> Option.map ~f:allocInDevice |> all_opt in
        let%map mappedArgs =
          bodyAllocations
          |> List.map ~f:(fun { binding; type' } ->
            let%map memRef =
              mallocDevice
                { element = Array type'; size = Add d }
                (Identifier.name binding)
            in
            ({ binding; value = Ref memRef } : device McCorn.Expr.letArg))
          |> all
        in
        Some (ReduceSeq { arg; zero; mappedArgs; body; d; itemPad; character; type' })
      | Some (Fold { zeroArg; arrayArgs; body; d; itemPad; character; type' }) ->
        let%bind body, bodyAllocations = extractAllocations @@ allocInDevice body
        and zeroValue = allocInDevice zeroArg.zeroValue in
        let%map mappedArgs =
          bodyAllocations
          |> List.map ~f:(fun { binding; type' } ->
            let%map memRef =
              mallocDevice
                { element = Array type'; size = Add d }
                (Identifier.name binding)
            in
            ({ binding; value = Ref memRef } : device McCorn.Expr.letArg))
          |> all
        in
        Some
          (Fold
             { zeroArg = { zeroBinding = zeroArg.zeroBinding; zeroValue }
             ; arrayArgs
             ; mappedArgs
             ; body
             ; d
             ; itemPad
             ; character
             ; type'
             })
      | Some (Scatter { valuesArg; indicesArg; dIn; dOut; type' }) ->
        let%map memRef = mallocDevice (typeAsArray type') "scatter-array" in
        Some (Scatter { valuesArg; indicesArg; dIn; dOut; mem = Ref memRef; type' })
    in
    let%map mapMemRef = mallocDevice (typeAsArray (List.nth_exn type' 0)) "loop" in
    LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; mapMem = Ref mapMemRef
      ; consumer
      ; type'
      }
  | Literal literal -> return @@ Literal literal
  | Box { indices; body; bodyType; type' } ->
    let%map body = allocInDevice body in
    Box { indices; body; bodyType; type' }
  | Values { elements; type' } ->
    let%map elements = elements |> List.map ~f:allocInDevice |> all in
    Values { elements; type' }
  | ScalarPrimitive { op; args; type' } ->
    let%map args = args |> List.map ~f:allocInDevice |> all in
    ScalarPrimitive { op; args; type' }
  | TupleDeref { tuple; index; type' } ->
    let%map tuple = allocInDevice tuple in
    TupleDeref { tuple; index; type' }
  | SubArray { arrayArg; indexArg; type' } ->
    let%map arrayArg = allocInDevice arrayArg
    and indexArg = allocInDevice indexArg in
    SubArray { arrayArg; indexArg; type' }
  | Append { args; type' } ->
    let%map args = args |> List.map ~f:allocInDevice |> all
    and memRef = mallocDevice (typeAsArray type') "appended-array" in
    Append { args; mem = Ref memRef; type' }
  | Zip { zipArg; nestCount; type' } ->
    let%map zipArg = allocInDevice zipArg in
    Zip { zipArg; nestCount; type' }
  | Unzip { unzipArg; type' } ->
    let%map unzipArg = allocInDevice unzipArg in
    Unzip { unzipArg; type' }
;;

let rec allocInHost (expr : host Corn.Expr.t) : (host McCorn.Expr.t, _) AllocState.u =
  let open AllocState in
  let open Let_syntax in
  let open McCorn.Expr in
  match expr with
  | Ref ref -> return @@ Ref ref
  | Frame { elements; dimension; type' } ->
    let%map elements = elements |> List.map ~f:allocInHost |> all in
    Frame
      { elements
      ; dimension
      ; mem = Malloc { hostOrDevice = MallocHost; type' = typeAsArray type' }
      ; type'
      }
  | BoxValue { box; type' } ->
    let%map box = allocInHost box in
    BoxValue { box; type' }
  | IndexLet { indexArgs; body; type' } ->
    let%map indexArgs =
      indexArgs
      |> List.map ~f:(fun { indexBinding; indexValue; sort } ->
        let%map indexValue =
          match indexValue with
          | Runtime value ->
            let%map value = allocInHost value in
            Runtime value
          | FromBox { box; i } ->
            let%map box = allocInHost box in
            FromBox { box; i }
        in
        { indexBinding; indexValue; sort })
      |> all
    and body = allocInHost body in
    IndexLet { indexArgs; body; type' }
  | ReifyIndex reifyIndex -> return @@ ReifyIndex reifyIndex
  | Let { args; body; type' } ->
    let%map args =
      args
      |> List.map ~f:(fun { binding; value } ->
        let%map value = allocInHost value in
        { binding; value })
      |> all
    and body = allocInHost body in
    Let { args; body; type' }
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
    let%map mapBody = allocInHost mapBody
    and consumer =
      match consumer with
      | None -> return None
      | Some (ReduceSeq { arg; zero; body; d; itemPad; character; type' }) ->
        let%map zero = zero |> Option.map ~f:allocInHost |> all_opt
        and body = allocInHost body in
        Some
          (ReduceSeq { arg; zero; mappedArgs = []; body; d; itemPad; character; type' })
      | Some (Fold { zeroArg; arrayArgs; body; d; itemPad; character; type' }) ->
        let%map zeroValue = allocInHost zeroArg.zeroValue
        and body = allocInHost body in
        Some
          (Fold
             { zeroArg = { zeroBinding = zeroArg.zeroBinding; zeroValue }
             ; arrayArgs
             ; mappedArgs = []
             ; body
             ; d
             ; itemPad
             ; character
             ; type'
             })
      | Some (Scatter { valuesArg; indicesArg; dIn; dOut; type' }) ->
        return
        @@ Some
             (Scatter
                { valuesArg
                ; indicesArg
                ; dIn
                ; dOut
                ; mem = Malloc { hostOrDevice = MallocHost; type' = typeAsArray type' }
                ; type'
                })
    in
    LoopBlock
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapBodyMatcher
      ; mapResults
      ; mapMem =
          Malloc
            { hostOrDevice = MallocHost
            ; type' = { element = McCorn.Expr.type' mapBody; size = frameShape }
            }
      ; consumer
      ; type'
      }
  | LoopKernel _ -> raise Unimplemented.default
  | MapKernel _ -> raise Unimplemented.default
  | Literal literal -> return @@ Literal literal
  | Box { indices; body; bodyType; type' } ->
    let%map body = allocInHost body in
    Box { indices; body; bodyType; type' }
  | Values { elements; type' } ->
    let%map elements = elements |> List.map ~f:allocInHost |> all in
    Values { elements; type' }
  | ScalarPrimitive { op; args; type' } ->
    let%map args = args |> List.map ~f:allocInHost |> all in
    ScalarPrimitive { op; args; type' }
  | TupleDeref { tuple; index; type' } ->
    let%map tuple = allocInHost tuple in
    TupleDeref { tuple; index; type' }
  | SubArray { arrayArg; indexArg; type' } ->
    let%map arrayArg = allocInHost arrayArg
    and indexArg = allocInHost indexArg in
    SubArray { arrayArg; indexArg; type' }
  | Append { args; type' } ->
    let%map args = args |> List.map ~f:allocInHost |> all in
    Append
      { args
      ; mem = Malloc { hostOrDevice = MallocHost; type' = typeAsArray type' }
      ; type'
      }
  | Zip { zipArg; nestCount; type' } ->
    let%map zipArg = allocInHost zipArg in
    Zip { zipArg; nestCount; type' }
  | Unzip { unzipArg; type' } ->
    let%map unzipArg = allocInHost unzipArg in
    Unzip { unzipArg; type' }
  | IfParallelismHitsCutoff { parallelism; cutoff; then'; else'; type' } ->
    let%map then' = allocInHost then'
    and else' = allocInHost else' in
    IfParallelismHitsCutoff { parallelism; cutoff; then'; else'; type' }
;;

let alloc (expr : Corn.t) : (McCorn.t, _) CompilerState.u =
  let open AllocState.Let_syntax in
  allocInHost expr
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Corn.t
  type output = McCorn.t
  type error = (SB.source option, string) Source.annotate

  let name = "Alloc"

  let run input =
    CompilerPipeline.S.makeF ~f:(fun state -> CompilerState.run (alloc input) state)
  ;;
end
