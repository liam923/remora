open! Base
module Index = Acorn.Index
module Type = Acorn.Type

type host = Acorn.Expr.host [@@deriving sexp_of]
type device = Acorn.Expr.device [@@deriving sexp_of]

let getUsesInDimension Index.{ const = _; refs } =
  refs |> Map.keys |> Set.of_list (module Identifier)
;;

let getUsesInShapeElement = function
  | Index.Add dim -> getUsesInDimension dim
  | Index.ShapeRef ref -> Set.singleton (module Identifier) ref
;;

let rec getUsesInType =
  let open Corn.Type in
  function
  | Literal _ -> Set.empty (module Identifier)
  | Array { element; size } ->
    Set.union (getUsesInType element) (getUsesInShapeElement size)
  | Sigma { parameters; body } ->
    let params =
      parameters |> List.map ~f:(fun p -> p.binding) |> Set.of_list (module Identifier)
    in
    Set.diff (getUsesInType body) params
  | Tuple elements ->
    elements |> List.map ~f:getUsesInType |> Set.union_list (module Identifier)
;;

let rec typeAsArray (type' : Type.t) ~expectedSize : Type.array =
  match type' with
  | Array array ->
    assert (Index.equal_shapeElement array.size expectedSize);
    array
  | Tuple elements ->
    Type.
      { element =
          Tuple
            (List.map elements ~f:(fun elementType ->
               (typeAsArray ~expectedSize elementType).element))
      ; size = expectedSize
      }
  | Sigma _ | Literal _ ->
    raise @@ Unreachable.Error "Expected array type or tuple of arrays"
;;

let rec typeAsTuple (type' : Type.t) : Type.tuple =
  match type' with
  | Array { element; size } ->
    List.map (typeAsTuple element) ~f:(fun element -> Type.Array { element; size })
  | Sigma { parameters; body } ->
    List.map (typeAsTuple body) ~f:(fun element ->
      Type.Sigma { parameters; body = element })
  | Tuple elements -> elements
  | Literal _ -> raise @@ Unimplemented.Error "expected tuple type"
;;

module AllocAcc = struct
  type allocation =
    { binding : Identifier.t
    ; hostOrDevice : Acorn.Mem.mallocHostOrDevice
    ; uses : Set.M(Identifier).t
    ; type' : Type.t
    }

  type ('a, 'e) t = ('a * allocation list, 'e) CompilerState.u

  include Monad.Make2 (struct
      type nonrec ('a, 'e) t = ('a, 'e) t

      let return v = CompilerState.return (v, [])

      let bind v ~f =
        let open CompilerState.Let_syntax in
        let%bind a, aAcc = v in
        let%map b, bAcc = f a in
        b, bAcc @ aAcc
      ;;

      let map =
        `Custom
          (fun v ~f ->
            let open CompilerState.Let_syntax in
            let%map a, acc = v in
            f a, acc)
      ;;
    end)

  let createId name =
    let open CompilerState.Let_syntax in
    let%map id = CompilerState.createId name in
    id, []
  ;;

  let malloc ~hostOrDevice type' name =
    let open CompilerState.Let_syntax in
    let%map binding, _ = createId name in
    let acc = { binding; hostOrDevice; uses = getUsesInType type'; type' } in
    let ref = Acorn.Mem.Ref { id = binding; type' } in
    ref, [ acc ]
  ;;

  let extractAllocations prog =
    let open CompilerState in
    let open Let_syntax in
    let%map result, acc = prog in
    (result, acc), []
  ;;

  let all_opt prog =
    let open CompilerState in
    let open Let_syntax in
    match prog with
    | Some prog ->
      let%map result, acc = prog in
      Some result, acc
    | None -> return (None, [])
  ;;

  let allocationToMemArg ?(multiplier = None) { binding; hostOrDevice; uses = _; type' }
    : Acorn.Expr.memArg
    =
    { memBinding = binding
    ; mem =
        Malloc
          { hostOrDevice
          ; type' =
              (match multiplier with
               | None -> type'
               | Some size -> Array { element = type'; size })
          }
    }
  ;;
end

type ('l, 'e) captureAvoider =
  { inExpr :
      capturesToAvoid:Set.M(Identifier).t
      -> ('l Acorn.Expr.sansCaptures, 'e) AllocAcc.t
      -> ('l Acorn.Expr.sansCaptures, 'e) AllocAcc.t
  (* ; inStatement :
      capturesToAvoid:Set.M(Identifier).t
      -> ('l Acorn.Expr.statementSansCaptures, 'e) AllocAcc.t
      -> ('l Acorn.Expr.statementSansCaptures, 'e) AllocAcc.t *)
  }

let hostAvoidCapturesPoly
  : type s.
    makeMemLet:(s Acorn.Expr.memLet -> s)
    -> capturesToAvoid:Set.M(Identifier).t
    -> (s, 'e) AllocAcc.t
    -> (s, 'e) AllocAcc.t
  =
  fun ~makeMemLet ~capturesToAvoid prog ->
  let open CompilerState in
  let open Let_syntax in
  let%bind result, allocs = prog in
  let allocsToDeclare, allocsToPropogate =
    List.partition_map allocs ~f:(fun (alloc : AllocAcc.allocation) ->
      if Set.is_empty (Set.inter capturesToAvoid alloc.uses)
      then Second alloc
      else First (AllocAcc.allocationToMemArg alloc))
  in
  let result =
    match allocsToDeclare with
    | [] -> result
    | _ :: _ ->
      if List.is_empty allocsToDeclare
      then result
      else makeMemLet { memArgs = allocsToDeclare; body = result }
  in
  return (result, allocsToPropogate)
;;

let hostAvoidCaptures () : (host, _) captureAvoider =
  { inExpr = hostAvoidCapturesPoly ~makeMemLet:(fun memLet -> Acorn.Expr.MemLet memLet)
  (* ; inStatement =
      hostAvoidCapturesPoly ~makeMemLet:(fun memLet -> Acorn.Expr.SMemLet memLet) *)
  }
;;

let deviceAvoidCapturesPoly ~capturesToAvoid prog =
  let open CompilerState in
  let open Let_syntax in
  let%bind result, acc = prog in
  let usesInAcc =
    acc
    |> List.map ~f:(fun (alloc : AllocAcc.allocation) -> alloc.uses)
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

let deviceAvoidCaptures () : (device, _) captureAvoider =
  { inExpr = deviceAvoidCapturesPoly
  (* ; inStatement = deviceAvoidCapturesPoly  *)
  }
;;

type targetAddr =
  | TargetValue of Acorn.Mem.t
  | TargetValues of targetAddr option list
[@@deriving sexp_of]

type 'l allocResult =
  { expr : 'l Acorn.Expr.sansCaptures
  ; statement : 'l Acorn.Expr.statementSansCaptures
  }

let rec allocRequest
  : type l.
    (l, 'e) captureAvoider
    -> mallocLoc:Acorn.Mem.mallocHostOrDevice
    -> writeToAddr:targetAddr option
    -> l Corn.Expr.t
    -> (l allocResult, 'e) AllocAcc.t
  =
  fun avoidCaptures ~mallocLoc ~writeToAddr:targetAddr expr ->
  let open AllocAcc in
  let open Let_syntax in
  let open Acorn in
  let alloc = allocRequest avoidCaptures ~mallocLoc in
  let getExpr { expr; statement = _ } = expr in
  let getStatement { expr = _; statement } = statement in
  let allocStatement ~writeToAddr expr = alloc ~writeToAddr expr >>| getStatement in
  let allocExpr ~writeToAddr expr = alloc ~writeToAddr expr >>| getExpr in
  let rec partialUnwrittenExprToAllocResult ~targetAddr ~exprToWriteExtractor expr =
    match targetAddr with
    | None ->
      return { expr; statement = ComputeForSideEffects (exprToWriteExtractor expr) }
    | Some (TargetValue mem) ->
      let%map binding = createId "expr-result" in
      let ref = Expr.{ id = binding; type' = Expr.type' expr } in
      { expr =
          Expr.Let
            { args = [ { binding; value = expr } ]
            ; body =
                Expr.eseq
                  ~statements:
                    [ Expr.putmem ~expr:(exprToWriteExtractor (Ref ref)) ~addr:mem ]
                  ~expr:(Ref ref)
            }
      ; statement = Expr.putmem ~expr ~addr:mem
      }
    | Some (TargetValues targetAddrs) ->
      let%bind binding = createId "expr-result" in
      let ref = Expr.{ id = binding; type' = Expr.type' expr } in
      let%map elements =
        targetAddrs
        |> List.mapi ~f:(fun index targetAddr ->
          partialUnwrittenExprToAllocResult
            ~targetAddr
            ~exprToWriteExtractor
            (Expr.tupleDeref ~tuple:(Ref ref) ~index))
        |> all
      in
      { expr =
          Let
            { args = [ { binding; value = expr } ]
            ; body =
                Expr.Values
                  { elements = List.map elements ~f:(fun e -> e.expr)
                  ; type' = typeAsTuple @@ Expr.type' expr
                  }
            }
      ; statement =
          SLet
            { args = [ { binding; value = expr } ]
            ; body = Statements (List.map elements ~f:(fun e -> e.statement))
            }
      }
  in
  let unwrittenExprToAllocResult expr =
    partialUnwrittenExprToAllocResult ~targetAddr ~exprToWriteExtractor:(fun e -> e) expr
  in
  let writtenExprToAllocResult expr = { expr; statement = ComputeForSideEffects expr } in
  let statementToAllocResult ~mem statement =
    { expr = Expr.eseq ~statements:[ statement ] ~expr:(Expr.getmem mem); statement }
  in
  let rec getMemForResult ?(targetAddr = targetAddr) type' name =
    match targetAddr with
    | None -> malloc ~hostOrDevice:mallocLoc type' name
    | Some (TargetValue memVal) -> return memVal
    | Some (TargetValues targetAddrs) ->
      let type' = typeAsTuple type' in
      List.zip_exn type' targetAddrs
      |> List.mapi ~f:(fun i (type', targetAddr) ->
        getMemForResult ~targetAddr type' [%string "%{name}.%{i#Int}"])
      |> all
      >>| Mem.values
  in
  let allocLoopBlock
    : type lOuter lInner seqOrPar.
      ((lOuter, lInner, seqOrPar, unit) Expr.loopBlock -> lOuter Expr.sansCaptures)
      -> (lOuter, 'e) captureAvoider
      -> Acorn.Mem.mallocHostOrDevice
      -> (lInner, 'e) captureAvoider
      -> Acorn.Mem.mallocHostOrDevice
      -> (lOuter, lInner, seqOrPar) Corn.Expr.loopBlock
      -> (lOuter allocResult, _) AllocAcc.t
    =
    fun wrapLoopBlock
        outerCaptureAvoider
        outerMallocLoc
        innerCaptureAvoider
        innerMallocLoc
        { frameShape
        ; mapArgs
        ; mapIotas
        ; mapBody
        ; mapBodyMatcher
        ; mapResults
        ; consumer
        ; type'
        } ->
    let mapTargetAddr, consumerTargetAddr =
      match targetAddr with
      | None -> None, None
      | Some (TargetValue targetAddr) ->
        ( Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index:0))
        , Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index:1)) )
      | Some (TargetValues [ mapTarget; consumerTarget ]) -> mapTarget, consumerTarget
      | Some (TargetValues _) -> raise @@ Unreachable.Error "Expected 2 element tuple"
    in
    let bindingsForMapBody =
      List.map mapArgs ~f:(fun arg -> arg.binding)
      @ List.map mapIotas ~f:(fun iota -> iota.iota)
      |> Set.of_list (module Identifier)
    in
    let rec createTargetAddrMapArgs type' = function
      | None ->
        let%bind mem = malloc ~hostOrDevice:mallocLoc type' "map-mem" in
        createTargetAddrMapArgs type' (Some (TargetValue mem))
      | Some (TargetValue targetValue) ->
        let type' = typeAsArray ~expectedSize:frameShape @@ Mem.type' targetValue in
        let%map binding =
          createId
            (match targetValue with
             | Ref ref -> Identifier.name ref.id
             | _ -> "map-mem")
        in
        ( Some (TargetValue (Ref { id = binding; type' = type'.element }))
        , [ Expr.{ memBinding = binding; mem = targetValue } ]
        , targetValue )
      | Some (TargetValues targetValues) ->
        let elementTypes = typeAsTuple type' in
        let%map targets, memArgs, mapMems =
          List.zip_exn elementTypes targetValues
          |> List.map ~f:(fun (t, e) -> createTargetAddrMapArgs t e)
          |> all
          >>| List.unzip3
        in
        ( Some (TargetValues targets)
        , List.concat memArgs
        , Mem.Values { elements = mapMems; type' } )
    in
    let%bind mapResultsAddr, mapTargetAddrMemArgs, mapResultMem =
      createTargetAddrMapArgs (List.nth_exn type' 0) mapTargetAddr
    in
    let mapResultTargets =
      match mapResultsAddr with
      | None ->
        mapResults
        |> List.map ~f:(fun id -> id, None)
        |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
      | Some (TargetValue targetAddr) ->
        mapResults
        |> List.mapi ~f:(fun index id ->
          id, Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index)))
        |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
      | Some (TargetValues targetAddrs) ->
        List.zip_exn mapResults targetAddrs
        |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
    in
    let rec createTargetAddr (mapBodyMatcher : Expr.tupleMatch) =
      match mapBodyMatcher with
      | Binding binding -> Map.find mapResultTargets binding |> Option.value ~default:None
      | Unpack elements -> Some (TargetValues (List.map elements ~f:createTargetAddr))
    in
    let mapBodyTargetAddr = createTargetAddr mapBodyMatcher in
    let%bind mapBody, mapBodyAllocations =
      allocRequest
        innerCaptureAvoider
        ~mallocLoc:innerMallocLoc
        ~writeToAddr:mapBodyTargetAddr
        mapBody
      >>| getExpr
      |> innerCaptureAvoider.inExpr ~capturesToAvoid:bindingsForMapBody
      |> extractAllocations
    in
    let mapMemArgs =
      mapTargetAddrMemArgs
      @ List.map mapBodyAllocations ~f:(allocationToMemArg ~multiplier:(Some frameShape))
    in
    let processReduce
      Corn.Expr.{ arg; zero; body; d; itemPad; character; type' = reduceType }
      =
      let%map zero =
        zero
        |> Option.map ~f:(fun zero ->
          allocRequest
            outerCaptureAvoider
            ~mallocLoc:outerMallocLoc
            ~writeToAddr:None
            zero
          >>| getExpr)
        |> all_opt
      and body, bodyAllocations =
        allocRequest innerCaptureAvoider ~mallocLoc:innerMallocLoc ~writeToAddr:None body
        >>| getExpr
        |> innerCaptureAvoider.inExpr
             ~capturesToAvoid:
               (Set.of_list (module Identifier) [ arg.firstBinding; arg.secondBinding ])
        |> extractAllocations
      in
      Expr.
        { arg
        ; zero
        ; mappedMemArgs =
            List.map bodyAllocations ~f:(allocationToMemArg ~multiplier:(Some frameShape))
        ; body
        ; d
        ; itemPad
        ; character
        ; type' = reduceType
        }
    in
    let processConsumer
      : type t.
        (lOuter, lInner, t) Corn.Expr.consumerOp option
        -> ((lOuter, lInner, t, unit) Expr.consumerOp option * bool, _) AllocAcc.t
      = function
      | None -> return (None, false)
      | Some (ReduceSeq reduce) ->
        let%map reduce = processReduce reduce in
        Some (Expr.ReduceSeq reduce), true
      | Some (ReducePar reduce) ->
        let%map reduce = processReduce reduce in
        Some (Expr.ReducePar reduce), true
      | Some (Fold { zeroArg; arrayArgs; body; d; itemPad; character; type' }) ->
        let%map zeroValue =
          allocRequest
            outerCaptureAvoider
            ~mallocLoc:outerMallocLoc
            ~writeToAddr:None
            zeroArg.zeroValue
          >>| getExpr
        and body, bodyAllocations =
          allocRequest
            innerCaptureAvoider
            ~mallocLoc:innerMallocLoc
            ~writeToAddr:None
            body
          >>| getExpr
          |> innerCaptureAvoider.inExpr
               ~capturesToAvoid:
                 (Set.of_list
                    (module Identifier)
                    ([ zeroArg.zeroBinding ]
                     @ List.map arrayArgs ~f:(fun arg -> arg.binding)))
          |> extractAllocations
        in
        ( Some
            (Expr.Fold
               { zeroArg = { zeroBinding = zeroArg.zeroBinding; zeroValue }
               ; arrayArgs
               ; mappedMemArgs =
                   List.map
                     bodyAllocations
                     ~f:(allocationToMemArg ~multiplier:(Some frameShape))
               ; body
               ; d
               ; itemPad
               ; character
               ; type'
               })
        , true )
      | Some (Scatter { valuesArg; indicesArg; dIn; dOut; type' }) ->
        let%map mem =
          getMemForResult ~targetAddr:consumerTargetAddr type' "scatter-array"
        in
        Some (Expr.Scatter { valuesArg; indicesArg; dIn; dOut; mem; type' }), false
    in
    let%bind consumer, consumerCopyRequired = processConsumer consumer in
    let loopBlock : (lOuter, lInner, seqOrPar, unit) Expr.loopBlock =
      { frameShape
      ; mapArgs
      ; mapIotas
      ; mapBody
      ; mapMemArgs
      ; mapBodyMatcher
      ; mapResults
      ; mapResultMem
      ; consumer
      ; type'
      }
    in
    if consumerCopyRequired
    then
      partialUnwrittenExprToAllocResult
        ~targetAddr:consumerTargetAddr
        ~exprToWriteExtractor:(fun loopBlock -> Expr.tupleDeref ~tuple:loopBlock ~index:1)
        (wrapLoopBlock loopBlock)
    else return @@ writtenExprToAllocResult (wrapLoopBlock loopBlock)
  in
  match expr with
  | Ref { id; type' } ->
    (* TODO: make let write immediately to targetAddr so no copy is necessary *)
    unwrittenExprToAllocResult (Ref { id; type' })
  | Frame { elements; dimension = _; type' } ->
    let%bind mem = getMemForResult type' "frame-array" in
    let%map statements =
      elements
      |> List.mapi ~f:(fun offset element ->
        let elementType = Corn.Expr.type' element in
        let elementTarget =
          Some
            (TargetValue
               (Mem.Index
                  { mem
                  ; offset = Index.dimensionConstant offset
                  ; elementType
                  ; type' = elementType
                  }))
        in
        allocStatement ~writeToAddr:elementTarget element)
      |> all
    in
    statementToAllocResult ~mem (Statements statements)
  | BoxValue { box; type' } ->
    let%bind box = allocExpr ~writeToAddr:None box in
    unwrittenExprToAllocResult @@ BoxValue { box; type' }
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
            let%map value = allocExpr ~writeToAddr:None value in
            Expr.Runtime value
          | FromBox { box; i } ->
            let%map box = allocExpr ~writeToAddr:None box in
            Expr.FromBox { box; i }
        in
        Expr.{ indexBinding; indexValue; sort })
      |> all
    and body =
      avoidCaptures.inExpr ~capturesToAvoid:argBindings
      @@ allocExpr ~writeToAddr:targetAddr body
    in
    writtenExprToAllocResult @@ IndexLet { indexArgs; body; type' }
  | ReifyIndex { index = Dimension dimIndex; type' = _ } ->
    unwrittenExprToAllocResult @@ ReifyDimensionIndex { dimIndex }
  | ReifyIndex { index = Shape shapeIndex; type' } ->
    (match type' with
     | Sigma { parameters = _; body = bodyType } ->
       let%map mem = getMemForResult bodyType "reify-index-array" in
       statementToAllocResult ~mem (ReifyShapeIndexToBox { shapeIndex; mem })
     | _ ->
       let%map mem = getMemForResult type' "reify-index-array" in
       statementToAllocResult ~mem (ReifyShapeIndexToArray { shapeIndex; mem }))
  | Let { args; body; type' = _ } ->
    let argBindings =
      args |> List.map ~f:(fun arg -> arg.binding) |> Set.of_list (module Identifier)
    in
    let%map args =
      args
      |> List.map ~f:(fun { binding; value } ->
        let%map value = allocExpr ~writeToAddr:None value in
        Expr.{ binding; value })
      |> all
    and body =
      avoidCaptures.inExpr ~capturesToAvoid:argBindings
      @@ allocExpr ~writeToAddr:targetAddr body
    in
    writtenExprToAllocResult @@ Let { args; body }
  | LoopBlock loopBlock ->
    allocLoopBlock
      (fun loopBlock -> Expr.LoopBlock loopBlock)
      avoidCaptures
      mallocLoc
      avoidCaptures
      mallocLoc
      loopBlock
  | LoopKernel loopBlock ->
    allocLoopBlock
      (fun loopBlock -> Expr.LoopKernel { kernel = loopBlock; captures = () })
      (hostAvoidCaptures ())
      MallocHost
      (deviceAvoidCaptures ())
      MallocDevice
      loopBlock
  | MapKernel mapKernel ->
    let rec allocMapKernel
      ~writeToAddr:targetAddr
      Corn.Expr.
        { frameShape; mapArgs; mapIotas; mapBody; mapBodyMatcher; mapResults; type' }
      : ((unit Expr.mapKernel, unit) Expr.kernel, _) AllocAcc.t
      =
      let bindingsForMapBody =
        List.map mapArgs ~f:(fun arg -> arg.binding)
        @ List.map mapIotas ~f:(fun iota -> iota.iota)
        |> Set.of_list (module Identifier)
      in
      let rec createTargetAddrMapArgs type' = function
        | None ->
          let%bind mem = malloc ~hostOrDevice:mallocLoc type' "map-mem" in
          createTargetAddrMapArgs type' (Some (TargetValue mem))
        | Some (TargetValue targetValue) ->
          let type' = typeAsArray ~expectedSize:frameShape @@ Mem.type' targetValue in
          let%map binding =
            createId
              (match targetValue with
               | Ref ref -> Identifier.name ref.id
               | _ -> "map-mem")
          in
          ( Some (TargetValue (Ref { id = binding; type' = type'.element }))
          , [ Expr.{ memBinding = binding; mem = targetValue } ]
          , targetValue )
        | Some (TargetValues targetValues) ->
          let elementTypes = typeAsTuple type' in
          let%map targets, memArgs, mapMems =
            List.zip_exn elementTypes targetValues
            |> List.map ~f:(fun (t, e) -> createTargetAddrMapArgs t e)
            |> all
            >>| List.unzip3
          in
          ( Some (TargetValues targets)
          , List.concat memArgs
          , Mem.Values { elements = mapMems; type' } )
      in
      let%bind mapResultTargetAddr, mapTargetAddrMemArgs, mapResultMem =
        createTargetAddrMapArgs type' targetAddr
      in
      let mapResultTargets =
        match mapResultTargetAddr with
        | None ->
          mapResults
          |> List.map ~f:(fun id -> id, None)
          |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
        | Some (TargetValue targetAddr) ->
          mapResults
          |> List.mapi ~f:(fun index id ->
            id, Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index)))
          |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
        | Some (TargetValues targetAddrs) ->
          List.zip_exn mapResults targetAddrs
          |> Map.of_alist_reduce (module Identifier) ~f:(fun a _ -> a)
      in
      let rec createTargetAddr (mapBodyMatcher : Expr.tupleMatch) =
        match mapBodyMatcher with
        | Binding binding ->
          Map.find mapResultTargets binding |> Option.value ~default:None
        | Unpack elements -> Some (TargetValues (List.map elements ~f:createTargetAddr))
      in
      let mapBodyTargetAddr = createTargetAddr mapBodyMatcher in
      let rec allocMapBody ~writeToAddr:targetAddr (mapBody : Corn.Expr.mapBody) =
        match mapBody with
        | MapBodyMap mapKernel ->
          let%map mapKernel, allocs =
            allocMapKernel ~writeToAddr:targetAddr mapKernel
            |> deviceAvoidCapturesPoly ~capturesToAvoid:bindingsForMapBody
            |> extractAllocations
          in
          Expr.{ statements = []; subMaps = [ mapKernel.kernel ] }, allocs
        | MapBodyExpr expr ->
          let%map expr, allocs =
            allocDevice ~writeToAddr:targetAddr expr
            |> deviceAvoidCapturesPoly ~capturesToAvoid:bindingsForMapBody
            |> extractAllocations
          in
          Expr.{ statements = [ expr.statement ]; subMaps = [] }, allocs
        | MapBodyValues elements ->
          let elementsAndTargetAddrs =
            match targetAddr with
            | None -> List.map elements ~f:(fun element -> element, None)
            | Some (TargetValue targetAddr) ->
              List.mapi elements ~f:(fun index element ->
                element, Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index)))
            | Some (TargetValues targetAddrs) -> List.zip_exn elements targetAddrs
          in
          let%map subBodies, allocss =
            elementsAndTargetAddrs
            |> List.map ~f:(fun (element, targetAddr) ->
              allocMapBody ~writeToAddr:targetAddr element)
            |> all
            >>| List.unzip
          in
          ( Expr.
              { statements = List.bind subBodies ~f:(fun subBody -> subBody.statements)
              ; subMaps = List.bind subBodies ~f:(fun subBody -> subBody.subMaps)
              }
          , List.concat allocss )
        | MapBodyDeref { tuple; index } ->
          let rec mapBodyType : Corn.Expr.mapBody -> Type.t = function
            | MapBodyMap mapKernel -> mapKernel.type'
            | MapBodyExpr expr -> Corn.Expr.type' expr
            | MapBodyValues elements -> Tuple (List.map elements ~f:mapBodyType)
            | MapBodyDeref { tuple; index } ->
              let tupleType = typeAsTuple @@ mapBodyType tuple in
              List.nth_exn tupleType index
          in
          let tupleSize = List.length @@ typeAsTuple (mapBodyType tuple) in
          let tupleTargetAddrs =
            List.init tupleSize ~f:(fun i -> if i = index then targetAddr else None)
          in
          allocMapBody ~writeToAddr:(Some (TargetValues tupleTargetAddrs)) tuple
      in
      let%map mapBody, mapBodyAllocations =
        allocMapBody ~writeToAddr:mapBodyTargetAddr mapBody
      in
      let mapMemArgs =
        mapTargetAddrMemArgs
        @ List.map
            mapBodyAllocations
            ~f:(allocationToMemArg ~multiplier:(Some frameShape))
      in
      ({ kernel =
           { frameShape
           ; mapArgs
           ; mapIotas
           ; mapBody
           ; mapMemArgs
           ; mapBodyMatcher
           ; mapResults
           ; mapResultMem
           ; type'
           }
       ; captures = ()
       }
        : (unit Expr.mapKernel, unit) Expr.kernel)
    in
    let%map mapKernel = allocMapKernel ~writeToAddr:targetAddr mapKernel in
    statementToAllocResult ~mem:mapKernel.kernel.mapResultMem @@ MapKernel mapKernel
  | Literal literal -> unwrittenExprToAllocResult @@ Literal literal
  | Box { indices; body; bodyType; type' } ->
    let%bind body = allocExpr ~writeToAddr:None body in
    unwrittenExprToAllocResult @@ Box { indices; body; bodyType; type' }
  | Values { elements; type' } ->
    let elementsAndTargetAddrs =
      match targetAddr with
      | None -> List.map elements ~f:(fun element -> element, None)
      | Some (TargetValue targetAddr) ->
        List.mapi elements ~f:(fun index element ->
          element, Some (TargetValue (Mem.tupleDeref ~tuple:targetAddr ~index)))
      | Some (TargetValues targetAddrs) -> List.zip_exn elements targetAddrs
    in
    let%map elements =
      elementsAndTargetAddrs
      |> List.map ~f:(fun (element, targetAddr) -> alloc ~writeToAddr:targetAddr element)
      |> all
    in
    (* Write is ensured because the elements were directly written to the target addrs *)
    (* EnsureWrite.mark @@ Values { elements; type' } *)
    { expr = Values { elements = List.map elements ~f:(fun r -> r.expr); type' }
    ; statement = Statements (List.map elements ~f:(fun r -> r.statement))
    }
  | ScalarPrimitive { op; args; type' } ->
    let%bind args = args |> List.map ~f:(allocExpr ~writeToAddr:None) |> all in
    unwrittenExprToAllocResult @@ ScalarPrimitive { op; args; type' }
  | TupleDeref { tuple; index; type' } ->
    let tupleSize =
      match Corn.Expr.type' tuple with
      | Tuple elements -> List.length elements
      | _ -> raise @@ Unreachable.Error "expected tuple type"
    in
    let tupleTargetAddrs =
      List.init tupleSize ~f:(fun i -> if i = index then targetAddr else None)
    in
    let%map tuple = alloc ~writeToAddr:(Some (TargetValues tupleTargetAddrs)) tuple in
    { expr = TupleDeref { tuple = tuple.expr; index; type' }
    ; statement = tuple.statement
    }
  | SubArray { arrayArg; indexArg; type' } ->
    let%bind arrayArg = allocExpr ~writeToAddr:None arrayArg
    and indexArg = allocExpr ~writeToAddr:None indexArg in
    unwrittenExprToAllocResult @@ SubArray { arrayArg; indexArg; type' }
  | Append { args; type' } ->
    let%bind mem = getMemForResult type' "append-array" in
    let%map statements =
      args
      |> List.folding_map ~init:(Index.dimensionConstant 0) ~f:(fun offset arg ->
        let argType =
          match Corn.Expr.type' arg with
          | Array array -> array
          | _ -> raise @@ Unreachable.Error "expected array type"
        in
        let elementType = argType.element in
        let argCount =
          match argType.size with
          | Add dim -> dim
          | ShapeRef _ -> raise @@ Unreachable.Error "expected dimension index"
        in
        let newOffset = Index.addDimensions offset argCount in
        let argTarget =
          Some
            (TargetValue (Mem.Index { mem; offset; elementType; type' = Array argType }))
        in
        newOffset, allocStatement ~writeToAddr:argTarget arg)
      |> all
    in
    statementToAllocResult ~mem (Statements statements)
  | Zip { zipArg; nestCount = _; type' = _ } -> alloc ~writeToAddr:targetAddr zipArg
  | Unzip { unzipArg; type' = _ } -> alloc ~writeToAddr:targetAddr unzipArg
  | IfParallelismHitsCutoff { parallelism; cutoff; then'; else'; type' } ->
    let%bind then' = allocExpr ~writeToAddr:targetAddr then'
    and else' = allocExpr ~writeToAddr:targetAddr else' in
    unwrittenExprToAllocResult
    @@ IfParallelismHitsCutoff { parallelism; cutoff; then'; else'; type' }

and allocHost ~writeToAddr expr =
  allocRequest (hostAvoidCaptures ()) ~mallocLoc:MallocHost ~writeToAddr expr

and allocDevice ~writeToAddr expr : (device allocResult, _) AllocAcc.t =
  allocRequest (deviceAvoidCaptures ()) ~mallocLoc:MallocDevice ~writeToAddr expr
;;

let alloc (expr : Corn.t)
  : (Acorn.sansCaptures, ('s option, string) Source.annotate) CompilerState.u
  =
  let open CompilerState.Let_syntax in
  let%map result, acc = allocHost ~writeToAddr:None expr in
  Acorn.Expr.MemLet
    { memArgs = List.map acc ~f:AllocAcc.allocationToMemArg; body = result.expr }
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Corn.t
  type output = Acorn.sansCaptures
  type error = (SB.source option, string) Source.annotate

  let name = "Alloc"

  let run input =
    CompilerPipeline.S.makeF ~f:(fun state -> CompilerState.run (alloc input) state)
  ;;
end
