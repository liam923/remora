open! Base
open Nucleus

module BindingSet = struct
  type t =
    | Bindings of Set.M(Identifier).t
    | All
  [@@deriving sexp_of]

  let union a b =
    match a, b with
    | Bindings a, Bindings b -> Bindings (Set.union a b)
    | All, _ | _, All -> All
  ;;

  let of_list l = Bindings (Set.of_list (module Identifier) l)
end

module Counts : sig
  type t [@@deriving sexp_of]

  val empty : t
  val one : Identifier.t -> t
  val get : t -> Identifier.t -> int
  val merge : t list -> t
  val usesAny : t -> BindingSet.t -> bool
end = struct
  type t = int Map.M(Identifier).t [@@deriving sexp_of]

  let empty = Map.empty (module Identifier)
  let one id = Map.singleton (module Identifier) id 1
  let get counts id = Map.find counts id |> Option.value ~default:0

  let merge countss =
    List.fold countss ~init:empty ~f:(fun acc more ->
      Map.merge_skewed acc more ~combine:(fun ~key:_ a b -> a + b))
  ;;

  let usesAny counts bindings =
    match bindings with
    | BindingSet.Bindings bindings ->
      let keys = Map.keys counts |> Set.of_list (module Identifier) in
      not (Set.are_disjoint keys bindings)
    | BindingSet.All -> true
  ;;
end

let scalar atom =
  Nucleus.Expr.(
    AtomAsArray { element = atom; type' = { element = atomType atom; shape = [] } })
;;

(* Determine if an expression is a constant expression or if it requires
   computation. i.e. 7 and x are nonComputational,
   while (+ 7 x) is computational *)
let nonComputational =
  let rec nonComputationalArray : Expr.array -> bool = function
    | Ref _ -> true
    | AtomAsArray atomicArray -> nonComputationalAtom atomicArray.element
    | Frame frame -> List.for_all frame.elements ~f:nonComputationalArray
    | BoxValue { box; type' = _ } -> nonComputationalArray box
    | IndexLet _ -> false
    | ReifyIndex _ -> false
    | ArrayPrimitive _ -> false
  and nonComputationalAtom : Expr.atom -> bool = function
    | Box _ -> false
    | Literal (IntLiteral _ | CharacterLiteral _ | BooleanLiteral _) -> true
    | ArrayAsAtom arrayAtomic -> nonComputationalArray arrayAtomic.array
    | AtomicPrimitive _ -> false
    | Values values -> List.for_all values.elements ~f:nonComputationalAtom
    | TupleDeref { tuple; index = _; type' = _ } -> nonComputationalAtom tuple
  in
  nonComputationalArray
;;

let getCounts =
  let getCountsIndex : Index.t -> Counts.t = function
    | Shape elements ->
      elements
      |> List.map ~f:(function
        | ShapeRef ref -> Counts.one ref
        | Add { const = _; refs } ->
          refs |> Map.keys |> List.map ~f:Counts.one |> Counts.merge)
      |> Counts.merge
    | Dimension { const = _; refs } ->
      refs |> Map.keys |> List.map ~f:Counts.one |> Counts.merge
  in
  let rec getCountsArray : Expr.array -> Counts.t = function
    | Ref { id; type' = _ } -> Counts.one id
    | AtomAsArray { element; type' = _ } -> getCountsAtom element
    | Frame { elements; dimensions = _; type' = _ } ->
      elements |> List.map ~f:getCountsArray |> Counts.merge
    | BoxValue { box; type' = _ } -> getCountsArray box
    | IndexLet { indexArgs; body; type' = _ } ->
      let indexValueCounts =
        List.map indexArgs ~f:(fun arg ->
          match arg.indexValue with
          | Runtime value -> getCountsArray value
          | FromBox { box; i = _ } -> getCountsArray box)
      and bodyCounts = getCountsArray body in
      Counts.merge (bodyCounts :: indexValueCounts)
    | ReifyIndex { index; type' = _ } -> getCountsIndex index
    | ArrayPrimitive (Map { args; iotaVar = _; body; frameShape; type' = _ }) ->
      let argsCounts =
        args
        |> List.map ~f:(fun { binding = _; value } -> getCountsArray value)
        |> Counts.merge
      and bodyCounts = getCountsArray body
      and frameShapeCounts = getCountsIndex (Shape frameShape) in
      Counts.merge [ argsCounts; bodyCounts; frameShapeCounts ]
    | ArrayPrimitive
        (Reduce
          { args
          ; body
          ; zero
          ; d
          ; itemPad
          ; cellShape
          ; associative = _
          ; character = _
          ; type' = _
          }) ->
      let argsCounts =
        args
        |> List.map ~f:(fun { firstBinding = _; secondBinding = _; value } ->
          getCountsArray value)
        |> Counts.merge
      and bodyCounts = getCountsArray body
      and zeroCounts =
        zero |> Option.map ~f:getCountsArray |> Option.value ~default:Counts.empty
      and dCounts = getCountsIndex (Dimension d)
      and itemPadCounts = getCountsIndex (Shape itemPad)
      and cellShapeCounts = getCountsIndex (Shape cellShape) in
      Counts.merge
        [ argsCounts; bodyCounts; zeroCounts; dCounts; itemPadCounts; cellShapeCounts ]
    | ArrayPrimitive
        (Fold
          { zeroArgs; arrayArgs; body; d; itemPad; cellShape; character = _; type' = _ })
      ->
      let zeroCounts =
        zeroArgs
        |> List.map ~f:(fun { binding = _; value } -> getCountsArray value)
        |> Counts.merge
      and arrayCounts =
        arrayArgs
        |> List.map ~f:(fun { binding = _; value } -> getCountsArray value)
        |> Counts.merge
      and bodyCounts = getCountsArray body
      and dCounts = getCountsIndex (Dimension d)
      and itemPadCounts = getCountsIndex (Shape itemPad)
      and cellShapeCounts = getCountsIndex (Shape cellShape) in
      Counts.merge
        [ zeroCounts; arrayCounts; bodyCounts; dCounts; itemPadCounts; cellShapeCounts ]
    | ArrayPrimitive (Append { arg1; arg2; d1; d2; cellShape; type' = _ }) ->
      Counts.merge
        [ getCountsArray arg1
        ; getCountsArray arg2
        ; getCountsIndex (Dimension d1)
        ; getCountsIndex (Dimension d2)
        ; getCountsIndex (Shape cellShape)
        ]
    | ArrayPrimitive (Index { arrayArg; indexArg; s; cellShape; l; type' = _ }) ->
      Counts.merge
        [ getCountsArray arrayArg
        ; getCountsArray indexArg
        ; getCountsIndex (Shape s)
        ; getCountsIndex (Shape cellShape)
        ; getCountsIndex (Dimension l)
        ]
    | ArrayPrimitive (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' = _ })
      ->
      Counts.merge
        [ getCountsArray valuesArg
        ; getCountsArray indicesArg
        ; getCountsIndex (Dimension dIn)
        ; getCountsIndex (Dimension dOut)
        ; getCountsIndex (Shape cellShape)
        ]
  and getCountsAtom : Expr.atom -> Counts.t = function
    | Literal (IntLiteral _) -> Counts.empty
    | Literal (CharacterLiteral _) -> Counts.empty
    | Literal (BooleanLiteral _) -> Counts.empty
    | Box { indices; body; bodyType = _; type' = _ } ->
      let indexCounts = List.map indices ~f:getCountsIndex
      and bodyCounts = getCountsArray body in
      Counts.merge (bodyCounts :: indexCounts)
    | ArrayAsAtom { array; type' = _ } -> getCountsArray array
    | AtomicPrimitive { op = _; args; type' = _ } ->
      args |> List.map ~f:getCountsAtom |> Counts.merge
    | Values { elements; type' = _ } ->
      elements |> List.map ~f:getCountsAtom |> Counts.merge
    | TupleDeref { tuple; index = _; type' = _ } -> getCountsAtom tuple
  in
  getCountsArray
;;

let rec subArray subs : Expr.array -> Expr.array = function
  | Ref { id; type' = _ } as expr ->
    (match Map.find subs id with
     | Some subValue -> subValue
     | None -> expr)
  | AtomAsArray { element; type' } ->
    AtomAsArray { element = subAtom subs element; type' }
  | Frame { elements; dimensions; type' } ->
    let elements = List.map elements ~f:(subArray subs) in
    Frame { elements; dimensions; type' }
  | BoxValue { box; type' } -> BoxValue { box = subArray subs box; type' }
  | IndexLet { indexArgs; body; type' } ->
    let indexArgs =
      List.map indexArgs ~f:(fun { indexBinding; indexValue; sort } ->
        Expr.
          { indexBinding
          ; indexValue =
              (match indexValue with
               | Runtime value -> Runtime (subArray subs value)
               | FromBox { box; i } -> FromBox { box = subArray subs box; i })
          ; sort
          })
    and body = subArray subs body in
    IndexLet { indexArgs; body; type' }
  | ReifyIndex { index; type' } -> ReifyIndex { index; type' }
  | ArrayPrimitive (Map { args; iotaVar; body; frameShape; type' }) ->
    let args =
      List.map args ~f:(fun { binding; value } ->
        Expr.{ binding; value = subArray subs value })
    and body = subArray subs body in
    ArrayPrimitive (Map { args; iotaVar; body; frameShape; type' })
  | ArrayPrimitive
      (Reduce { args; body; zero; d; itemPad; cellShape; associative; character; type' })
    ->
    let args =
      List.map args ~f:(fun { firstBinding; secondBinding; value } : Expr.reduceArg ->
        { firstBinding; secondBinding; value = subArray subs value })
    and body = subArray subs body
    and zero = Option.map zero ~f:(subArray subs) in
    ArrayPrimitive
      (Reduce { args; body; zero; d; itemPad; cellShape; associative; character; type' })
  | ArrayPrimitive
      (Fold { zeroArgs; arrayArgs; body; d; itemPad; cellShape; character; type' }) ->
    let zeroArgs =
      List.map zeroArgs ~f:(fun { binding; value } ->
        Expr.{ binding; value = subArray subs value })
    and arrayArgs =
      List.map arrayArgs ~f:(fun { binding; value } ->
        Expr.{ binding; value = subArray subs value })
    and body = subArray subs body in
    ArrayPrimitive
      (Fold { zeroArgs; arrayArgs; body; d; itemPad; cellShape; character; type' })
  | ArrayPrimitive (Append { arg1; arg2; d1; d2; cellShape; type' }) ->
    let arg1 = subArray subs arg1
    and arg2 = subArray subs arg2 in
    ArrayPrimitive (Append { arg1; arg2; d1; d2; cellShape; type' })
  | ArrayPrimitive (Index { arrayArg; indexArg; s; cellShape; l; type' }) ->
    let arrayArg = subArray subs arrayArg
    and indexArg = subArray subs indexArg in
    ArrayPrimitive (Index { arrayArg; indexArg; s; cellShape; l; type' })
  | ArrayPrimitive (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' }) ->
    let valuesArg = subArray subs valuesArg
    and indicesArg = subArray subs indicesArg in
    Expr.ArrayPrimitive (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' })

and subAtom subs : Expr.atom -> Expr.atom = function
  | Literal (IntLiteral _ | CharacterLiteral _ | BooleanLiteral _) as lit -> lit
  | Box { indices; body; bodyType; type' } ->
    let body = subArray subs body in
    Box { indices; body; bodyType; type' }
  | ArrayAsAtom { array; type' } ->
    let array = subArray subs array in
    ArrayAsAtom { array; type' }
  | AtomicPrimitive { op; args; type' } ->
    let args = List.map args ~f:(subAtom subs) in
    AtomicPrimitive { op; args; type' }
  | Values { elements; type' } ->
    let elements = List.map elements ~f:(subAtom subs) in
    Values { elements; type' }
  | TupleDeref { tuple; index; type' } ->
    let tuple = subAtom subs tuple in
    TupleDeref { tuple; index; type' }
;;

(* Perform the following optimizations:
   - Copy propogation
   - Delete unused variables
   - Inline variables only used once
   - Inline variables with constant value
   - Constant folding *)
let rec optimizeArray : Expr.array -> Expr.array = function
  | Ref _ as ref -> ref
  | AtomAsArray { element; type' } ->
    let element = optimizeAtom element in
    Expr.AtomAsArray { element; type' }
  | Frame { elements; dimensions; type' } ->
    let elements = List.map elements ~f:optimizeArray in
    (* Flatten the frame if all elements are frames *)
    let frames =
      elements
      |> List.map ~f:(fun e ->
        match e with
        | Frame f -> Some f
        | _ -> None)
      |> List.fold ~init:(Some []) ~f:(fun prev next ->
        let%map.Option prev = prev
        and next = next in
        next :: prev)
      |> Option.map ~f:List.rev
    in
    (match frames with
     | None | Some [] ->
       (* Not all frames (or it is empty) so proceed normally *)
       Expr.Frame { elements; dimensions; type' }
     | Some (headFrame :: restFrames) ->
       (* All frames, so need to flatten *)
       let frames : Expr.frame NeList.t = headFrame :: restFrames in
       let commonDims, numCommonDims =
         frames
         |> NeList.map ~f:(fun f -> f.dimensions, List.length f.dimensions)
         |> NeList.min_elt ~compare:(fun (_, a) (_, b) -> Int.compare a b)
       in
       let elements =
         frames
         |> NeList.to_list
         |> List.bind ~f:(fun { elements; dimensions; type' } ->
           let _, keptDimensions = List.split_n dimensions numCommonDims in
           match keptDimensions with
           | [] -> elements
           | _ :: _ as keptDimensions ->
             let keptNumPerFrame = List.fold keptDimensions ~init:1 ~f:( * ) in
             elements
             |> List.groupi ~break:(fun i _ _ -> i % keptNumPerFrame = 0)
             |> List.map ~f:(fun elements ->
               Expr.Frame
                 { elements
                 ; dimensions = keptDimensions
                 ; type' =
                     { element = type'.element
                     ; shape = List.drop type'.shape numCommonDims
                     }
                 }))
       in
       let dimensions = dimensions @ commonDims in
       Expr.Frame { elements; dimensions; type' })
  | BoxValue { box; type' } ->
    let box = optimizeArray box in
    (match box with
     | AtomAsArray
         { element = Box { indices = _; body; bodyType = _; type' = _ }; type' = _ } ->
       body
     | _ -> BoxValue { box; type' })
  | IndexLet { indexArgs; body; type' } ->
    (* TODO: remove unused bindings and inline ones known statically *)
    let indexArgs =
      List.map indexArgs ~f:(fun { indexBinding; indexValue; sort } ->
        Expr.
          { indexBinding
          ; indexValue =
              (match indexValue with
               | Runtime value -> Runtime (optimizeArray value)
               | FromBox { box; i } -> FromBox { box = optimizeArray box; i })
          ; sort
          })
    in
    let body = optimizeArray body in
    IndexLet { indexArgs; body; type' }
  | ReifyIndex { index = Dimension d; type' = _ } as original ->
    if Map.is_empty d.refs
    then
      (* The dimension is known, so we can sub it in *)
      scalar (Literal (IntLiteral d.const))
    else original
  | ReifyIndex { index = Shape shape; type' } as original ->
    let revDims =
      List.fold shape ~init:(Some []) ~f:(fun revDims nextDim ->
        match revDims, nextDim with
        | Some revDims, Add d when Map.is_empty d.refs -> Some (d.const :: revDims)
        | _ -> None)
    in
    (match revDims with
     | Some revDims ->
       let dims = List.rev revDims in
       Expr.Frame
         { elements = List.map dims ~f:(fun dim -> scalar (Literal (IntLiteral dim)))
         ; dimensions = [ List.length dims ]
         ; type'
         }
     | None -> original)
  | ArrayPrimitive (Append { arg1; arg2; d1; d2; cellShape; type' }) as append ->
    let arg1 = optimizeArray arg1
    and arg2 = optimizeArray arg2 in
    (match arg1, arg2 with
     | ( Frame { elements = elements1; dimensions = dim1 :: restDims1; type' = _ }
       , Frame { elements = elements2; dimensions = dim2 :: restDims2; type' = _ } ) ->
       let chunkElements elements restDims =
         let chunkLength = List.fold restDims ~init:1 ~f:( * ) in
         let rawChunks = List.groupi elements ~break:(fun i _ _ -> i % chunkLength = 0) in
         let chunks =
           List.map rawChunks ~f:(fun elements ->
             let firstElement = List.hd_exn elements in
             let elementType = Expr.arrayType firstElement in
             let dimsAsShape =
               List.map restDims ~f:(fun d -> Nucleus.Index.(Add (dimensionConstant d)))
             in
             Expr.Frame
               { elements
               ; dimensions = restDims
               ; type' =
                   { element = elementType.element
                   ; shape = dimsAsShape @ elementType.shape
                   }
               })
         in
         chunks
       in
       let chunks1 = chunkElements elements1 restDims1
       and chunks2 = chunkElements elements2 restDims2 in
       let appendedChunks = chunks1 @ chunks2 in
       let frame =
         Expr.Frame
           { elements = appendedChunks
           ; dimensions = [ dim1 + dim2 ]
           ; type' = Expr.arrayType append
           }
       in
       optimizeArray frame
     | _ -> Expr.ArrayPrimitive (Append { arg1; arg2; d1; d2; cellShape; type' }))
  | ArrayPrimitive (Map { args; iotaVar = Some iotaVar; body; frameShape = []; type' }) ->
    let body =
      subArray
        (Map.singleton (module Identifier) iotaVar (scalar (Literal (IntLiteral 0))))
        body
    in
    optimizeArray
      (Expr.ArrayPrimitive (Map { args; iotaVar = None; body; frameShape = []; type' }))
  | ArrayPrimitive (Map { args; iotaVar = None; body; frameShape = []; type' }) ->
    (* Do an initial simplification of the argument values and the body *)
    let args =
      List.map args ~f:(fun Expr.{ binding; value } ->
        let value = optimizeArray value in
        Expr.{ binding; value })
    in
    let body = optimizeArray body in
    (* Until it cannot be done anymore, inline args that can be propogated and then
       simplify the body. Also, remove unused args. *)
    let rec loop (args : Expr.arg list) body =
      let bodyCounts = getCounts body in
      let _, argsToPropogate, args =
        List.partition3_map args ~f:(fun arg ->
          match Counts.get bodyCounts arg.binding with
          | 0 ->
            (* DISCARD!!! *)
            `Fst ()
          | 1 -> `Snd arg
          | _ when nonComputational arg.value -> `Snd arg
          | _ -> `Trd arg)
      in
      match argsToPropogate with
      | [] -> args, body
      | _ :: _ as argsToPropogate ->
        let subs =
          List.fold
            argsToPropogate
            ~init:(Map.empty (module Identifier))
            ~f:(fun subs (arg : Expr.arg) ->
              Map.set subs ~key:arg.binding ~data:arg.value)
        in
        let body = subArray subs body in
        let body = optimizeArray body in
        loop args body
    in
    let args, body = loop args body in
    (* If args are now empty, just return the body, as the map is now redundant *)
    (match args with
     | [] -> body
     | args -> ArrayPrimitive (Map { args; iotaVar = None; body; frameShape = []; type' }))
  | ArrayPrimitive (Map { args; iotaVar; body; frameShape = _ :: _ as frameShape; type' })
    ->
    let body = optimizeArray body in
    (* Simplify the args, removing unused ones *)
    let bodyCounts = getCounts body in
    let args =
      args
      |> List.filter ~f:(fun arg -> (* DISCARD!!! *)
                                    Counts.get bodyCounts arg.binding > 0)
      |> List.map ~f:(fun { binding; value } ->
        let value = optimizeArray value in
        Expr.{ binding; value })
    in
    Expr.ArrayPrimitive (Map { args; iotaVar; body; frameShape; type' })
  | ArrayPrimitive
      (Reduce { args; body; zero; d; itemPad; cellShape; associative; character; type' })
    ->
    let body = optimizeArray body
    and zero = Option.map zero ~f:optimizeArray in
    (* Simplify the args, removing unused ones *)
    let bodyCounts = getCounts body in
    let args =
      args
      |> List.filter ~f:(fun arg ->
        (* DISCARD!!! *)
        Counts.get bodyCounts arg.firstBinding > 0
        || Counts.get bodyCounts arg.secondBinding > 0)
      |> List.map ~f:(fun { firstBinding; secondBinding; value } ->
        let value = optimizeArray value in
        ({ firstBinding; secondBinding; value } : Expr.reduceArg))
    in
    Expr.ArrayPrimitive
      (Reduce { args; body; zero; d; itemPad; cellShape; associative; character; type' })
  | ArrayPrimitive
      (Fold { zeroArgs; arrayArgs; body; d; itemPad; cellShape; character; type' }) ->
    let body = optimizeArray body
    and zeroArgs =
      List.map zeroArgs ~f:(fun { binding; value } ->
        Expr.{ binding; value = optimizeArray value })
    in
    (* Simplify the zero args, removing unused ones *)
    let bodyCounts = getCounts body in
    let arrayArgs =
      arrayArgs
      |> List.filter ~f:(fun arg -> (* DISCARD!!! *)
                                    Counts.get bodyCounts arg.binding > 0)
      |> List.map ~f:(fun { binding; value } ->
        let value = optimizeArray value in
        Expr.{ binding; value })
    in
    Expr.ArrayPrimitive
      (Fold { zeroArgs; arrayArgs; body; d; itemPad; cellShape; character; type' })
  | ArrayPrimitive (Index { arrayArg; indexArg; s; cellShape; l; type' }) ->
    let arrayArg = optimizeArray arrayArg
    and indexArg = optimizeArray indexArg in
    Expr.ArrayPrimitive (Index { arrayArg; indexArg; s; cellShape; l; type' })
  | ArrayPrimitive (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' }) ->
    let valuesArg = optimizeArray valuesArg
    and indicesArg = optimizeArray indicesArg in
    Expr.ArrayPrimitive (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' })

and optimizeAtom : Expr.atom -> Expr.atom = function
  | Box { indices; body; bodyType; type' } ->
    let body = optimizeArray body in
    Expr.Box { indices; body; bodyType; type' }
  | Literal _ as literal -> literal
  | ArrayAsAtom { array; type' } ->
    let array = optimizeArray array in
    (match array with
     | AtomAsArray { element; type' = _ } -> element
     | _ -> Expr.ArrayAsAtom { array; type' })
  | AtomicPrimitive { op; args; type' } ->
    let args = List.map args ~f:optimizeAtom in
    (* Do constant folding: *)
    (match op, args with
     | Add, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (IntLiteral (a + b))
     | Add, [ Literal (IntLiteral 0); value ] | Add, [ value; Literal (IntLiteral 0) ] ->
       value
     | Sub, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (IntLiteral (a - b))
     | Sub, [ value; Literal (IntLiteral 0) ] -> value
     | Mul, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (IntLiteral (a * b))
     | Mul, [ Literal (IntLiteral 1); value ] | Mul, [ value; Literal (IntLiteral 1) ] ->
       value
     | Mul, [ Literal (IntLiteral 0); _ ] | Mul, [ _; Literal (IntLiteral 0) ] ->
       (* DISCARD!!! *)
       Literal (IntLiteral 0)
     | Div, [ Literal (IntLiteral a); Literal (IntLiteral b) ] ->
       Literal (IntLiteral (a / b))
     | Div, [ value; Literal (IntLiteral 1) ] -> value
     | _ -> Expr.AtomicPrimitive { op; args; type' })
  | Values { elements; type' } ->
    let elements = List.map elements ~f:optimizeAtom in
    Values { elements; type' }
  | TupleDeref { tuple; index; type' } ->
    let tuple = optimizeAtom tuple in
    (match tuple with
     | Values { elements; type' = _ } -> List.nth_exn elements index
     | _ -> TupleDeref { tuple; index; type' })
;;

type hoisting =
  { variableDeclaration : Expr.arg
  ; counts : Counts.t
  }
[@@deriving sexp_of]

let hoistDeclarationsMap l ~f =
  let results, hoistings = l |> List.map ~f |> List.unzip in
  results, List.join hoistings
;;

let minDimensionSize (dimension : Index.dimension) = dimension.const

let minShapeSize (shape : Index.shape) =
  let minComponentSizes =
    List.map shape ~f:(function
      | Add d -> minDimensionSize d
      | ShapeRef _ -> 0)
  in
  List.fold minComponentSizes ~init:1 ~f:( * )
;;

(* Hoist variables that can be hoisted. Maps are also cleaned up while doing
   this. (nested maps with empty frames that can be flattened are, and maps
   with empty frames and no args are removed) *)
let rec hoistDeclarationsInArray : Expr.array -> Expr.array * hoisting list = function
  | Ref _ as ref -> ref, []
  | AtomAsArray { element; type' } ->
    let element, hoistings = hoistDeclarationsInAtom element in
    Expr.AtomAsArray { element; type' }, hoistings
  | Frame { elements; dimensions; type' } ->
    let elements, hoistings = hoistDeclarationsMap elements ~f:hoistDeclarationsInArray in
    Expr.Frame { elements; dimensions; type' }, hoistings
  | BoxValue { box; type' } ->
    let box, hoistings = hoistDeclarationsInArray box in
    BoxValue { box; type' }, hoistings
  | IndexLet { indexArgs; body; type' } ->
    let bindings =
      indexArgs |> List.map ~f:(fun arg -> arg.indexBinding) |> BindingSet.of_list
    in
    let indexArgs, indexValueHoistings =
      hoistDeclarationsMap indexArgs ~f:(fun { indexBinding; indexValue; sort } ->
        let indexValue, hoistings =
          match indexValue with
          | Runtime value ->
            let value, hoistings = hoistDeclarationsInArray value in
            Expr.Runtime value, hoistings
          | FromBox { box; i } ->
            let box, hoistings = hoistDeclarationsInArray box in
            Expr.FromBox { box; i }, hoistings
        in
        Expr.{ indexBinding; indexValue; sort }, hoistings)
    in
    let body, bodyHoistings = hoistDeclarationsInBody body ~bindings in
    Expr.IndexLet { indexArgs; body; type' }, indexValueHoistings @ bodyHoistings
  | ReifyIndex _ as reify -> reify, []
  | ArrayPrimitive (Append { arg1; arg2; d1; d2; cellShape; type' }) ->
    let arg1, hoistings1 = hoistDeclarationsInArray arg1
    and arg2, hoistings2 = hoistDeclarationsInArray arg2 in
    ( Expr.ArrayPrimitive (Append { arg1; arg2; d1; d2; cellShape; type' })
    , hoistings1 @ hoistings2 )
  | ArrayPrimitive (Map { args; iotaVar = Some iotaVar; body; frameShape = []; type' }) ->
    let body =
      subArray
        (Map.singleton (module Identifier) iotaVar (scalar (Literal (IntLiteral 0))))
        body
    in
    hoistDeclarationsInArray
      (Expr.ArrayPrimitive (Map { args; iotaVar = None; body; frameShape = []; type' }))
  | ArrayPrimitive (Map { args; iotaVar = None; body; frameShape = []; type' = _ }) ->
    let body, bodyHoistings = hoistDeclarationsInArray body in
    let args, argHoistings =
      hoistDeclarationsMap args ~f:(fun { binding; value } ->
        let value, hoistings = hoistDeclarationsInArray value in
        Expr.{ binding; value }, hoistings)
    in
    let argsAsHoistings =
      List.map args ~f:(fun arg ->
        { variableDeclaration = arg; counts = getCounts arg.value })
    in
    body, argHoistings @ argsAsHoistings @ bodyHoistings
  | ArrayPrimitive (Map { args; iotaVar; body; frameShape = _ :: _ as frameShape; type' })
    ->
    let bindings =
      if minShapeSize frameShape > 0
      then (
        (* The loop will be run at least once, so hoisting out of the loop
           is ok *)
        let argBindings = args |> List.map ~f:(fun arg -> arg.binding) in
        Option.to_list iotaVar @ argBindings |> BindingSet.of_list)
      else
        (* The loop could be run 0 times, so nothing can be hoisted out *)
        BindingSet.All
    in
    let body, bodyHoistings = hoistDeclarationsInBody body ~bindings in
    let args, argHoistings =
      hoistDeclarationsMap args ~f:(fun { binding; value } ->
        let value, hoistings = hoistDeclarationsInArray value in
        Expr.{ binding; value }, hoistings)
    in
    ( Expr.ArrayPrimitive (Map { args; iotaVar; body; frameShape; type' })
    , argHoistings @ bodyHoistings )
  | ArrayPrimitive
      (Reduce { args; body; zero; d; itemPad; cellShape; associative; character; type' })
    ->
    let bindings =
      if minDimensionSize d >= 2
      then
        args
        |> List.bind ~f:(fun arg -> [ arg.firstBinding; arg.secondBinding ])
        |> BindingSet.of_list
      else
        (* The body of the reduce is not guaranteed to run, so hoisting is unsafe *)
        BindingSet.All
    in
    let body, bodyHoistings = hoistDeclarationsInBody body ~bindings in
    let args, argHoistings =
      hoistDeclarationsMap args ~f:(fun { firstBinding; secondBinding; value } ->
        let value, hoistings = hoistDeclarationsInArray value in
        ({ firstBinding; secondBinding; value } : Expr.reduceArg), hoistings)
    in
    let zero, zeroHoistings =
      match zero with
      | None -> None, []
      | Some zero ->
        let zero, hoistings = hoistDeclarationsInArray zero in
        Some zero, hoistings
    in
    ( Expr.ArrayPrimitive
        (Reduce { args; body; zero; d; itemPad; cellShape; associative; character; type' })
    , argHoistings @ bodyHoistings @ zeroHoistings )
  | ArrayPrimitive
      (Fold { zeroArgs; arrayArgs; body; d; itemPad; cellShape; character; type' }) ->
    let bindings =
      if minDimensionSize d >= 1
      then
        zeroArgs @ arrayArgs |> List.map ~f:(fun arg -> arg.binding) |> BindingSet.of_list
      else
        (* The body of the fold is not guaranteed to run, so hoisting is unsafe *)
        BindingSet.All
    in
    let body, bodyHoistings = hoistDeclarationsInBody body ~bindings in
    let zeroArgs, zeroArgHoistings =
      hoistDeclarationsMap zeroArgs ~f:(fun { binding; value } ->
        let value, hoistings = hoistDeclarationsInArray value in
        Expr.{ binding; value }, hoistings)
    in
    let arrayArgs, arrayArgHoistings =
      hoistDeclarationsMap arrayArgs ~f:(fun { binding; value } ->
        let value, hoistings = hoistDeclarationsInArray value in
        Expr.{ binding; value }, hoistings)
    in
    ( Expr.ArrayPrimitive
        (Fold { zeroArgs; arrayArgs; body; d; itemPad; cellShape; character; type' })
    , zeroArgHoistings @ arrayArgHoistings @ bodyHoistings )
  | ArrayPrimitive (Index { arrayArg; indexArg; s; cellShape; l; type' }) ->
    let arrayArg, arrayHoistings = hoistDeclarationsInArray arrayArg
    and indexArg, indexHoistings = hoistDeclarationsInArray indexArg in
    ( Expr.ArrayPrimitive (Index { arrayArg; indexArg; s; cellShape; l; type' })
    , arrayHoistings @ indexHoistings )
  | ArrayPrimitive (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' }) ->
    let valuesArg, valuesHoistings = hoistDeclarationsInArray valuesArg
    and indicesArg, indicesHoistings = hoistDeclarationsInArray indicesArg in
    ( Expr.ArrayPrimitive (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' })
    , valuesHoistings @ indicesHoistings )

and hoistDeclarationsInAtom : Expr.atom -> Expr.atom * hoisting list = function
  | Box { indices; body; bodyType; type' } ->
    let body, hoistings = hoistDeclarationsInArray body in
    Box { indices; body; bodyType; type' }, hoistings
  | Literal _ as literal -> literal, []
  | ArrayAsAtom { array; type' } ->
    let array, hoistings = hoistDeclarationsInArray array in
    ArrayAsAtom { array; type' }, hoistings
  | AtomicPrimitive { op; args; type' } ->
    let args, hoistings = hoistDeclarationsMap args ~f:hoistDeclarationsInAtom in
    AtomicPrimitive { op; args; type' }, hoistings
  | Values { elements; type' } ->
    let elements, hoistings = hoistDeclarationsMap elements ~f:hoistDeclarationsInAtom in
    Values { elements; type' }, hoistings
  | TupleDeref { tuple; index; type' } ->
    let tuple, hoistings = hoistDeclarationsInAtom tuple in
    TupleDeref { tuple; index; type' }, hoistings

and hoistDeclarationsInBody body ~bindings : Expr.array * hoisting list =
  (* Simplify the body *)
  let body, bodyHoistings = hoistDeclarationsInArray body in
  (* Find which hoistings need to be declared due to a dependency on bindings.
     This can be indirect through another hoisting since hoistings can depend
     on each other. *)
  let findHoistingsToDeclare hoistings =
    let rec loop ~toDeclare ~queue ~barrier =
      let newDelcarations, queue =
        List.partition_map queue ~f:(fun hoisting ->
          if Counts.usesAny hoisting.counts barrier
          then First hoisting
          else Second hoisting)
      in
      match newDelcarations with
      | [] -> toDeclare, queue
      | _ :: _ ->
        let toDeclare = newDelcarations @ toDeclare in
        let barrier =
          List.fold toDeclare ~init:barrier ~f:(fun barrier dec ->
            match barrier with
            | All -> All
            | Bindings bindings ->
              Bindings (Set.add bindings dec.variableDeclaration.binding))
        in
        loop ~toDeclare ~queue ~barrier
    in
    loop ~toDeclare:[] ~queue:hoistings ~barrier:bindings
  in
  let hoistingsToDeclare, hoistingsToPropogate = findHoistingsToDeclare bodyHoistings in
  (* Hoistings in hoistingsToDeclare can depend on each other.
     resolveHoistingDependencies creates a list of lists of the hoistings,
     where each list of hoistings depends only on the lists following it *)
  let resolveHoistingDependencies hoistings =
    let rec loop hoistings undeclared declared =
      match hoistings with
      | [] -> declared
      | _ :: _ as hoistings ->
        (* Hoistings can't depend on each other cyclicly, so there is always
           going to be at least one declarable *)
        let declarable, undeclarable =
          List.partition_map hoistings ~f:(fun h ->
            if Counts.usesAny h.counts (Bindings undeclared) then Second h else First h)
        in
        assert (not (List.is_empty declarable));
        let newUndeclared =
          List.fold declarable ~init:undeclared ~f:(fun undeclared declaration ->
            Set.remove undeclared declaration.variableDeclaration.binding)
        in
        loop undeclarable newUndeclared (declarable :: declared)
    in
    loop
      hoistings
      (hoistings
       |> List.map ~f:(fun h -> h.variableDeclaration.binding)
       |> Set.of_list (module Identifier))
      []
  in
  let hoistingsToDeclare = resolveHoistingDependencies hoistingsToDeclare in
  (* Modify the body to declare the hoistings *)
  let body =
    List.fold hoistingsToDeclare ~init:body ~f:(fun body hoistings ->
      Expr.ArrayPrimitive
        (Map
           { args = List.map hoistings ~f:(fun h -> h.variableDeclaration)
           ; iotaVar = None
           ; body
           ; frameShape = []
           ; type' = Expr.arrayType body
           }))
  in
  body, hoistingsToPropogate
;;

module HoistState = struct
  include State

  type state = CompilerState.state
  type ('a, 'e) u = (state, 'a, 'e) t

  let createId name =
    make ~f:(fun state ->
      State.run
        (Identifier.create
           name
           ~getCounter:(fun (s : state) -> s.idCounter)
           ~setCounter:(fun _ idCounter -> CompilerState.{ idCounter }))
        state)
  ;;

  let toSimplifyState (s : ('t, _) u) : (CompilerState.state, 't, _) State.t = s
end

let hoistExpressionsMap l ~f =
  let open HoistState.Let_syntax in
  let%map results, hoistings = l |> List.map ~f |> HoistState.all |> HoistState.unzip in
  results, List.join hoistings
;;

(* Hoist expressions that remain constant over a loop out of the loop.
   loopBarreir is the set of variables that are declared by the loop surrounding
   the current expression. Expressions will be hoisted iff they don't use
   any variables in the loopBarrier AND the expression is "computational".
   (see the function nonComputational) *)
let rec hoistExpressionsInArray loopBarrier (array : Expr.array)
  : (Expr.array * hoisting list, _) HoistState.u
  =
  let open HoistState.Let_syntax in
  let counts = getCounts array in
  if (not (Counts.usesAny counts loopBarrier)) && not (nonComputational array)
  then (
    (* This expression can and should be hoisted, so create a variable for it *)
    let%map id = HoistState.createId "hoistedExp" in
    let hoisting = { variableDeclaration = { binding = id; value = array }; counts } in
    Expr.Ref { id; type' = Expr.arrayType array }, [ hoisting ])
  else (
    match array with
    | Ref _ as ref -> return (ref, [])
    | AtomAsArray { element; type' } ->
      let%map element, hoistings = hoistExpressionsInAtom loopBarrier element in
      Expr.AtomAsArray { element; type' }, hoistings
    | Frame { elements; dimensions; type' } ->
      let%map elements, hoistings =
        hoistExpressionsMap elements ~f:(hoistExpressionsInArray loopBarrier)
      in
      Expr.Frame { elements; dimensions; type' }, hoistings
    | BoxValue { box; type' } ->
      let%map box, hoistings = hoistExpressionsInArray loopBarrier box in
      Expr.BoxValue { box; type' }, hoistings
    | IndexLet { indexArgs; body; type' } ->
      let bindings =
        indexArgs |> List.map ~f:(fun arg -> arg.indexBinding) |> BindingSet.of_list
      in
      let%map indexArgs, indexValueHoistings =
        hoistExpressionsMap indexArgs ~f:(fun { indexBinding; indexValue; sort } ->
          let%map indexValue, hoistings =
            match indexValue with
            | Runtime value ->
              let%map value, hoistings = hoistExpressionsInArray loopBarrier value in
              Expr.Runtime value, hoistings
            | FromBox { box; i } ->
              let%map box, hoistings = hoistExpressionsInArray loopBarrier box in
              Expr.FromBox { box; i }, hoistings
          in
          Expr.{ indexBinding; indexValue; sort }, hoistings)
      and body, bodyHoistings = hoistExpressionsInBody loopBarrier body ~bindings in
      Expr.IndexLet { indexArgs; body; type' }, indexValueHoistings @ bodyHoistings
    | ReifyIndex _ as reify -> return (reify, [])
    | ArrayPrimitive (Append { arg1; arg2; d1; d2; cellShape; type' }) ->
      let%map arg1, hoistings1 = hoistExpressionsInArray loopBarrier arg1
      and arg2, hoistings2 = hoistExpressionsInArray loopBarrier arg2 in
      ( Expr.ArrayPrimitive (Append { arg1; arg2; d1; d2; cellShape; type' })
      , hoistings1 @ hoistings2 )
    | ArrayPrimitive (Map { args; iotaVar; body; frameShape; type' }) ->
      let%bind args, argHoistings =
        hoistExpressionsMap args ~f:(fun { binding; value } ->
          let%map value, hoistings = hoistExpressionsInArray loopBarrier value in
          Expr.{ binding; value }, hoistings)
      in
      let bindings =
        if minShapeSize frameShape > 0
        then (
          (* The loop will be run at least once, so hoisting out of the loop
             is ok *)
          let argBindings = args |> List.map ~f:(fun arg -> arg.binding) in
          Option.to_list iotaVar @ argBindings |> BindingSet.of_list)
        else
          (* The loop could be run 0 times, so nothing can be hoisted out *)
          BindingSet.All
      in
      (* If the map is not just a let, then we update the loop barrier to be
         the args of the map *)
      let bodyLoopBarrier =
        match frameShape with
        | [] -> loopBarrier
        | _ :: _ -> bindings
      in
      let%map body, bodyHoistings =
        hoistExpressionsInBody bodyLoopBarrier body ~bindings
      in
      ( Expr.ArrayPrimitive (Map { args; iotaVar; body; frameShape; type' })
      , argHoistings @ bodyHoistings )
    | ArrayPrimitive
        (Reduce
          { args; body; zero; d; itemPad; cellShape; associative; character; type' }) ->
      let%bind args, argHoistings =
        hoistExpressionsMap args ~f:(fun { firstBinding; secondBinding; value } ->
          let%map value, hoistings = hoistExpressionsInArray loopBarrier value in
          ({ firstBinding; secondBinding; value } : Expr.reduceArg), hoistings)
      in
      let bindings =
        if minDimensionSize d >= 2
        then
          args
          |> List.bind ~f:(fun arg -> [ arg.firstBinding; arg.secondBinding ])
          |> BindingSet.of_list
        else
          (* The body of the reduce is not guaranteed to run, so hoisting is unsafe *)
          BindingSet.All
      in
      let%map body, bodyHoistings = hoistExpressionsInBody bindings body ~bindings
      and zero, zeroHoistings =
        match zero with
        | None -> return (None, [])
        | Some zero ->
          let%map zero, hoistings = hoistExpressionsInArray loopBarrier zero in
          Some zero, hoistings
      in
      ( Expr.ArrayPrimitive
          (Reduce
             { args; body; zero; d; itemPad; cellShape; associative; character; type' })
      , argHoistings @ bodyHoistings @ zeroHoistings )
    | ArrayPrimitive
        (Fold { zeroArgs; arrayArgs; body; d; itemPad; cellShape; character; type' }) ->
      let%bind zeroArgs, zeroArgsHoistings =
        hoistExpressionsMap zeroArgs ~f:(fun { binding; value } ->
          let%map value, hoistings = hoistExpressionsInArray loopBarrier value in
          Expr.{ binding; value }, hoistings)
      and arrayArgs, arrayArgsHoistings =
        hoistExpressionsMap arrayArgs ~f:(fun { binding; value } ->
          let%map value, hoistings = hoistExpressionsInArray loopBarrier value in
          Expr.{ binding; value }, hoistings)
      in
      let bindings =
        if minDimensionSize d >= 1
        then
          zeroArgs @ arrayArgs
          |> List.map ~f:(fun arg -> arg.binding)
          |> BindingSet.of_list
        else
          (* The body of the fold is not guaranteed to run, so hoisting is unsafe *)
          BindingSet.All
      in
      let%map body, bodyHoistings = hoistExpressionsInBody bindings body ~bindings in
      ( Expr.ArrayPrimitive
          (Fold { zeroArgs; arrayArgs; body; d; itemPad; cellShape; character; type' })
      , zeroArgsHoistings @ arrayArgsHoistings @ bodyHoistings )
    | ArrayPrimitive (Index { arrayArg; indexArg; s; cellShape; l; type' }) ->
      let%map arrayArg, arrayHoistings = hoistExpressionsInArray loopBarrier arrayArg
      and indexArg, indexHoistings = hoistExpressionsInArray loopBarrier indexArg in
      ( Expr.ArrayPrimitive (Index { arrayArg; indexArg; s; cellShape; l; type' })
      , arrayHoistings @ indexHoistings )
    | ArrayPrimitive (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' }) ->
      let%map valuesArg, valuesHoistings = hoistExpressionsInArray loopBarrier valuesArg
      and indicesArg, indicesHoistings = hoistExpressionsInArray loopBarrier indicesArg in
      ( Expr.ArrayPrimitive
          (Scatter { valuesArg; indicesArg; dIn; dOut; cellShape; type' })
      , valuesHoistings @ indicesHoistings ))

and hoistExpressionsInAtom loopBarrier
  : Expr.atom -> (Expr.atom * hoisting list, _) HoistState.u
  =
  let open HoistState.Let_syntax in
  function
  | Box { indices; body; bodyType; type' } ->
    let%map body, hoistings = hoistExpressionsInArray loopBarrier body in
    Expr.Box { indices; body; bodyType; type' }, hoistings
  | Literal _ as literal -> return (literal, [])
  | ArrayAsAtom { array; type' } ->
    let%map array, hoistings = hoistExpressionsInArray loopBarrier array in
    Expr.ArrayAsAtom { array; type' }, hoistings
  | AtomicPrimitive { op; args; type' } ->
    let%map args, hoistings =
      hoistExpressionsMap args ~f:(hoistExpressionsInAtom loopBarrier)
    in
    Expr.AtomicPrimitive { op; args; type' }, hoistings
  | Values { elements; type' } ->
    let%map elements, hoistings =
      hoistExpressionsMap elements ~f:(hoistExpressionsInAtom loopBarrier)
    in
    Expr.Values { elements; type' }, hoistings
  | TupleDeref { tuple; index; type' } ->
    let%map tuple, hoistings = hoistExpressionsInAtom loopBarrier tuple in
    Expr.TupleDeref { tuple; index; type' }, hoistings

and hoistExpressionsInBody loopBarrier body ~bindings =
  let open HoistState.Let_syntax in
  (* Hoist from the body, and then determine which hoistings can be propogated
     outside the body, and which need to be declared in the body *)
  let extendedLoopBarrier = BindingSet.union loopBarrier bindings in
  let%bind body, bodyHoistings = hoistExpressionsInArray extendedLoopBarrier body in
  let hoistingsToDeclare, hoistingsToPropogate =
    List.partition_map bodyHoistings ~f:(fun hoisting ->
      if Counts.usesAny hoisting.counts bindings
      then First hoisting.variableDeclaration
      else Second hoisting)
  in
  (* The hoistings' values may have expression of their own that can be hoisted
     beyond this body *)
  let%map hoistingsToDeclare, moreHoistingsToPropogate =
    hoistExpressionsMap hoistingsToDeclare ~f:(fun { binding; value } ->
      let%map value, hoistings = hoistExpressionsInArray loopBarrier value in
      Expr.{ binding; value }, hoistings)
  in
  (* Declare the hoistings that need to be declared. *)
  let body =
    match hoistingsToDeclare with
    | [] ->
      (* None to declare *)
      body
    | _ :: _ as hoistingsToDeclare ->
      (* There are some to be declared, so wrap the body in a map *)
      Expr.ArrayPrimitive
        (Map
           { args = hoistingsToDeclare
           ; iotaVar = None
           ; body
           ; frameShape = []
           ; type' = Expr.arrayType body
           })
  in
  body, moreHoistingsToPropogate @ hoistingsToPropogate
;;

let simplify expr =
  let open State.Let_syntax in
  let rec loop expr =
    (* Hoist variables that can be hoisted *)
    let hoisted, hoistings = hoistDeclarationsInBody expr ~bindings:All in
    assert (List.length hoistings = 0);
    let unoptimized = hoisted in
    let optimized = optimizeArray unoptimized in
    if Expr.equal_array unoptimized optimized
    then (
      let%map result, hoistings =
        hoistExpressionsInBody All optimized ~bindings:All |> HoistState.toSimplifyState
      in
      assert (List.length hoistings = 0);
      result)
    else loop optimized
  in
  loop expr
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nucleus.t
  type output = Nucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "Simplify"

  let run input =
    CompilerPipeline.S.make ~f:(fun state -> State.run (simplify input) state)
  ;;
end
