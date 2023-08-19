open! Base
open InlineNucleus

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
  InlineNucleus.Expr.(
    Scalar { element = atom; type' = { element = atomType atom; shape = [] } })
;;

(* Determine if an expression is a constant expression or if it requires
   computation. i.e. 7 and x are nonComputational,
   while (+ 7 x) is computational *)
let nonComputational =
  let rec nonComputationalArray : Expr.array -> bool = function
    | Ref _ -> true
    | Scalar scalar -> nonComputationalAtom scalar.element
    | Frame frame -> List.for_all frame.elements ~f:nonComputationalArray
    | Unbox _ -> false
    | PrimitiveCall _ -> false
    | IntrinsicCall _ -> false
  and nonComputationalAtom : Expr.atom -> bool = function
    | Box _ -> false
    | Literal (IntLiteral _ | CharacterLiteral _ | BooleanLiteral _ | UnitLiteral) -> true
  in
  nonComputationalArray
;;

let getCounts =
  let rec getCountsArray : Expr.array -> Counts.t = function
    | Ref { id; type' = _ } -> Counts.one id
    | Scalar { element; type' = _ } -> getCountsAtom element
    | Frame { elements; dimensions = _; type' = _ } ->
      elements |> List.map ~f:getCountsArray |> Counts.merge
    | Unbox { indexBindings = _; boxBindings; body; type' = _ } ->
      let boxBindingsCounts =
        boxBindings
        |> List.map ~f:(fun { binding = _; box } -> getCountsArray box)
        |> Counts.merge
      and bodyCounts = getCountsArray body in
      Counts.merge [ boxBindingsCounts; bodyCounts ]
    | PrimitiveCall { op = _; args; type' = _ } ->
      args |> List.map ~f:getCountsArray |> Counts.merge
    | IntrinsicCall (Map { args; body; frameShape = _; type' = _ }) ->
      let argsCounts =
        args
        |> List.map ~f:(fun { binding = _; value } -> getCountsArray value)
        |> Counts.merge
      and bodyCounts = getCountsArray body in
      Counts.merge [ argsCounts; bodyCounts ]
    | IntrinsicCall
        (Reduce { args; body; t = _; dSub1 = _; itemPad = _; cellShape = _; type' = _ })
      ->
      let argsCounts =
        args
        |> List.map ~f:(fun { firstBinding = _; secondBinding = _; value } ->
          getCountsArray value)
        |> Counts.merge
      and bodyCounts = getCountsArray body in
      Counts.merge [ argsCounts; bodyCounts ]
    | IntrinsicCall
        (Scan { args; body; t = _; dSub1 = _; itemPad = _; cellShape = _; type' = _ }) ->
      let argsCounts =
        args
        |> List.map ~f:(fun { firstBinding = _; secondBinding = _; value } ->
          getCountsArray value)
        |> Counts.merge
      and bodyCounts = getCountsArray body in
      Counts.merge [ argsCounts; bodyCounts ]
    | IntrinsicCall (Length { arg; t = _; d = _; cellShape = _; type' = _ }) ->
      getCountsArray arg
    | IntrinsicCall (Filter { array; flags; t = _; d = _; cellShape = _; type' = _ }) ->
      Counts.merge [ getCountsArray array; getCountsArray flags ]
    | IntrinsicCall
        (Append { arg1; arg2; t = _; d1 = _; d2 = _; cellShape = _; type' = _ }) ->
      Counts.merge [ getCountsArray arg1; getCountsArray arg2 ]
  and getCountsAtom : Expr.atom -> Counts.t = function
    | Literal (IntLiteral _) -> Counts.empty
    | Literal (CharacterLiteral _) -> Counts.empty
    | Literal (BooleanLiteral _) -> Counts.empty
    | Literal UnitLiteral -> Counts.empty
    | Box { indices = _; body; bodyType = _; type' = _ } -> getCountsArray body
  in
  getCountsArray
;;

let rec subArray subs : Expr.array -> Expr.array = function
  | Ref { id; type' = _ } as expr ->
    (match Map.find subs id with
     | Some subValue -> subValue
     | None -> expr)
  | Scalar { element; type' } -> Scalar { element = subAtom subs element; type' }
  | Frame { elements; dimensions; type' } ->
    let elements = List.map elements ~f:(subArray subs) in
    Frame { elements; dimensions; type' }
  | Unbox { indexBindings; boxBindings; body; type' } ->
    let body = subArray subs body in
    let boxBindings =
      List.map boxBindings ~f:(fun { binding; box } : Expr.unboxBinding ->
        { binding; box = subArray subs box })
    in
    Unbox { indexBindings; boxBindings; body; type' }
  | PrimitiveCall { op; args; type' } ->
    let args = List.map args ~f:(subArray subs) in
    PrimitiveCall { op; args; type' }
  | IntrinsicCall (Map { args; body; frameShape; type' }) ->
    let args =
      List.map args ~f:(fun { binding; value } : Expr.mapArg ->
        { binding; value = subArray subs value })
    and body = subArray subs body in
    IntrinsicCall (Map { args; body; frameShape; type' })
  | IntrinsicCall (Reduce { args; body; t; dSub1; itemPad; cellShape; type' }) ->
    let args =
      List.map args ~f:(fun { firstBinding; secondBinding; value } : Expr.reduceArg ->
        { firstBinding; secondBinding; value = subArray subs value })
    and body = subArray subs body in
    IntrinsicCall (Reduce { args; body; t; dSub1; itemPad; cellShape; type' })
  | IntrinsicCall (Scan { args; body; t; dSub1; itemPad; cellShape; type' }) ->
    let args =
      List.map args ~f:(fun { firstBinding; secondBinding; value } : Expr.reduceArg ->
        { firstBinding; secondBinding; value = subArray subs value })
    and body = subArray subs body in
    IntrinsicCall (Scan { args; body; t; dSub1; itemPad; cellShape; type' })
  | IntrinsicCall (Length { arg; t; d; cellShape; type' }) ->
    let arg = subArray subs arg in
    IntrinsicCall (Length { arg; t; d; cellShape; type' })
  | IntrinsicCall (Filter { array; flags; t; d; cellShape; type' }) ->
    let array = subArray subs array
    and flags = subArray subs flags in
    IntrinsicCall (Filter { array; flags; t; d; cellShape; type' })
  | IntrinsicCall (Append { arg1; arg2; t; d1; d2; cellShape; type' }) ->
    let arg1 = subArray subs arg1
    and arg2 = subArray subs arg2 in
    IntrinsicCall (Append { arg1; arg2; t; d1; d2; cellShape; type' })

and subAtom subs : Expr.atom -> Expr.atom = function
  | Literal (IntLiteral _ | CharacterLiteral _ | BooleanLiteral _) as lit -> lit
  | Literal UnitLiteral as lit -> lit
  | Box { indices; body; bodyType; type' } ->
    let body = subArray subs body in
    Box { indices; body; bodyType; type' }
;;

(* Perform the following optimizations:
   - Copy propogation
   - Delete unused variables
   - Inline variables only used once
   - Inline variables with constant value
   - Constant folding *)
let rec optimizeArray : Expr.array -> Expr.array = function
  | Ref _ as ref -> ref
  | Scalar { element; type' } ->
    let element = optimizeAtom element in
    Expr.Scalar { element; type' }
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
  | Unbox { indexBindings; boxBindings; body; type' } ->
    let body = optimizeArray body in
    (* Simplify the args, removing unused ones *)
    let bodyCounts = getCounts body in
    let boxBindings =
      boxBindings
      |> List.filter ~f:(fun binding -> Counts.get bodyCounts binding.binding > 0)
      |> List.map ~f:(fun { binding; box } ->
        let box = optimizeArray box in
        ({ binding; box } : Expr.unboxBinding))
    in
    Expr.Unbox { indexBindings; boxBindings; body; type' }
  | PrimitiveCall { op; args; type' } ->
    let args = List.map args ~f:optimizeArray in
    (* Do constant folding: *)
    (match op, args with
     | ( Add
       , [ Scalar { element = Literal (IntLiteral a); type' = _ }
         ; Scalar { element = Literal (IntLiteral b); type' = _ }
         ] ) -> scalar (Literal (IntLiteral (a + b)))
     | Add, [ Scalar { element = Literal (IntLiteral 0); type' = _ }; value ]
     | Add, [ value; Scalar { element = Literal (IntLiteral 0); type' = _ } ] -> value
     | ( Sub
       , [ Scalar { element = Literal (IntLiteral a); type' = _ }
         ; Scalar { element = Literal (IntLiteral b); type' = _ }
         ] ) -> scalar (Literal (IntLiteral (a - b)))
     | Sub, [ value; Scalar { element = Literal (IntLiteral 0); type' = _ } ] -> value
     | ( Mul
       , [ Scalar { element = Literal (IntLiteral a); type' = _ }
         ; Scalar { element = Literal (IntLiteral b); type' = _ }
         ] ) -> scalar (Literal (IntLiteral (a * b)))
     | Mul, [ Scalar { element = Literal (IntLiteral 1); type' = _ }; value ]
     | Mul, [ value; Scalar { element = Literal (IntLiteral 1); type' = _ } ] -> value
     | ( Div
       , [ Scalar { element = Literal (IntLiteral a); type' = _ }
         ; Scalar { element = Literal (IntLiteral b); type' = _ }
         ] ) -> scalar (Literal (IntLiteral (a / b)))
     | Div, [ value; Scalar { element = Literal (IntLiteral 1); type' = _ } ] -> value
     | _ -> Expr.PrimitiveCall { op; args; type' })
  | IntrinsicCall (Length { arg; t; d; cellShape; type' }) ->
    if Map.is_empty d.refs
    then (* The dimension is known statically *)
      scalar (Expr.Literal (IntLiteral d.const))
    else (
      let arg = optimizeArray arg in
      Expr.IntrinsicCall (Length { arg; t; d; cellShape; type' }))
  | IntrinsicCall (Append { arg1; arg2; t; d1; d2; cellShape; type' }) as append ->
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
               List.map restDims ~f:(fun d ->
                 InlineNucleus.Index.(Add (dimensionConstant d)))
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
     | _ -> Expr.IntrinsicCall (Append { arg1; arg2; t; d1; d2; cellShape; type' }))
  | IntrinsicCall (Filter { array; flags; t; d; cellShape; type' }) ->
    let array = optimizeArray array
    and flags = optimizeArray flags in
    IntrinsicCall (Filter { array; flags; t; d; cellShape; type' })
  | IntrinsicCall (Map { args; body; frameShape = []; type' }) ->
    (* Do an initial simplification of the argument values and the body *)
    let args =
      List.map args ~f:(fun { binding; value } : Expr.mapArg ->
        let value = optimizeArray value in
        { binding; value })
    in
    let body = optimizeArray body in
    (* Until it cannot be done anymore, inline args that can be propogated and then
       simplify the body. Also, remove unused args. *)
    let rec loop (args : Expr.mapArg list) body =
      let bodyCounts = getCounts body in
      let _, argsToPropogate, args =
        List.partition3_map args ~f:(fun arg ->
          match Counts.get bodyCounts arg.binding with
          | 0 -> `Fst ()
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
            ~f:(fun subs (arg : Expr.mapArg) ->
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
     | _ :: _ as args -> IntrinsicCall (Map { args; body; frameShape = []; type' }))
  | IntrinsicCall (Map { args; body; frameShape = _ :: _ as frameShape; type' }) ->
    let body = optimizeArray body in
    (* Simplify the args, removing unused ones *)
    let bodyCounts = getCounts body in
    let args =
      args
      |> List.filter ~f:(fun arg -> Counts.get bodyCounts arg.binding > 0)
      |> List.map ~f:(fun { binding; value } ->
        let value = optimizeArray value in
        ({ binding; value } : Expr.mapArg))
    in
    Expr.IntrinsicCall (Map { args; body; frameShape; type' })
  | IntrinsicCall (Reduce { args; body; t; dSub1; itemPad; cellShape; type' }) ->
    let body = optimizeArray body in
    (* Simplify the args, removing unused ones *)
    let bodyCounts = getCounts body in
    let args =
      args
      |> List.filter ~f:(fun arg ->
        Counts.get bodyCounts arg.firstBinding > 0
        || Counts.get bodyCounts arg.secondBinding > 0)
      |> List.map ~f:(fun { firstBinding; secondBinding; value } ->
        let value = optimizeArray value in
        ({ firstBinding; secondBinding; value } : Expr.reduceArg))
    in
    Expr.IntrinsicCall (Reduce { args; body; t; dSub1; itemPad; cellShape; type' })
  | IntrinsicCall (Scan { args; body; t; dSub1; itemPad; cellShape; type' }) ->
    let body = optimizeArray body in
    (* Simplify the args, removing unused ones *)
    let bodyCounts = getCounts body in
    let args =
      args
      |> List.filter ~f:(fun arg ->
        Counts.get bodyCounts arg.firstBinding > 0
        || Counts.get bodyCounts arg.secondBinding > 0)
      |> List.map ~f:(fun { firstBinding; secondBinding; value } ->
        let value = optimizeArray value in
        ({ firstBinding; secondBinding; value } : Expr.reduceArg))
    in
    Expr.IntrinsicCall (Scan { args; body; t; dSub1; itemPad; cellShape; type' })

and optimizeAtom : Expr.atom -> Expr.atom = function
  | Box { indices; body; bodyType; type' } ->
    let body = optimizeArray body in
    Expr.Box { indices; body; bodyType; type' }
  | Literal _ as literal -> literal
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
end

type hoisting =
  { variableDeclaration : Expr.mapArg
  ; counts : Counts.t
  }
[@@deriving sexp_of]

let hoistDeclarationsMap l ~f =
  let results, hoistings = l |> List.map ~f |> List.unzip in
  results, List.join hoistings
;;

(* Hoist variables that can be hoisted. Maps are also cleaned up while doing
   this. (nested maps with empty frames that can be flattened are, and maps
   with empty frames and no args are removed) *)
let rec hoistDeclarationsInArray : Expr.array -> Expr.array * hoisting list = function
  | Ref _ as ref -> ref, []
  | Scalar { element; type' } ->
    let element, hoistings = hoistDeclarationsInAtom element in
    Expr.Scalar { element; type' }, hoistings
  | Frame { elements; dimensions; type' } ->
    let elements, hoistings = hoistDeclarationsMap elements ~f:hoistDeclarationsInArray in
    Expr.Frame { elements; dimensions; type' }, hoistings
  | Unbox { indexBindings; boxBindings; body; type' } ->
    let bindings =
      boxBindings |> List.map ~f:(fun b -> b.binding) |> BindingSet.of_list
    in
    let body, bodyHoistings = hoistDeclarationsInBody body ~bindings in
    let boxBindings, boxBindingHoistings =
      hoistDeclarationsMap boxBindings ~f:(fun { binding; box } ->
        let box, hoistings = hoistDeclarationsInArray box in
        ({ binding; box } : Expr.unboxBinding), hoistings)
    in
    ( Expr.Unbox { indexBindings; boxBindings; body; type' }
    , boxBindingHoistings @ bodyHoistings )
  | PrimitiveCall { op; args; type' } ->
    let args, hoistings = hoistDeclarationsMap args ~f:hoistDeclarationsInArray in
    PrimitiveCall { op; args; type' }, hoistings
  | IntrinsicCall (Length { arg; t; d; cellShape; type' }) ->
    let arg, hoistings = hoistDeclarationsInArray arg in
    Expr.IntrinsicCall (Length { arg; t; d; cellShape; type' }), hoistings
  | IntrinsicCall (Filter { array; flags; t; d; cellShape; type' }) ->
    let array, arrayHoistings = hoistDeclarationsInArray array
    and flags, flagHoistings = hoistDeclarationsInArray flags in
    ( Expr.IntrinsicCall (Filter { array; flags; t; d; cellShape; type' })
    , arrayHoistings @ flagHoistings )
  | IntrinsicCall (Append { arg1; arg2; t; d1; d2; cellShape; type' }) ->
    let arg1, hoistings1 = hoistDeclarationsInArray arg1
    and arg2, hoistings2 = hoistDeclarationsInArray arg2 in
    ( Expr.IntrinsicCall (Append { arg1; arg2; t; d1; d2; cellShape; type' })
    , hoistings1 @ hoistings2 )
  | IntrinsicCall (Map { args; body; frameShape = []; type' = _ }) ->
    let body, bodyHoistings = hoistDeclarationsInArray body in
    let args, argHoistings =
      hoistDeclarationsMap args ~f:(fun { binding; value } ->
        let value, hoistings = hoistDeclarationsInArray value in
        ({ binding; value } : Expr.mapArg), hoistings)
    in
    let argsAsHoistings =
      List.map args ~f:(fun arg ->
        { variableDeclaration = arg; counts = getCounts arg.value })
    in
    body, argHoistings @ argsAsHoistings @ bodyHoistings
  | IntrinsicCall (Map { args; body; frameShape = _ :: _ as frameShape; type' }) ->
    let bindings = args |> List.map ~f:(fun arg -> arg.binding) |> BindingSet.of_list in
    let body, bodyHoistings = hoistDeclarationsInBody body ~bindings in
    let args, argHoistings =
      hoistDeclarationsMap args ~f:(fun { binding; value } ->
        let value, hoistings = hoistDeclarationsInArray value in
        ({ binding; value } : Expr.mapArg), hoistings)
    in
    ( Expr.IntrinsicCall (Map { args; body; frameShape; type' })
    , argHoistings @ bodyHoistings )
  | IntrinsicCall (Reduce { args; body; t; dSub1; itemPad; cellShape; type' }) ->
    let bindings =
      args
      |> List.bind ~f:(fun arg -> [ arg.firstBinding; arg.secondBinding ])
      |> BindingSet.of_list
    in
    let body, bodyHoistings = hoistDeclarationsInBody body ~bindings in
    let args, argHoistings =
      hoistDeclarationsMap args ~f:(fun { firstBinding; secondBinding; value } ->
        let value, hoistings = hoistDeclarationsInArray value in
        ({ firstBinding; secondBinding; value } : Expr.reduceArg), hoistings)
    in
    ( Expr.IntrinsicCall (Reduce { args; body; t; dSub1; itemPad; cellShape; type' })
    , argHoistings @ bodyHoistings )
  | IntrinsicCall (Scan { args; body; t; dSub1; itemPad; cellShape; type' }) ->
    let bindings =
      args
      |> List.bind ~f:(fun arg -> [ arg.firstBinding; arg.secondBinding ])
      |> BindingSet.of_list
    in
    let body, bodyHoistings = hoistDeclarationsInBody body ~bindings in
    let args, argHoistings =
      hoistDeclarationsMap args ~f:(fun { firstBinding; secondBinding; value } ->
        let value, hoistings = hoistDeclarationsInArray value in
        ({ firstBinding; secondBinding; value } : Expr.reduceArg), hoistings)
    in
    ( Expr.IntrinsicCall (Scan { args; body; t; dSub1; itemPad; cellShape; type' })
    , argHoistings @ bodyHoistings )

and hoistDeclarationsInAtom : Expr.atom -> Expr.atom * hoisting list = function
  | Box { indices; body; bodyType; type' } ->
    let body, hoistings = hoistDeclarationsInArray body in
    Box { indices; body; bodyType; type' }, hoistings
  | Literal _ as literal -> literal, []

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
      Expr.IntrinsicCall
        (Map
           { args = List.map hoistings ~f:(fun h -> h.variableDeclaration)
           ; body
           ; frameShape = []
           ; type' = Expr.arrayType body
           }))
  in
  body, hoistingsToPropogate
;;

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
    | Scalar { element; type' } ->
      let%map element, hoistings = hoistExpressionsInAtom loopBarrier element in
      Expr.Scalar { element; type' }, hoistings
    | Frame { elements; dimensions; type' } ->
      let%map elements, hoistings =
        hoistExpressionsMap elements ~f:(hoistExpressionsInArray loopBarrier)
      in
      Expr.Frame { elements; dimensions; type' }, hoistings
    | Unbox { indexBindings; boxBindings; body; type' } ->
      let%bind boxBindings, boxBindingHoistings =
        hoistExpressionsMap boxBindings ~f:(fun { binding; box } ->
          let%map box, hoistings = hoistExpressionsInArray loopBarrier box in
          ({ binding; box } : Expr.unboxBinding), hoistings)
      in
      let bindings =
        boxBindings |> List.map ~f:(fun b -> b.binding) |> BindingSet.of_list
      in
      let%map body, bodyHoistings = hoistExpressionsInBody loopBarrier body ~bindings in
      ( Expr.Unbox { indexBindings; boxBindings; body; type' }
      , boxBindingHoistings @ bodyHoistings )
    | PrimitiveCall { op; args; type' } ->
      let%map args, hoistings =
        hoistExpressionsMap args ~f:(hoistExpressionsInArray loopBarrier)
      in
      Expr.PrimitiveCall { op; args; type' }, hoistings
    | IntrinsicCall (Length { arg; t; d; cellShape; type' }) ->
      let%map arg, hoistings = hoistExpressionsInArray loopBarrier arg in
      Expr.IntrinsicCall (Length { arg; t; d; cellShape; type' }), hoistings
    | IntrinsicCall (Append { arg1; arg2; t; d1; d2; cellShape; type' }) ->
      let%map arg1, hoistings1 = hoistExpressionsInArray loopBarrier arg1
      and arg2, hoistings2 = hoistExpressionsInArray loopBarrier arg2 in
      ( Expr.IntrinsicCall (Append { arg1; arg2; t; d1; d2; cellShape; type' })
      , hoistings1 @ hoistings2 )
    | IntrinsicCall (Filter { array; flags; t; d; cellShape; type' }) ->
      let%map array, arrayHoistings = hoistExpressionsInArray loopBarrier array
      and flags, flagHoistings = hoistExpressionsInArray loopBarrier flags in
      ( Expr.IntrinsicCall (Filter { array; flags; t; d; cellShape; type' })
      , arrayHoistings @ flagHoistings )
    | IntrinsicCall (Map { args; body; frameShape; type' }) ->
      let%bind args, argHoistings =
        hoistExpressionsMap args ~f:(fun { binding; value } ->
          let%map value, hoistings = hoistExpressionsInArray loopBarrier value in
          ({ binding; value } : Expr.mapArg), hoistings)
      in
      let bindings = args |> List.map ~f:(fun arg -> arg.binding) |> BindingSet.of_list in
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
      ( Expr.IntrinsicCall (Map { args; body; frameShape; type' })
      , argHoistings @ bodyHoistings )
    | IntrinsicCall (Reduce { args; body; t; dSub1; itemPad; cellShape; type' }) ->
      let%bind args, argHoistings =
        hoistExpressionsMap args ~f:(fun { firstBinding; secondBinding; value } ->
          let%map value, hoistings = hoistExpressionsInArray loopBarrier value in
          ({ firstBinding; secondBinding; value } : Expr.reduceArg), hoistings)
      in
      let bindings =
        args
        |> List.bind ~f:(fun arg -> [ arg.firstBinding; arg.secondBinding ])
        |> BindingSet.of_list
      in
      let%map body, bodyHoistings = hoistExpressionsInBody bindings body ~bindings in
      ( Expr.IntrinsicCall (Reduce { args; body; t; dSub1; itemPad; cellShape; type' })
      , argHoistings @ bodyHoistings )
    | IntrinsicCall (Scan { args; body; t; dSub1; itemPad; cellShape; type' }) ->
      let%bind args, argHoistings =
        hoistExpressionsMap args ~f:(fun { firstBinding; secondBinding; value } ->
          let%map value, hoistings = hoistExpressionsInArray loopBarrier value in
          ({ firstBinding; secondBinding; value } : Expr.reduceArg), hoistings)
      in
      let bindings =
        args
        |> List.bind ~f:(fun arg -> [ arg.firstBinding; arg.secondBinding ])
        |> BindingSet.of_list
      in
      let%map body, bodyHoistings = hoistExpressionsInBody bindings body ~bindings in
      ( Expr.IntrinsicCall (Scan { args; body; t; dSub1; itemPad; cellShape; type' })
      , argHoistings @ bodyHoistings ))

and hoistExpressionsInAtom loopBarrier
  : Expr.atom -> (Expr.atom * hoisting list, _) HoistState.u
  =
  let open HoistState.Let_syntax in
  function
  | Box { indices; body; bodyType; type' } ->
    let%map body, hoistings = hoistExpressionsInArray loopBarrier body in
    Expr.Box { indices; body; bodyType; type' }, hoistings
  | Literal _ as literal -> return (literal, [])

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
      ({ binding; value } : Expr.mapArg), hoistings)
  in
  (* Declare the hoistings that need to be declared. *)
  let body =
    match hoistingsToDeclare with
    | [] ->
      (* None to declare *)
      body
    | _ :: _ as hoistingsToDeclare ->
      (* There are some to be declared, so wrap the body in a map *)
      Expr.IntrinsicCall
        (Map
           { args = hoistingsToDeclare
           ; body
           ; frameShape = []
           ; type' = Expr.arrayType body
           })
  in
  body, moreHoistingsToPropogate @ hoistingsToPropogate
;;

let simplify expr =
  let open HoistState.Let_syntax in
  let rec loop expr =
    (* Hoist variables that can be hoisted *)
    let hoisted, hoistings = hoistDeclarationsInBody expr ~bindings:All in
    assert (List.length hoistings = 0);
    let unoptimized = hoisted in
    let optimized = optimizeArray hoisted in
    if Expr.equal_array unoptimized optimized
    then (
      let%map result, hoistings = hoistExpressionsInBody All optimized ~bindings:All in
      assert (List.length hoistings = 0);
      result)
    else loop optimized
  in
  loop expr
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = InlineNucleus.t
  type output = InlineNucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "Simplify"

  let run input =
    CompilerPipeline.S.make ~f:(fun state -> State.run (simplify input) state)
  ;;
end
