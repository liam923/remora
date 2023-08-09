open! Base
open InlineNucleus

type bindingSet =
  | Bindings of Set.M(Identifier).t
  | All
[@@deriving sexp_of]

module Counts : sig
  type t [@@deriving sexp_of]

  val empty : t
  val one : Identifier.t -> t
  val get : t -> Identifier.t -> int
  val merge : t list -> t
  val usesAny : t -> bindingSet -> bool
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
    | Bindings bindings ->
      let keys = Map.keys counts |> Set.of_list (module Identifier) in
      not (Set.are_disjoint keys bindings)
    | All -> true
  ;;
end

let scalar atom =
  InlineNucleus.Expr.(
    Scalar { element = atom; type' = { element = atomType atom; shape = [] } })
;;

type hoisting =
  { variableDeclaration : Expr.mapArg
  ; counts : Counts.t
  }
[@@deriving sexp_of]

(* Determine if an expression is a constant expression or if it requires
   computation. i.e. 7 and x are nonComputational,
   while (+ 7 x) is computational *)
let nonComputational =
  let rec nonComputationalArray : Expr.array -> bool = function
    | Ref _ -> true
    | Scalar scalar -> nonComputationalAtom scalar.element
    | Frame _ -> false (* TODO: might want to allow for small frames *)
    | Unbox _ -> false
    | PrimitiveCall _ -> false
    | IntrinsicCall _ -> false
  and nonComputationalAtom : Expr.atom -> bool = function
    | Box _ -> false
    | Literal (IntLiteral _) -> true
    | Literal (CharacterLiteral _) -> true
    | Literal UnitLiteral -> true
  in
  nonComputationalArray
;;

let getCounts =
  let rec getCountsArray : Expr.array -> Counts.t = function
    | Ref { id; type' = _ } -> Counts.one id
    | Scalar { element; type' = _ } -> getCountsAtom element
    | Frame { elements; dimensions = _; type' = _ } ->
      elements |> List.map ~f:getCountsArray |> Counts.merge
    | Unbox { indexBindings = _; boxBindings = _; body; type' = _ } -> getCountsArray body
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
    | IntrinsicCall (Length { arg; t = _; d = _; cellShape = _; type' = _ }) ->
      getCountsArray arg
  and getCountsAtom : Expr.atom -> Counts.t = function
    | Literal (IntLiteral _) -> Counts.empty
    | Literal (CharacterLiteral _) -> Counts.empty
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
  | IntrinsicCall (Length { arg; t; d; cellShape; type' }) ->
    let arg = subArray subs arg in
    IntrinsicCall (Length { arg; t; d; cellShape; type' })

and subAtom subs : Expr.atom -> Expr.atom = function
  | Literal (IntLiteral _) as lit -> lit
  | Literal (CharacterLiteral _) as lit -> lit
  | Literal UnitLiteral as lit -> lit
  | Box { indices; body; bodyType; type' } ->
    let body = subArray subs body in
    Box { indices; body; bodyType; type' }
;;

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
         |> NeList.max_elt ~compare:(fun (_, a) (_, b) -> Int.compare a b)
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
  | IntrinsicCall (Map { args; body; frameShape = []; type' }) ->
    (* Do an initial simplification of the argument values and the body *)
    let args =
      List.map args ~f:(fun { binding; value } : Expr.mapArg ->
        let value = optimizeArray value in
        { binding; value })
    in
    let body = optimizeArray body in
    (* Until it cannot be done anymore, inline args that can be propogated () and then
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
    IntrinsicCall (Map { args; body; frameShape = []; type' })
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

and optimizeAtom : Expr.atom -> Expr.atom = function
  | Box { indices; body; bodyType; type' } ->
    let body = optimizeArray body in
    Expr.Box { indices; body; bodyType; type' }
  | Literal _ as literal -> literal
;;

let hoistMap l ~f =
  let results, hoistings = l |> List.map ~f |> List.unzip in
  results, List.join hoistings
;;

let rec hoistDeclarationsInArray : Expr.array -> Expr.array * hoisting list = function
  | Ref _ as ref -> ref, []
  | Scalar { element; type' } ->
    let element, hoistings = hoistDeclarationsInAtom element in
    Expr.Scalar { element; type' }, hoistings
  | Frame { elements; dimensions; type' } ->
    let elements, hoistings = hoistMap elements ~f:hoistDeclarationsInArray in
    Expr.Frame { elements; dimensions; type' }, hoistings
  | Unbox { indexBindings; boxBindings; body; type' } ->
    let bindings =
      boxBindings |> List.map ~f:(fun b -> b.binding) |> Set.of_list (module Identifier)
    in
    let body, bodyHoistings =
      hoistDeclarationsInBody body ~bindings:(Bindings bindings)
    in
    let boxBindings, boxBindingHoistings =
      hoistMap boxBindings ~f:(fun { binding; box } ->
        let box, hoistings = hoistDeclarationsInArray box in
        ({ binding; box } : Expr.unboxBinding), hoistings)
    in
    ( Expr.Unbox { indexBindings; boxBindings; body; type' }
    , boxBindingHoistings @ bodyHoistings )
  | PrimitiveCall { op; args; type' } ->
    let args, hoistings = hoistMap args ~f:hoistDeclarationsInArray in
    PrimitiveCall { op; args; type' }, hoistings
  | IntrinsicCall (Length { arg; t; d; cellShape; type' }) ->
    let arg, hoistings = hoistDeclarationsInArray arg in
    Expr.IntrinsicCall (Length { arg; t; d; cellShape; type' }), hoistings
  | IntrinsicCall (Map { args; body; frameShape = []; type' = _ }) ->
    let body, bodyHoistings = hoistDeclarationsInArray body in
    let args, argHoistings =
      hoistMap args ~f:(fun { binding; value } ->
        let value, hoistings = hoistDeclarationsInArray value in
        ({ binding; value } : Expr.mapArg), hoistings)
    in
    let argsAsHoistings =
      List.map args ~f:(fun arg ->
        { variableDeclaration = arg; counts = getCounts arg.value })
    in
    body, argHoistings @ argsAsHoistings @ bodyHoistings
  | IntrinsicCall (Map { args; body; frameShape = _ :: _ as frameShape; type' }) ->
    let bindings =
      args |> List.map ~f:(fun arg -> arg.binding) |> Set.of_list (module Identifier)
    in
    let body, bodyHoistings =
      hoistDeclarationsInBody body ~bindings:(Bindings bindings)
    in
    let args, argHoistings =
      hoistMap args ~f:(fun { binding; value } ->
        let value, hoistings = hoistDeclarationsInArray value in
        ({ binding; value } : Expr.mapArg), hoistings)
    in
    ( Expr.IntrinsicCall (Map { args; body; frameShape; type' })
    , argHoistings @ bodyHoistings )
  | IntrinsicCall (Reduce { args; body; t; dSub1; itemPad; cellShape; type' }) ->
    let bindings =
      args
      |> List.bind ~f:(fun arg -> [ arg.firstBinding; arg.secondBinding ])
      |> Set.of_list (module Identifier)
    in
    let body, bodyHoistings =
      hoistDeclarationsInBody body ~bindings:(Bindings bindings)
    in
    let args, argHoistings =
      hoistMap args ~f:(fun { firstBinding; secondBinding; value } ->
        let value, hoistings = hoistDeclarationsInArray value in
        ({ firstBinding; secondBinding; value } : Expr.reduceArg), hoistings)
    in
    ( Expr.IntrinsicCall (Reduce { args; body; t; dSub1; itemPad; cellShape; type' })
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

let simplify expr =
  let rec loop expr =
    let hoisted, hoistings = hoistDeclarationsInBody expr ~bindings:All in
    assert (List.length hoistings = 0);
    let unoptimized = hoisted in
    let optimized = optimizeArray hoisted in
    if Expr.equal_array unoptimized optimized then optimized else loop optimized
  in
  loop expr (* TODO: hoist expressions *)
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = InlineNucleus.t
  type output = InlineNucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "Simplify"
  let run input = CompilerPipeline.S.make ~f:(fun state -> state, simplify input)
end
