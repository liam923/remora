open! Base

module type S2 = sig
  type ('a, 'e) m
  type ('s, 'a, 'e) t

  include Monad.S3 with type ('a, 's, 'e) t := ('s, 'a, 'e) t

  val run : ('s, 'a, 'e) t -> 's -> ('s * 'a, 'e) m
  val runA : ('s, 'a, 'e) t -> 's -> ('a, 'e) m
  val transform : ('s, 'a, 'e) t -> f:('s -> 'a -> 's * 'b) -> ('s, 'b, 'e) t
  val transformF : ('s, 'a, 'e) t -> f:('s -> 'a -> ('s * 'b, 'e) m) -> ('s, 'b, 'e) t
  val inspect : f:('s -> 'c) -> ('s, 'c, 'e) t
  val inspectF : f:('s -> ('c, 'e) m) -> ('s, 'c, 'e) t
  val modify : f:('s -> 's) -> ('s, unit, 'e) t
  val modifyF : f:('s -> ('s, 'e) m) -> ('s, unit, 'e) t
  val get : unit -> ('s, 's, 'e) t
  val set : 's -> ('s, unit, 'e) t
  val setF : ('s, 'e) m -> ('s, unit, 'e) t
  val returnF : ('a, 'e) m -> ('s, 'a, 'e) t
  val make : f:('s -> 's * 'a) -> ('s, 'a, 'e) t
  val makeF : f:('s -> ('s * 'a, 'e) m) -> ('s, 'a, 'e) t
  val all_map : ('k, ('s, 'ok, 'err) t, 'cmp) Map.t -> ('s, ('k, 'ok, 'cmp) Map.t, 'err) t
  val all_opt : ('s, 'ok, 'err) t option -> ('s, 'ok option, 'err) t
  val unzip : ('s, ('a * 'b) list, 'e) t -> ('s, 'a list * 'b list, 'e) t
end

module Make2 (M : Monad.S2) = struct
  type ('a, 'e) m = ('a, 'e) M.t
  type ('s, 'a, 'e) t = ('s -> ('s * 'a, 'e) M.t, 'e) M.t

  let run runF initialState = M.bind runF ~f:(fun f -> f initialState)
  let runA runF initialState = M.map (run runF initialState) ~f:(fun (_, r) -> r)

  include Monad.Make3 (struct
      type nonrec ('a, 's, 'e) t = ('s, 'a, 'e) t

      let bind runF ~f =
        M.map runF ~f:(fun run inState ->
          let%bind.M interimState, interimValue = run inState in
          let%bind.M fRun = f interimValue in
          let%map.M outState, outValue = fRun interimState in
          outState, outValue)
      ;;

      let map runF ~f =
        M.map runF ~f:(fun run inState ->
          let%map.M outState, interimValue = run inState in
          outState, f interimValue)
      ;;

      let map = `Custom map
      let return value = M.return (fun state -> M.return (state, value))
    end)

  let transform runF ~f =
    M.map runF ~f:(fun run inState ->
      let%map.M interimState, interimValue = run inState in
      f interimState interimValue)
  ;;

  let transformF runF ~f =
    M.map runF ~f:(fun run inState ->
      let%bind.M interimState, interimValue = run inState in
      f interimState interimValue)
  ;;

  let inspect ~f = M.return (fun state -> M.return (state, f state))
  let inspectF ~f = M.return (fun state -> M.map (f state) ~f:(fun value -> state, value))
  let modify ~f = M.return (fun state -> M.return (f state, ()))

  let modifyF ~f =
    M.return (fun state -> M.map (f state) ~f:(fun outState -> outState, ()))
  ;;

  let get () = M.return (fun state -> M.return (state, state))
  let set state = M.return (fun _ -> M.return (state, ()))
  let setF stateF = M.return (fun _ -> M.map stateF ~f:(fun state -> state, ()))
  let returnF value = M.map value ~f:(fun value state -> M.return (state, value))
  let make ~f = M.return (fun state -> M.return (f state))
  let makeF ~f = M.return f

  let all_map map =
    Map.fold
      map
      ~init:(return (Map.empty (Map.comparator_s map)))
      ~f:(fun ~key ~data acc ->
        let open Let_syntax in
        let%map acc = acc
        and data = data in
        Map.set acc ~key ~data)
  ;;

  let all_opt = function
    | Some s -> s >>| fun e -> Some e
    | None -> return None
  ;;

  let unzip = map ~f:List.unzip
end

module type S2WithError = sig
  include S2

  val both : ('s, 'a, 'e) t -> ('s, 'b, 'e) t -> ('s, 'a * 'b, 'e) t
  val allNE : ('s, 'ok, 'err) t NeList.t -> ('s, 'ok NeList.t, 'err) t
end

module Make2WithError (M : MonadWithError.S2) = struct
  include Make2 (M)

  let goodBoth a b =
    M.return (fun inState ->
      M.bindWithError
        (run a inState)
        ~f:(fun (interimState, aValue) ->
          M.map (run b interimState) ~f:(fun (outState, bValue) ->
            outState, (aValue, bValue)))
        ~error:(fun _ -> M.ignore_m (run b inState)))
  ;;

  let both = goodBoth

  module Let_syntax = struct
    include Let_syntax

    module Let_syntax = struct
      include Let_syntax

      let both = goodBoth
    end
  end

  let rec all = function
    | [] -> return []
    | headR :: restL ->
      let open Let_syntax in
      let%map head = headR
      and rest = all restL in
      head :: rest
  ;;

  let allNE NeList.(headR :: restL) =
    let open Let_syntax in
    let%map head = headR
    and rest = all restL in
    NeList.(head :: rest)
  ;;

  let all_unit list =
    let open Let_syntax in
    let%map _ = all list in
    ()
  ;;

  let all_map map =
    Map.fold
      map
      ~init:(return (Map.empty (Map.comparator_s map)))
      ~f:(fun ~key ~data acc ->
        let open Let_syntax in
        let%map acc = acc
        and data = data in
        Map.set acc ~key ~data)
  ;;
end
