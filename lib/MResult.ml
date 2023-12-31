open! Base

type ('ok, 'err) t =
  | MOk of 'ok
  | Errors of 'err NeList.t
[@@deriving sexp, eq, ord]

type 'e error = 'e NeList.t

let goodBoth a b =
  match a, b with
  | MOk a, MOk b -> MOk (a, b)
  | MOk _, (Errors _ as errs) | (Errors _ as errs), MOk _ -> errs
  | Errors errs1, Errors errs2 -> Errors (NeList.append errs1 errs2)
;;

let both = goodBoth

include MonadWithError.Make2 (struct
    type 'e error = 'e NeList.t
    type nonrec ('a, 'e) t = ('a, 'e) t

    let bind x ~f =
      match x with
      | Errors _ as x -> x
      | MOk x -> f x
    ;;

    let map x ~f =
      match x with
      | Errors _ as x -> x
      | MOk x -> MOk (f x)
    ;;

    let map = `Custom map
    let return x = MOk x

    let bindWithError x ~f ~error =
      match x with
      | MOk success -> f success
      | Errors errors ->
        (match error errors with
         | MOk () -> Errors errors
         | Errors moreErrors -> Errors (NeList.append errors moreErrors))
    ;;
  end)

module Let_syntax = struct
  include Let_syntax

  module Let_syntax = struct
    include Let_syntax

    let both = goodBoth
  end
end

let rec all = function
  | [] -> MOk []
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

let rec all_unit = function
  | [] -> return ()
  | headR :: restL ->
    let open Let_syntax in
    let%map () = headR
    and () = all_unit restL in
    ()
;;

let ofOption o ~err =
  match o with
  | Some v -> MOk v
  | None -> Errors [ err ]
;;

let err e = Errors [ e ]
let require b error = if b then MOk () else Errors [ error ]

let traverseOpt = function
  | Some r -> map r ~f:(fun e -> Some e)
  | None -> MOk None
;;

exception MResultError of string NeList.t [@@deriving sexp]

let assertNoErrors mr ~f =
  match mr with
  | MOk value -> value
  | Errors errors -> raise (MResultError (NeList.map errors ~f))
;;
