open! Base

type ('ok, 'err) t =
  | MOk of 'ok
  | Errors of 'err Non_empty_list.t

include Monad.Make2 (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

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
end)

let both a b =
  match a, b with
  | MOk a, MOk b -> MOk (a, b)
  | MOk _, (Errors _ as errs) | (Errors _ as errs), MOk _ -> errs
  | Errors errs1, Errors errs2 -> Errors (Non_empty_list.append errs1 errs2)
;;

module Let_syntax = struct
  include Let_syntax

  module Let_syntax = struct
    include Let_syntax

    let both = both
  end
end

let all = function
  | [] -> MOk []
  | headR :: restL ->
    let open Let_syntax in
    let%map head = headR
    and rest = all restL in
    head :: rest
;;

let allNE (Non_empty_list.( :: ) (headR, restL)) =
  let open Let_syntax in
  let%map head = headR
  and rest = all restL in
  Non_empty_list.( :: ) (head, rest)
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
