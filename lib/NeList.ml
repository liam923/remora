(* This is taken from the opam package neList: https://github.com/johnyob/ocaml-non-empty-list, 
but modified in order to add some additional functionality *)

open! Base

let invalid_argf = Printf.invalid_argf

type 'a t = ( :: ) of 'a * 'a list [@@deriving eq, ord, show]

let sexp_of_t sexpOfElem (x :: xs) = List.sexp_of_t sexpOfElem (x :: xs)

let t_of_sexp elemOfSexp sexp =
  match List.t_of_sexp elemOfSexp sexp with
  | x :: xs -> x :: xs
  | _ -> assert false
;;

type 'a neList = 'a t

let init n ~f =
  if n < 1 then invalid_argf "NeList.init %d" n ();
  (* Compute tail list before head to maintain decending order of evaluation. *)
  let xs = List.init (n - 1) ~f:(fun i -> f (i + 1)) in
  f 0 :: xs
;;

let of_list xs =
  match xs with
  | [] -> None
  | x :: xs -> Some (x :: xs)
;;

let of_list_exn =
  let of_list_exn xs =
    match of_list xs with
    | None -> invalid_arg "NeList.of_list_exn"
    | Some t -> t
  in
  of_list_exn
;;

let of_array arr = of_list (Array.to_list arr)

let of_array_exn =
  let of_array_exn arr =
    match of_array arr with
    | None -> invalid_arg "NeList.of_array_exn"
    | Some t -> t
  in
  of_array_exn
;;

let to_list (x :: xs) : 'a list = x :: xs [@@inline]
let to_array t = Array.of_list (to_list t)
let length (_ :: xs) = List.length xs + 1

let is_singleton xs =
  match xs with
  | [ _ ] -> true
  | _ -> false
;;

let cons x1 (x2 :: xs) = x1 :: x2 :: xs
let append (x :: xs) (y :: ys) = x :: (xs @ (y :: ys))

let rev_append (x :: xs) t2 =
  let rec loop acc mid xs =
    match xs with
    | [] -> mid :: acc
    | x :: xs -> loop (mid :: acc) x xs
  in
  loop (to_list t2) x xs
;;

let hd (x :: _) = x
let tl (_ :: xs) = xs

let rec last t =
  match t with
  | [ x ] -> x
  | _ :: x :: xs -> last (x :: xs)
;;

let rec lastWithPriors t =
  match t with
  | [ last ] -> [], last
  | head :: restHead :: restTail ->
    let restSplit, last = lastWithPriors (restHead :: restTail) in
    head :: restSplit, last
;;

let concat (xs :: xss) =
  let x = hd xs
  and xs = tl xs @ List.concat (List.map ~f:to_list xss) in
  x :: xs
;;

let nth (x :: xs) n = if n = 0 then Some x else List.nth xs (n - 1)

let nth_exn t n =
  match nth t n with
  | None ->
    invalid_argf "NeList.nth_exn %d for non-empty list of length %d" n (length t) ()
  | Some x -> x
;;

let mem (x :: xs) y ~equal = equal x y || List.mem ~equal xs y
let find (x :: xs) ~f = if f x then Some x else List.find xs ~f

let find_exn =
  let not_found = Not_found_s (Atom "NeList.find_exn: not found") in
  let find_exn t ~f =
    match find t ~f with
    | None -> raise not_found
    | Some x -> x
  in
  find_exn
;;

let findi t ~f = List.findi (to_list t) ~f

let findi_exn =
  let not_found = Not_found_s (Atom "NeList.findi_exn: not found") in
  let findi_exn t ~f =
    match findi t ~f with
    | None -> raise not_found
    | Some x -> x
  in
  findi_exn
;;

let find_map t ~f = List.find_map (to_list t) ~f

let find_map_exn =
  let nf = Not_found_s (Atom "NeList.find_map_exn: not found") in
  let find_map_exn t ~f =
    match find_map t ~f with
    | None -> raise nf
    | Some x -> x
  in
  find_map_exn
;;

let find_mapi t ~f =
  let rec loop i t =
    match t with
    | [ x ] -> Option.(f i x >>| fun y -> i, y)
    | x1 :: x2 :: xs ->
      (match f i x1 with
       | None -> loop (i + 1) (x2 :: xs)
       | Some y -> Some (i, y))
  in
  loop 0 t
;;

let find_mapi_exn =
  let not_found = Not_found_s (Atom "NeList.find_mapi_exn: not found") in
  let find_mapi_exn t ~f =
    match find_mapi t ~f with
    | None -> raise not_found
    | Some x -> x
  in
  find_mapi_exn
;;

module Or_unequal_lengths = struct
  include List.Or_unequal_lengths

  include Monad.Make (struct
    type nonrec 'a t = 'a t

    let return x = Ok x

    let bind m ~f =
      match m with
      | Ok x -> f x
      | Unequal_lengths -> Unequal_lengths
    ;;

    let map = `Define_using_bind
  end)
end

open Or_unequal_lengths

let length_mismatch name ~f t1 t2 =
  match f t1 t2 with
  | Ok t -> t
  | Unequal_lengths ->
    let l1 = length t1 in
    let l2 = length t2 in
    invalid_argf "NeList.%s: length mismatch. %d <> %d" name l1 l2 ()
;;

let rev (x :: xs) =
  let rec loop acc mid xs =
    match xs with
    | [] -> mid :: acc
    | x :: xs -> loop (mid :: acc) x xs
  in
  loop [] x xs
;;

let fold_left t ~init ~f = List.fold_left (to_list t) ~init ~f
let reduce (x :: xs) ~f = List.fold_left xs ~init:x ~f

let fold_lefti (x :: xs) ~init ~f =
  let rec loop i acc xs =
    match xs with
    | [] -> acc
    | x :: xs -> loop (i + 1) (f i acc x) xs
  in
  loop 1 (f 0 init x) xs
;;

let fold_right t ~init ~f = List.fold_right (to_list t) ~init ~f

let fold_righti t ~init ~f =
  let n = length t in
  fold_lefti (rev t) ~init ~f:(fun i acc x -> f (n - i) x acc)
;;

let map (x :: xs) ~f = f x :: List.map xs ~f
let concat_map t ~f = concat (map t ~f)

let rev_map (x :: xs) ~f =
  let rec loop acc mid xs =
    match xs with
    | [] -> mid :: acc
    | x :: xs -> loop (mid :: acc) (f x) xs
  in
  loop [] (f x) xs
;;

let rev_mapi (x :: xs) ~f =
  let rec loop i acc mid xs =
    match xs with
    | [] -> mid :: acc
    | x :: xs -> loop (i + 1) (mid :: acc) (f i x) xs
  in
  loop 1 [] (f 0 x) xs
;;

let mapi t ~f = rev (rev_mapi t ~f)
let concat_mapi t ~f = concat (mapi t ~f)

let rev_map2 (x :: xs) (y :: ys) ~f =
  let rec loop acc mid xs ys =
    match xs, ys with
    | [], [] -> Ok (mid :: acc)
    | x :: xs, y :: ys -> loop (mid :: acc) (f x y) xs ys
    | _, _ -> Unequal_lengths
  in
  loop [] (f x y) xs ys
;;

let rev_map2_exn t1 t2 ~f = length_mismatch "rev_map2_exn" ~f:(rev_map2 ~f) t1 t2
let map2 t1 t2 ~f = rev_map2 t1 t2 ~f >>| rev
let map2_exn t1 t2 ~f = length_mismatch "map2_exn" ~f:(map2 ~f) t1 t2

let iter (x :: xs) ~f =
  f x;
  List.iter xs ~f
;;

let iteri (x :: xs) ~f =
  f 0 x;
  List.iteri xs ~f:(fun i x -> f (i + 1) x)
;;

let iter2 (x :: xs) (y :: ys) ~f =
  f x y;
  List.iter2 xs ys ~f
;;

let iter2_exn t1 t2 ~f = length_mismatch "iter2_exn" ~f:(iter2 ~f) t1 t2
let for_all (x :: xs) ~f = f x && List.for_all xs ~f
let for_alli (x :: xs) ~f = f 0 x && List.for_alli xs ~f:(fun i x -> f (i + 1) x)
let for_all2 t1 t2 ~f = List.for_all2 (to_list t1) (to_list t2) ~f
let for_all2_exn t1 t2 ~f = length_mismatch "for_all2_exn" ~f:(for_all2 ~f) t1 t2
let exists (x :: xs) ~f = f x || List.exists xs ~f
let existsi (x :: xs) ~f = f 0 x && List.existsi xs ~f:(fun i x -> f (i + 1) x)
let exists2 t1 t2 ~f = List.exists2 (to_list t1) (to_list t2) ~f
let exists2_exn t1 t2 ~f = length_mismatch "exists2_exn" ~f:(exists2 ~f) t1 t2
let filter t ~f = List.filter (to_list t) ~f
let filter_map t ~f = List.filter_map (to_list t) ~f
let filteri t ~f = List.filteri (to_list t) ~f
let filter_mapi t ~f = List.filter_mapi (to_list t) ~f
let rev_filter t ~f = List.rev_filter (to_list t) ~f
let rev_filter_map t ~f = List.rev_filter_map (to_list t) ~f
let rev_filter_mapi t ~f = List.rev_filter_mapi (to_list t) ~f
let count t ~f = List.count (to_list t) ~f
let counti t ~f = List.counti (to_list t) ~f
let zip (x :: xs) (y :: ys) = List.zip xs ys >>| fun zs -> (x, y) :: zs
let zip_exn t1 t2 = length_mismatch "zip_exn" ~f:zip t1 t2

let unzip ((x, y) :: zs) =
  let xs, ys = List.unzip zs in
  x :: xs, y :: ys
;;

let partition_map t ~f = List.partition_map (to_list t) ~f
let partition_tf t ~f = List.partition_tf (to_list t) ~f
let split_n t n = List.split_n (to_list t) n

include Monad.Make (struct
  type nonrec 'a t = 'a neList

  let return x = [ x ]
  let bind m ~f = concat_map m ~f
  let map = `Custom map
end)

let all_options (x :: xs) =
  let open Option.Let_syntax in
  let%map x = x
  and xs = Option.all xs in
  x :: xs
;;
