open! Base

type 's t =
  | ParensList of 's t list * 's
  | BracksList of 's t list * 's
  | String of string * 's
  | Integer of int * 's
  | Identifier of string * 's

let rec mapSource texp ~f =
  match texp with
  | ParensList (list, source) -> ParensList (List.map list ~f:(mapSource ~f), f source)
  | BracksList (list, source) -> BracksList (List.map list ~f:(mapSource ~f), f source)
  | String (str, source) -> String (str, f source)
  | Integer (i, source) -> Integer (i, f source)
  | Identifier (id, source) -> Identifier (id, f source)
;;
