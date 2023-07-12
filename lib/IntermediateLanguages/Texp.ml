open! Base

type braceType =
  | Parens
  | Bracks
[@@deriving sexp_of]

type 's t =
  | List of
      { braceType : braceType
      ; elements : 's t list (* The source of the left and right brace *)
      ; braceSources : 's * 's
      }
  | String of string * 's
  | Integer of int * 's
  | Symbol of string * 's
[@@deriving sexp_of]

let source (type s) (module SB : Source.BuilderT with type source = s) : s t -> s
  = function
  | List { braceType = _; elements = _; braceSources = left, right } ->
    SB.merge left right
  | String (_, source) -> source
  | Integer (_, source) -> source
  | Symbol (_, source) -> source
;;
