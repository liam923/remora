open! Base

type braceType =
  | Paren
  | Square
[@@deriving sexp_of]

type 's t =
  | List of
      { braceType : braceType
      ; elements : 's t list (* The source of the left and right brace *)
      ; braceSources : 's * 's
      }
  | WithCurlies of
      { base : 's t
      ; leftElements : 's t list
      ; rightElements : 's t list
      ; curlySources : 's * 's
      ; splitSource : 's
      }
  | String of string * 's
  | Integer of int * 's
  | Symbol of string * 's
[@@deriving sexp_of]

let source (type s) (module SB : Source.BuilderT with type source = s) : s t -> s
  = function
  | List { braceType = _; elements = _; braceSources = left, right } ->
    SB.merge left right
  | WithCurlies
      { base = _
      ; leftElements = _
      ; rightElements = _
      ; curlySources = left, right
      ; splitSource = _
      } -> SB.merge left right
  | String (_, source) -> source
  | Integer (_, source) -> source
  | Symbol (_, source) -> source
;;
