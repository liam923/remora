open! Base

type 's t =
  | ParenList of
      { elements : 's t list
      ; braceSources : 's * 's
      }
  | SquareList of
      { elements : 's t list
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
  | ParenList { elements = _; braceSources = left, right } -> SB.merge left right
  | SquareList { elements = _; braceSources = left, right } -> SB.merge left right
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
