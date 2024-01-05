open! Base

type 's t =
  | ParenList of
      { elements : 's t list
      ; braceSources : 's * 's (* The source of the left and right parens *)
      }
  | SquareList of
      { elements : 's t list
      ; braceSources : 's * 's (* The source of the left and right brackets *)
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
  | Float of float * 's
  | Symbol of string * 's
[@@deriving sexp_of]

val source : (module Source.BuilderT with type source = 's) -> 's t -> 's
