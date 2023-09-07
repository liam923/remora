open! Base

type braceType =
  | Paren
  | Square

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

val source : (module Source.BuilderT with type source = 's) -> 's t -> 's
