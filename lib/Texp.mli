open! Base

type braceType =
  | Parens
  | Bracks

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

val source : (module Source.BuilderT with type source = 's) -> 's t -> 's
