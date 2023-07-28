open! Base
open Remora

let runAndPrint
  (pipeline :
    ( CompilerState.state
    , 'a
    , string
    , ('s option, string) Source.annotate )
    CompilerPipeline.t)
  (input : 'a)
  : unit
  =
  match
    CompilerPipeline.S.runA (CompilerPipeline.make pipeline input) CompilerState.initial
  with
  | MOk str -> Stdio.print_endline str
  | Errors errs ->
    NeList.iter errs ~f:(fun err -> Stdio.prerr_endline [%string "Error: %{err.elem}"])
;;
