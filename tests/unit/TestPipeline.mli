open! Base
open Remora

val runAndPrint
  :  ( CompilerState.state
     , 'a
     , string
     , ('s option, string) Source.annotate )
     CompilerPipeline.t
  -> 'a
  -> unit
