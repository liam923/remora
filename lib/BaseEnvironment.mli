open! Base

module type S = sig
  val make : unit -> (CompilerState.state, Environment.t, _) CompilerState.t
end

module Base : S
module Stdlib : S
