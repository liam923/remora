open! Base

type error

val errorMessage : error -> string
val errorType : error -> [ `Sort | `Kind | `Type ]

type ('s, 't) checkResult =
  (CompilerState.state, 't, ('s, error) Source.annotate) CompilerState.t

module Sort : sig
  val check : env:Environment.t -> 's Ast.Index.t -> ('s, Nucleus.Index.t) checkResult

  val checkAndExpectDim
    :  env:Environment.t
    -> 's Ast.Index.t
    -> ('s, Nucleus.Index.dimension) checkResult

  val checkAndExpectShape
    :  env:Environment.t
    -> 's Ast.Index.t
    -> ('s, Nucleus.Index.shape) checkResult
end

module Kind : sig
  val check : env:Environment.t -> 's Ast.Type.t -> ('s, Nucleus.Type.t) checkResult

  val checkAndExpectArray
    :  env:Environment.t
    -> 's Ast.Type.t
    -> ('s, Nucleus.Type.array) checkResult

  val checkAndExpectAtom
    :  env:Environment.t
    -> 's Ast.Type.t
    -> ('s, Nucleus.Type.atom) checkResult
end

module Type : sig
  val check : env:Environment.t -> 's Ast.Expr.t -> ('s, Nucleus.Expr.t) checkResult

  val checkAndExpectArray
    :  env:Environment.t
    -> 's Ast.Expr.t
    -> ('s, Nucleus.Expr.array) checkResult

  val checkAndExpectAtom
    :  env:Environment.t
    -> 's Ast.Expr.t
    -> ('s, Nucleus.Expr.atom) checkResult
end

val check : env:Environment.t -> 's Ast.t -> ('s, Nucleus.t) checkResult
