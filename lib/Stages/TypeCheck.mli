open! Base

type error

val errorMessage : error -> string
val errorType : error -> [ `Sort | `Kind | `Type ]

type ('s, 't) checkResult =
  (CompilerState.state, 't, ('s, error) Source.annotate) CompilerState.t

module Sort : sig
  val check : env:Environment.t -> 's Ast.Index.t -> ('s, Typed.Index.t) checkResult

  val checkAndExpectDim
    :  env:Environment.t
    -> 's Ast.Index.t
    -> ('s, Typed.Index.dimension) checkResult

  val checkAndExpectShape
    :  env:Environment.t
    -> 's Ast.Index.t
    -> ('s, Typed.Index.shape) checkResult
end

module Kind : sig
  val check : env:Environment.t -> 's Ast.Type.t -> ('s, Typed.Type.t) checkResult

  val checkAndExpectArray
    :  env:Environment.t
    -> 's Ast.Type.t
    -> ('s, Typed.Type.array) checkResult

  val checkAndExpectAtom
    :  env:Environment.t
    -> 's Ast.Type.t
    -> ('s, Typed.Type.atom) checkResult
end

module Type : sig
  val check : env:Environment.t -> 's Ast.Expr.t -> ('s, Typed.Expr.t) checkResult

  val checkAndExpectArray
    :  env:Environment.t
    -> 's Ast.Expr.t
    -> ('s, Typed.Expr.array) checkResult

  val checkAndExpectAtom
    :  env:Environment.t
    -> 's Ast.Expr.t
    -> ('s, Typed.Expr.atom) checkResult
end

val check : env:Environment.t -> 's Ast.t -> ('s, Typed.t) checkResult
