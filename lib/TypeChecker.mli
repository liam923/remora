open! Base
open Ast

type error

val errorMessage : error -> string
val errorType : error -> [ `Sort | `Kind | `Type ]

type ('s, 't) checkResult = ('t, ('s, error) Source.annotate) MResult.t

val checkSort : Environment.t -> 's Untyped.Index.t -> ('s, Typed.Index.t) checkResult
val checkKind : Environment.t -> 's Untyped.Type.t -> ('s, Typed.Type.t) checkResult
val checkType : Environment.t -> 's Untyped.Expr.t -> ('s, Typed.Expr.t) checkResult
