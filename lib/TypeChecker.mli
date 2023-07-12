open! Base

type error

val errorMessage : error -> string
val errorType : error -> [ `Sort | `Kind | `Type ]

type ('s, 't) checkResult = ('t, ('s, error) Source.annotate) MResult.t

val checkSort : 's Ast.Index.t -> ('s, Core.Index.t) checkResult
val checkKind : 's Ast.Type.t -> ('s, Core.Type.t) checkResult
val checkType : 's Ast.Expr.t -> ('s, Core.Expr.t) checkResult
