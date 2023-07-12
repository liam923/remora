open! Base

type error

val errorMessage : error -> string
val errorType : error -> [ `Sort | `Kind | `Type ]

type ('s, 't) checkResult = ('t, ('s, error) Source.annotate) MResult.t

val checkSort : 's Ast.Index.t -> ('s, Nucleus.Index.t) checkResult
val checkKind : 's Ast.Type.t -> ('s, Nucleus.Type.t) checkResult
val checkType : 's Ast.Expr.t -> ('s, Nucleus.Expr.t) checkResult

module Stage (SB : Source.BuilderT) :
  Pipeline.Stage
    with type input = SB.source Ast.Expr.t
    with type output = Nucleus.Expr.t
    with type error = (SB.source, string) Source.annotate
