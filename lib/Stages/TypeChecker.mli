open! Base

type error

val errorMessage : error -> string
val errorType : error -> [ `Sort | `Kind | `Type ]

type ('s, 't) checkResult =
  (CompilerState.state, 't, ('s, error) Source.annotate) CompilerState.t

val checkSort : 's Ast.Index.t -> ('s, Nucleus.Index.t) checkResult
val checkKind : 's Ast.Type.t -> ('s, Nucleus.Type.t) checkResult
val checkType : 's Ast.Expr.t -> ('s, Nucleus.Expr.t) checkResult
val checkProgram : 's Ast.t -> ('s, Nucleus.t) checkResult

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = SB.source Ast.t
    with type output = Nucleus.t
    with type error = (SB.source option, string) Source.annotate
