open! Base

module Sort (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = SB.source Ast.Index.t
    with type output = Typed.Index.t
    with type error = (SB.source option, string) Source.annotate

module Kind (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = SB.source Ast.Type.t
    with type output = Typed.Type.t
    with type error = (SB.source option, string) Source.annotate

module Type (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = SB.source Ast.Expr.t
    with type output = Typed.Expr.t
    with type error = (SB.source option, string) Source.annotate

module M (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = SB.source Ast.t
    with type output = Typed.t
    with type error = (SB.source option, string) Source.annotate
