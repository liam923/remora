open! Base

type error

val errorMessage : error -> string
val errorType : error -> [ `Sort | `Kind | `Type ]

type ('s, 't) checkResult =
  (CompilerState.state, 't, ('s, error) Source.annotate) CompilerState.t

module Sort : sig
  val check : 's Ast.Index.t -> ('s, Nucleus.Index.t) checkResult

  module Stage (SB : Source.BuilderT) :
    CompilerPipeline.Stage
      with type state = CompilerState.state
      with type input = SB.source Ast.Index.t
      with type output = Nucleus.Index.t
      with type error = (SB.source option, string) Source.annotate
end

module Kind : sig
  val check : 's Ast.Type.t -> ('s, Nucleus.Type.t) checkResult

  module Stage (SB : Source.BuilderT) :
    CompilerPipeline.Stage
      with type state = CompilerState.state
      with type input = SB.source Ast.Type.t
      with type output = Nucleus.Type.t
      with type error = (SB.source option, string) Source.annotate
end

module Type : sig
  val check : 's Ast.Expr.t -> ('s, Nucleus.Expr.t) checkResult

  module Stage (SB : Source.BuilderT) :
    CompilerPipeline.Stage
      with type state = CompilerState.state
      with type input = SB.source Ast.Expr.t
      with type output = Nucleus.Expr.t
      with type error = (SB.source option, string) Source.annotate
end

val check : 's Ast.t -> ('s, Nucleus.t) checkResult

module Stage (SB : Source.BuilderT) :
  CompilerPipeline.Stage
    with type state = CompilerState.state
    with type input = SB.source Ast.t
    with type output = Nucleus.t
    with type error = (SB.source option, string) Source.annotate
