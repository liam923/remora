open! Base

module Sort (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = SB.source Ast.Index.t
  type output = Nucleus.Index.t
  type error = (SB.source option, string) Source.annotate

  let name = "Sort Check"

  let run input =
    CompilerPipeline.S.makeF ~f:(fun state ->
      let%bind.MResult state, env =
        CompilerState.run (BaseEnvironment.Stdlib.make ()) state
      in
      match CompilerState.run (TypeCheck.Sort.check ~env input) state with
      | MOk _ as expr -> expr
      | Errors errs ->
        Errors
          (NeList.map errs ~f:(fun { elem = err; source } ->
             Source.{ elem = TypeCheck.errorMessage err; source = Some source })))
  ;;
end

module Kind (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = SB.source Ast.Type.t
  type output = Nucleus.Type.t
  type error = (SB.source option, string) Source.annotate

  let name = "Kind Check"

  let run input =
    CompilerPipeline.S.makeF ~f:(fun state ->
      let%bind.MResult state, env =
        CompilerState.run (BaseEnvironment.Stdlib.make ()) state
      in
      match CompilerState.run (TypeCheck.Kind.check ~env input) state with
      | MOk _ as expr -> expr
      | Errors errs ->
        Errors
          (NeList.map errs ~f:(fun { elem = err; source } ->
             Source.{ elem = TypeCheck.errorMessage err; source = Some source })))
  ;;
end

module Type (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = SB.source Ast.Expr.t
  type output = Nucleus.Expr.t
  type error = (SB.source option, string) Source.annotate

  let name = "Type Check"

  let run input =
    CompilerPipeline.S.makeF ~f:(fun state ->
      let%bind.MResult state, env =
        CompilerState.run (BaseEnvironment.Stdlib.make ()) state
      in
      match CompilerState.run (TypeCheck.Type.check ~env input) state with
      | MOk _ as expr -> expr
      | Errors errs ->
        Errors
          (NeList.map errs ~f:(fun { elem = err; source } ->
             Source.{ elem = TypeCheck.errorMessage err; source = Some source })))
  ;;
end

module M (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = SB.source Ast.t
  type output = Nucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "Type Check"

  let run input =
    CompilerPipeline.S.makeF ~f:(fun state ->
      let%bind.MResult state, env =
        CompilerState.run (BaseEnvironment.Stdlib.make ()) state
      in
      match CompilerState.run (TypeCheck.check ~env input) state with
      | MOk _ as expr -> expr
      | Errors errs ->
        Errors
          (NeList.map errs ~f:(fun { elem = err; source } ->
             Source.{ elem = TypeCheck.errorMessage err; source = Some source })))
  ;;
end
