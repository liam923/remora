open! Base

type state = unit

module MonoStateT = struct
  include StateT.Make2WithError (MResult)
  (* 
  type nonrec state = state

  let ok = return
  let err error = returnF (MResult.err error)
  let errs errors = returnF (MResult.Errors errors)

  let ofOption o ~err:error =
    match o with
    | Some v -> return v
    | None -> err error
  ;;

  let require b error = if b then return () else err error

  let traverseOpt = function
    | Some r -> map r ~f:(fun e -> Some e)
    | None -> return None
  ;; *)
end

(* open MonoStateT.Let_syntax *)

type 't result = (state, 't, string) MonoStateT.t

(* module MonomorphIndex = struct
  let monomorph : Nucleus.Index.t -> MonoNucleus.Index.t result =
    Error.raise (Error.of_string "unimplemented")
  ;;
end

module MonomorphType = struct
  let monomorph : Nucleus.Type.t -> MonoNucleus.Type.t result =
    Error.raise (Error.of_string "unimplemented")
  ;;
end *)

module MonomorphExpr = struct
  let monomorph (_ : Nucleus.Expr.t) : MonoNucleus.Expr.t result =
    Error.raise (Error.of_string "unimplemented")
  ;;
end

let monomorphize expr = MonoStateT.runA (MonomorphExpr.monomorph expr) ()

module Stage (SB : Source.BuilderT) = struct
  type input = Nucleus.t
  type output = MonoNucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "Monomorphize"

  let run input =
    match monomorphize input with
    | MOk _ as expr -> expr
    | Errors errs ->
      Errors (NeList.map errs ~f:(fun err -> Source.{ elem = err; source = None }))
  ;;
end
