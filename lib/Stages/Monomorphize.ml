open! Base
module Mono = MonoNucleus
module Poly = Nucleus
(* open Poly *)

module MonoState = struct
  include CompilerState
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

(* open MonoState.Let_syntax

type tstack = Type.t list list
type istack = Index.t list list

type stack =
  { t : tstack
  ; i : istack
  }

type tqueue = ((tstack * Identifier.t) Map.M(Canonical.Type).t) Map.M(Identifier).t
type iqueue = (istack * Identifier.t) Map.M(Identifier).t

type queue =
  { t : tqueue
  ; i : iqueue
  }

type paramRequests = queue list
type 't result = (CompilerState.state, 't, string) MonoState.t

type 't morphedExpr =
  { expr : 't
  ; queue : queue
  ; paramRequests : paramRequests option
  }

let mapMorphedExpr { expr; queue; paramRequests } ~f =
  { expr = f expr; queue; paramRequests }
;;

let rec morphAtom (stack : stack) : Poly.Expr.atom -> Mono.Expr.atom morphedExpr result
  = function
  | TermLambda _ -> Error.raise (Error.of_string "")
  | TypeLambda _ -> Error.raise (Error.of_string "")
  | IndexLambda _ -> Error.raise (Error.of_string "")
  | Box _ -> Error.raise (Error.of_string "")
  | Tuple _ -> Error.raise (Error.of_string "")
  | Literal (IntLiteral i) ->
    MonoState.return
      { expr = Mono.Expr.Literal (IntLiteral i)
      ; queue = { t = Map.empty (module Identifier); i = Map.empty (module Identifier) }
      ; paramRequests = None
      }
  | Literal (CharacterLiteral c) ->
    MonoState.return
      { expr = Mono.Expr.Literal (CharacterLiteral c)
      ; queue = { t = Map.empty (module Identifier); i = Map.empty (module Identifier) }
      ; paramRequests = None
      }

and morphArray (stack : stack) : Poly.Expr.array -> Mono.Expr.array morphedExpr result
  = function
  | Ref _ -> Error.raise (Error.of_string "")
  | Arr _ -> Error.raise (Error.of_string "")
  | Frame _ -> Error.raise (Error.of_string "")
  | TermApplication _ -> Error.raise (Error.of_string "")
  | TypeApplication { tFunc; args; type' = _ } ->
    morphArray { stack with t = args :: stack.t } tFunc
  | IndexApplication { iFunc; args; type' = _ } ->
    morphArray { stack with i = args :: stack.i } iFunc
  | Unbox _ -> Error.raise (Error.of_string "")
  | Let _ -> Error.raise (Error.of_string "")
  | TupleLet _ -> Error.raise (Error.of_string "")

and morph (stack : stack) : Poly.Expr.t -> Mono.Expr.t morphedExpr result = function
  | Array array ->
    let%map morphed = morphArray stack array in
    mapMorphedExpr morphed ~f:(fun array -> Mono.Expr.Array array)
  | Atom atom ->
    let%map morphed = morphAtom stack atom in
    mapMorphedExpr morphed ~f:(fun atom -> Mono.Expr.Atom atom)
;; *)

let monomorphize _ = Error.raise (Error.of_string "")

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nucleus.t
  type output = MonoNucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "Monomorphize"
  let run input = monomorphize input
end
