(* open! Base
module Mono = MonoNucleus
module Poly = Nucleus
open Poly

module MonoState = struct
  include CompilerState

  let createMonoId canonId : (state, Identifier.t, _) t =
    make ~f:(fun state ->
        State.run
          (Identifier.create
             (Identifier.name canonId)
             ~getCounter:(fun (s : state) -> s.idCounter)
             ~setCounter:(fun _ idCounter -> CompilerState.{ idCounter }))
          state)
  ;;
end

open MonoState.Let_syntax

type application =
  | TypeApp of Type.t list
  | IndexApp of Index.t list
  | TupleDeref of int
[@@deriving sexp]

type appStack = application list [@@deriving sexp]

module CanonicalAppStack = struct
  module T = struct
    type t =
      | TypeApp of Canonical.Type.t list
      | IndexApp of Canonical.Index.t list
      | TupleDeref of int
    [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)

  let from =
    List.map ~f:(fun (app : application) ->
        match app with
        | TypeApp types -> TypeApp (List.map types ~f:Canonical.Type.from)
        | IndexApp indices -> IndexApp (List.map indices ~f:Canonical.Index.from)
        | TupleDeref i -> TupleDeref i)
  ;;
end

type cacheEntry =
  { morphedValue : Mono.Expr.array option
  ; newBinding : Identifier.t
  }

type variableCache = cacheEntry Map.M(CanonicalAppStack).t Map.M(Identifier).t
type paramRequests = appStack list list
type 't result = (CompilerState.state, 't, string) MonoState.t

type 't morphedExpr =
  { expr : 't
  ; variableCache : variableCache
  ; paramRequestsStack : paramRequests list
  }

let mapMorphedExpr { expr; variableCache; paramRequestsStack } ~f =
  { expr = f expr; variableCache; paramRequestsStack }
;;

let singleton atom =
  Mono.Expr.Arr
    { elements = [ atom ]
    ; dimensions = []
    ; type' = { element = Mono.Expr.atomType atom; shape = [] }
    }
;;

let rec morphAtom (stack : appStack)
    : Poly.Expr.atom -> Mono.Expr.array morphedExpr result
  = function
  | TermLambda { params; body; type' = _ } ->
    let%map { expr = body
            ; variableCache = bodyCache
            ; paramRequestsStack = bodyParamStack
            }
      =
      morphArray stack body
    in
    let paramRequests =
      List.map params ~f:(fun param ->
          Map.find bodyQ param.binding
          |> Option.value ~default:[]
          |> List.map ~f:(fun qe -> qe.stack))
    in
    let params =
      List.bind params ~f:(fun param ->
          let newParams =
            Map.find bodyQ param.binding
            |> Option.value ~default:[]
            |> List.map ~f:(fun qe ->
                   { binding = qe.newBinding
                   ; bound = morphArrayType qe.stack param.bound
                   })
          in
          param :: newParams)
    in
    let queue =
      List.fold params ~init:bodyQ ~f:(fun q param -> Map.remove q param.binding)
    in
    let type' =
      Mono.Type.
        { parameters = List.map params ~f:(fun p -> p.bound)
        ; return = Mono.Expr.arrayType body
        }
    in
    { expr = singleton (Mono.Expr.TermLambda { params; body; type' })
    ; queue
    ; paramRequestsStack = paramRequests :: bodyParamStack
    }
  | TypeLambda { params; body; type' = _ } ->
    (match stack with
    | TypeApp types :: restStack ->
      let subs =
        List.zip_exn params types
        |> List.fold
             ~init:(Map.empty (module Identifier))
             ~f:(fun subs (param, sub) -> Map.set subs ~key:param.binding ~data:sub)
      in
      let subbedBody = Substitute.subTypesIntoArrayExpr subs body in
      morphArray restStack subbedBody
    | _ as stack ->
      Error.raise_s
        (Sexp.List
           [ Sexp.Atom "Expected type application at head of stack"
           ; Sexp.List [ Sexp.Atom "stack"; sexp_of_appStack stack ]
           ]))
  | IndexLambda { params; body; type' = _ } ->
    (match stack with
    | IndexApp indices :: restStack ->
      let subs =
        List.zip_exn params indices
        |> List.fold
             ~init:(Map.empty (module Identifier))
             ~f:(fun subs (param, sub) -> Map.set subs ~key:param.binding ~data:sub)
      in
      let subbedBody = Substitute.subIndicesIntoArrayExpr subs body in
      morphArray restStack subbedBody
    | _ as stack ->
      Error.raise_s
        (Sexp.List
           [ Sexp.Atom "Expected index application at head of stack"
           ; Sexp.List [ Sexp.Atom "stack"; sexp_of_appStack stack ]
           ]))
  | Box _ -> Error.raise (Error.of_string "")
  | Tuple _ -> Error.raise (Error.of_string "")
  | Literal (IntLiteral i) ->
    MonoState.return
      { expr = singleton (Mono.Expr.Literal (IntLiteral i))
      ; queue = Map.empty (module Identifier)
      ; paramRequestsStack = []
      }
  | Literal (CharacterLiteral c) ->
    MonoState.return
      { expr = singleton (Mono.Expr.Literal (CharacterLiteral c))
      ; queue = Map.empty (module Identifier)
      ; paramRequestsStack = []
      }

and morphArray (stack : appStack) : Poly.Expr.array -> Mono.Expr.array morphedExpr result
  = function
  | Ref { id = canonId; type' } ->
    let%map monoId = MonoState.createMonoId canonId in
    { expr = Mono.Expr.Ref { id = monoId; type' }
    ; queue =
        { t =
            Map.singleton
              (module Identifier)
              canonId
              [ { stack = stack.t; newBinding = monoId } ]
        ; i =
            Map.singleton
              (module Identifier)
              canonId
              [ { stack = stack.i; newBinding = monoId } ]
        }
    ; paramRequestsStack = []
    }
  | Arr _ -> Error.raise (Error.of_string "")
  | Frame _ -> Error.raise (Error.of_string "")
  | TermApplication _ -> Error.raise (Error.of_string "")
  | TypeApplication { tFunc; args; type' = _ } -> morphArray (TypeApp args :: stack) tFunc
  | IndexApplication { iFunc; args; type' = _ } ->
    morphArray (IndexApp args :: stack) iFunc
  | Unbox _ -> Error.raise (Error.of_string "")
  | Let _ -> Error.raise (Error.of_string "")
  | TupleLet _ -> Error.raise (Error.of_string "")

and morph (stack : appStack) : Poly.Expr.t -> Mono.Expr.t morphedExpr result = function
  | Array array ->
    let%map morphed = morphArray stack array in
    mapMorphedExpr morphed ~f:(fun array -> Mono.Expr.Array array)
  | Atom atom ->
    let%map morphed = morphAtom stack atom in
    mapMorphedExpr morphed ~f:(fun atom -> Mono.Expr.Array atom)
;;

let monomorphize _ = Error.raise (Error.of_string "")

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = Nucleus.t
  type output = MonoNucleus.t
  type error = (SB.source option, string) Source.annotate

  let name = "Monomorphize"
  let run input = monomorphize input
end *)
