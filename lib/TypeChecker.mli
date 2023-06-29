open! Base
open Ast

type error

val errorMessage : error -> string
val errorType : error -> [ `Sort | `Kind | `Type ]

type ('s, 't) checkResult = ('t, ('s, error) withSource) MResult.t

type 'v envEntry =
  { e : 'v
  ; id : Typed.Identifier.t
  }

type env =
  { sorts : Sort.t envEntry Map.M(String).t
  ; kinds : Kind.t envEntry Map.M(String).t
  ; types : Typed.Type.t envEntry Map.M(String).t
  }

val baseEnv : env
val checkSort : env -> 's Untyped.Index.t -> ('s, Typed.Index.t) checkResult
val checkKind : env -> 's Untyped.Type.t -> ('s, Typed.Type.t) checkResult
val checkType : env -> 's Untyped.Expr.t -> ('s, Typed.Expr.t) checkResult
