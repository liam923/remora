open! Base
open Nucleus

val subIndicesIntoDimIndex
  :  Index.t Map.M(Identifier).t
  -> Index.dimension
  -> Index.dimension

val subIndicesIntoShapeIndex : Index.t Map.M(Identifier).t -> Index.shape -> Index.shape
val subIndicesIntoIndex : Index.t Map.M(Identifier).t -> Index.t -> Index.t
val subIndicesIntoArrayType : Index.t Map.M(Identifier).t -> Type.array -> Type.array
val subIndicesIntoAtomType : Index.t Map.M(Identifier).t -> Type.atom -> Type.atom
val subIndicesIntoType : Index.t Map.M(Identifier).t -> Type.t -> Type.t
val subTypesIntoArrayType : Type.t Map.M(Identifier).t -> Type.array -> Type.array
val subTypesIntoAtomType : Type.t Map.M(Identifier).t -> Type.atom -> Type.atom
val subTypesIntoType : Type.t Map.M(Identifier).t -> Type.t -> Type.t
val subIndicesIntoArrayExpr : Index.t Map.M(Identifier).t -> Expr.array -> Expr.array
val subIndicesIntoAtomExpr : Index.t Map.M(Identifier).t -> Expr.atom -> Expr.atom
val subTypesIntoArrayExpr : Type.t Map.M(Identifier).t -> Expr.array -> Expr.array
val subTypesIntoAtomExpr : Type.t Map.M(Identifier).t -> Expr.atom -> Expr.atom
