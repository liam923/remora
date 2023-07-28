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
val subIndicesIntoForallType : Index.t Map.M(Identifier).t -> Type.forall -> Type.forall
val subIndicesIntoPiType : Index.t Map.M(Identifier).t -> Type.pi -> Type.pi
val subTypesIntoArrayType : Type.t Map.M(Identifier).t -> Type.array -> Type.array
val subTypesIntoAtomType : Type.t Map.M(Identifier).t -> Type.atom -> Type.atom
val subTypesIntoType : Type.t Map.M(Identifier).t -> Type.t -> Type.t
val subTypesIntoForallType : Type.t Map.M(Identifier).t -> Type.forall -> Type.forall
val subTypesIntoPiType : Type.t Map.M(Identifier).t -> Type.pi -> Type.pi
val subIndicesIntoArrayExpr : Index.t Map.M(Identifier).t -> Expr.array -> Expr.array
val subIndicesIntoAtomExpr : Index.t Map.M(Identifier).t -> Expr.atom -> Expr.atom

val subIndicesIntoTermLambda
  :  Index.t Map.M(Identifier).t
  -> Expr.termLambda
  -> Expr.termLambda

val subTypesIntoArrayExpr : Type.t Map.M(Identifier).t -> Expr.array -> Expr.array
val subTypesIntoAtomExpr : Type.t Map.M(Identifier).t -> Expr.atom -> Expr.atom

val subTypesIntoTermLambda
  :  Type.t Map.M(Identifier).t
  -> Expr.termLambda
  -> Expr.termLambda
