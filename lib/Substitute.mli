open! Base
open Nucleus

val subIndicesIntoDimIndex
  :  Index.t Map.M(Identifier).t
  -> Index.dimension
  -> Index.dimension

val subIndicesIntoShapeIndex : Index.t Map.M(Identifier).t -> Index.shape -> Index.shape
val subIndicesIntoArrayType : Index.t Map.M(Identifier).t -> Type.array -> Type.array
val subIndicesIntoAtomType : Index.t Map.M(Identifier).t -> Type.atom -> Type.atom
val subTypesIntoArrayType : Type.t Map.M(Identifier).t -> Type.array -> Type.array
val subTypesIntoAtomType : Type.t Map.M(Identifier).t -> Type.atom -> Type.atom
