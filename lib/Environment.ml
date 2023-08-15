open! Base
open Nucleus

type t =
  { sorts : Index.t Map.M(String).t
  ; kinds : Type.t Map.M(String).t
  ; types : Expr.array Map.M(String).t
  }

module type IdentifierGenerator = sig
  include StateT.S2

  type state

  val createId : string -> (state, Identifier.t, 'e) t
end

module Base (Gen : IdentifierGenerator) = struct
  let make () =
    let open Gen.Let_syntax in
    let makeSubEnv namesAndEntries =
      let%map alist =
        namesAndEntries
        |> List.map ~f:(function name, value ->
          let%map value = value in
          name, value)
        |> Gen.all
      in
      Map.of_alist_exn (module String) alist
    in
    let%bind sorts = makeSubEnv [] in
    let%bind kinds =
      makeSubEnv
        [ "int", Gen.return (Type.Atom (Literal IntLiteral))
        ; "char", Gen.return (Type.Atom (Literal CharacterLiteral))
        ; "bool", Gen.return (Type.Atom (Literal BooleanLiteral))
        ]
    in
    let scalarType element = Type.Arr { element; shape = [] } in
    let intArr = scalarType (Literal IntLiteral) in
    let%map types =
      makeSubEnv
        [ ( "+"
          , Gen.return
              (Expr.Primitive
                 { func = Add
                 ; type' =
                     scalarType
                       (Type.Func { parameters = [ intArr; intArr ]; return = intArr })
                 }) )
        ; ( "-"
          , Gen.return
              (Expr.Primitive
                 { func = Sub
                 ; type' =
                     scalarType
                       (Type.Func { parameters = [ intArr; intArr ]; return = intArr })
                 }) )
        ; ( "*"
          , Gen.return
              (Expr.Primitive
                 { func = Mul
                 ; type' =
                     scalarType
                       (Type.Func { parameters = [ intArr; intArr ]; return = intArr })
                 }) )
        ; ( "/"
          , Gen.return
              (Expr.Primitive
                 { func = Div
                 ; type' =
                     scalarType
                       (Type.Func { parameters = [ intArr; intArr ]; return = intArr })
                 }) )
        ; ( "length"
          , let%map t = Gen.createId "t"
            and d = Gen.createId "d"
            and cellShape = Gen.createId "@cell-shape" in
            let type' =
              scalarType
                (Type.Pi
                   { parameters =
                       [ { binding = d; bound = Sort.Dim }
                       ; { binding = cellShape; bound = Sort.Shape }
                       ]
                   ; body =
                       scalarType
                         (Type.Forall
                            { parameters = [ { binding = t; bound = Kind.Atom } ]
                            ; body =
                                scalarType
                                  (Type.Func
                                     { parameters =
                                         [ Arr
                                             { element = AtomRef t
                                             ; shape =
                                                 [ Add (Index.dimensionRef d)
                                                 ; ShapeRef cellShape
                                                 ]
                                             }
                                         ]
                                     ; return = intArr
                                     })
                            })
                   })
            in
            Expr.Primitive { func = Length; type' } )
        ; ( "reduce"
          , let%map itemPad = Gen.createId "@item-pad"
            and cellShape = Gen.createId "@cell-shape"
            and dSub1 = Gen.createId "d-1"
            and t = Gen.createId "t" in
            let type' =
              scalarType
                (Type.Pi
                   { parameters =
                       [ { binding = dSub1; bound = Sort.Dim }
                       ; { binding = itemPad; bound = Sort.Shape }
                       ; { binding = cellShape; bound = Sort.Shape }
                       ]
                   ; body =
                       scalarType
                         (Type.Forall
                            { parameters = [ { binding = t; bound = Kind.Atom } ]
                            ; body =
                                scalarType
                                  (Type.Func
                                     { parameters =
                                         [ scalarType
                                             (Func
                                                { parameters =
                                                    [ Arr
                                                        { element = AtomRef t
                                                        ; shape = [ ShapeRef cellShape ]
                                                        }
                                                    ; Arr
                                                        { element = AtomRef t
                                                        ; shape = [ ShapeRef cellShape ]
                                                        }
                                                    ]
                                                ; return =
                                                    Arr
                                                      { element = AtomRef t
                                                      ; shape = [ ShapeRef cellShape ]
                                                      }
                                                })
                                         ; Arr
                                             { element = AtomRef t
                                             ; shape =
                                                 [ Add
                                                     { const = 1
                                                     ; refs =
                                                         Map.singleton
                                                           (module Identifier)
                                                           dSub1
                                                           1
                                                     }
                                                 ; ShapeRef itemPad
                                                 ; ShapeRef cellShape
                                                 ]
                                             }
                                         ]
                                     ; return =
                                         Arr
                                           { element = AtomRef t
                                           ; shape =
                                               [ ShapeRef itemPad; ShapeRef cellShape ]
                                           }
                                     })
                            })
                   })
            in
            Expr.Primitive { func = Reduce; type' } )
        ; ( "scan"
          , let%map itemPad = Gen.createId "@item-pad"
            and cellShape = Gen.createId "@cell-shape"
            and dSub1 = Gen.createId "d-1"
            and t = Gen.createId "t" in
            let type' =
              scalarType
                (Type.Pi
                   { parameters =
                       [ { binding = dSub1; bound = Sort.Dim }
                       ; { binding = itemPad; bound = Sort.Shape }
                       ; { binding = cellShape; bound = Sort.Shape }
                       ]
                   ; body =
                       scalarType
                         (Type.Forall
                            { parameters = [ { binding = t; bound = Kind.Atom } ]
                            ; body =
                                scalarType
                                  (Type.Func
                                     { parameters =
                                         [ scalarType
                                             (Func
                                                { parameters =
                                                    [ Arr
                                                        { element = AtomRef t
                                                        ; shape = [ ShapeRef cellShape ]
                                                        }
                                                    ; Arr
                                                        { element = AtomRef t
                                                        ; shape = [ ShapeRef cellShape ]
                                                        }
                                                    ]
                                                ; return =
                                                    Arr
                                                      { element = AtomRef t
                                                      ; shape = [ ShapeRef cellShape ]
                                                      }
                                                })
                                         ; Arr
                                             { element = AtomRef t
                                             ; shape =
                                                 [ Add
                                                     { const = 1
                                                     ; refs =
                                                         Map.singleton
                                                           (module Identifier)
                                                           dSub1
                                                           1
                                                     }
                                                 ; ShapeRef itemPad
                                                 ; ShapeRef cellShape
                                                 ]
                                             }
                                         ]
                                     ; return =
                                         Arr
                                           { element = AtomRef t
                                           ; shape =
                                               [ Add
                                                   { const = 1
                                                   ; refs =
                                                       Map.singleton
                                                         (module Identifier)
                                                         dSub1
                                                         1
                                                   }
                                               ; ShapeRef itemPad
                                               ; ShapeRef cellShape
                                               ]
                                           }
                                     })
                            })
                   })
            in
            Expr.Primitive { func = Scan; type' } )
        ; ( "filter"
          , let%map t = Gen.createId "t"
            and d = Gen.createId "d"
            and cellShape = Gen.createId "@cell-shape"
            and dOut = Gen.createId "@d-out" in
            let type' =
              scalarType
                (Type.Pi
                   { parameters =
                       [ { binding = d; bound = Sort.Dim }
                       ; { binding = cellShape; bound = Sort.Shape }
                       ]
                   ; body =
                       scalarType
                         (Type.Forall
                            { parameters = [ { binding = t; bound = Kind.Atom } ]
                            ; body =
                                scalarType
                                  (Type.Func
                                     { parameters =
                                         [ Arr
                                             { element = AtomRef t
                                             ; shape =
                                                 [ Add (Index.dimensionRef d)
                                                 ; ShapeRef cellShape
                                                 ]
                                             }
                                         ; Arr
                                             { element = Literal BooleanLiteral
                                             ; shape =
                                                 [ Add (Index.dimensionRef d)
                                                 ; ShapeRef cellShape
                                                 ]
                                             }
                                         ]
                                     ; return =
                                         scalarType
                                           (Sigma
                                              { parameters =
                                                  [ { binding = dOut; bound = Sort.Dim } ]
                                              ; body =
                                                  Arr
                                                    { element = AtomRef t
                                                    ; shape =
                                                        [ Add (Index.dimensionRef dOut)
                                                        ; ShapeRef cellShape
                                                        ]
                                                    }
                                              })
                                     })
                            })
                   })
            in
            Expr.Primitive { func = Filter; type' } )
        ; ( "append"
          , let%map t = Gen.createId "t"
            and d1 = Gen.createId "d1"
            and d2 = Gen.createId "d2"
            and cellShape = Gen.createId "@cell-shape" in
            let type' =
              scalarType
                (Type.Pi
                   { parameters =
                       [ { binding = d1; bound = Sort.Dim }
                       ; { binding = d2; bound = Sort.Dim }
                       ; { binding = cellShape; bound = Sort.Shape }
                       ]
                   ; body =
                       scalarType
                         (Type.Forall
                            { parameters = [ { binding = t; bound = Kind.Atom } ]
                            ; body =
                                scalarType
                                  (Type.Func
                                     { parameters =
                                         [ Arr
                                             { element = AtomRef t
                                             ; shape =
                                                 [ Add (Index.dimensionRef d1)
                                                 ; ShapeRef cellShape
                                                 ]
                                             }
                                         ; Arr
                                             { element = AtomRef t
                                             ; shape =
                                                 [ Add (Index.dimensionRef d2)
                                                 ; ShapeRef cellShape
                                                 ]
                                             }
                                         ]
                                     ; return =
                                         Arr
                                           { element = AtomRef t
                                           ; shape =
                                               [ Add
                                                   { const = 0
                                                   ; refs =
                                                       Map.of_alist_exn
                                                         (module Identifier)
                                                         [ d1, 1; d2, 1 ]
                                                   }
                                               ; ShapeRef cellShape
                                               ]
                                           }
                                     })
                            })
                   })
            in
            Expr.Primitive { func = Append; type' } )
        ]
    in
    { sorts; kinds; types }
  ;;
end
