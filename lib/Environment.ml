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
        ]
    in
    let scalarType element = Type.Arr { element; shape = [] } in
    let intArr = scalarType (Literal IntLiteral) in
    let scalar element =
      Expr.Scalar { element; type' = { element = Expr.atomType element; shape = [] } }
    in
    let%map types =
      makeSubEnv
        [ ( "+"
          , Gen.return
              (scalar
                 (Expr.BuiltInFunction
                    { func = Add
                    ; type' =
                        Type.Func { parameters = [ intArr; intArr ]; return = intArr }
                    })) )
        ; ( "-"
          , Gen.return
              (scalar
                 (Expr.BuiltInFunction
                    { func = Sub
                    ; type' =
                        Type.Func { parameters = [ intArr; intArr ]; return = intArr }
                    })) )
        ; ( "*"
          , Gen.return
              (scalar
                 (Expr.BuiltInFunction
                    { func = Mul
                    ; type' =
                        Type.Func { parameters = [ intArr; intArr ]; return = intArr }
                    })) )
        ; ( "/"
          , Gen.return
              (scalar
                 (Expr.BuiltInFunction
                    { func = Div
                    ; type' =
                        Type.Func { parameters = [ intArr; intArr ]; return = intArr }
                    })) )
        ; ( "length"
          , let%map t = Gen.createId "t"
            and d = Gen.createId "d"
            and s = Gen.createId "@s" in
            let type' =
              Type.Pi
                { parameters =
                    [ { binding = d; bound = Sort.Dim }
                    ; { binding = s; bound = Sort.Shape }
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
                                              [ Add (Index.dimensionRef d); ShapeRef s ]
                                          }
                                      ]
                                  ; return = intArr
                                  })
                         })
                }
            in
            scalar (Expr.BuiltInFunction { func = Length; type' }) )
        ]
    in
    { sorts; kinds; types }
  ;;
end
