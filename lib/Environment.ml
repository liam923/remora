open! Base
open Ast

type 'v entry =
  { e : 'v
  ; id : Typed.Identifier.t
  }

type t =
  { sorts : Sort.t entry Map.M(String).t
  ; kinds : Kind.t entry Map.M(String).t
  ; types : Typed.Type.array entry Map.M(String).t
  ; literalType : Typed.Expr.literalValue -> Typed.Type.atom
  }

module type IdentifierGenerator = sig
  include StateT.S2

  type state

  val createId : string -> (state, Typed.Identifier.t, 'e) t
end

module Base (Gen : IdentifierGenerator) = struct
  let make () =
    let open Gen.Let_syntax in
    let makeSubEnv namesAndValues =
      let%map alist =
        namesAndValues
        |> List.map ~f:(function name, value ->
               let%map id = Gen.createId name
               and value = value in
               name, { id; e = value })
        |> Gen.all
      in
      Map.of_alist_exn (module String) alist
    in
    let%bind sorts = makeSubEnv [] in
    let%bind kinds =
      makeSubEnv [ "int", Gen.return Kind.Atom; "char", Gen.return Kind.Atom ]
    in
    let scalar element = Typed.Type.Arr { element; shape = [] } in
    let intRef = Typed.Type.AtomRef (Map.find_exn kinds "int").id in
    let%map types =
      makeSubEnv
        Ast.Typed.
          [ ( "+"
            , Gen.return
                (scalar
                   (Type.Func
                      { parameters = [ scalar intRef; scalar intRef ]
                      ; return = scalar intRef
                      })) )
          ; ( "-"
            , Gen.return
                (scalar
                   (Type.Func
                      { parameters = [ scalar intRef; scalar intRef ]
                      ; return = scalar intRef
                      })) )
          ; ( "*"
            , Gen.return
                (scalar
                   (Type.Func
                      { parameters = [ scalar intRef; scalar intRef ]
                      ; return = scalar intRef
                      })) )
          ; ( "/"
            , Gen.return
                (scalar
                   (Type.Func
                      { parameters = [ scalar intRef; scalar intRef ]
                      ; return = scalar intRef
                      })) )
          ; ( "length"
            , let%map t = Gen.createId "t"
              and d = Gen.createId "d"
              and s = Gen.createId "@s" in
              scalar
                (Type.Pi
                   { parameters =
                       [ { binding = d; bound = Sort.Dim }
                       ; { binding = s; bound = Sort.Shape }
                       ]
                   ; body =
                       scalar
                         (Type.Forall
                            { parameters = [ { binding = t; bound = Kind.Atom } ]
                            ; body =
                                scalar
                                  (Type.Func
                                     { parameters =
                                         [ Arr
                                             { element = AtomRef t
                                             ; shape =
                                                 [ Add (Index.dimensionRef d)
                                                 ; ShapeRef s
                                                 ]
                                             }
                                         ]
                                     ; return = scalar intRef
                                     })
                            })
                   }) )
          ]
    in
    let literalType = function
      | Typed.Expr.IntLiteral _ -> Typed.Type.AtomRef (Map.find_exn kinds "int").id
      | Typed.Expr.CharacterLiteral _ -> Typed.Type.AtomRef (Map.find_exn kinds "char").id
    in
    { sorts; kinds; types; literalType }
  ;;
end
