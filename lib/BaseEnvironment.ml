open! Base
open Typed

module type S = sig
  val make : unit -> (CompilerState.state, Environment.t, _) CompilerState.t
end

module Base : S = struct
  let make () =
    let open CompilerState.Let_syntax in
    let makeSubEnv namesAndEntries =
      let%map alist =
        namesAndEntries
        |> List.map ~f:(function name, value ->
          let%map value = value in
          name, value)
        |> CompilerState.all
      in
      Map.of_alist_exn (module String) alist
    in
    let%bind sorts = makeSubEnv [] in
    let%bind kinds =
      makeSubEnv
        [ "int", return (Type.Atom (Literal IntLiteral))
        ; "char", return (Type.Atom (Literal CharacterLiteral))
        ; "bool", return (Type.Atom (Literal BooleanLiteral))
        ]
    in
    let%map types = makeSubEnv [] in
    Environment.{ sorts; kinds; types }
  ;;
end

module Stdlib : S = struct
  type entryValue =
    | Intrinsic of
        { makeValue : Type.array -> Expr.array
        ; type' : string
        }
    | Expression of string

  type entry =
    { name : string
    ; value : entryValue
    }

  let entries : entry list =
    [ { name = "+"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { func = Add; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "-"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { func = Sub; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "*"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { func = Mul; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "/"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { func = Div; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "="
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { func = Equal; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = "length"
      ; value =
          Expression
            {|
            (i-fn (d @cell-shape) (t-fn (t) (fn ([arr [t d @cell-shape]])
              (reify-dimension d))))
            |}
      }
    ; { name = "reduce"
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive
                    { func =
                        Reduce
                          { associative = true
                          ; explicitZero = false
                          ; character = `Reduce
                          }
                    ; type'
                    })
            ; type' =
                {|
                (Pi (d-1 @item-pad @cell-shape)
                  (Forall (t)
                    (-> ((-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])
                         [t (+ d-1 1) @item-pad @cell-shape])
                        [t @item-pad @cell-shape])))
                |}
            }
      }
    ; { name = "scan"
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive
                    { func =
                        Reduce
                          { associative = true; explicitZero = false; character = `Scan }
                    ; type'
                    })
            ; type' =
                {|
                (Pi (d-1 @item-pad @cell-shape)
                  (Forall (t)
                    (-> ((-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])
                         [t (+ d-1 1) @item-pad @cell-shape])
                        [t (+ d-1 1) @item-pad @cell-shape])))
                |}
            }
      }
    ; { name = "fold"
      ; value =
          Intrinsic
            { makeValue =
                (fun type' -> Expr.Primitive { func = Fold { character = `Fold }; type' })
            ; type' =
                {|
                (Pi (d @item-pad @cell-shape)
                  (Forall (t @u)
                    (-> ((-> (@u [t @cell-shape]) @u)
                         [t d @item-pad @cell-shape])
                        @u)))
                |}
            }
      }
    ; { name = "filter"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { func = Filter; type' })
            ; type' =
                {|
                (Pi (d @cell-shape)
                  (Forall (t)
                    (-> ([t d @cell-shape]
                         [bool d @cell-shape])
                        (Sigma (d-out) [t d-out @cell-shape]))))
                |}
            }
      }
    ; { name = "append"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { func = Append; type' })
            ; type' =
                {|
                (Pi (d1 d2 @cell-shape)
                  (Forall (t)
                    (-> ([t d1 @cell-shape]
                         [t d2 @cell-shape])
                        [t (+ d1 d2) @cell-shape])))
                |}
            }
      }
    ]
  ;;

  let make () =
    let open CompilerState.Let_syntax in
    let%bind baseEnv = Base.make () in
    let%map extendedEnv =
      List.fold entries ~init:(return baseEnv) ~f:(fun env { name; value } ->
        let%bind env = env in
        let typesEnv = env.types in
        let%map value =
          match value with
          | Expression expr ->
            let%bind parsed =
              CompilerState.return
                (MResult.assertNoErrors
                   (Parse.Default.ExprParser.parseString expr)
                   ~f:(fun (err, _) ->
                     [%string "Parsing error when making %{name} in stdlib: %{err}"]))
            in
            CompilerState.make ~f:(fun state ->
              let check = TypeCheck.Type.checkAndExpectArray ~env parsed in
              let result = CompilerState.run check state in
              MResult.assertNoErrors result ~f:(fun err ->
                let errMsg = TypeCheck.errorMessage err.elem in
                [%string "Type error when making %{name} in stdlib: %{errMsg}"]))
          | Intrinsic { makeValue; type' } ->
            let%bind parsed =
              CompilerState.return
                (MResult.assertNoErrors
                   (Parse.Default.TypeParser.parseString type')
                   ~f:(fun (err, _) ->
                     [%string "Parsing error when making %{name} in stdlib: %{err}"]))
            in
            let%map type' =
              CompilerState.make ~f:(fun state ->
                let check = TypeCheck.Kind.checkAndExpectArray ~env parsed in
                let result = CompilerState.run check state in
                MResult.assertNoErrors result ~f:(fun err ->
                  let errMsg = TypeCheck.errorMessage err.elem in
                  [%string "Type error when making %{name} in stdlib: %{errMsg}"]))
            in
            makeValue type'
        in
        let typesEnv = Map.set typesEnv ~key:name ~data:value in
        { env with types = typesEnv })
    in
    extendedEnv
  ;;
end
