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
        ; "float", return (Type.Atom (Literal FloatLiteral))
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
    | LibraryFunction of
        { libName : string
        ; argTypes : string list
        ; retType : string
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
            { makeValue = (fun type' -> Expr.Primitive { name = Func Add; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "-"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Sub; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "*"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Mul; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "/"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Div; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "%"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Mod; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "+."
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func AddF; type' })
            ; type' = "(-> (float float) float)"
            }
      }
    ; { name = "-."
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func SubF; type' })
            ; type' = "(-> (float float) float)"
            }
      }
    ; { name = "*."
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func MulF; type' })
            ; type' = "(-> (float float) float)"
            }
      }
    ; { name = "/."
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func DivF; type' })
            ; type' = "(-> (float float) float)"
            }
      }
    ; { name = "="
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Equal; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = "!="
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Ne; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = ">"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Gt; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = ">="
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func GtEq; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = "<"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Lt; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = "<="
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func LtEq; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = ">."
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func GtF; type' })
            ; type' = "(-> (float float) bool)"
            }
      }
    ; { name = ">=."
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func GtEqF; type' })
            ; type' = "(-> (float float) bool)"
            }
      }
    ; { name = "<."
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func LtF; type' })
            ; type' = "(-> (float float) bool)"
            }
      }
    ; { name = "<=."
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func LtEqF; type' })
            ; type' = "(-> (float float) bool)"
            }
      }
    ; { name = "and"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func And; type' })
            ; type' = "(-> (bool bool) bool)"
            }
      }
    ; { name = "or"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Or; type' })
            ; type' = "(-> (bool bool) bool)"
            }
      }
    ; { name = "not"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Not; type' })
            ; type' = "(-> (bool) bool)"
            }
      }
    ; { name = "int->float"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func IntToFloat; type' })
            ; type' = "(-> (int) float)"
            }
      }
    ; { name = "float->int"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func FloatToInt; type' })
            ; type' = "(-> (float) int)"
            }
      }
    ; { name = "int->bool"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func IntToBool; type' })
            ; type' = "(-> (int) bool)"
            }
      }
    ; { name = "bool->int"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func BoolToInt; type' })
            ; type' = "(-> (bool) int)"
            }
      }
    ; { name = "if"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func If; type' })
            ; type' = "(Forall (@t) (-> (bool @t @t) @t))"
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
                    { name =
                        Func
                          (Reduce
                             { associative = true
                             ; explicitZero = false
                             ; character = Reduce
                             })
                    ; type'
                    })
            ; type' =
                {|
                (Pi (d-1 @cell-shape)
                  (Forall (t)
                    (-> ((-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])
                         [t (+ d-1 1) @cell-shape])
                        [t @cell-shape])))
                |}
            }
      }
    ; { name = "reduce-zero"
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive
                    { name =
                        Func
                          (Reduce
                             { associative = true
                             ; explicitZero = true
                             ; character = Reduce
                             })
                    ; type'
                    })
            ; type' =
                {|
                (Pi (d @cell-shape)
                  (Forall (t)
                    (-> ((-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])
                         [t @cell-shape]
                         [t d @cell-shape])
                        [t @cell-shape])))
                |}
            }
      }
    ; { name = "reduce-non-assoc"
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive
                    { name =
                        Func
                          (Reduce
                             { associative = false
                             ; explicitZero = false
                             ; character = Reduce
                             })
                    ; type'
                    })
            ; type' =
                {|
                  (Pi (d-1 @cell-shape)
                    (Forall (t)
                      (-> ((-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])
                           [t (+ d-1 1) @cell-shape])
                          [t @cell-shape])))
                  |}
            }
      }
    ; { name = "scan"
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive
                    { name =
                        Func
                          (Reduce
                             { associative = true
                             ; explicitZero = false
                             ; character = Scan
                             })
                    ; type'
                    })
            ; type' =
                {|
                (Pi (d-1 @cell-shape)
                  (Forall (t)
                    (-> ((-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])
                         [t (+ d-1 1) @cell-shape])
                        [t (+ d-1 1) @cell-shape])))
                |}
            }
      }
    ; { name = "scan-zero"
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive
                    { name =
                        Func
                          (Reduce
                             { associative = true; explicitZero = true; character = Scan })
                    ; type'
                    })
            ; type' =
                {|
                (Pi (d @cell-shape)
                  (Forall (t)
                    (-> ((-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])
                         [t @cell-shape]
                         [t d @cell-shape])
                        [t d @cell-shape])))
                |}
            }
      }
    ; { name = "open-scan"
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive
                    { name =
                        Func
                          (Reduce
                             { associative = true
                             ; explicitZero = false
                             ; character = OpenScan
                             })
                    ; type'
                    })
            ; type' =
                {|
                (Pi (d-1 @cell-shape)
                  (Forall (t)
                    (-> ((-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])
                         [t (+ d-1 1) @cell-shape])
                        [t (+ d-1 2) @cell-shape])))
                |}
            }
      }
    ; { name = "open-scan-zero"
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive
                    { name =
                        Func
                          (Reduce
                             { associative = true
                             ; explicitZero = true
                             ; character = OpenScan
                             })
                    ; type'
                    })
            ; type' =
                {|
                (Pi (d @cell-shape)
                  (Forall (t)
                    (-> ((-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])
                         [t @cell-shape]
                         [t d @cell-shape])
                        [t (+ d 1) @cell-shape])))
                |}
            }
      }
    ; { name = "fold"
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive { name = Func (Fold { character = Fold }); type' })
            ; type' =
                {|
                (Pi (d @cell-shape)
                  (Forall (t @u)
                    (-> ((-> (@u [t @cell-shape]) @u)
                         @u
                         [t d @cell-shape])
                        @u)))
                |}
            }
      }
    ; { name = "append"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Append; type' })
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
    ; { name = "iota"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Val Iota; type' })
            ; type' = {| (Pi (@s) [int @s]) |}
            }
      }
    ; { name = "index"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Index; type' })
            ; type' =
                {|
                (Pi (@s @cell-shape l)
                  (Forall (t)
                    (-> ([t @s @cell-shape]
                         [int l])
                        [t @cell-shape])))
                |}
            }
      }
    ; { name = "scatter"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Scatter; type' })
            ; type' =
                {|
                  (Pi (d-in d-out @cell-shape)
                    (Forall (t)
                      (-> ([t d-in @cell-shape]
                           [int d-in])
                          [t [d-out @cell-shape]])))
                  |}
            }
      }
    ; { name = "replicate"
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Replicate; type' })
            ; type' =
                {|
                  (Pi (@s @cell-shape)
                    (Forall (t)
                      (-> ([t [@cell-shape]])
                          [t [@s @cell-shape]])))
                  |}
            }
      }
    ; { name = "filter"
      ; value =
          Expression
            {|
            (i-fn (l @cell-shape)
              (t-fn (t)
                (fn ([arr [t l @cell-shape]] [flags [bool l]])
                  (define locs-raw (scan-zero{int | l []} + 0 (bool->int flags)))
                  (define locs (if{int | } flags locs-raw (replicate{int | [l] []} -1)))
                  (define result-size (index{int | [l] [] 1} locs-raw [(reify-dimension l)]))
                  (lift [d result-size]
                    (scatter{t | l d @cell-shape} arr locs)))))
            |}
      }
    ; { name = "reverse"
      ; value =
          Expression
            {|
            (i-fn (l @cell-shape)
              (t-fn (t)
                (fn ([arr [t l @cell-shape]])
                  (define indices (- (- (reify-dimension l) 1) iota{ | [l]}))
                  (scatter{t | l l @cell-shape} arr indices))))
            |}
      }
    ; { name = "sin"
      ; value =
          LibraryFunction
            { libName = "std::sin"; argTypes = [ "float" ]; retType = "float" }
      }
    ; { name = "cos"
      ; value =
          LibraryFunction
            { libName = "std::cos"; argTypes = [ "float" ]; retType = "float" }
      }
    ; { name = "tan"
      ; value =
          LibraryFunction
            { libName = "std::tan"; argTypes = [ "float" ]; retType = "float" }
      }
    ; { name = "arcsin"
      ; value =
          LibraryFunction
            { libName = "std::asin"; argTypes = [ "float" ]; retType = "float" }
      }
    ; { name = "arccos"
      ; value =
          LibraryFunction
            { libName = "std::acos"; argTypes = [ "float" ]; retType = "float" }
      }
    ; { name = "arctan"
      ; value =
          LibraryFunction
            { libName = "std::atan"; argTypes = [ "float" ]; retType = "float" }
      }
    ; { name = "abs"
      ; value = Expression {| (fn ([x int]) (if{int | } (>= x 0) x (* x -1))) |}
      }
    ; { name = "abs."
      ; value = Expression {| (fn ([x float]) (if{float | } (>=. x 0.) x (*. x -1.))) |}
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
          | LibraryFunction { libName; argTypes; retType } ->
            let%bind parsedArgTypes =
              CompilerState.return
                (argTypes
                 |> List.map ~f:Parse.Default.TypeParser.parseString
                 |> MResult.all
                 |> MResult.assertNoErrors ~f:(fun (err, _) ->
                   [%string "Parsing error when making %{name} in stdlib: %{err}"]))
            and parsedRetType =
              CompilerState.return
                (MResult.assertNoErrors
                   (Parse.Default.TypeParser.parseString retType)
                   ~f:(fun (err, _) ->
                     [%string "Parsing error when making %{name} in stdlib: %{err}"]))
            in
            let%map argTypes =
              CompilerState.make ~f:(fun state ->
                let check =
                  parsedArgTypes
                  |> List.map ~f:(TypeCheck.Kind.checkAndExpectArray ~env)
                  |> CompilerState.all
                in
                let result = CompilerState.run check state in
                MResult.assertNoErrors result ~f:(fun err ->
                  let errMsg = TypeCheck.errorMessage err.elem in
                  [%string "Type error when making %{name} in stdlib: %{errMsg}"]))
            and retType =
              CompilerState.make ~f:(fun state ->
                let check = TypeCheck.Kind.checkAndExpectArray ~env parsedRetType in
                let result = CompilerState.run check state in
                MResult.assertNoErrors result ~f:(fun err ->
                  let errMsg = TypeCheck.errorMessage err.elem in
                  [%string "Type error when making %{name} in stdlib: %{errMsg}"]))
            in
            Expr.Primitive
              { name = Func (LibFun { name; libName; argTypes; retType })
              ; type' =
                  Arr
                    { element = Func { parameters = argTypes; return = retType }
                    ; shape = []
                    }
              }
        in
        let typesEnv = Map.set typesEnv ~key:name ~data:value in
        { env with types = typesEnv })
    in
    extendedEnv
  ;;
end
