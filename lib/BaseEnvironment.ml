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
    ; userVisible : bool
        (** if false, the value is only available in definitions of other environment entries and not in user programs *)
    ; value : entryValue
    }

  let entries : entry list =
    [ { name = "+"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Add; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "-"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Sub; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "*"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Mul; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "/"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Div; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "%"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Mod; type' })
            ; type' = "(-> (int int) int)"
            }
      }
    ; { name = "+."
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func AddF; type' })
            ; type' = "(-> (float float) float)"
            }
      }
    ; { name = "-."
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func SubF; type' })
            ; type' = "(-> (float float) float)"
            }
      }
    ; { name = "*."
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func MulF; type' })
            ; type' = "(-> (float float) float)"
            }
      }
    ; { name = "/."
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func DivF; type' })
            ; type' = "(-> (float float) float)"
            }
      }
    ; { name = "="
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Equal; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = "!="
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Ne; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = ">"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Gt; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = ">="
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func GtEq; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = "<"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Lt; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = "<="
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func LtEq; type' })
            ; type' = "(-> (int int) bool)"
            }
      }
    ; { name = ">."
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func GtF; type' })
            ; type' = "(-> (float float) bool)"
            }
      }
    ; { name = ">=."
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func GtEqF; type' })
            ; type' = "(-> (float float) bool)"
            }
      }
    ; { name = "<."
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func LtF; type' })
            ; type' = "(-> (float float) bool)"
            }
      }
    ; { name = "<=."
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func LtEqF; type' })
            ; type' = "(-> (float float) bool)"
            }
      }
    ; { name = "and"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func And; type' })
            ; type' = "(-> (bool bool) bool)"
            }
      }
    ; { name = "or"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Or; type' })
            ; type' = "(-> (bool bool) bool)"
            }
      }
    ; { name = "not"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func Not; type' })
            ; type' = "(-> (bool) bool)"
            }
      }
    ; { name = "int->float"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func IntToFloat; type' })
            ; type' = "(-> (int) float)"
            }
      }
    ; { name = "float->int"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func FloatToInt; type' })
            ; type' = "(-> (float) int)"
            }
      }
    ; { name = "int->bool"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func IntToBool; type' })
            ; type' = "(-> (int) bool)"
            }
      }
    ; { name = "bool->int"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func BoolToInt; type' })
            ; type' = "(-> (bool) int)"
            }
      }
    ; { name = "if"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Func If; type' })
            ; type' = "(Forall (@t) (-> (bool @t @t) @t))"
            }
      }
    ; { name = "length"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (d @cell-shape) (t-fn (t) (fn ([arr [t d @cell-shape]])
              (reify-dimension d))))
            |}
      }
    ; { name = "iota"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue = (fun type' -> Expr.Primitive { name = Val Iota; type' })
            ; type' = {| (Pi (@s) [int @s]) |}
            }
      }
    ; { name = "replicate"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (@s @cell-shape)
              (t-fn (t)
                (fn ([v [t @cell-shape]])
                  (define (make [foo int] [v [t @cell-shape]])
                    v)
                  (make iota{ | [@s]} v)
                )))
            |}
      }
    ; { name = "contiguous-subarray"
      ; userVisible = false
      ; value =
          Intrinsic
            { makeValue =
                (fun type' -> Expr.Primitive { name = Func ContiguousSubArray; type' })
            ; type' =
                {|
                (Pi (@original-shape @result-shape @cell-shape l)
                  (Forall (t)
                    (-> ([t @original-shape @cell-shape]
                          [int l])
                        [t @result-shape @cell-shape])))
                |}
            }
      }
    ; { name = "index"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (@s @cell-shape l)
              (t-fn (t)
                (fn ([arr [t @s @cell-shape]] [index [int l]])
                  (contiguous-subarray{t | @s [] @cell-shape l} arr index))))
            |}
      }
    ; { name = "head"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (d-1 @cell-shape)
              (t-fn (t)
                (fn ([arr [t (+ d-1 1) @cell-shape]])
                  (contiguous-subarray{t | [(+ d-1 1)] [] @cell-shape 1} arr [0]))))
            |}
      }
    ; { name = "tail"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (d-1 @cell-shape)
              (t-fn (t)
                (fn ([arr [t (+ d-1 1) @cell-shape]])
                  (contiguous-subarray{t | [(+ d-1 1)] [d-1] @cell-shape 1} arr [1]))))
            |}
      }
    ; { name = "subvector"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (d-in d-out @cell-shape)
              (t-fn (t)
                (fn ([arr [t d-in @cell-shape]] [i int])
                  (contiguous-subarray{t | [d-in] [d-out] @cell-shape 1} arr [i]))))
            |}
      }
    ; { name = "append"
      ; userVisible = true
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
    ; { name = "scatter"
      ; userVisible = true
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
    ; { name = "reverse"
      ; userVisible = true
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
    ; { name = "reduce-init-no-padding"
      ; userVisible = false
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive { name = Func (Reduce { character = Reduce }); type' })
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
    ; { name = "reduce-init"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (d @item-pad @cell-shape)
              (t-fn (t)
                (fn ([f (-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])]
                      [init [t @cell-shape]]
                      [arr [t d @item-pad @cell-shape]])
                  (define (up-ranked-f [a [t @item-pad @cell-shape]]
                                        [b [t @item-pad @cell-shape]])
                    (f a b))
                  (reduce-init-no-padding{t | d (++ @item-pad @cell-shape)}
                    up-ranked-f
                    (replicate{t | @item-pad @cell-shape} init)
                    arr))))
            |}
      }
    ; { name = "reduce"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (d-1 @item-pad @cell-shape)
              (t-fn (t)
                (fn ([op (-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])]
                     [arr [t (+ d-1 1) @item-pad @cell-shape]])
                  (reduce-init{t | d-1 @item-pad @cell-shape}
                    op
                    (head{t | d-1 (++ @item-pad @cell-shape)} arr)
                    (tail{t | d-1 (++ @item-pad @cell-shape)} arr)))))
            |}
      }
    ; { name = "scan-init-no-padding"
      ; userVisible = false
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive { name = Func (Reduce { character = Scan }); type' })
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
    ; { name = "scan-init"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (d @item-pad @cell-shape)
              (t-fn (t)
                (fn ([f (-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])]
                      [init [t @cell-shape]]
                      [arr [t d @item-pad @cell-shape]])
                  (define (up-ranked-f [a [t @item-pad @cell-shape]]
                                       [b [t @item-pad @cell-shape]])
                    (f a b))
                  (scan-init-no-padding{t | d (++ @item-pad @cell-shape)}
                    up-ranked-f
                    (replicate{t | @item-pad @cell-shape} init)
                    arr))))
            |}
      }
    ; { name = "iscan"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (d-1 @item-pad @cell-shape)
              (t-fn (t)
                (fn ([op (-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])]
                     [arr [t (+ d-1 1) @item-pad @cell-shape]])
                  (scan-init{t | d-1 @item-pad @cell-shape}
                    op
                    (head{t | d-1 (++ @item-pad @cell-shape)} arr)
                    (tail{t | d-1 (++ @item-pad @cell-shape)} arr)))))
            |}
      }
    ; { name = "iscan-init"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (d @item-pad @cell-shape)
              (t-fn (t)
                (fn ([op (-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])]
                     [init [t @cell-shape]]
                     [arr [t d @item-pad @cell-shape]])
                  (tail{t | d (++ @item-pad @cell-shape)} (scan-init{t | d @item-pad @cell-shape}
                                                            op
                                                            init
                                                            arr)))))
            |}
      }
    ; { name = "open-scan-init"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (d @item-pad @cell-shape)
              (t-fn (t)
                (fn ([op (-> ([t @cell-shape] [t @cell-shape]) [t @cell-shape])]
                     [init [t @cell-shape]]
                     [arr [t d @item-pad @cell-shape]])
                  (subvector{t | (+ d 1) d (++ @item-pad @cell-shape)}
                    (scan-init{t | d @item-pad @cell-shape} op
                                                            init
                                                            arr)
                    0))))
            |}
      }
    ; { name = "fold"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive
                    { name = Func (Fold { character = Fold; reverse = false }); type' })
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
    ; { name = "fold-right"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive
                    { name = Func (Fold { character = Fold; reverse = true }); type' })
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
    ; { name = "trace"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive
                    { name = Func (Fold { character = Trace; reverse = false }); type' })
            ; type' =
                {|
                (Pi (d @t-cell-shape @u-cell-shape)
                  (Forall (t u)
                    (-> ((-> ([u @u-cell-shape] [t @t-cell-shape]) [u @u-cell-shape])
                          [u @u-cell-shape]
                          [t d @t-cell-shape])
                        [u (+ d 1) @u-cell-shape])))
                |}
            }
      }
    ; { name = "trace-right"
      ; userVisible = true
      ; value =
          Intrinsic
            { makeValue =
                (fun type' ->
                  Expr.Primitive
                    { name = Func (Fold { character = Trace; reverse = true }); type' })
            ; type' =
                {|
                (Pi (d @t-cell-shape @u-cell-shape)
                  (Forall (t u)
                    (-> ((-> ([u @u-cell-shape] [t @t-cell-shape]) [u @u-cell-shape])
                          [u @u-cell-shape]
                          [t d @t-cell-shape])
                        [u (+ d 1) @u-cell-shape])))
                |}
            }
      }
    ; { name = "filter"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (l @cell-shape)
              (t-fn (t)
                (fn ([arr [t l @cell-shape]] [flags [bool l]])
                  (define locs-raw (- (iscan-init{int | l [] []} + 0 (bool->int flags)) 1))
                  (define locs (if{int | } flags locs-raw (replicate{int | [l] []} -1)))
                  (define result-size (+ 1 (index{int | [l] [] 1} locs-raw [(- (reify-dimension l) 1)])))
                  (lift [d result-size]
                    (scatter{t | l d @cell-shape} arr locs)))))
            |}
      }
    ; { name = "sin"
      ; userVisible = true
      ; value =
          LibraryFunction
            { libName = "std::sin"; argTypes = [ "float" ]; retType = "float" }
      }
    ; { name = "cos"
      ; userVisible = true
      ; value =
          LibraryFunction
            { libName = "std::cos"; argTypes = [ "float" ]; retType = "float" }
      }
    ; { name = "tan"
      ; userVisible = true
      ; value =
          LibraryFunction
            { libName = "std::tan"; argTypes = [ "float" ]; retType = "float" }
      }
    ; { name = "arcsin"
      ; userVisible = true
      ; value =
          LibraryFunction
            { libName = "std::asin"; argTypes = [ "float" ]; retType = "float" }
      }
    ; { name = "arccos"
      ; userVisible = true
      ; value =
          LibraryFunction
            { libName = "std::acos"; argTypes = [ "float" ]; retType = "float" }
      }
    ; { name = "arctan"
      ; userVisible = true
      ; value =
          LibraryFunction
            { libName = "std::atan"; argTypes = [ "float" ]; retType = "float" }
      }
    ; { name = "abs"
      ; userVisible = true
      ; value = Expression {| (fn ([x int]) (if{int | } (>= x 0) x (* x -1))) |}
      }
    ; { name = "abs."
      ; userVisible = true
      ; value = Expression {| (fn ([x float]) (if{float | } (>=. x 0.) x (*. x -1.))) |}
      }
    ; { name = "max"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (l-1)
              (fn ([arr [int (+ l-1 1)]])
                (reduce{int | l-1 [] []}
                  (fn ([a int] [b int])
                    (if{int | } (> a b) a b))
                  arr)))
            |}
      }
    ; { name = "max."
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (l-1)
              (fn ([arr [float (+ l-1 1)]])
                (reduce{float | l-1 [] []}
                  (fn ([a float] [b float])
                    (if{float | } (>. a b) a b))
                  arr)))
            |}
      }
    ; { name = "min"
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (l-1)
              (fn ([arr [int (+ l-1 1)]])
                (reduce{int | l-1 [] []}
                  (fn ([a int] [b int])
                    (if{int | } (< a b) a b))
                  arr)))
            |}
      }
    ; { name = "min."
      ; userVisible = true
      ; value =
          Expression
            {|
            (i-fn (l-1)
              (fn ([arr [float (+ l-1 1)]])
                (reduce{float | l-1 [] []}
                  (fn ([a float] [b float])
                    (if{float | } (<. a b) a b))
                  arr)))
            |}
      }
    ]
  ;;

  let make () =
    let open CompilerState.Let_syntax in
    let%bind baseEnv = Base.make () in
    let%bind extendedEnv =
      List.fold
        entries
        ~init:(return baseEnv)
        ~f:(fun env { name; userVisible = _; value } ->
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
    let userEnv =
      List.fold
        entries
        ~init:extendedEnv
        ~f:(fun currEnv { name; userVisible; value = _ } ->
          if userVisible
          then currEnv
          else { currEnv with types = Map.remove currEnv.types name })
    in
    return userEnv
  ;;
end
