open! Base
open Remora

let%expect_test "parse index" =
  let parseAndPrint str =
    match Parse.Unit.IndexParser.parseString str with
    | MOk result ->
      [%sexp_of: unit Ast.Index.t] result |> Sexp.to_string_hum |> Stdio.print_endline
    | Errors errs ->
      let errsSexp = [%sexp_of: (string * unit) list] (NeList.to_list errs) in
      let errsStr = Sexp.to_string_hum errsSexp in
      Error.raise (Error.of_string [%string "Got errors: `%{errsStr}` parsing `%{str}`"])
  in
  parseAndPrint "a";
  [%expect {| (Ref a) |}];
  parseAndPrint "[1 2 3]";
  [%expect {| (Slice ((Dimension 1) (Dimension 2) (Dimension 3))) |}];
  parseAndPrint "[1 2 [3 4 5]]";
  [%expect
    {|
    (Slice
     ((Dimension 1) (Dimension 2)
      (Slice ((Dimension 3) (Dimension 4) (Dimension 5))))) |}];
  parseAndPrint "[a b c]";
  [%expect {| (Slice ((Ref a) (Ref b) (Ref c))) |}];
  parseAndPrint "(+ a b)";
  [%expect {| (Add ((Ref a) (Ref b))) |}];
  parseAndPrint "(++ a b)";
  [%expect {| (Append ((Ref a) (Ref b))) |}];
  parseAndPrint "(shape a b)";
  [%expect {| (Shape ((Ref a) (Ref b))) |}];
  parseAndPrint "(shape a @c [1 2 3 (++ 4 5)] (+ 4))";
  [%expect
    {|
    (Shape
     ((Ref a) (Ref @c)
      (Slice
       ((Dimension 1) (Dimension 2) (Dimension 3)
        (Append ((Dimension 4) (Dimension 5)))))
      (Add ((Dimension 4))))) |}]
;;

let%expect_test "parse type" =
  let parseAndPrint str =
    match Parse.Unit.TypeParser.parseString str with
    | MOk result ->
      [%sexp_of: unit Ast.Type.t] result |> Sexp.to_string_hum |> Stdio.print_endline
    | Errors errs ->
      let errsSexp = [%sexp_of: (string * unit) list] (NeList.to_list errs) in
      let errsStr = Sexp.to_string_hum errsSexp in
      Error.raise (Error.of_string [%string "Got errors: `%{errsStr}` parsing `%{str}`"])
  in
  parseAndPrint "a";
  [%expect {| (Ref a) |}];
  parseAndPrint "int";
  [%expect {| (Ref int) |}];
  parseAndPrint "char";
  [%expect {| (Ref char) |}];
  parseAndPrint "(-> (int) int)";
  [%expect {| (Func ((parameters ((Ref int))) (return (Ref int)))) |}];
  parseAndPrint "(-> (int int) int)";
  [%expect {| (Func ((parameters ((Ref int) (Ref int))) (return (Ref int)))) |}];
  parseAndPrint "(-> () int)";
  [%expect {| (Func ((parameters ()) (return (Ref int)))) |}];
  parseAndPrint "(→ (int) int)";
  [%expect {| (Func ((parameters ((Ref int))) (return (Ref int)))) |}];
  parseAndPrint "(Arr int [1 2 3])";
  [%expect
    {|
    (Arr
     ((element (Ref int))
      (shape (Slice ((Dimension 1) (Dimension 2) (Dimension 3)))))) |}];
  parseAndPrint "[int 1 2 3]";
  [%expect
    {|
    (Arr
     ((element (Ref int))
      (shape (Slice ((Dimension 1) (Dimension 2) (Dimension 3)))))) |}];
  parseAndPrint "[(-> (int) [char 5]) 1 2 3]";
  [%expect
    {|
    (Arr
     ((element
       (Func
        ((parameters ((Ref int)))
         (return (Arr ((element (Ref char)) (shape (Slice ((Dimension 5))))))))))
      (shape (Slice ((Dimension 1) (Dimension 2) (Dimension 3)))))) |}];
  parseAndPrint "(Forall (a @b c @d) (-> (a @b c) @d))";
  [%expect
    {|
    (Forall
     ((parameters
       (((binding a) (bound Atom)) ((binding @b) (bound Array))
        ((binding c) (bound Atom)) ((binding @d) (bound Array))))
      (body (Func ((parameters ((Ref a) (Ref @b) (Ref c))) (return (Ref @d))))))) |}];
  parseAndPrint "(∀ (a @b c @d) [a 1 2 3])";
  [%expect
    {|
    (Forall
     ((parameters
       (((binding a) (bound Atom)) ((binding @b) (bound Array))
        ((binding c) (bound Atom)) ((binding @d) (bound Array))))
      (body
       (Arr
        ((element (Ref a))
         (shape (Slice ((Dimension 1) (Dimension 2) (Dimension 3))))))))) |}];
  parseAndPrint "(Pi (a @b c @d) (-> (a @b c) @d))";
  [%expect
    {|
    (Pi
     ((parameters
       (((binding a) (bound Dim)) ((binding @b) (bound Shape))
        ((binding c) (bound Dim)) ((binding @d) (bound Shape))))
      (body (Func ((parameters ((Ref a) (Ref @b) (Ref c))) (return (Ref @d))))))) |}];
  parseAndPrint "(Π (a @b c @d) [a 1 2 3])";
  [%expect
    {|
    (Pi
     ((parameters
       (((binding a) (bound Dim)) ((binding @b) (bound Shape))
        ((binding c) (bound Dim)) ((binding @d) (bound Shape))))
      (body
       (Arr
        ((element (Ref a))
         (shape (Slice ((Dimension 1) (Dimension 2) (Dimension 3))))))))) |}];
  parseAndPrint "(Sigma (a @b c @d) (-> (a @b c) @d))";
  [%expect
    {|
    (Sigma
     ((parameters
       (((binding a) (bound Dim)) ((binding @b) (bound Shape))
        ((binding c) (bound Dim)) ((binding @d) (bound Shape))))
      (body (Func ((parameters ((Ref a) (Ref @b) (Ref c))) (return (Ref @d))))))) |}];
  parseAndPrint "(Σ (a @b c @d) [a 1 2 3])";
  [%expect
    {|
    (Sigma
     ((parameters
       (((binding a) (bound Dim)) ((binding @b) (bound Shape))
        ((binding c) (bound Dim)) ((binding @d) (bound Shape))))
      (body
       (Arr
        ((element (Ref a))
         (shape (Slice ((Dimension 1) (Dimension 2) (Dimension 3))))))))) |}]
;;

let%expect_test "parse expression" =
  let parseAndPrint str =
    match Parse.Unit.ExprParser.parseString str with
    | MOk result ->
      [%sexp_of: unit Ast.Expr.t] result |> Sexp.to_string_hum |> Stdio.print_endline
    | Errors errs ->
      let errsSexp = [%sexp_of: (string * unit) list] (NeList.to_list errs) in
      let errsStr = Sexp.to_string_hum errsSexp in
      Error.raise (Error.of_string [%string "Got errors: `%{errsStr}` parsing `%{str}`"])
  in
  parseAndPrint "a";
  [%expect {| (Ref a) |}];
  parseAndPrint "(+ a b)";
  [%expect {| (TermApplication ((func (Ref +)) (args ((Ref a) (Ref b))))) |}];
  parseAndPrint "123";
  [%expect {| (IntLiteral 123) |}];
  parseAndPrint {| "hello world" |};
  [%expect
    {|
    (Arr
     ((dimensions (11))
      (elements
       ((CharacterLiteral h) (CharacterLiteral e) (CharacterLiteral l)
        (CharacterLiteral l) (CharacterLiteral o) (CharacterLiteral " ")
        (CharacterLiteral w) (CharacterLiteral o) (CharacterLiteral r)
        (CharacterLiteral l) (CharacterLiteral d))))) |}];
  parseAndPrint "[#false #f #true #t]";
  [%expect
    {|
    (Frame
     ((dimensions (4))
      (elements
       ((BooleanLiteral false) (BooleanLiteral false) (BooleanLiteral true)
        (BooleanLiteral true))))) |}];
  parseAndPrint "[1 2 3 4 5]";
  [%expect
    {|
    (Frame
     ((dimensions (5))
      (elements
       ((IntLiteral 1) (IntLiteral 2) (IntLiteral 3) (IntLiteral 4)
        (IntLiteral 5))))) |}];
  parseAndPrint "[[1 2] [3 4]]";
  [%expect
    {|
    (Frame
     ((dimensions (2))
      (elements
       ((Frame ((dimensions (2)) (elements ((IntLiteral 1) (IntLiteral 2)))))
        (Frame ((dimensions (2)) (elements ((IntLiteral 3) (IntLiteral 4))))))))) |}];
  parseAndPrint "(array [2 2] 1 2 3 4)";
  [%expect
    {|
    (Arr
     ((dimensions (2 2))
      (elements ((IntLiteral 1) (IntLiteral 2) (IntLiteral 3) (IntLiteral 4))))) |}];
  parseAndPrint "(array [2 0 2] int)";
  [%expect {| (EmptyArr ((dimensions (2 0 2)) (elementType (Ref int)))) |}];
  parseAndPrint "(frame [2 2] 1 2 3 4)";
  [%expect
    {|
    (Frame
     ((dimensions (2 2))
      (elements ((IntLiteral 1) (IntLiteral 2) (IntLiteral 3) (IntLiteral 4))))) |}];
  parseAndPrint "(frame [2 0 2] int)";
  [%expect {| (EmptyFrame ((dimensions (2 0 2)) (elementType (Ref int)))) |}];
  parseAndPrint "(let [x 5] x)";
  [%expect
    {|
    (Let
     ((param ((binding x) (bound ()))) (value (IntLiteral 5)) (body (Ref x)))) |}];
  parseAndPrint "(let [x : int 5] x)";
  [%expect
    {|
    (Let
     ((param ((binding x) (bound ((Ref int))))) (value (IntLiteral 5))
      (body (Ref x)))) |}];
  parseAndPrint "(define foo 5) foo";
  [%expect
    {|
    (Let
     ((param ((binding foo) (bound ()))) (value (IntLiteral 5)) (body (Ref foo)))) |}];
  parseAndPrint "(define foo 5) (define bar (+ 1 foo)) [foo bar]";
  [%expect
    {|
    (Let
     ((param ((binding foo) (bound ()))) (value (IntLiteral 5))
      (body
       (Let
        ((param ((binding bar) (bound ())))
         (value
          (TermApplication ((func (Ref +)) (args ((IntLiteral 1) (Ref foo))))))
         (body (Frame ((dimensions (2)) (elements ((Ref foo) (Ref bar))))))))))) |}];
  parseAndPrint "(define (foo [x int] [y int]) (+ x y)) (foo 10 15)";
  [%expect
    {|
    (Let
     ((param ((binding foo) (bound ())))
      (value
       (TermLambda
        ((params
          (((binding x) (bound (Ref int))) ((binding y) (bound (Ref int)))))
         (body (TermApplication ((func (Ref +)) (args ((Ref x) (Ref y)))))))))
      (body
       (TermApplication
        ((func (Ref foo)) (args ((IntLiteral 10) (IntLiteral 15)))))))) |}];
  parseAndPrint "(define (foo [x int] [y int]) (define z (+ x y)) z) (foo 10 15)";
  [%expect
    {|
    (Let
     ((param ((binding foo) (bound ())))
      (value
       (TermLambda
        ((params
          (((binding x) (bound (Ref int))) ((binding y) (bound (Ref int)))))
         (body
          (Let
           ((param ((binding z) (bound ())))
            (value (TermApplication ((func (Ref +)) (args ((Ref x) (Ref y))))))
            (body (Ref z))))))))
      (body
       (TermApplication
        ((func (Ref foo)) (args ((IntLiteral 10) (IntLiteral 15)))))))) |}];
  parseAndPrint
    "(define (foo [x [int 4 5]] [y [int 2]]) (define z : int (+ x y)) z) (foo 10 15)";
  [%expect
    {|
    (Let
     ((param ((binding foo) (bound ())))
      (value
       (TermLambda
        ((params
          (((binding x)
            (bound
             (Arr
              ((element (Ref int)) (shape (Slice ((Dimension 4) (Dimension 5))))))))
           ((binding y)
            (bound (Arr ((element (Ref int)) (shape (Slice ((Dimension 2))))))))))
         (body
          (Let
           ((param ((binding z) (bound ((Ref int)))))
            (value (TermApplication ((func (Ref +)) (args ((Ref x) (Ref y))))))
            (body (Ref z))))))))
      (body
       (TermApplication
        ((func (Ref foo)) (args ((IntLiteral 10) (IntLiteral 15)))))))) |}];
  parseAndPrint "(λ ([x int] [y [int 5 6]]) (+ x y))";
  [%expect
    {|
    (TermLambda
     ((params
       (((binding x) (bound (Ref int)))
        ((binding y)
         (bound
          (Arr
           ((element (Ref int)) (shape (Slice ((Dimension 5) (Dimension 6))))))))))
      (body (TermApplication ((func (Ref +)) (args ((Ref x) (Ref y)))))))) |}];
  parseAndPrint "(fn ([x int] [y [int 5 6]]) (define foo (+ x y)) foo)";
  [%expect
    {|
    (TermLambda
     ((params
       (((binding x) (bound (Ref int)))
        ((binding y)
         (bound
          (Arr
           ((element (Ref int)) (shape (Slice ((Dimension 5) (Dimension 6))))))))))
      (body
       (Let
        ((param ((binding foo) (bound ())))
         (value (TermApplication ((func (Ref +)) (args ((Ref x) (Ref y))))))
         (body (Ref foo))))))) |}];
  parseAndPrint "(define (foo{@t|} [x @t]) x) foo";
  [%expect
    {|
    (Let
     ((param ((binding foo) (bound ())))
      (value
       (TypeLambda
        ((params (((binding @t) (bound Array))))
         (body
          (TermLambda ((params (((binding x) (bound (Ref @t))))) (body (Ref x))))))))
      (body (Ref foo)))) |}];
  parseAndPrint "(define (foo{t|} [x t]) x) foo";
  [%expect
    {|
    (Let
     ((param ((binding foo) (bound ())))
      (value
       (TypeLambda
        ((params (((binding t) (bound Atom))))
         (body
          (TermLambda ((params (((binding x) (bound (Ref t))))) (body (Ref x))))))))
      (body (Ref foo)))) |}];
  parseAndPrint "(define (foo{|@i} [x [int @i]]) x) foo";
  [%expect
    {|
    (Let
     ((param ((binding foo) (bound ())))
      (value
       (IndexLambda
        ((params (((binding @i) (bound Shape))))
         (body
          (TermLambda
           ((params
             (((binding x)
               (bound (Arr ((element (Ref int)) (shape (Slice ((Ref @i))))))))))
            (body (Ref x))))))))
      (body (Ref foo)))) |}];
  parseAndPrint "(define (foo{|i} [x [int i]]) x) foo";
  [%expect
    {|
    (Let
     ((param ((binding foo) (bound ())))
      (value
       (IndexLambda
        ((params (((binding i) (bound Dim))))
         (body
          (TermLambda
           ((params
             (((binding x)
               (bound (Arr ((element (Ref int)) (shape (Slice ((Ref i))))))))))
            (body (Ref x))))))))
      (body (Ref foo)))) |}];
  parseAndPrint "(define (foo{@t t|@i i} [x @t] [y [t i @i]]) x) foo";
  [%expect
    {|
    (Let
     ((param ((binding foo) (bound ())))
      (value
       (TypeLambda
        ((params (((binding @t) (bound Array)) ((binding t) (bound Atom))))
         (body
          (IndexLambda
           ((params (((binding @i) (bound Shape)) ((binding i) (bound Dim))))
            (body
             (TermLambda
              ((params
                (((binding x) (bound (Ref @t)))
                 ((binding y)
                  (bound
                   (Arr ((element (Ref t)) (shape (Slice ((Ref i) (Ref @i))))))))))
               (body (Ref x)))))))))))
      (body (Ref foo)))) |}];
  parseAndPrint "(Tλ (@t) (λ ([x @t]) x))";
  [%expect
    {|
    (TypeLambda
     ((params (((binding @t) (bound Array))))
      (body
       (TermLambda ((params (((binding x) (bound (Ref @t))))) (body (Ref x))))))) |}];
  parseAndPrint "(t-fn (t) (λ ([x t]) x))";
  [%expect
    {|
    (TypeLambda
     ((params (((binding t) (bound Atom))))
      (body
       (TermLambda ((params (((binding x) (bound (Ref t))))) (body (Ref x))))))) |}];
  parseAndPrint "(Iλ (@i) (λ ([x [int i]]) x))";
  [%expect
    {|
    (IndexLambda
     ((params (((binding @i) (bound Shape))))
      (body
       (TermLambda
        ((params
          (((binding x)
            (bound (Arr ((element (Ref int)) (shape (Slice ((Ref i))))))))))
         (body (Ref x))))))) |}];
  parseAndPrint "(i-fn (i) (λ ([x [int i]]) x))";
  [%expect
    {|
    (IndexLambda
     ((params (((binding i) (bound Dim))))
      (body
       (TermLambda
        ((params
          (((binding x)
            (bound (Arr ((element (Ref int)) (shape (Slice ((Ref i))))))))))
         (body (Ref x))))))) |}];
  parseAndPrint "(t-app (t-fn (@t) @t) int)";
  [%expect
    {|
    (TypeApplication
     ((tFunc
       (TypeLambda ((params (((binding @t) (bound Array)))) (body (Ref @t)))))
      (args ((Ref int)))))|}];
  parseAndPrint "(t-fn (@t) @t){int|}";
  [%expect
    {|
    (TypeApplication
     ((tFunc
       (TypeLambda ((params (((binding @t) (bound Array)))) (body (Ref @t)))))
      (args ((Ref int)))))|}];
  parseAndPrint "(t-app (t-fn (@t u) @t) int char)";
  [%expect
    {|
    (TypeApplication
     ((tFunc
       (TypeLambda
        ((params (((binding @t) (bound Array)) ((binding u) (bound Atom))))
         (body (Ref @t)))))
      (args ((Ref int) (Ref char)))))|}];
  parseAndPrint "(t-fn (@t u) @t){int char|}";
  [%expect
    {|
    (TypeApplication
     ((tFunc
       (TypeLambda
        ((params (((binding @t) (bound Array)) ((binding u) (bound Atom))))
         (body (Ref @t)))))
      (args ((Ref int) (Ref char)))))|}];
  parseAndPrint "(t-app (t-fn () int))";
  [%expect
    {|
    (TypeApplication
     ((tFunc (TypeLambda ((params ()) (body (Ref int))))) (args ())))|}];
  parseAndPrint "(i-app (i-fn (@t) [int @t]) [1 2])";
  [%expect
    {|
    (IndexApplication
     ((iFunc
       (IndexLambda
        ((params (((binding @t) (bound Shape))))
         (body (Frame ((dimensions (2)) (elements ((Ref int) (Ref @t)))))))))
      (args ((Slice ((Dimension 1) (Dimension 2)))))))|}];
  parseAndPrint "(i-fn (@t) [int @t]){|[1 2]}";
  [%expect
    {|
    (IndexApplication
     ((iFunc
       (IndexLambda
        ((params (((binding @t) (bound Shape))))
         (body (Frame ((dimensions (2)) (elements ((Ref int) (Ref @t)))))))))
      (args ((Slice ((Dimension 1) (Dimension 2))))))) |}];
  parseAndPrint "(i-app (i-fn (@t u) [int @t u]) [1 2] 1)";
  [%expect
    {|
    (IndexApplication
     ((iFunc
       (IndexLambda
        ((params (((binding @t) (bound Shape)) ((binding u) (bound Dim))))
         (body
          (Frame ((dimensions (3)) (elements ((Ref int) (Ref @t) (Ref u)))))))))
      (args ((Slice ((Dimension 1) (Dimension 2))) (Dimension 1)))))|}];
  parseAndPrint "(i-fn (@t u) [int @t u]){|[1 2] 1}";
  [%expect
    {|
    (IndexApplication
     ((iFunc
       (IndexLambda
        ((params (((binding @t) (bound Shape)) ((binding u) (bound Dim))))
         (body
          (Frame ((dimensions (3)) (elements ((Ref int) (Ref @t) (Ref u)))))))))
      (args ((Slice ((Dimension 1) (Dimension 2))) (Dimension 1))))) |}];
  parseAndPrint {| reduce{int | 1 [] []} |};
  [%expect
    {|
    (TypeApplication
     ((tFunc
       (IndexApplication
        ((iFunc (Ref reduce)) (args ((Dimension 1) (Slice ()) (Slice ()))))))
      (args ((Ref int))))) |}];
  parseAndPrint
    {|
  (boxes (len) [char len] [5]
    ((6) "Monday" )
    ((7) "Tuesday" )
    ((9) "Wednesday")
    ((8) "Thursday" )
    ((6) "Friday" ))
  |};
  [%expect
    {|
    (Boxes
     ((params (((binding len) (bound Dim))))
      (elementType (Arr ((element (Ref char)) (shape (Slice ((Ref len)))))))
      (dimensions (5))
      (elements
       (((indices ((Dimension 6)))
         (body
          (Arr
           ((dimensions (6))
            (elements
             ((CharacterLiteral M) (CharacterLiteral o) (CharacterLiteral n)
              (CharacterLiteral d) (CharacterLiteral a) (CharacterLiteral y)))))))
        ((indices ((Dimension 7)))
         (body
          (Arr
           ((dimensions (7))
            (elements
             ((CharacterLiteral T) (CharacterLiteral u) (CharacterLiteral e)
              (CharacterLiteral s) (CharacterLiteral d) (CharacterLiteral a)
              (CharacterLiteral y)))))))
        ((indices ((Dimension 9)))
         (body
          (Arr
           ((dimensions (9))
            (elements
             ((CharacterLiteral W) (CharacterLiteral e) (CharacterLiteral d)
              (CharacterLiteral n) (CharacterLiteral e) (CharacterLiteral s)
              (CharacterLiteral d) (CharacterLiteral a) (CharacterLiteral y)))))))
        ((indices ((Dimension 8)))
         (body
          (Arr
           ((dimensions (8))
            (elements
             ((CharacterLiteral T) (CharacterLiteral h) (CharacterLiteral u)
              (CharacterLiteral r) (CharacterLiteral s) (CharacterLiteral d)
              (CharacterLiteral a) (CharacterLiteral y)))))))
        ((indices ((Dimension 6)))
         (body
          (Arr
           ((dimensions (6))
            (elements
             ((CharacterLiteral F) (CharacterLiteral r) (CharacterLiteral i)
              (CharacterLiteral d) (CharacterLiteral a) (CharacterLiteral y)))))))))))|}];
  parseAndPrint
    {|
  (boxes (r c) [int r c] [3]
    ((2 2) [[1 2] [3 4]])
    ((1 3) [[10 100 1000]])
    ((3 1) [[10] [100] [1000]]))
  |};
  [%expect
    {|
    (Boxes
     ((params (((binding r) (bound Dim)) ((binding c) (bound Dim))))
      (elementType (Arr ((element (Ref int)) (shape (Slice ((Ref r) (Ref c)))))))
      (dimensions (3))
      (elements
       (((indices ((Dimension 2) (Dimension 2)))
         (body
          (Frame
           ((dimensions (2))
            (elements
             ((Frame
               ((dimensions (2)) (elements ((IntLiteral 1) (IntLiteral 2)))))
              (Frame
               ((dimensions (2)) (elements ((IntLiteral 3) (IntLiteral 4)))))))))))
        ((indices ((Dimension 1) (Dimension 3)))
         (body
          (Frame
           ((dimensions (1))
            (elements
             ((Frame
               ((dimensions (3))
                (elements ((IntLiteral 10) (IntLiteral 100) (IntLiteral 1000)))))))))))
        ((indices ((Dimension 3) (Dimension 1)))
         (body
          (Frame
           ((dimensions (3))
            (elements
             ((Frame ((dimensions (1)) (elements ((IntLiteral 10)))))
              (Frame ((dimensions (1)) (elements ((IntLiteral 100)))))
              (Frame ((dimensions (1)) (elements ((IntLiteral 1000)))))))))))))))|}];
  parseAndPrint "(box ((len 3)) [int len] [8 23 0])";
  [%expect
    {|
    (Boxes
     ((params (((binding len) (bound Dim))))
      (elementType (Arr ((element (Ref int)) (shape (Slice ((Ref len)))))))
      (dimensions ())
      (elements
       (((indices ((Dimension 3)))
         (body
          (Frame
           ((dimensions (3))
            (elements ((IntLiteral 8) (IntLiteral 23) (IntLiteral 0)))))))))))|}];
  parseAndPrint
    {|
    (unbox weekdays (day len)
      (= 6 ((t-app (i-app length len []) char) day)))
    |};
  [%expect
    {|
    (Unbox
     ((indexBindings (((binding len) (bound (Dim))))) (valueBinding day)
      (box (Ref weekdays))
      (body
       (TermApplication
        ((func (Ref =))
         (args
          ((IntLiteral 6)
           (TermApplication
            ((func
              (TypeApplication
               ((tFunc
                 (IndexApplication
                  ((iFunc (Ref length)) (args ((Ref len) (Slice ()))))))
                (args ((Ref char))))))
             (args ((Ref day))))))))))))|}]
;;
