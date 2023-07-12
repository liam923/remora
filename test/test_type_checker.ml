open! Base
open Remora

let%expect_test "check sort" =
  let checkAndPrint str =
    match Parser.Unit.IndexParser.parseString str with
    | MOk index ->
      (match TypeChecker.checkSort index with
      | MOk indexTyped ->
        [%sexp_of: Core.Index.t] indexTyped |> Sexp.to_string_hum |> Stdio.print_endline
      | Errors errs ->
        NeList.iter errs ~f:(fun err ->
            Stdio.prerr_endline [%string "Error: %{TypeChecker.errorMessage err.elem}"]))
    | Errors errs ->
      let errsSexp = [%sexp_of: (string * unit) list] (NeList.to_list errs) in
      let errsStr = Sexp.to_string_hum errsSexp in
      Error.raise (Error.of_string [%string "Got errors: `%{errsStr}` parsing `%{str}`"])
  in
  checkAndPrint {| 5 |};
  [%expect {| (Dimension ((const 5) (refs ()))) |}];
  checkAndPrint {| (+ 5 10) |};
  [%expect {| (Dimension ((const 15) (refs ()))) |}];
  checkAndPrint {| (shape 5 10 15) |};
  [%expect
    {|
    (Shape
     ((Add ((const 5) (refs ()))) (Add ((const 10) (refs ())))
      (Add ((const 15) (refs ()))))) |}];
  checkAndPrint {| [5 10 15] |};
  [%expect
    {|
    (Shape
     ((Add ((const 5) (refs ()))) (Add ((const 10) (refs ())))
      (Add ((const 15) (refs ()))))) |}];
  checkAndPrint {| (++ [1 2] [3 4]) |};
  [%expect
    {|
    (Shape
     ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
      (Add ((const 3) (refs ()))) (Add ((const 4) (refs ()))))) |}];
  checkAndPrint {| (++ 1 2 3) |};
  [%expect
    {|
    Error: Unexpected sort: expected `Dim`, got `Shape`
    Error: Unexpected sort: expected `Dim`, got `Shape`
    Error: Unexpected sort: expected `Dim`, got `Shape` |}];
  checkAndPrint {| x |};
  [%expect {| Error: Unbound index variable `x` |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect {| Error: Unexpected sort: expected `Dim`, got `Shape` |}]
;;

let%expect_test "check kind" =
  let checkAndPrint str =
    match Parser.Unit.TypeParser.parseString str with
    | MOk type' ->
      (match TypeChecker.checkKind type' with
      | MOk typeTyped ->
        [%sexp_of: Core.Type.t] typeTyped |> Sexp.to_string_hum |> Stdio.print_endline
      | Errors errs ->
        NeList.iter errs ~f:(fun err ->
            Stdio.prerr_endline [%string "Error: %{TypeChecker.errorMessage err.elem}"]))
    | Errors errs ->
      let errsSexp = [%sexp_of: (string * unit) list] (NeList.to_list errs) in
      let errsStr = Sexp.to_string_hum errsSexp in
      Error.raise (Error.of_string [%string "Got errors: `%{errsStr}` parsing `%{str}`"])
  in
  checkAndPrint {| int |};
  [%expect {| (Atom (AtomRef ((name int) (id 1)))) |}];
  checkAndPrint {| char |};
  [%expect {| (Atom (AtomRef ((name char) (id 2)))) |}];
  checkAndPrint {| foo |};
  [%expect {| Error: Unbound type variable `foo` |}];
  checkAndPrint {| (Arr int [1 2 3]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape
        ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
         (Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| (-> (int int) int) |};
  [%expect
    {|
    (Atom
     (Func
      ((parameters
        ((Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))
         (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))))
       (return (Arr ((element (AtomRef ((name int) (id 1)))) (shape ()))))))) |}];
  checkAndPrint {| (Forall (@t) @t) |};
  [%expect
    {|
    (Atom
     (Forall
      ((parameters (((binding ((name @t) (id 11))) (bound Array))))
       (body (ArrayRef ((name @t) (id 11))))))) |}];
  checkAndPrint {| (Arr (Forall (@t) @t) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Forall
         ((parameters (((binding ((name @t) (id 11))) (bound Array))))
          (body (ArrayRef ((name @t) (id 11)))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Forall (t) t) |};
  [%expect
    {|
    (Atom
     (Forall
      ((parameters (((binding ((name t) (id 11))) (bound Atom))))
       (body (Arr ((element (AtomRef ((name t) (id 11)))) (shape ()))))))) |}];
  checkAndPrint {| (Pi (@i) (Arr int @i)) |};
  [%expect
    {|
    (Atom
     (Pi
      ((parameters (((binding ((name @i) (id 11))) (bound Shape))))
       (body
        (Arr
         ((element (AtomRef ((name int) (id 1))))
          (shape ((ShapeRef ((name @i) (id 11))))))))))) |}];
  checkAndPrint {| (Arr (Pi (@i) [int @i]) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Pi
         ((parameters (((binding ((name @i) (id 11))) (bound Shape))))
          (body
           (Arr
            ((element (AtomRef ((name int) (id 1))))
             (shape ((ShapeRef ((name @i) (id 11)))))))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Pi (i) [int i i]) |};
  [%expect
    {|
    (Atom
     (Pi
      ((parameters (((binding ((name i) (id 11))) (bound Dim))))
       (body
        (Arr
         ((element (AtomRef ((name int) (id 1))))
          (shape
           ((Add ((const 0) (refs ((((name i) (id 11)) 1)))))
            (Add ((const 0) (refs ((((name i) (id 11)) 1))))))))))))) |}];
  checkAndPrint {| (Sigma (@i) (Arr int @i)) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters (((binding ((name @i) (id 11))) (bound Shape))))
       (body
        (Arr
         ((element (AtomRef ((name int) (id 1))))
          (shape ((ShapeRef ((name @i) (id 11))))))))))) |}];
  checkAndPrint {| (Arr (Sigma (@i) [int @i]) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding ((name @i) (id 11))) (bound Shape))))
          (body
           (Arr
            ((element (AtomRef ((name int) (id 1))))
             (shape ((ShapeRef ((name @i) (id 11)))))))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Sigma (i) [int i i]) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters (((binding ((name i) (id 11))) (bound Dim))))
       (body
        (Arr
         ((element (AtomRef ((name int) (id 1))))
          (shape
           ((Add ((const 0) (refs ((((name i) (id 11)) 1)))))
            (Add ((const 0) (refs ((((name i) (id 11)) 1))))))))))))) |}];
  checkAndPrint {| (Sigma (@i j) (Arr int [j @i 5 (+ j 10)])) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters
        (((binding ((name @i) (id 11))) (bound Shape))
         ((binding ((name j) (id 12))) (bound Dim))))
       (body
        (Arr
         ((element (AtomRef ((name int) (id 1))))
          (shape
           ((Add ((const 0) (refs ((((name j) (id 12)) 1)))))
            (ShapeRef ((name @i) (id 11))) (Add ((const 5) (refs ())))
            (Add ((const 10) (refs ((((name j) (id 12)) 1))))))))))))) |}];
  checkAndPrint {| (Tuple int char) |};
  [%expect
    {| (Atom (Tuple ((AtomRef ((name int) (id 1))) (AtomRef ((name char) (id 2)))))) |}]
;;

let%expect_test "check type" =
  let checkAndPrint str =
    match Parser.Unit.parseString str with
    | MOk expr ->
      (match TypeChecker.checkType expr with
      | MOk exprTyped ->
        [%sexp_of: Core.Expr.t] exprTyped |> Sexp.to_string_hum |> Stdio.print_endline;
        [%sexp_of: Core.Type.t] (Core.Expr.type' exprTyped)
        |> Sexp.to_string_hum
        |> fun typeStr -> [%string "Type: %{typeStr}"] |> Stdio.print_endline
      | Errors errs ->
        NeList.iter errs ~f:(fun err ->
            Stdio.prerr_endline [%string "Error: %{TypeChecker.errorMessage err.elem}"]))
    | Errors errs ->
      let errsSexp = [%sexp_of: (string * unit) list] (NeList.to_list errs) in
      let errsStr = Sexp.to_string_hum errsSexp in
      Error.raise (Error.of_string [%string "Got errors: `%{errsStr}` parsing `%{str}`"])
  in
  checkAndPrint {| 5 |};
  [%expect
    {|
    (Atom (Literal ((value (IntLiteral 5)))))
    Type: (Atom (AtomRef ((name int) (id 1)))) |}];
  checkAndPrint {| [1 2 3 4 5] |};
  [%expect
    {|
    (Array
     (Arr
      ((dimensions (5))
       (elements
        ((Literal ((value (IntLiteral 1)))) (Literal ((value (IntLiteral 2))))
         (Literal ((value (IntLiteral 3)))) (Literal ((value (IntLiteral 4))))
         (Literal ((value (IntLiteral 5)))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 5) (refs ())))))))) |}];
  checkAndPrint {| [[1 2] [3 4]] |};
  [%expect
    {|
    (Array
     (Arr
      ((dimensions (2 2))
       (elements
        ((Literal ((value (IntLiteral 1)))) (Literal ((value (IntLiteral 2))))
         (Literal ((value (IntLiteral 3)))) (Literal ((value (IntLiteral 4)))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| [[1 2 3] [4 5]] |};
  [%expect
    {| Error: Mismatched frame arrays; first array is type `[int 3]`, got `[int 2]` |}];
  checkAndPrint {| [[1 2 3] [4 5] [6]] |};
  [%expect
    {|
    Error: Mismatched frame arrays; first array is type `[int 3]`, got `[int 2]`
    Error: Mismatched frame arrays; first array is type `[int 3]`, got `[int 1]` |}];
  checkAndPrint {| [[1 2] "hi"] |};
  [%expect
    {| Error: Mismatched frame arrays; first array is type `[int 2]`, got `[char 2]` |}];
  checkAndPrint {| (+ 1 2) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func (Ref ((id ((name +) (id 3))))))
       (args
        ((Arr ((dimensions ()) (elements ((Literal ((value (IntLiteral 1))))))))
         (Arr ((dimensions ()) (elements ((Literal ((value (IntLiteral 2)))))))))))))
    Type: (Array (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))) |}];
  checkAndPrint {| (+ [1 2 3] [4 5 6]) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func (Ref ((id ((name +) (id 3))))))
       (args
        ((Arr
          ((dimensions (3))
           (elements
            ((Literal ((value (IntLiteral 1))))
             (Literal ((value (IntLiteral 2))))
             (Literal ((value (IntLiteral 3))))))))
         (Arr
          ((dimensions (3))
           (elements
            ((Literal ((value (IntLiteral 4))))
             (Literal ((value (IntLiteral 5))))
             (Literal ((value (IntLiteral 6)))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| [(+ [1 2 3] [4 5 6]) [7 8 9]] |};
  [%expect
    {|
    (Array
     (Frame
      ((dimensions (2))
       (arrays
        ((TermApplication
          ((func (Ref ((id ((name +) (id 3))))))
           (args
            ((Arr
              ((dimensions (3))
               (elements
                ((Literal ((value (IntLiteral 1))))
                 (Literal ((value (IntLiteral 2))))
                 (Literal ((value (IntLiteral 3))))))))
             (Arr
              ((dimensions (3))
               (elements
                ((Literal ((value (IntLiteral 4))))
                 (Literal ((value (IntLiteral 5))))
                 (Literal ((value (IntLiteral 6))))))))))))
         (Arr
          ((dimensions (3))
           (elements
            ((Literal ((value (IntLiteral 7))))
             (Literal ((value (IntLiteral 8))))
             (Literal ((value (IntLiteral 9)))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| (+ [1 2 3 4 5] 6) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func (Ref ((id ((name +) (id 3))))))
       (args
        ((Arr
          ((dimensions (5))
           (elements
            ((Literal ((value (IntLiteral 1))))
             (Literal ((value (IntLiteral 2))))
             (Literal ((value (IntLiteral 3))))
             (Literal ((value (IntLiteral 4))))
             (Literal ((value (IntLiteral 5))))))))
         (Arr ((dimensions ()) (elements ((Literal ((value (IntLiteral 6)))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 5) (refs ())))))))) |}];
  checkAndPrint {| (+ [1 2 3] [1 2]) |};
  [%expect {|
    Error: Function call has principal frame `[3]`, got frame `[2]` |}];
  checkAndPrint {| (+ [1 2] "hi") |};
  [%expect
    {|
    Error: Function expected argument with element type `int`, got `char` |}];
  checkAndPrint {|
  (define foo 5)
  foo
  |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name foo) (id 11)))
       (value
        (Arr ((dimensions ()) (elements ((Literal ((value (IntLiteral 5)))))))))
       (body (Ref ((id ((name foo) (id 11)))))))))
    Type: (Array (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))) |}];
  checkAndPrint {|
  (define (add [x int] [y int])
    (+ x y))
  (add 1 2)
  |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name add) (id 11)))
       (value
        (Arr
         ((dimensions ())
          (elements
           ((TermLambda
             ((params
               (((binding ((name x) (id 12)))
                 (bound
                  (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))))
                ((binding ((name y) (id 13)))
                 (bound
                  (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))))))
              (body
               (Array
                (TermApplication
                 ((func (Ref ((id ((name +) (id 3))))))
                  (args
                   ((Ref ((id ((name x) (id 12)))))
                    (Ref ((id ((name y) (id 13))))))))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name add) (id 11))))))
          (args
           ((Arr
             ((dimensions ()) (elements ((Literal ((value (IntLiteral 1))))))))
            (Arr
             ((dimensions ()) (elements ((Literal ((value (IntLiteral 2))))))))))))))))
    Type: (Array (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))) |}];
  checkAndPrint {| (fn ([x int]) x) |};
  [%expect
    {|
    (Atom
     (TermLambda
      ((params
        (((binding ((name x) (id 11)))
          (bound (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))))))
       (body (Array (Ref ((id ((name x) (id 11))))))))))
    Type: (Atom
     (Func
      ((parameters ((Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))))
       (return (Arr ((element (AtomRef ((name int) (id 1)))) (shape ()))))))) |}];
  checkAndPrint {| (fn ([x int]) "hello") |};
  [%expect
    {|
    (Atom
     (TermLambda
      ((params
        (((binding ((name x) (id 11)))
          (bound (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))))))
       (body
        (Array
         (Arr
          ((dimensions (5))
           (elements
            ((Literal ((value (CharacterLiteral h))))
             (Literal ((value (CharacterLiteral e))))
             (Literal ((value (CharacterLiteral l))))
             (Literal ((value (CharacterLiteral l))))
             (Literal ((value (CharacterLiteral o)))))))))))))
    Type: (Atom
     (Func
      ((parameters ((Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))))
       (return
        (Arr
         ((element (AtomRef ((name char) (id 2))))
          (shape ((Add ((const 5) (refs ()))))))))))) |}];
  checkAndPrint
    {|
    (define id (t-fn (@t)
      (fn ([x @t]) x)))
    ((t-app id int) 5)
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name id) (id 11)))
       (value
        (Arr
         ((dimensions ())
          (elements
           ((TypeLambda
             ((params (((binding ((name @t) (id 12))) (bound Array))))
              (body
               (Arr
                ((dimensions ())
                 (elements
                  ((TermLambda
                    ((params
                      (((binding ((name x) (id 13)))
                        (bound (ArrayRef ((name @t) (id 12)))))))
                     (body (Array (Ref ((id ((name x) (id 13)))))))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id ((name id) (id 11))))))
             (args
              ((Array (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))))))))
          (args
           ((Arr
             ((dimensions ()) (elements ((Literal ((value (IntLiteral 5))))))))))))))))
    Type: (Array (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))) |}];
  checkAndPrint
    {|
    (define id (t-fn (t)
      (fn ([x [t 2]]) x)))
    ((t-app id int) [1 2])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name id) (id 11)))
       (value
        (Arr
         ((dimensions ())
          (elements
           ((TypeLambda
             ((params (((binding ((name t) (id 12))) (bound Atom))))
              (body
               (Arr
                ((dimensions ())
                 (elements
                  ((TermLambda
                    ((params
                      (((binding ((name x) (id 13)))
                        (bound
                         (Arr
                          ((element (AtomRef ((name t) (id 12))))
                           (shape ((Add ((const 2) (refs ())))))))))))
                     (body (Array (Ref ((id ((name x) (id 13)))))))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id ((name id) (id 11))))))
             (args ((Atom (AtomRef ((name int) (id 1)))))))))
          (args
           ((Arr
             ((dimensions (2))
              (elements
               ((Literal ((value (IntLiteral 1))))
                (Literal ((value (IntLiteral 2))))))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint
    {|
    (define id (t-fn (@t)
      (fn ([x @t]) x)))
    ((t-app id [char 5]) "hello")
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name id) (id 11)))
       (value
        (Arr
         ((dimensions ())
          (elements
           ((TypeLambda
             ((params (((binding ((name @t) (id 12))) (bound Array))))
              (body
               (Arr
                ((dimensions ())
                 (elements
                  ((TermLambda
                    ((params
                      (((binding ((name x) (id 13)))
                        (bound (ArrayRef ((name @t) (id 12)))))))
                     (body (Array (Ref ((id ((name x) (id 13)))))))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id ((name id) (id 11))))))
             (args
              ((Array
                (Arr
                 ((element (AtomRef ((name char) (id 2))))
                  (shape ((Add ((const 5) (refs ())))))))))))))
          (args
           ((Arr
             ((dimensions (5))
              (elements
               ((Literal ((value (CharacterLiteral h))))
                (Literal ((value (CharacterLiteral e))))
                (Literal ((value (CharacterLiteral l))))
                (Literal ((value (CharacterLiteral l))))
                (Literal ((value (CharacterLiteral o))))))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name char) (id 2))))
       (shape ((Add ((const 5) (refs ())))))))) |}];
  checkAndPrint
    {|
    (define id (t-fn (@t)
      (fn ([x @t]) x)))
    ((t-app id [char 5]) "hello")
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name id) (id 11)))
       (value
        (Arr
         ((dimensions ())
          (elements
           ((TypeLambda
             ((params (((binding ((name @t) (id 12))) (bound Array))))
              (body
               (Arr
                ((dimensions ())
                 (elements
                  ((TermLambda
                    ((params
                      (((binding ((name x) (id 13)))
                        (bound (ArrayRef ((name @t) (id 12)))))))
                     (body (Array (Ref ((id ((name x) (id 13)))))))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id ((name id) (id 11))))))
             (args
              ((Array
                (Arr
                 ((element (AtomRef ((name char) (id 2))))
                  (shape ((Add ((const 5) (refs ())))))))))))))
          (args
           ((Arr
             ((dimensions (5))
              (elements
               ((Literal ((value (CharacterLiteral h))))
                (Literal ((value (CharacterLiteral e))))
                (Literal ((value (CharacterLiteral l))))
                (Literal ((value (CharacterLiteral l))))
                (Literal ((value (CharacterLiteral o))))))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name char) (id 2))))
       (shape ((Add ((const 5) (refs ())))))))) |}];
  checkAndPrint
    {|
    (define id (t-fn (t)
      (fn ([x t]) x)))
    ((t-app id [char 5]) "hello")
    |};
  [%expect {| Error: Unexpected sort: expected `Atom`, got `Array` |}];
  checkAndPrint
    {|
    (define foo (i-fn (@i)
      (fn ([x [int 1 @i]]) x)))
    ((i-app foo [2]) [[1 2]])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name foo) (id 11)))
       (value
        (Arr
         ((dimensions ())
          (elements
           ((IndexLambda
             ((params (((binding ((name @i) (id 12))) (bound Shape))))
              (body
               (Arr
                ((dimensions ())
                 (elements
                  ((TermLambda
                    ((params
                      (((binding ((name x) (id 13)))
                        (bound
                         (Arr
                          ((element (AtomRef ((name int) (id 1))))
                           (shape
                            ((Add ((const 1) (refs ())))
                             (ShapeRef ((name @i) (id 12)))))))))))
                     (body (Array (Ref ((id ((name x) (id 13)))))))))))))))))))))
       (body
        (TermApplication
         ((func
           (IndexApplication
            ((iFunc (Ref ((id ((name foo) (id 11))))))
             (args ((Shape ((Add ((const 2) (refs ()))))))))))
          (args
           ((Arr
             ((dimensions (1 2))
              (elements
               ((Literal ((value (IntLiteral 1))))
                (Literal ((value (IntLiteral 2))))))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint
    {|
    (define foo (i-fn (@i)
      (fn ([x [int 1 @i]]) x)))
    ((i-app foo 2) [[1 2]])
    |};
  [%expect {| Error: Unexpected sort: expected `Dim`, got `Shape` |}];
  checkAndPrint
    {|
    (define foo (i-fn (@i)
      (fn ([x [int 1 @i]]) x)))
    ((i-app foo [2]) [[1 2] [3 4]])
    |};
  [%expect
    {|
    Error: Function expected argument with cell shape `[1 2]`, got `[2 2]` |}];
  checkAndPrint {| (define x : int 5) x |};
  checkAndPrint
    {|
    (define foo (i-fn (i)
      (fn ([x [int 1 i]]) x)))
    ((i-app foo 2) [[1 2]])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name x) (id 11)))
       (value
        (Arr ((dimensions ()) (elements ((Literal ((value (IntLiteral 5)))))))))
       (body (Ref ((id ((name x) (id 11)))))))))
    Type: (Array (Arr ((element (AtomRef ((name int) (id 1)))) (shape ()))))
    (Array
     (Let
      ((binding ((name foo) (id 11)))
       (value
        (Arr
         ((dimensions ())
          (elements
           ((IndexLambda
             ((params (((binding ((name i) (id 12))) (bound Dim))))
              (body
               (Arr
                ((dimensions ())
                 (elements
                  ((TermLambda
                    ((params
                      (((binding ((name x) (id 13)))
                        (bound
                         (Arr
                          ((element (AtomRef ((name int) (id 1))))
                           (shape
                            ((Add ((const 1) (refs ())))
                             (Add ((const 0) (refs ((((name i) (id 12)) 1)))))))))))))
                     (body (Array (Ref ((id ((name x) (id 13)))))))))))))))))))))
       (body
        (TermApplication
         ((func
           (IndexApplication
            ((iFunc (Ref ((id ((name foo) (id 11))))))
             (args ((Dimension ((const 2) (refs ()))))))))
          (args
           ((Arr
             ((dimensions (1 2))
              (elements
               ((Literal ((value (IntLiteral 1))))
                (Literal ((value (IntLiteral 2))))))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint
    {|
    (define foo (i-fn (i)
      (fn ([x [int 1 i]]) x)))
    ((i-app foo [1 2]) [[1 2]])
    |};
  [%expect {| Error: Unexpected sort: expected `Dim`, got `Shape` |}];
  checkAndPrint {| (define x : [char 5] "hello") x |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name x) (id 11)))
       (value
        (Arr
         ((dimensions (5))
          (elements
           ((Literal ((value (CharacterLiteral h))))
            (Literal ((value (CharacterLiteral e))))
            (Literal ((value (CharacterLiteral l))))
            (Literal ((value (CharacterLiteral l))))
            (Literal ((value (CharacterLiteral o)))))))))
       (body (Ref ((id ((name x) (id 11)))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name char) (id 2))))
       (shape ((Add ((const 5) (refs ())))))))) |}];
  checkAndPrint {| (define x : int "hello") x |};
  [%expect {|
    Error: Let expected a value of type `int`, got `[char 5]` |}];
  checkAndPrint {| (define x : (Tuple int int) (tuple 5 5)) x |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name x) (id 11)))
       (value
        (Arr
         ((dimensions ())
          (elements
           ((Tuple
             ((elements
               ((Literal ((value (IntLiteral 5))))
                (Literal ((value (IntLiteral 5)))))))))))))
       (body (Ref ((id ((name x) (id 11)))))))))
    Type: (Array
     (Arr
      ((element
        (Tuple ((AtomRef ((name int) (id 1))) (AtomRef ((name int) (id 1))))))
       (shape ())))) |}];
  checkAndPrint
    {|
    (define weekdays
      (boxes (len) [char len] [5]
        ((6) "Monday" )
        ((7) "Tuesday" )
        ((9) "Wednesday")
        ((8) "Thursday" )
        ((6) "Friday" )))

    (unbox weekdays (day len)
      ((t-app (i-app length len []) char) day))
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name weekdays) (id 11)))
       (value
        (Arr
         ((dimensions (5))
          (elements
           ((Box
             ((indices ((Dimension ((const 6) (refs ())))))
              (body
               (Arr
                ((dimensions (6))
                 (elements
                  ((Literal ((value (CharacterLiteral M))))
                   (Literal ((value (CharacterLiteral o))))
                   (Literal ((value (CharacterLiteral n))))
                   (Literal ((value (CharacterLiteral d))))
                   (Literal ((value (CharacterLiteral a))))
                   (Literal ((value (CharacterLiteral y)))))))))
              (bodyType
               (Arr
                ((element (AtomRef ((name char) (id 2))))
                 (shape ((Add ((const 0) (refs ((((name len) (id 12)) 1))))))))))))
            (Box
             ((indices ((Dimension ((const 7) (refs ())))))
              (body
               (Arr
                ((dimensions (7))
                 (elements
                  ((Literal ((value (CharacterLiteral T))))
                   (Literal ((value (CharacterLiteral u))))
                   (Literal ((value (CharacterLiteral e))))
                   (Literal ((value (CharacterLiteral s))))
                   (Literal ((value (CharacterLiteral d))))
                   (Literal ((value (CharacterLiteral a))))
                   (Literal ((value (CharacterLiteral y)))))))))
              (bodyType
               (Arr
                ((element (AtomRef ((name char) (id 2))))
                 (shape ((Add ((const 0) (refs ((((name len) (id 12)) 1))))))))))))
            (Box
             ((indices ((Dimension ((const 9) (refs ())))))
              (body
               (Arr
                ((dimensions (9))
                 (elements
                  ((Literal ((value (CharacterLiteral W))))
                   (Literal ((value (CharacterLiteral e))))
                   (Literal ((value (CharacterLiteral d))))
                   (Literal ((value (CharacterLiteral n))))
                   (Literal ((value (CharacterLiteral e))))
                   (Literal ((value (CharacterLiteral s))))
                   (Literal ((value (CharacterLiteral d))))
                   (Literal ((value (CharacterLiteral a))))
                   (Literal ((value (CharacterLiteral y)))))))))
              (bodyType
               (Arr
                ((element (AtomRef ((name char) (id 2))))
                 (shape ((Add ((const 0) (refs ((((name len) (id 12)) 1))))))))))))
            (Box
             ((indices ((Dimension ((const 8) (refs ())))))
              (body
               (Arr
                ((dimensions (8))
                 (elements
                  ((Literal ((value (CharacterLiteral T))))
                   (Literal ((value (CharacterLiteral h))))
                   (Literal ((value (CharacterLiteral u))))
                   (Literal ((value (CharacterLiteral r))))
                   (Literal ((value (CharacterLiteral s))))
                   (Literal ((value (CharacterLiteral d))))
                   (Literal ((value (CharacterLiteral a))))
                   (Literal ((value (CharacterLiteral y)))))))))
              (bodyType
               (Arr
                ((element (AtomRef ((name char) (id 2))))
                 (shape ((Add ((const 0) (refs ((((name len) (id 12)) 1))))))))))))
            (Box
             ((indices ((Dimension ((const 6) (refs ())))))
              (body
               (Arr
                ((dimensions (6))
                 (elements
                  ((Literal ((value (CharacterLiteral F))))
                   (Literal ((value (CharacterLiteral r))))
                   (Literal ((value (CharacterLiteral i))))
                   (Literal ((value (CharacterLiteral d))))
                   (Literal ((value (CharacterLiteral a))))
                   (Literal ((value (CharacterLiteral y)))))))))
              (bodyType
               (Arr
                ((element (AtomRef ((name char) (id 2))))
                 (shape ((Add ((const 0) (refs ((((name len) (id 12)) 1)))))))))))))))))
       (body
        (Unbox
         ((indexBindings (((name len) (id 13))))
          (valueBinding ((name day) (id 14)))
          (box (Ref ((id ((name weekdays) (id 11))))))
          (body
           (TermApplication
            ((func
              (TypeApplication
               ((tFunc
                 (IndexApplication
                  ((iFunc (Ref ((id ((name length) (id 7))))))
                   (args
                    ((Dimension ((const 0) (refs ((((name len) (id 13)) 1)))))
                     (Shape ()))))))
                (args ((Atom (AtomRef ((name char) (id 2)))))))))
             (args ((Ref ((id ((name day) (id 14))))))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 5) (refs ())))))))) |}];
  checkAndPrint
    {|
    (boxes (r c) [int r c] [3]
      ((2 2) [[1 2] [3 4]])
      ((1 3) [[10 100 1000]])
      ((3 1) [[10] [100] [1000]]))
    |};
  [%expect
    {|
    (Array
     (Arr
      ((dimensions (3))
       (elements
        ((Box
          ((indices
            ((Dimension ((const 2) (refs ()))) (Dimension ((const 2) (refs ())))))
           (body
            (Arr
             ((dimensions (2 2))
              (elements
               ((Literal ((value (IntLiteral 1))))
                (Literal ((value (IntLiteral 2))))
                (Literal ((value (IntLiteral 3))))
                (Literal ((value (IntLiteral 4)))))))))
           (bodyType
            (Arr
             ((element (AtomRef ((name int) (id 1))))
              (shape
               ((Add ((const 0) (refs ((((name r) (id 11)) 1)))))
                (Add ((const 0) (refs ((((name c) (id 12)) 1))))))))))))
         (Box
          ((indices
            ((Dimension ((const 1) (refs ()))) (Dimension ((const 3) (refs ())))))
           (body
            (Arr
             ((dimensions (1 3))
              (elements
               ((Literal ((value (IntLiteral 10))))
                (Literal ((value (IntLiteral 100))))
                (Literal ((value (IntLiteral 1000)))))))))
           (bodyType
            (Arr
             ((element (AtomRef ((name int) (id 1))))
              (shape
               ((Add ((const 0) (refs ((((name r) (id 11)) 1)))))
                (Add ((const 0) (refs ((((name c) (id 12)) 1))))))))))))
         (Box
          ((indices
            ((Dimension ((const 3) (refs ()))) (Dimension ((const 1) (refs ())))))
           (body
            (Arr
             ((dimensions (3 1))
              (elements
               ((Literal ((value (IntLiteral 10))))
                (Literal ((value (IntLiteral 100))))
                (Literal ((value (IntLiteral 1000)))))))))
           (bodyType
            (Arr
             ((element (AtomRef ((name int) (id 1))))
              (shape
               ((Add ((const 0) (refs ((((name r) (id 11)) 1)))))
                (Add ((const 0) (refs ((((name c) (id 12)) 1)))))))))))))))))
    Type: (Array
     (Arr
      ((element
        (Sigma
         ((parameters
           (((binding ((name r) (id 11))) (bound Dim))
            ((binding ((name c) (id 12))) (bound Dim))))
          (body
           (Arr
            ((element (AtomRef ((name int) (id 1))))
             (shape
              ((Add ((const 0) (refs ((((name r) (id 11)) 1)))))
               (Add ((const 0) (refs ((((name c) (id 12)) 1)))))))))))))
       (shape ((Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| (box ((len 3)) [int len] [8 23 0]) |};
  [%expect
    {|
    (Array
     (Arr
      ((dimensions ())
       (elements
        ((Box
          ((indices ((Dimension ((const 3) (refs ())))))
           (body
            (Arr
             ((dimensions (3))
              (elements
               ((Literal ((value (IntLiteral 8))))
                (Literal ((value (IntLiteral 23))))
                (Literal ((value (IntLiteral 0)))))))))
           (bodyType
            (Arr
             ((element (AtomRef ((name int) (id 1))))
              (shape ((Add ((const 0) (refs ((((name len) (id 11)) 1)))))))))))))))))
    Type: (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding ((name len) (id 11))) (bound Dim))))
          (body
           (Arr
            ((element (AtomRef ((name int) (id 1))))
             (shape ((Add ((const 0) (refs ((((name len) (id 11)) 1)))))))))))))
       (shape ())))) |}];
  checkAndPrint
    {|
    (boxes (@shape) [int @shape] [3]
      (([2 2]) [[1 2] [3 4]])
      (([1 3]) [[10 100 1000]])
      (([3 1]) [[10] [100] [1000]]))
    |};
  [%expect
    {|
    (Array
     (Arr
      ((dimensions (3))
       (elements
        ((Box
          ((indices
            ((Shape ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))
           (body
            (Arr
             ((dimensions (2 2))
              (elements
               ((Literal ((value (IntLiteral 1))))
                (Literal ((value (IntLiteral 2))))
                (Literal ((value (IntLiteral 3))))
                (Literal ((value (IntLiteral 4)))))))))
           (bodyType
            (Arr
             ((element (AtomRef ((name int) (id 1))))
              (shape ((ShapeRef ((name @shape) (id 11))))))))))
         (Box
          ((indices
            ((Shape ((Add ((const 1) (refs ()))) (Add ((const 3) (refs ())))))))
           (body
            (Arr
             ((dimensions (1 3))
              (elements
               ((Literal ((value (IntLiteral 10))))
                (Literal ((value (IntLiteral 100))))
                (Literal ((value (IntLiteral 1000)))))))))
           (bodyType
            (Arr
             ((element (AtomRef ((name int) (id 1))))
              (shape ((ShapeRef ((name @shape) (id 11))))))))))
         (Box
          ((indices
            ((Shape ((Add ((const 3) (refs ()))) (Add ((const 1) (refs ())))))))
           (body
            (Arr
             ((dimensions (3 1))
              (elements
               ((Literal ((value (IntLiteral 10))))
                (Literal ((value (IntLiteral 100))))
                (Literal ((value (IntLiteral 1000)))))))))
           (bodyType
            (Arr
             ((element (AtomRef ((name int) (id 1))))
              (shape ((ShapeRef ((name @shape) (id 11)))))))))))))))
    Type: (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding ((name @shape) (id 11))) (bound Shape))))
          (body
           (Arr
            ((element (AtomRef ((name int) (id 1))))
             (shape ((ShapeRef ((name @shape) (id 11)))))))))))
       (shape ((Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| (box ((len 4)) [int len] [8 23 0]) |};
  [%expect {| Error: Expected type `[int 4]`, got `[int 3]` |}];
  checkAndPrint {| (box ((len 3)) [int 1 len] [8 23 0]) |};
  [%expect {| Error: Expected type `[int 1 3]`, got `[int 3]` |}];
  checkAndPrint {| (box ((len [3])) [int len] [8 23 0]) |};
  [%expect {| Error: Unexpected sort: expected `Dim`, got `Shape` |}];
  checkAndPrint {| ([+ -] 1 2) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func
        (Frame
         ((dimensions (2))
          (arrays
           ((Ref ((id ((name +) (id 3))))) (Ref ((id ((name -) (id 4))))))))))
       (args
        ((Arr ((dimensions ()) (elements ((Literal ((value (IntLiteral 1))))))))
         (Arr ((dimensions ()) (elements ((Literal ((value (IntLiteral 2)))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| ([+ -] [1 2] 3) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func
        (Frame
         ((dimensions (2))
          (arrays
           ((Ref ((id ((name +) (id 3))))) (Ref ((id ((name -) (id 4))))))))))
       (args
        ((Arr
          ((dimensions (2))
           (elements
            ((Literal ((value (IntLiteral 1))))
             (Literal ((value (IntLiteral 2))))))))
         (Arr ((dimensions ()) (elements ((Literal ((value (IntLiteral 3)))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| ([+ -] [1 2] [3 4]) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func
        (Frame
         ((dimensions (2))
          (arrays
           ((Ref ((id ((name +) (id 3))))) (Ref ((id ((name -) (id 4))))))))))
       (args
        ((Arr
          ((dimensions (2))
           (elements
            ((Literal ((value (IntLiteral 1))))
             (Literal ((value (IntLiteral 2))))))))
         (Arr
          ((dimensions (2))
           (elements
            ((Literal ((value (IntLiteral 3))))
             (Literal ((value (IntLiteral 4)))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| ([+ -] [1 2 3] [4 5]) |};
  [%expect {| Error: Function call has principal frame `[2]`, got frame `[3]` |}];
  checkAndPrint {| ([+ -] [1 2 3] [4 5 6]) |};
  [%expect
    {|
    Error: Function call has principal frame `[2]`, got frame `[3]`
    Error: Function call has principal frame `[2]`, got frame `[3]` |}];
  checkAndPrint {|
    (define (foo [x [int 2]]) x)
    (foo [1 2])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name foo) (id 11)))
       (value
        (Arr
         ((dimensions ())
          (elements
           ((TermLambda
             ((params
               (((binding ((name x) (id 12)))
                 (bound
                  (Arr
                   ((element (AtomRef ((name int) (id 1))))
                    (shape ((Add ((const 2) (refs ())))))))))))
              (body (Array (Ref ((id ((name x) (id 12))))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name foo) (id 11))))))
          (args
           ((Arr
             ((dimensions (2))
              (elements
               ((Literal ((value (IntLiteral 1))))
                (Literal ((value (IntLiteral 2))))))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {|
    (define (foo [x [int 2]]) 5)
    (foo [1 2])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name foo) (id 11)))
       (value
        (Arr
         ((dimensions ())
          (elements
           ((TermLambda
             ((params
               (((binding ((name x) (id 12)))
                 (bound
                  (Arr
                   ((element (AtomRef ((name int) (id 1))))
                    (shape ((Add ((const 2) (refs ())))))))))))
              (body
               (Array
                (Arr
                 ((dimensions ())
                  (elements ((Literal ((value (IntLiteral 5)))))))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name foo) (id 11))))))
          (args
           ((Arr
             ((dimensions (2))
              (elements
               ((Literal ((value (IntLiteral 1))))
                (Literal ((value (IntLiteral 2))))))))))))))))
    Type: (Array (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))) |}];
  checkAndPrint {|
    (define (foo [x [int 2]]) 5)
    (foo [[1 2] [3 4]])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name foo) (id 11)))
       (value
        (Arr
         ((dimensions ())
          (elements
           ((TermLambda
             ((params
               (((binding ((name x) (id 12)))
                 (bound
                  (Arr
                   ((element (AtomRef ((name int) (id 1))))
                    (shape ((Add ((const 2) (refs ())))))))))))
              (body
               (Array
                (Arr
                 ((dimensions ())
                  (elements ((Literal ((value (IntLiteral 5)))))))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name foo) (id 11))))))
          (args
           ((Arr
             ((dimensions (2 2))
              (elements
               ((Literal ((value (IntLiteral 1))))
                (Literal ((value (IntLiteral 2))))
                (Literal ((value (IntLiteral 3))))
                (Literal ((value (IntLiteral 4))))))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {|
    (define (foo [x [int 2]]) 5)
    (foo [[1 2 3] [4 5 6]])
    |};
  [%expect {| Error: Function expected argument with cell shape `[2]`, got `[2 3]` |}];
  checkAndPrint
    {|
    (define left (t-fn (@t) (fn ([l @t] [r @t]) l)))
    (define right (t-fn (@t) (fn ([l @t] [r @t]) r)))
    (define funs [left right])

    (define foo ((t-app funs int) 1 2))
    (define bar ((t-app funs [char 2]) "hi" "ih"))

    "hello world"
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name left) (id 11)))
       (value
        (Arr
         ((dimensions ())
          (elements
           ((TypeLambda
             ((params (((binding ((name @t) (id 12))) (bound Array))))
              (body
               (Arr
                ((dimensions ())
                 (elements
                  ((TermLambda
                    ((params
                      (((binding ((name l) (id 13)))
                        (bound (ArrayRef ((name @t) (id 12)))))
                       ((binding ((name r) (id 14)))
                        (bound (ArrayRef ((name @t) (id 12)))))))
                     (body (Array (Ref ((id ((name l) (id 13)))))))))))))))))))))
       (body
        (Let
         ((binding ((name right) (id 15)))
          (value
           (Arr
            ((dimensions ())
             (elements
              ((TypeLambda
                ((params (((binding ((name @t) (id 16))) (bound Array))))
                 (body
                  (Arr
                   ((dimensions ())
                    (elements
                     ((TermLambda
                       ((params
                         (((binding ((name l) (id 17)))
                           (bound (ArrayRef ((name @t) (id 16)))))
                          ((binding ((name r) (id 18)))
                           (bound (ArrayRef ((name @t) (id 16)))))))
                        (body (Array (Ref ((id ((name r) (id 18)))))))))))))))))))))
          (body
           (Let
            ((binding ((name funs) (id 19)))
             (value
              (Frame
               ((dimensions (2))
                (arrays
                 ((Ref ((id ((name left) (id 11)))))
                  (Ref ((id ((name right) (id 15))))))))))
             (body
              (Let
               ((binding ((name foo) (id 20)))
                (value
                 (TermApplication
                  ((func
                    (TypeApplication
                     ((tFunc (Ref ((id ((name funs) (id 19))))))
                      (args
                       ((Array
                         (Arr
                          ((element (AtomRef ((name int) (id 1)))) (shape ())))))))))
                   (args
                    ((Arr
                      ((dimensions ())
                       (elements ((Literal ((value (IntLiteral 1))))))))
                     (Arr
                      ((dimensions ())
                       (elements ((Literal ((value (IntLiteral 2)))))))))))))
                (body
                 (Let
                  ((binding ((name bar) (id 21)))
                   (value
                    (TermApplication
                     ((func
                       (TypeApplication
                        ((tFunc (Ref ((id ((name funs) (id 19))))))
                         (args
                          ((Array
                            (Arr
                             ((element (AtomRef ((name char) (id 2))))
                              (shape ((Add ((const 2) (refs ())))))))))))))
                      (args
                       ((Arr
                         ((dimensions (2))
                          (elements
                           ((Literal ((value (CharacterLiteral h))))
                            (Literal ((value (CharacterLiteral i))))))))
                        (Arr
                         ((dimensions (2))
                          (elements
                           ((Literal ((value (CharacterLiteral i))))
                            (Literal ((value (CharacterLiteral h)))))))))))))
                   (body
                    (Arr
                     ((dimensions (11))
                      (elements
                       ((Literal ((value (CharacterLiteral h))))
                        (Literal ((value (CharacterLiteral e))))
                        (Literal ((value (CharacterLiteral l))))
                        (Literal ((value (CharacterLiteral l))))
                        (Literal ((value (CharacterLiteral o))))
                        (Literal ((value (CharacterLiteral " "))))
                        (Literal ((value (CharacterLiteral w))))
                        (Literal ((value (CharacterLiteral o))))
                        (Literal ((value (CharacterLiteral r))))
                        (Literal ((value (CharacterLiteral l))))
                        (Literal ((value (CharacterLiteral d))))))))))))))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name char) (id 2))))
       (shape ((Add ((const 11) (refs ())))))))) |}]
;;
