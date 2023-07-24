open! Base
open Remora

let%expect_test "check sort" =
  let checkAndPrint str =
    match Parser.Unit.IndexParser.parseString str with
    | MOk index ->
      (match CompilerState.runA (TypeChecker.checkSort index) CompilerState.initial with
       | MOk indexTyped ->
         [%sexp_of: Nucleus.Index.t] indexTyped
         |> Sexp.to_string_hum
         |> Stdio.print_endline
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
      (match CompilerState.runA (TypeChecker.checkKind type') CompilerState.initial with
       | MOk typeTyped ->
         [%sexp_of: Nucleus.Type.t] typeTyped |> Sexp.to_string_hum |> Stdio.print_endline
       | Errors errs ->
         NeList.iter errs ~f:(fun err ->
           Stdio.prerr_endline [%string "Error: %{TypeChecker.errorMessage err.elem}"]))
    | Errors errs ->
      let errsSexp = [%sexp_of: (string * unit) list] (NeList.to_list errs) in
      let errsStr = Sexp.to_string_hum errsSexp in
      Error.raise (Error.of_string [%string "Got errors: `%{errsStr}` parsing `%{str}`"])
  in
  checkAndPrint {| int |};
  [%expect {| (Atom (Literal IntLiteral)) |}];
  checkAndPrint {| char |};
  [%expect {| (Atom (Literal CharacterLiteral)) |}];
  checkAndPrint {| foo |};
  [%expect {| Error: Unbound type variable `foo` |}];
  checkAndPrint {| (Arr int [1 2 3]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element (Literal IntLiteral))
       (shape
        ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
         (Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| (-> (int int) int) |};
  [%expect
    {|
    (Atom
     (Func
      ((parameters
        ((Arr ((element (Literal IntLiteral)) (shape ())))
         (Arr ((element (Literal IntLiteral)) (shape ())))))
       (return (Arr ((element (Literal IntLiteral)) (shape ()))))))) |}];
  checkAndPrint {| (Forall (@t) @t) |};
  [%expect
    {|
    (Atom
     (Forall
      ((parameters (((binding ((name @t) (id 3))) (bound Array))))
       (body (ArrayRef ((name @t) (id 3))))))) |}];
  checkAndPrint {| (Arr (Forall (@t) @t) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Forall
         ((parameters (((binding ((name @t) (id 3))) (bound Array))))
          (body (ArrayRef ((name @t) (id 3)))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Forall (t) t) |};
  [%expect
    {|
    (Atom
     (Forall
      ((parameters (((binding ((name t) (id 3))) (bound Atom))))
       (body (Arr ((element (AtomRef ((name t) (id 3)))) (shape ()))))))) |}];
  checkAndPrint {| (Pi (@i) (Arr int @i)) |};
  [%expect
    {|
    (Atom
     (Pi
      ((parameters (((binding ((name @i) (id 3))) (bound Shape))))
       (body
        (Arr
         ((element (Literal IntLiteral)) (shape ((ShapeRef ((name @i) (id 3))))))))))) |}];
  checkAndPrint {| (Arr (Pi (@i) [int @i]) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Pi
         ((parameters (((binding ((name @i) (id 3))) (bound Shape))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape ((ShapeRef ((name @i) (id 3)))))))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Pi (i) [int i i]) |};
  [%expect
    {|
    (Atom
     (Pi
      ((parameters (((binding ((name i) (id 3))) (bound Dim))))
       (body
        (Arr
         ((element (Literal IntLiteral))
          (shape
           ((Add ((const 0) (refs ((((name i) (id 3)) 1)))))
            (Add ((const 0) (refs ((((name i) (id 3)) 1))))))))))))) |}];
  checkAndPrint {| (Sigma (@i) (Arr int @i)) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters (((binding ((name @i) (id 3))) (bound Shape))))
       (body
        (Arr
         ((element (Literal IntLiteral)) (shape ((ShapeRef ((name @i) (id 3))))))))))) |}];
  checkAndPrint {| (Arr (Sigma (@i) [int @i]) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding ((name @i) (id 3))) (bound Shape))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape ((ShapeRef ((name @i) (id 3)))))))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Sigma (i) [int i i]) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters (((binding ((name i) (id 3))) (bound Dim))))
       (body
        (Arr
         ((element (Literal IntLiteral))
          (shape
           ((Add ((const 0) (refs ((((name i) (id 3)) 1)))))
            (Add ((const 0) (refs ((((name i) (id 3)) 1))))))))))))) |}];
  checkAndPrint {| (Sigma (@i j) (Arr int [j @i 5 (+ j 10)])) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters
        (((binding ((name @i) (id 3))) (bound Shape))
         ((binding ((name j) (id 4))) (bound Dim))))
       (body
        (Arr
         ((element (Literal IntLiteral))
          (shape
           ((Add ((const 0) (refs ((((name j) (id 4)) 1)))))
            (ShapeRef ((name @i) (id 3))) (Add ((const 5) (refs ())))
            (Add ((const 10) (refs ((((name j) (id 4)) 1))))))))))))) |}];
  checkAndPrint {| (Tuple int char) |};
  [%expect {| (Atom (Tuple ((Literal IntLiteral) (Literal CharacterLiteral)))) |}]
;;

let%expect_test "check type" =
  let checkAndPrint str =
    match Parser.Unit.parseString str with
    | MOk expr ->
      (match CompilerState.runA (TypeChecker.checkType expr) CompilerState.initial with
       | MOk exprTyped ->
         [%sexp_of: Nucleus.Expr.t] exprTyped |> Sexp.to_string_hum |> Stdio.print_endline;
         [%sexp_of: Nucleus.Type.t] (Nucleus.Expr.type' exprTyped)
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
    (Atom (Literal (IntLiteral 5)))
    Type: (Atom (Literal IntLiteral)) |}];
  checkAndPrint {| [1 2 3 4 5] |};
  [%expect
    {|
    (Array
     (Frame
      ((dimensions (5))
       (elements
        ((Scalar ((element (Literal (IntLiteral 1)))))
         (Scalar ((element (Literal (IntLiteral 2)))))
         (Scalar ((element (Literal (IntLiteral 3)))))
         (Scalar ((element (Literal (IntLiteral 4)))))
         (Scalar ((element (Literal (IntLiteral 5))))))))))
    Type: (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 5) (refs ())))))))) |}];
  checkAndPrint {| [[1 2] [3 4]] |};
  [%expect
    {|
    (Array
     (Frame
      ((dimensions (2))
       (elements
        ((Frame
          ((dimensions (2))
           (elements
            ((Scalar ((element (Literal (IntLiteral 1)))))
             (Scalar ((element (Literal (IntLiteral 2)))))))))
         (Frame
          ((dimensions (2))
           (elements
            ((Scalar ((element (Literal (IntLiteral 3)))))
             (Scalar ((element (Literal (IntLiteral 4))))))))))))))
    Type: (Array
     (Arr
      ((element (Literal IntLiteral))
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
      ((func (Scalar ((element (BuiltInFunction ((func Add)))))))
       (args
        ((Scalar ((element (Literal (IntLiteral 1)))))
         (Scalar ((element (Literal (IntLiteral 2))))))))))
    Type: (Array (Arr ((element (Literal IntLiteral)) (shape ())))) |}];
  checkAndPrint {| (+ [1 2 3] [4 5 6]) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func (Scalar ((element (BuiltInFunction ((func Add)))))))
       (args
        ((Frame
          ((dimensions (3))
           (elements
            ((Scalar ((element (Literal (IntLiteral 1)))))
             (Scalar ((element (Literal (IntLiteral 2)))))
             (Scalar ((element (Literal (IntLiteral 3)))))))))
         (Frame
          ((dimensions (3))
           (elements
            ((Scalar ((element (Literal (IntLiteral 4)))))
             (Scalar ((element (Literal (IntLiteral 5)))))
             (Scalar ((element (Literal (IntLiteral 6))))))))))))))
    Type: (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| [(+ [1 2 3] [4 5 6]) [7 8 9]] |};
  [%expect
    {|
    (Array
     (Frame
      ((dimensions (2))
       (elements
        ((TermApplication
          ((func (Scalar ((element (BuiltInFunction ((func Add)))))))
           (args
            ((Frame
              ((dimensions (3))
               (elements
                ((Scalar ((element (Literal (IntLiteral 1)))))
                 (Scalar ((element (Literal (IntLiteral 2)))))
                 (Scalar ((element (Literal (IntLiteral 3)))))))))
             (Frame
              ((dimensions (3))
               (elements
                ((Scalar ((element (Literal (IntLiteral 4)))))
                 (Scalar ((element (Literal (IntLiteral 5)))))
                 (Scalar ((element (Literal (IntLiteral 6)))))))))))))
         (Frame
          ((dimensions (3))
           (elements
            ((Scalar ((element (Literal (IntLiteral 7)))))
             (Scalar ((element (Literal (IntLiteral 8)))))
             (Scalar ((element (Literal (IntLiteral 9))))))))))))))
    Type: (Array
     (Arr
      ((element (Literal IntLiteral))
       (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| (+ [1 2 3 4 5] 6) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func (Scalar ((element (BuiltInFunction ((func Add)))))))
       (args
        ((Frame
          ((dimensions (5))
           (elements
            ((Scalar ((element (Literal (IntLiteral 1)))))
             (Scalar ((element (Literal (IntLiteral 2)))))
             (Scalar ((element (Literal (IntLiteral 3)))))
             (Scalar ((element (Literal (IntLiteral 4)))))
             (Scalar ((element (Literal (IntLiteral 5)))))))))
         (Scalar ((element (Literal (IntLiteral 6))))))))))
    Type: (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 5) (refs ())))))))) |}];
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
      ((binding ((name foo) (id 3)))
       (value (Scalar ((element (Literal (IntLiteral 5))))))
       (body (Ref ((id ((name foo) (id 3)))))))))
    Type: (Array (Arr ((element (Literal IntLiteral)) (shape ())))) |}];
  checkAndPrint {|
  (define (add [x int] [y int])
    (+ x y))
  (add 1 2)
  |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name add) (id 3)))
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding ((name x) (id 4)))
                (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
               ((binding ((name y) (id 5)))
                (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
             (body
              (TermApplication
               ((func (Scalar ((element (BuiltInFunction ((func Add)))))))
                (args
                 ((Ref ((id ((name x) (id 4))))) (Ref ((id ((name y) (id 5))))))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name add) (id 3))))))
          (args
           ((Scalar ((element (Literal (IntLiteral 1)))))
            (Scalar ((element (Literal (IntLiteral 2)))))))))))))
    Type: (Array (Arr ((element (Literal IntLiteral)) (shape ())))) |}];
  checkAndPrint {| (fn ([x int]) x) |};
  [%expect
    {|
    (Atom
     (TermLambda
      ((params
        (((binding ((name x) (id 3)))
          (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
       (body (Ref ((id ((name x) (id 3)))))))))
    Type: (Atom
     (Func
      ((parameters ((Arr ((element (Literal IntLiteral)) (shape ())))))
       (return (Arr ((element (Literal IntLiteral)) (shape ()))))))) |}];
  checkAndPrint {| (fn ([x int]) "hello") |};
  [%expect
    {|
    (Atom
     (TermLambda
      ((params
        (((binding ((name x) (id 3)))
          (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
       (body
        (Frame
         ((dimensions (5))
          (elements
           ((Scalar ((element (Literal (CharacterLiteral h)))))
            (Scalar ((element (Literal (CharacterLiteral e)))))
            (Scalar ((element (Literal (CharacterLiteral l)))))
            (Scalar ((element (Literal (CharacterLiteral l)))))
            (Scalar ((element (Literal (CharacterLiteral o)))))))))))))
    Type: (Atom
     (Func
      ((parameters ((Arr ((element (Literal IntLiteral)) (shape ())))))
       (return
        (Arr
         ((element (Literal CharacterLiteral))
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
      ((binding ((name id) (id 3)))
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding ((name @t) (id 4))) (bound Array))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name x) (id 5)))
                      (bound (ArrayRef ((name @t) (id 4)))))))
                   (body (Ref ((id ((name x) (id 5))))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id ((name id) (id 3))))))
             (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
          (args ((Scalar ((element (Literal (IntLiteral 5)))))))))))))
    Type: (Array (Arr ((element (Literal IntLiteral)) (shape ())))) |}];
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
      ((binding ((name id) (id 3)))
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding ((name t) (id 4))) (bound Atom))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name x) (id 5)))
                      (bound
                       (Arr
                        ((element (AtomRef ((name t) (id 4))))
                         (shape ((Add ((const 2) (refs ())))))))))))
                   (body (Ref ((id ((name x) (id 5))))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id ((name id) (id 3))))))
             (args ((Atom (Literal IntLiteral)))))))
          (args
           ((Frame
             ((dimensions (2))
              (elements
               ((Scalar ((element (Literal (IntLiteral 1)))))
                (Scalar ((element (Literal (IntLiteral 2)))))))))))))))))
    Type: (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))) |}];
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
      ((binding ((name id) (id 3)))
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding ((name @t) (id 4))) (bound Array))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name x) (id 5)))
                      (bound (ArrayRef ((name @t) (id 4)))))))
                   (body (Ref ((id ((name x) (id 5))))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id ((name id) (id 3))))))
             (args
              ((Array
                (Arr
                 ((element (Literal CharacterLiteral))
                  (shape ((Add ((const 5) (refs ())))))))))))))
          (args
           ((Frame
             ((dimensions (5))
              (elements
               ((Scalar ((element (Literal (CharacterLiteral h)))))
                (Scalar ((element (Literal (CharacterLiteral e)))))
                (Scalar ((element (Literal (CharacterLiteral l)))))
                (Scalar ((element (Literal (CharacterLiteral l)))))
                (Scalar ((element (Literal (CharacterLiteral o)))))))))))))))))
    Type: (Array
     (Arr
      ((element (Literal CharacterLiteral))
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
      ((binding ((name id) (id 3)))
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding ((name @t) (id 4))) (bound Array))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name x) (id 5)))
                      (bound (ArrayRef ((name @t) (id 4)))))))
                   (body (Ref ((id ((name x) (id 5))))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id ((name id) (id 3))))))
             (args
              ((Array
                (Arr
                 ((element (Literal CharacterLiteral))
                  (shape ((Add ((const 5) (refs ())))))))))))))
          (args
           ((Frame
             ((dimensions (5))
              (elements
               ((Scalar ((element (Literal (CharacterLiteral h)))))
                (Scalar ((element (Literal (CharacterLiteral e)))))
                (Scalar ((element (Literal (CharacterLiteral l)))))
                (Scalar ((element (Literal (CharacterLiteral l)))))
                (Scalar ((element (Literal (CharacterLiteral o)))))))))))))))))
    Type: (Array
     (Arr
      ((element (Literal CharacterLiteral))
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
      ((binding ((name foo) (id 3)))
       (value
        (Scalar
         ((element
           (IndexLambda
            ((params (((binding ((name @i) (id 4))) (bound Shape))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name x) (id 5)))
                      (bound
                       (Arr
                        ((element (Literal IntLiteral))
                         (shape
                          ((Add ((const 1) (refs ())))
                           (ShapeRef ((name @i) (id 4)))))))))))
                   (body (Ref ((id ((name x) (id 5))))))))))))))))))
       (body
        (TermApplication
         ((func
           (IndexApplication
            ((iFunc (Ref ((id ((name foo) (id 3))))))
             (args ((Shape ((Add ((const 2) (refs ()))))))))))
          (args
           ((Frame
             ((dimensions (1))
              (elements
               ((Frame
                 ((dimensions (2))
                  (elements
                   ((Scalar ((element (Literal (IntLiteral 1)))))
                    (Scalar ((element (Literal (IntLiteral 2)))))))))))))))))))))
    Type: (Array
     (Arr
      ((element (Literal IntLiteral))
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
      ((binding ((name x) (id 3)))
       (value (Scalar ((element (Literal (IntLiteral 5))))))
       (body (Ref ((id ((name x) (id 3)))))))))
    Type: (Array (Arr ((element (Literal IntLiteral)) (shape ()))))
    (Array
     (Let
      ((binding ((name foo) (id 3)))
       (value
        (Scalar
         ((element
           (IndexLambda
            ((params (((binding ((name i) (id 4))) (bound Dim))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name x) (id 5)))
                      (bound
                       (Arr
                        ((element (Literal IntLiteral))
                         (shape
                          ((Add ((const 1) (refs ())))
                           (Add ((const 0) (refs ((((name i) (id 4)) 1)))))))))))))
                   (body (Ref ((id ((name x) (id 5))))))))))))))))))
       (body
        (TermApplication
         ((func
           (IndexApplication
            ((iFunc (Ref ((id ((name foo) (id 3))))))
             (args ((Dimension ((const 2) (refs ()))))))))
          (args
           ((Frame
             ((dimensions (1))
              (elements
               ((Frame
                 ((dimensions (2))
                  (elements
                   ((Scalar ((element (Literal (IntLiteral 1)))))
                    (Scalar ((element (Literal (IntLiteral 2)))))))))))))))))))))
    Type: (Array
     (Arr
      ((element (Literal IntLiteral))
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
      ((binding ((name x) (id 3)))
       (value
        (Frame
         ((dimensions (5))
          (elements
           ((Scalar ((element (Literal (CharacterLiteral h)))))
            (Scalar ((element (Literal (CharacterLiteral e)))))
            (Scalar ((element (Literal (CharacterLiteral l)))))
            (Scalar ((element (Literal (CharacterLiteral l)))))
            (Scalar ((element (Literal (CharacterLiteral o))))))))))
       (body (Ref ((id ((name x) (id 3)))))))))
    Type: (Array
     (Arr
      ((element (Literal CharacterLiteral))
       (shape ((Add ((const 5) (refs ())))))))) |}];
  checkAndPrint {| (define x : int "hello") x |};
  [%expect {|
    Error: Let expected a value of type `int`, got `[char 5]` |}];
  checkAndPrint {| (define x : (Tuple int int) (tuple 5 5)) x |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name x) (id 3)))
       (value
        (Scalar
         ((element
           (Tuple
            ((elements ((Literal (IntLiteral 5)) (Literal (IntLiteral 5))))))))))
       (body (Ref ((id ((name x) (id 3)))))))))
    Type: (Array
     (Arr
      ((element (Tuple ((Literal IntLiteral) (Literal IntLiteral)))) (shape ())))) |}];
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
      ((binding ((name weekdays) (id 3)))
       (value
        (Frame
         ((dimensions (5))
          (elements
           ((Scalar
             ((element
               (Box
                ((indices ((Dimension ((const 6) (refs ())))))
                 (body
                  (Frame
                   ((dimensions (6))
                    (elements
                     ((Scalar ((element (Literal (CharacterLiteral M)))))
                      (Scalar ((element (Literal (CharacterLiteral o)))))
                      (Scalar ((element (Literal (CharacterLiteral n)))))
                      (Scalar ((element (Literal (CharacterLiteral d)))))
                      (Scalar ((element (Literal (CharacterLiteral a)))))
                      (Scalar ((element (Literal (CharacterLiteral y))))))))))
                 (bodyType
                  (Arr
                   ((element (Literal CharacterLiteral))
                    (shape ((Add ((const 0) (refs ((((name len) (id 4)) 1)))))))))))))))
            (Scalar
             ((element
               (Box
                ((indices ((Dimension ((const 7) (refs ())))))
                 (body
                  (Frame
                   ((dimensions (7))
                    (elements
                     ((Scalar ((element (Literal (CharacterLiteral T)))))
                      (Scalar ((element (Literal (CharacterLiteral u)))))
                      (Scalar ((element (Literal (CharacterLiteral e)))))
                      (Scalar ((element (Literal (CharacterLiteral s)))))
                      (Scalar ((element (Literal (CharacterLiteral d)))))
                      (Scalar ((element (Literal (CharacterLiteral a)))))
                      (Scalar ((element (Literal (CharacterLiteral y))))))))))
                 (bodyType
                  (Arr
                   ((element (Literal CharacterLiteral))
                    (shape ((Add ((const 0) (refs ((((name len) (id 4)) 1)))))))))))))))
            (Scalar
             ((element
               (Box
                ((indices ((Dimension ((const 9) (refs ())))))
                 (body
                  (Frame
                   ((dimensions (9))
                    (elements
                     ((Scalar ((element (Literal (CharacterLiteral W)))))
                      (Scalar ((element (Literal (CharacterLiteral e)))))
                      (Scalar ((element (Literal (CharacterLiteral d)))))
                      (Scalar ((element (Literal (CharacterLiteral n)))))
                      (Scalar ((element (Literal (CharacterLiteral e)))))
                      (Scalar ((element (Literal (CharacterLiteral s)))))
                      (Scalar ((element (Literal (CharacterLiteral d)))))
                      (Scalar ((element (Literal (CharacterLiteral a)))))
                      (Scalar ((element (Literal (CharacterLiteral y))))))))))
                 (bodyType
                  (Arr
                   ((element (Literal CharacterLiteral))
                    (shape ((Add ((const 0) (refs ((((name len) (id 4)) 1)))))))))))))))
            (Scalar
             ((element
               (Box
                ((indices ((Dimension ((const 8) (refs ())))))
                 (body
                  (Frame
                   ((dimensions (8))
                    (elements
                     ((Scalar ((element (Literal (CharacterLiteral T)))))
                      (Scalar ((element (Literal (CharacterLiteral h)))))
                      (Scalar ((element (Literal (CharacterLiteral u)))))
                      (Scalar ((element (Literal (CharacterLiteral r)))))
                      (Scalar ((element (Literal (CharacterLiteral s)))))
                      (Scalar ((element (Literal (CharacterLiteral d)))))
                      (Scalar ((element (Literal (CharacterLiteral a)))))
                      (Scalar ((element (Literal (CharacterLiteral y))))))))))
                 (bodyType
                  (Arr
                   ((element (Literal CharacterLiteral))
                    (shape ((Add ((const 0) (refs ((((name len) (id 4)) 1)))))))))))))))
            (Scalar
             ((element
               (Box
                ((indices ((Dimension ((const 6) (refs ())))))
                 (body
                  (Frame
                   ((dimensions (6))
                    (elements
                     ((Scalar ((element (Literal (CharacterLiteral F)))))
                      (Scalar ((element (Literal (CharacterLiteral r)))))
                      (Scalar ((element (Literal (CharacterLiteral i)))))
                      (Scalar ((element (Literal (CharacterLiteral d)))))
                      (Scalar ((element (Literal (CharacterLiteral a)))))
                      (Scalar ((element (Literal (CharacterLiteral y))))))))))
                 (bodyType
                  (Arr
                   ((element (Literal CharacterLiteral))
                    (shape ((Add ((const 0) (refs ((((name len) (id 4)) 1))))))))))))))))))))
       (body
        (Unbox
         ((indexBindings (((name len) (id 5))))
          (valueBinding ((name day) (id 6)))
          (box (Ref ((id ((name weekdays) (id 3))))))
          (body
           (TermApplication
            ((func
              (TypeApplication
               ((tFunc
                 (IndexApplication
                  ((iFunc (Scalar ((element (BuiltInFunction ((func Length)))))))
                   (args
                    ((Dimension ((const 0) (refs ((((name len) (id 5)) 1)))))
                     (Shape ()))))))
                (args ((Atom (Literal CharacterLiteral)))))))
             (args ((Ref ((id ((name day) (id 6))))))))))))))))
    Type: (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 5) (refs ())))))))) |}];
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
     (Frame
      ((dimensions (3))
       (elements
        ((Scalar
          ((element
            (Box
             ((indices
               ((Dimension ((const 2) (refs ())))
                (Dimension ((const 2) (refs ())))))
              (body
               (Frame
                ((dimensions (2))
                 (elements
                  ((Frame
                    ((dimensions (2))
                     (elements
                      ((Scalar ((element (Literal (IntLiteral 1)))))
                       (Scalar ((element (Literal (IntLiteral 2)))))))))
                   (Frame
                    ((dimensions (2))
                     (elements
                      ((Scalar ((element (Literal (IntLiteral 3)))))
                       (Scalar ((element (Literal (IntLiteral 4))))))))))))))
              (bodyType
               (Arr
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 0) (refs ((((name r) (id 3)) 1)))))
                   (Add ((const 0) (refs ((((name c) (id 4)) 1)))))))))))))))
         (Scalar
          ((element
            (Box
             ((indices
               ((Dimension ((const 1) (refs ())))
                (Dimension ((const 3) (refs ())))))
              (body
               (Frame
                ((dimensions (1))
                 (elements
                  ((Frame
                    ((dimensions (3))
                     (elements
                      ((Scalar ((element (Literal (IntLiteral 10)))))
                       (Scalar ((element (Literal (IntLiteral 100)))))
                       (Scalar ((element (Literal (IntLiteral 1000))))))))))))))
              (bodyType
               (Arr
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 0) (refs ((((name r) (id 3)) 1)))))
                   (Add ((const 0) (refs ((((name c) (id 4)) 1)))))))))))))))
         (Scalar
          ((element
            (Box
             ((indices
               ((Dimension ((const 3) (refs ())))
                (Dimension ((const 1) (refs ())))))
              (body
               (Frame
                ((dimensions (3))
                 (elements
                  ((Frame
                    ((dimensions (1))
                     (elements ((Scalar ((element (Literal (IntLiteral 10)))))))))
                   (Frame
                    ((dimensions (1))
                     (elements ((Scalar ((element (Literal (IntLiteral 100)))))))))
                   (Frame
                    ((dimensions (1))
                     (elements
                      ((Scalar ((element (Literal (IntLiteral 1000))))))))))))))
              (bodyType
               (Arr
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 0) (refs ((((name r) (id 3)) 1)))))
                   (Add ((const 0) (refs ((((name c) (id 4)) 1))))))))))))))))))))
    Type: (Array
     (Arr
      ((element
        (Sigma
         ((parameters
           (((binding ((name r) (id 3))) (bound Dim))
            ((binding ((name c) (id 4))) (bound Dim))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape
              ((Add ((const 0) (refs ((((name r) (id 3)) 1)))))
               (Add ((const 0) (refs ((((name c) (id 4)) 1)))))))))))))
       (shape ((Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| (box ((len 3)) [int len] [8 23 0]) |};
  [%expect
    {|
    (Array
     (Frame
      ((dimensions ())
       (elements
        ((Scalar
          ((element
            (Box
             ((indices ((Dimension ((const 3) (refs ())))))
              (body
               (Frame
                ((dimensions (3))
                 (elements
                  ((Scalar ((element (Literal (IntLiteral 8)))))
                   (Scalar ((element (Literal (IntLiteral 23)))))
                   (Scalar ((element (Literal (IntLiteral 0))))))))))
              (bodyType
               (Arr
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 0) (refs ((((name len) (id 3)) 1))))))))))))))))))))
    Type: (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding ((name len) (id 3))) (bound Dim))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ((((name len) (id 3)) 1)))))))))))))
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
     (Frame
      ((dimensions (3))
       (elements
        ((Scalar
          ((element
            (Box
             ((indices
               ((Shape ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))
              (body
               (Frame
                ((dimensions (2))
                 (elements
                  ((Frame
                    ((dimensions (2))
                     (elements
                      ((Scalar ((element (Literal (IntLiteral 1)))))
                       (Scalar ((element (Literal (IntLiteral 2)))))))))
                   (Frame
                    ((dimensions (2))
                     (elements
                      ((Scalar ((element (Literal (IntLiteral 3)))))
                       (Scalar ((element (Literal (IntLiteral 4))))))))))))))
              (bodyType
               (Arr
                ((element (Literal IntLiteral))
                 (shape ((ShapeRef ((name @shape) (id 3)))))))))))))
         (Scalar
          ((element
            (Box
             ((indices
               ((Shape ((Add ((const 1) (refs ()))) (Add ((const 3) (refs ())))))))
              (body
               (Frame
                ((dimensions (1))
                 (elements
                  ((Frame
                    ((dimensions (3))
                     (elements
                      ((Scalar ((element (Literal (IntLiteral 10)))))
                       (Scalar ((element (Literal (IntLiteral 100)))))
                       (Scalar ((element (Literal (IntLiteral 1000))))))))))))))
              (bodyType
               (Arr
                ((element (Literal IntLiteral))
                 (shape ((ShapeRef ((name @shape) (id 3)))))))))))))
         (Scalar
          ((element
            (Box
             ((indices
               ((Shape ((Add ((const 3) (refs ()))) (Add ((const 1) (refs ())))))))
              (body
               (Frame
                ((dimensions (3))
                 (elements
                  ((Frame
                    ((dimensions (1))
                     (elements ((Scalar ((element (Literal (IntLiteral 10)))))))))
                   (Frame
                    ((dimensions (1))
                     (elements ((Scalar ((element (Literal (IntLiteral 100)))))))))
                   (Frame
                    ((dimensions (1))
                     (elements
                      ((Scalar ((element (Literal (IntLiteral 1000))))))))))))))
              (bodyType
               (Arr
                ((element (Literal IntLiteral))
                 (shape ((ShapeRef ((name @shape) (id 3))))))))))))))))))
    Type: (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding ((name @shape) (id 3))) (bound Shape))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape ((ShapeRef ((name @shape) (id 3)))))))))))
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
          (elements
           ((Scalar ((element (BuiltInFunction ((func Add))))))
            (Scalar ((element (BuiltInFunction ((func Sub)))))))))))
       (args
        ((Scalar ((element (Literal (IntLiteral 1)))))
         (Scalar ((element (Literal (IntLiteral 2))))))))))
    Type: (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| ([+ -] [1 2] 3) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func
        (Frame
         ((dimensions (2))
          (elements
           ((Scalar ((element (BuiltInFunction ((func Add))))))
            (Scalar ((element (BuiltInFunction ((func Sub)))))))))))
       (args
        ((Frame
          ((dimensions (2))
           (elements
            ((Scalar ((element (Literal (IntLiteral 1)))))
             (Scalar ((element (Literal (IntLiteral 2)))))))))
         (Scalar ((element (Literal (IntLiteral 3))))))))))
    Type: (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| ([+ -] [1 2] [3 4]) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func
        (Frame
         ((dimensions (2))
          (elements
           ((Scalar ((element (BuiltInFunction ((func Add))))))
            (Scalar ((element (BuiltInFunction ((func Sub)))))))))))
       (args
        ((Frame
          ((dimensions (2))
           (elements
            ((Scalar ((element (Literal (IntLiteral 1)))))
             (Scalar ((element (Literal (IntLiteral 2)))))))))
         (Frame
          ((dimensions (2))
           (elements
            ((Scalar ((element (Literal (IntLiteral 3)))))
             (Scalar ((element (Literal (IntLiteral 4))))))))))))))
    Type: (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))) |}];
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
      ((binding ((name foo) (id 3)))
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding ((name x) (id 4)))
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 2) (refs ())))))))))))
             (body (Ref ((id ((name x) (id 4))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name foo) (id 3))))))
          (args
           ((Frame
             ((dimensions (2))
              (elements
               ((Scalar ((element (Literal (IntLiteral 1)))))
                (Scalar ((element (Literal (IntLiteral 2)))))))))))))))))
    Type: (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {|
    (define (foo [x [int 2]]) 5)
    (foo [1 2])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name foo) (id 3)))
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding ((name x) (id 4)))
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 2) (refs ())))))))))))
             (body (Scalar ((element (Literal (IntLiteral 5))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name foo) (id 3))))))
          (args
           ((Frame
             ((dimensions (2))
              (elements
               ((Scalar ((element (Literal (IntLiteral 1)))))
                (Scalar ((element (Literal (IntLiteral 2)))))))))))))))))
    Type: (Array (Arr ((element (Literal IntLiteral)) (shape ())))) |}];
  checkAndPrint {|
    (define (foo [x [int 2]]) 5)
    (foo [[1 2] [3 4]])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name foo) (id 3)))
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding ((name x) (id 4)))
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 2) (refs ())))))))))))
             (body (Scalar ((element (Literal (IntLiteral 5))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name foo) (id 3))))))
          (args
           ((Frame
             ((dimensions (2))
              (elements
               ((Frame
                 ((dimensions (2))
                  (elements
                   ((Scalar ((element (Literal (IntLiteral 1)))))
                    (Scalar ((element (Literal (IntLiteral 2)))))))))
                (Frame
                 ((dimensions (2))
                  (elements
                   ((Scalar ((element (Literal (IntLiteral 3)))))
                    (Scalar ((element (Literal (IntLiteral 4)))))))))))))))))))))
    Type: (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {|
    (define (foo [x [int 2]]) 5)
    (foo [[1 2 3] [4 5 6]])
    |};
  [%expect {| Error: Function expected argument with cell shape `[2]`, got `[2 3]` |}];
  checkAndPrint
    {|
    (define left (t-fn (@t) (fn ([l @t] [r @t]) l)))
    (define right (t-fn (@t) (fn ([l @t] [r @t]) r)))
    (define id (t-fn (@t) (fn ([x @t]) x)))
    (define funs ((t-app id [(∀ (@t) (→ (@t @t) @t)) 2]) [left right]))

    (define foo ((t-app funs int) 1 2))
    (define bar ((t-app funs [char 2]) "hi" "ih"))

    funs
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name left) (id 3)))
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding ((name @t) (id 4))) (bound Array))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name l) (id 5)))
                      (bound (ArrayRef ((name @t) (id 4)))))
                     ((binding ((name r) (id 6)))
                      (bound (ArrayRef ((name @t) (id 4)))))))
                   (body (Ref ((id ((name l) (id 5))))))))))))))))))
       (body
        (Let
         ((binding ((name right) (id 7)))
          (value
           (Scalar
            ((element
              (TypeLambda
               ((params (((binding ((name @t) (id 8))) (bound Array))))
                (body
                 (Scalar
                  ((element
                    (TermLambda
                     ((params
                       (((binding ((name l) (id 9)))
                         (bound (ArrayRef ((name @t) (id 8)))))
                        ((binding ((name r) (id 10)))
                         (bound (ArrayRef ((name @t) (id 8)))))))
                      (body (Ref ((id ((name r) (id 10))))))))))))))))))
          (body
           (Let
            ((binding ((name id) (id 11)))
             (value
              (Scalar
               ((element
                 (TypeLambda
                  ((params (((binding ((name @t) (id 12))) (bound Array))))
                   (body
                    (Scalar
                     ((element
                       (TermLambda
                        ((params
                          (((binding ((name x) (id 13)))
                            (bound (ArrayRef ((name @t) (id 12)))))))
                         (body (Ref ((id ((name x) (id 13))))))))))))))))))
             (body
              (Let
               ((binding ((name funs) (id 14)))
                (value
                 (TermApplication
                  ((func
                    (TypeApplication
                     ((tFunc (Ref ((id ((name id) (id 11))))))
                      (args
                       ((Array
                         (Arr
                          ((element
                            (Forall
                             ((parameters
                               (((binding ((name @t) (id 15))) (bound Array))))
                              (body
                               (Arr
                                ((element
                                  (Func
                                   ((parameters
                                     ((ArrayRef ((name @t) (id 15)))
                                      (ArrayRef ((name @t) (id 15)))))
                                    (return (ArrayRef ((name @t) (id 15)))))))
                                 (shape ())))))))
                           (shape ((Add ((const 2) (refs ())))))))))))))
                   (args
                    ((Frame
                      ((dimensions (2))
                       (elements
                        ((Ref ((id ((name left) (id 3)))))
                         (Ref ((id ((name right) (id 7))))))))))))))
                (body
                 (Let
                  ((binding ((name foo) (id 16)))
                   (value
                    (TermApplication
                     ((func
                       (TypeApplication
                        ((tFunc (Ref ((id ((name funs) (id 14))))))
                         (args
                          ((Array
                            (Arr ((element (Literal IntLiteral)) (shape ())))))))))
                      (args
                       ((Scalar ((element (Literal (IntLiteral 1)))))
                        (Scalar ((element (Literal (IntLiteral 2))))))))))
                   (body
                    (Let
                     ((binding ((name bar) (id 17)))
                      (value
                       (TermApplication
                        ((func
                          (TypeApplication
                           ((tFunc (Ref ((id ((name funs) (id 14))))))
                            (args
                             ((Array
                               (Arr
                                ((element (Literal CharacterLiteral))
                                 (shape ((Add ((const 2) (refs ())))))))))))))
                         (args
                          ((Frame
                            ((dimensions (2))
                             (elements
                              ((Scalar
                                ((element (Literal (CharacterLiteral h)))))
                               (Scalar
                                ((element (Literal (CharacterLiteral i)))))))))
                           (Frame
                            ((dimensions (2))
                             (elements
                              ((Scalar
                                ((element (Literal (CharacterLiteral i)))))
                               (Scalar
                                ((element (Literal (CharacterLiteral h))))))))))))))
                      (body (Ref ((id ((name funs) (id 14))))))))))))))))))))))))
    Type: (Array
     (Arr
      ((element
        (Forall
         ((parameters (((binding ((name @t) (id 15))) (bound Array))))
          (body
           (Arr
            ((element
              (Func
               ((parameters
                 ((ArrayRef ((name @t) (id 15))) (ArrayRef ((name @t) (id 15)))))
                (return (ArrayRef ((name @t) (id 15)))))))
             (shape ())))))))
       (shape ((Add ((const 2) (refs ())))))))) |}]
;;
