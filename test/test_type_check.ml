open! Base
open Remora

let%expect_test "check sort" =
  let pipeline =
    CompilerPipeline.(
      let module Parse = Parse.Make (Source.UnitBuilder) in
      (module Parse.IndexParser.Stage)
      @> (module TypeCheckStage.Sort (Source.UnitBuilder))
      @> (module Show.Stage (Typed.Index) (Source.UnitBuilder))
      @> empty)
  in
  let checkAndPrint = TestPipeline.runAndPrint pipeline in
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
  let pipeline =
    CompilerPipeline.(
      let module Parse = Parse.Make (Source.UnitBuilder) in
      (module Parse.TypeParser.Stage)
      @> (module TypeCheckStage.Kind (Source.UnitBuilder))
      @> (module Show.Stage (Typed.Type) (Source.UnitBuilder))
      @> empty)
  in
  let checkAndPrint = TestPipeline.runAndPrint pipeline in
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
      ((parameters (((binding ((name @t) (id 20))) (bound Array))))
       (body (ArrayRef ((name @t) (id 20))))))) |}];
  checkAndPrint {| (Arr (Forall (@t) @t) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Forall
         ((parameters (((binding ((name @t) (id 20))) (bound Array))))
          (body (ArrayRef ((name @t) (id 20)))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Forall (t) t) |};
  [%expect
    {|
    (Atom
     (Forall
      ((parameters (((binding ((name t) (id 20))) (bound Atom))))
       (body (Arr ((element (AtomRef ((name t) (id 20)))) (shape ()))))))) |}];
  checkAndPrint {| (Pi (@i) (Arr int @i)) |};
  [%expect
    {|
    (Atom
     (Pi
      ((parameters (((binding ((name @i) (id 20))) (bound Shape))))
       (body
        (Arr
         ((element (Literal IntLiteral))
          (shape ((ShapeRef ((name @i) (id 20))))))))))) |}];
  checkAndPrint {| (Arr (Pi (@i) [int @i]) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Pi
         ((parameters (((binding ((name @i) (id 20))) (bound Shape))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape ((ShapeRef ((name @i) (id 20)))))))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Pi (i) [int i i]) |};
  [%expect
    {|
    (Atom
     (Pi
      ((parameters (((binding ((name i) (id 20))) (bound Dim))))
       (body
        (Arr
         ((element (Literal IntLiteral))
          (shape
           ((Add ((const 0) (refs ((((name i) (id 20)) 1)))))
            (Add ((const 0) (refs ((((name i) (id 20)) 1))))))))))))) |}];
  checkAndPrint {| (Sigma (@i) (Arr int @i)) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters (((binding ((name @i) (id 20))) (bound Shape))))
       (body
        (Arr
         ((element (Literal IntLiteral))
          (shape ((ShapeRef ((name @i) (id 20))))))))))) |}];
  checkAndPrint {| (Arr (Sigma (@i) [int @i]) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding ((name @i) (id 20))) (bound Shape))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape ((ShapeRef ((name @i) (id 20)))))))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Sigma (i) [int i i]) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters (((binding ((name i) (id 20))) (bound Dim))))
       (body
        (Arr
         ((element (Literal IntLiteral))
          (shape
           ((Add ((const 0) (refs ((((name i) (id 20)) 1)))))
            (Add ((const 0) (refs ((((name i) (id 20)) 1))))))))))))) |}];
  checkAndPrint {| (Sigma (@i j) (Arr int [j @i 5 (+ j 10)])) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters
        (((binding ((name @i) (id 20))) (bound Shape))
         ((binding ((name j) (id 21))) (bound Dim))))
       (body
        (Arr
         ((element (Literal IntLiteral))
          (shape
           ((Add ((const 0) (refs ((((name j) (id 21)) 1)))))
            (ShapeRef ((name @i) (id 20))) (Add ((const 5) (refs ())))
            (Add ((const 10) (refs ((((name j) (id 21)) 1))))))))))))) |}];
  checkAndPrint {| (Tuple int char) |};
  [%expect {| (Atom (Tuple ((Literal IntLiteral) (Literal CharacterLiteral)))) |}]
;;

let%expect_test "check type" =
  let pipeline =
    CompilerPipeline.(
      let module Parse = Parse.Make (Source.UnitBuilder) in
      (module Parse.Stage)
      @> (module TypeCheckStage.Type (Source.UnitBuilder))
      @> (module Show.CustomStage
                   (struct
                     type t = Typed.Expr.t

                     let to_string expr =
                       String.concat_lines
                         [ Sexp.to_string_hum ([%sexp_of: Typed.Expr.t] expr)
                         ; "Type:"
                         ; Sexp.to_string_hum
                             ([%sexp_of: Typed.Type.t] (Typed.Expr.type' expr))
                         ]
                     ;;
                   end)
                   (Source.UnitBuilder))
      @> empty)
  in
  let checkAndPrint = TestPipeline.runAndPrint pipeline in
  checkAndPrint {| 5 |};
  [%expect
    {|
    (Atom (Literal (IntLiteral 5)))
    Type:
    (Atom (Literal IntLiteral)) |}];
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
    Type:
    (Array
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
    Type:
    (Array
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
      ((func (Primitive ((func Add))))
       (args
        ((Scalar ((element (Literal (IntLiteral 1)))))
         (Scalar ((element (Literal (IntLiteral 2))))))))))
    Type:
    (Array (Arr ((element (Literal IntLiteral)) (shape ())))) |}];
  checkAndPrint {| (+ [1 2 3] [4 5 6]) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func (Primitive ((func Add))))
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
    Type:
    (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| [(+ [1 2 3] [4 5 6]) [7 8 9]] |};
  [%expect
    {|
    (Array
     (Frame
      ((dimensions (2))
       (elements
        ((TermApplication
          ((func (Primitive ((func Add))))
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
    Type:
    (Array
     (Arr
      ((element (Literal IntLiteral))
       (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| (+ [1 2 3 4 5] 6) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func (Primitive ((func Add))))
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
    Type:
    (Array
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
      ((binding ((name foo) (id 20)))
       (value (Scalar ((element (Literal (IntLiteral 5))))))
       (body (Ref ((id ((name foo) (id 20)))))))))
    Type:
    (Array (Arr ((element (Literal IntLiteral)) (shape ())))) |}];
  checkAndPrint {|
  (define (add [x int] [y int])
    (+ x y))
  (add 1 2)
  |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name add) (id 20)))
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding ((name x) (id 21)))
                (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
               ((binding ((name y) (id 22)))
                (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
             (body
              (TermApplication
               ((func (Primitive ((func Add))))
                (args
                 ((Ref ((id ((name x) (id 21)))))
                  (Ref ((id ((name y) (id 22))))))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name add) (id 20))))))
          (args
           ((Scalar ((element (Literal (IntLiteral 1)))))
            (Scalar ((element (Literal (IntLiteral 2)))))))))))))
    Type:
    (Array (Arr ((element (Literal IntLiteral)) (shape ())))) |}];
  checkAndPrint {| (fn ([x int]) x) |};
  [%expect
    {|
    (Atom
     (TermLambda
      ((params
        (((binding ((name x) (id 20)))
          (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
       (body (Ref ((id ((name x) (id 20)))))))))
    Type:
    (Atom
     (Func
      ((parameters ((Arr ((element (Literal IntLiteral)) (shape ())))))
       (return (Arr ((element (Literal IntLiteral)) (shape ()))))))) |}];
  checkAndPrint {| (fn ([x int]) "hello") |};
  [%expect
    {|
    (Atom
     (TermLambda
      ((params
        (((binding ((name x) (id 20)))
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
    Type:
    (Atom
     (Func
      ((parameters ((Arr ((element (Literal IntLiteral)) (shape ())))))
       (return
        (Arr
         ((element (Literal CharacterLiteral))
          (shape ((Add ((const 5) (refs ()))))))))))) |}];
  checkAndPrint {|
      (define (foo [x bool]) x)
      (foo [#t #f])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name foo) (id 20)))
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding ((name x) (id 21)))
                (bound (Arr ((element (Literal BooleanLiteral)) (shape ())))))))
             (body (Ref ((id ((name x) (id 21))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name foo) (id 20))))))
          (args
           ((Frame
             ((dimensions (2))
              (elements
               ((Scalar ((element (Literal (BooleanLiteral true)))))
                (Scalar ((element (Literal (BooleanLiteral false)))))))))))))))))
    Type:
    (Array
     (Arr
      ((element (Literal BooleanLiteral)) (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {|
    (define (id{@t | } [x @t]) x)
    (id{int | } 5)
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name id) (id 20)))
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding ((name @t) (id 21))) (bound Array))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name x) (id 22)))
                      (bound (ArrayRef ((name @t) (id 21)))))))
                   (body (Ref ((id ((name x) (id 22))))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id ((name id) (id 20))))))
             (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
          (args ((Scalar ((element (Literal (IntLiteral 5)))))))))))))
    Type:
    (Array (Arr ((element (Literal IntLiteral)) (shape ())))) |}];
  checkAndPrint {|
    (define (id{t | } [x [t 2]]) x)
    (id{int | } [1 2])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name id) (id 20)))
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding ((name t) (id 21))) (bound Atom))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name x) (id 22)))
                      (bound
                       (Arr
                        ((element (AtomRef ((name t) (id 21))))
                         (shape ((Add ((const 2) (refs ())))))))))))
                   (body (Ref ((id ((name x) (id 22))))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id ((name id) (id 20))))))
             (args ((Atom (Literal IntLiteral)))))))
          (args
           ((Frame
             ((dimensions (2))
              (elements
               ((Scalar ((element (Literal (IntLiteral 1)))))
                (Scalar ((element (Literal (IntLiteral 2)))))))))))))))))
    Type:
    (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {|
    (define (id{@t | } [x @t]) x)
    (id{[char 5] | } "hello")
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name id) (id 20)))
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding ((name @t) (id 21))) (bound Array))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name x) (id 22)))
                      (bound (ArrayRef ((name @t) (id 21)))))))
                   (body (Ref ((id ((name x) (id 22))))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id ((name id) (id 20))))))
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
    Type:
    (Array
     (Arr
      ((element (Literal CharacterLiteral))
       (shape ((Add ((const 5) (refs ())))))))) |}];
  checkAndPrint {|
    (define (id{@t | } [x @t]) x)
    (id{[char 5] | } "hello")
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name id) (id 20)))
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding ((name @t) (id 21))) (bound Array))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name x) (id 22)))
                      (bound (ArrayRef ((name @t) (id 21)))))))
                   (body (Ref ((id ((name x) (id 22))))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id ((name id) (id 20))))))
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
    Type:
    (Array
     (Arr
      ((element (Literal CharacterLiteral))
       (shape ((Add ((const 5) (refs ())))))))) |}];
  checkAndPrint {|
    (define (id{t | } [x t]) x)
    (id{[char 5] | } "hello")
    |};
  [%expect {| Error: Unexpected sort: expected `Atom`, got `Array` |}];
  checkAndPrint
    {|
    (define (foo{ | @i} [x [int 1 @i]]) x)
    (foo{ | [2]} [[1 2]])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name foo) (id 20)))
       (value
        (Scalar
         ((element
           (IndexLambda
            ((params (((binding ((name @i) (id 21))) (bound Shape))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name x) (id 22)))
                      (bound
                       (Arr
                        ((element (Literal IntLiteral))
                         (shape
                          ((Add ((const 1) (refs ())))
                           (ShapeRef ((name @i) (id 21)))))))))))
                   (body (Ref ((id ((name x) (id 22))))))))))))))))))
       (body
        (TermApplication
         ((func
           (IndexApplication
            ((iFunc (Ref ((id ((name foo) (id 20))))))
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
    Type:
    (Array
     (Arr
      ((element (Literal IntLiteral))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint
    {|
    (define (foo{ | @i} [x [int 1 @i]]) x)
    (foo{ | 2} [[1 2]])
    |};
  [%expect {| Error: Unexpected sort: expected `Dim`, got `Shape` |}];
  checkAndPrint
    {|
    (define (foo{ | @i} [x [int 1 @i]]) x)
    (foo{ | [2]} [[1 2] [3 4]])
    |};
  [%expect
    {|
    Error: Function expected argument with cell shape `[1 2]`, got `[2 2]` |}];
  checkAndPrint {| (define x : int 5) x |};
  checkAndPrint {|
    (define (foo{ | i} [x [int 1 i]]) x)
    (foo{ | 2} [[1 2]])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name x) (id 20)))
       (value (Scalar ((element (Literal (IntLiteral 5))))))
       (body (Ref ((id ((name x) (id 20)))))))))
    Type:
    (Array (Arr ((element (Literal IntLiteral)) (shape ()))))

    (Array
     (Let
      ((binding ((name foo) (id 20)))
       (value
        (Scalar
         ((element
           (IndexLambda
            ((params (((binding ((name i) (id 21))) (bound Dim))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name x) (id 22)))
                      (bound
                       (Arr
                        ((element (Literal IntLiteral))
                         (shape
                          ((Add ((const 1) (refs ())))
                           (Add ((const 0) (refs ((((name i) (id 21)) 1)))))))))))))
                   (body (Ref ((id ((name x) (id 22))))))))))))))))))
       (body
        (TermApplication
         ((func
           (IndexApplication
            ((iFunc (Ref ((id ((name foo) (id 20))))))
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
    Type:
    (Array
     (Arr
      ((element (Literal IntLiteral))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint
    {|
    (define (foo{ | i} [x [int 1 i]]) x)
    (foo{ | [1 2]} [[1 2]])
    |};
  [%expect {| Error: Unexpected sort: expected `Dim`, got `Shape` |}];
  checkAndPrint {| (define x : [char 5] "hello") x |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name x) (id 20)))
       (value
        (Frame
         ((dimensions (5))
          (elements
           ((Scalar ((element (Literal (CharacterLiteral h)))))
            (Scalar ((element (Literal (CharacterLiteral e)))))
            (Scalar ((element (Literal (CharacterLiteral l)))))
            (Scalar ((element (Literal (CharacterLiteral l)))))
            (Scalar ((element (Literal (CharacterLiteral o))))))))))
       (body (Ref ((id ((name x) (id 20)))))))))
    Type:
    (Array
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
      ((binding ((name x) (id 20)))
       (value
        (Scalar
         ((element
           (Tuple
            ((elements ((Literal (IntLiteral 5)) (Literal (IntLiteral 5))))))))))
       (body (Ref ((id ((name x) (id 20)))))))))
    Type:
    (Array
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
      (length{char | len []} day))
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name weekdays) (id 20)))
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
                    (shape ((Add ((const 0) (refs ((((name len) (id 21)) 1)))))))))))))))
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
                    (shape ((Add ((const 0) (refs ((((name len) (id 21)) 1)))))))))))))))
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
                    (shape ((Add ((const 0) (refs ((((name len) (id 21)) 1)))))))))))))))
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
                    (shape ((Add ((const 0) (refs ((((name len) (id 21)) 1)))))))))))))))
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
                    (shape ((Add ((const 0) (refs ((((name len) (id 21)) 1))))))))))))))))))))
       (body
        (Unbox
         ((indexBindings (((name len) (id 22))))
          (valueBinding ((name day) (id 23)))
          (box (Ref ((id ((name weekdays) (id 20))))))
          (body
           (TermApplication
            ((func
              (TypeApplication
               ((tFunc
                 (IndexApplication
                  ((iFunc
                    (Scalar
                     ((element
                       (IndexLambda
                        ((params
                          (((binding ((name d) (id 0))) (bound Dim))
                           ((binding ((name @cell-shape) (id 1))) (bound Shape))))
                         (body
                          (Scalar
                           ((element
                             (TypeLambda
                              ((params
                                (((binding ((name t) (id 2))) (bound Atom))))
                               (body
                                (Scalar
                                 ((element
                                   (TermLambda
                                    ((params
                                      (((binding ((name arr) (id 3)))
                                        (bound
                                         (Arr
                                          ((element (AtomRef ((name t) (id 2))))
                                           (shape
                                            ((Add
                                              ((const 0)
                                               (refs ((((name d) (id 0)) 1)))))
                                             (ShapeRef
                                              ((name @cell-shape) (id 1)))))))))))
                                     (body
                                      (ReifyIndex
                                       ((index
                                         (Dimension
                                          ((const 0)
                                           (refs ((((name d) (id 0)) 1)))))))))))))))))))))))))))
                   (args
                    ((Dimension ((const 0) (refs ((((name len) (id 22)) 1)))))
                     (Shape ()))))))
                (args ((Atom (Literal CharacterLiteral)))))))
             (args ((Ref ((id ((name day) (id 23))))))))))))))))
    Type:
    (Array
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
                  ((Add ((const 0) (refs ((((name r) (id 20)) 1)))))
                   (Add ((const 0) (refs ((((name c) (id 21)) 1)))))))))))))))
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
                  ((Add ((const 0) (refs ((((name r) (id 20)) 1)))))
                   (Add ((const 0) (refs ((((name c) (id 21)) 1)))))))))))))))
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
                  ((Add ((const 0) (refs ((((name r) (id 20)) 1)))))
                   (Add ((const 0) (refs ((((name c) (id 21)) 1))))))))))))))))))))
    Type:
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters
           (((binding ((name r) (id 20))) (bound Dim))
            ((binding ((name c) (id 21))) (bound Dim))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape
              ((Add ((const 0) (refs ((((name r) (id 20)) 1)))))
               (Add ((const 0) (refs ((((name c) (id 21)) 1)))))))))))))
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
                 (shape ((Add ((const 0) (refs ((((name len) (id 20)) 1))))))))))))))))))))
    Type:
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding ((name len) (id 20))) (bound Dim))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ((((name len) (id 20)) 1)))))))))))))
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
                 (shape ((ShapeRef ((name @shape) (id 20)))))))))))))
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
                 (shape ((ShapeRef ((name @shape) (id 20)))))))))))))
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
                 (shape ((ShapeRef ((name @shape) (id 20))))))))))))))))))
    Type:
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding ((name @shape) (id 20))) (bound Shape))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape ((ShapeRef ((name @shape) (id 20)))))))))))
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
          (elements ((Primitive ((func Add))) (Primitive ((func Sub))))))))
       (args
        ((Scalar ((element (Literal (IntLiteral 1)))))
         (Scalar ((element (Literal (IntLiteral 2))))))))))
    Type:
    (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| ([+ -] [1 2] 3) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func
        (Frame
         ((dimensions (2))
          (elements ((Primitive ((func Add))) (Primitive ((func Sub))))))))
       (args
        ((Frame
          ((dimensions (2))
           (elements
            ((Scalar ((element (Literal (IntLiteral 1)))))
             (Scalar ((element (Literal (IntLiteral 2)))))))))
         (Scalar ((element (Literal (IntLiteral 3))))))))))
    Type:
    (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| ([+ -] [1 2] [3 4]) |};
  [%expect
    {|
    (Array
     (TermApplication
      ((func
        (Frame
         ((dimensions (2))
          (elements ((Primitive ((func Add))) (Primitive ((func Sub))))))))
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
    Type:
    (Array
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
      ((binding ((name foo) (id 20)))
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding ((name x) (id 21)))
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 2) (refs ())))))))))))
             (body (Ref ((id ((name x) (id 21))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name foo) (id 20))))))
          (args
           ((Frame
             ((dimensions (2))
              (elements
               ((Scalar ((element (Literal (IntLiteral 1)))))
                (Scalar ((element (Literal (IntLiteral 2)))))))))))))))))
    Type:
    (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {|
    (define (foo [x [int 2]]) 5)
    (foo [1 2])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name foo) (id 20)))
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding ((name x) (id 21)))
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 2) (refs ())))))))))))
             (body (Scalar ((element (Literal (IntLiteral 5))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name foo) (id 20))))))
          (args
           ((Frame
             ((dimensions (2))
              (elements
               ((Scalar ((element (Literal (IntLiteral 1)))))
                (Scalar ((element (Literal (IntLiteral 2)))))))))))))))))
    Type:
    (Array (Arr ((element (Literal IntLiteral)) (shape ())))) |}];
  checkAndPrint {|
    (define (foo [x [int 2]]) 5)
    (foo [[1 2] [3 4]])
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name foo) (id 20)))
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding ((name x) (id 21)))
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 2) (refs ())))))))))))
             (body (Scalar ((element (Literal (IntLiteral 5))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id ((name foo) (id 20))))))
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
    Type:
    (Array
     (Arr ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {|
    (define (foo [x [int 2]]) 5)
    (foo [[1 2 3] [4 5 6]])
    |};
  [%expect {| Error: Function expected argument with cell shape `[2]`, got `[2 3]` |}];
  checkAndPrint
    {|
    (define (left{@t | } [l @t] [r @t]) l)
    (define (right{@t | } [l @t] [r @t]) r)
    (define (id{@t | } [x @t]) x)
    (define funs (id{[(∀ (@t) (→ (@t @t) @t)) 2] | } [left right]))

    (define foo (funs{int | } 1 2))
    (define bar (funs{[char 2] | } "hi" "ih"))

    funs
    |};
  [%expect
    {|
    (Array
     (Let
      ((binding ((name left) (id 20)))
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding ((name @t) (id 21))) (bound Array))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding ((name l) (id 22)))
                      (bound (ArrayRef ((name @t) (id 21)))))
                     ((binding ((name r) (id 23)))
                      (bound (ArrayRef ((name @t) (id 21)))))))
                   (body (Ref ((id ((name l) (id 22))))))))))))))))))
       (body
        (Let
         ((binding ((name right) (id 24)))
          (value
           (Scalar
            ((element
              (TypeLambda
               ((params (((binding ((name @t) (id 25))) (bound Array))))
                (body
                 (Scalar
                  ((element
                    (TermLambda
                     ((params
                       (((binding ((name l) (id 26)))
                         (bound (ArrayRef ((name @t) (id 25)))))
                        ((binding ((name r) (id 27)))
                         (bound (ArrayRef ((name @t) (id 25)))))))
                      (body (Ref ((id ((name r) (id 27))))))))))))))))))
          (body
           (Let
            ((binding ((name id) (id 28)))
             (value
              (Scalar
               ((element
                 (TypeLambda
                  ((params (((binding ((name @t) (id 29))) (bound Array))))
                   (body
                    (Scalar
                     ((element
                       (TermLambda
                        ((params
                          (((binding ((name x) (id 30)))
                            (bound (ArrayRef ((name @t) (id 29)))))))
                         (body (Ref ((id ((name x) (id 30))))))))))))))))))
             (body
              (Let
               ((binding ((name funs) (id 31)))
                (value
                 (TermApplication
                  ((func
                    (TypeApplication
                     ((tFunc (Ref ((id ((name id) (id 28))))))
                      (args
                       ((Array
                         (Arr
                          ((element
                            (Forall
                             ((parameters
                               (((binding ((name @t) (id 32))) (bound Array))))
                              (body
                               (Arr
                                ((element
                                  (Func
                                   ((parameters
                                     ((ArrayRef ((name @t) (id 32)))
                                      (ArrayRef ((name @t) (id 32)))))
                                    (return (ArrayRef ((name @t) (id 32)))))))
                                 (shape ())))))))
                           (shape ((Add ((const 2) (refs ())))))))))))))
                   (args
                    ((Frame
                      ((dimensions (2))
                       (elements
                        ((Ref ((id ((name left) (id 20)))))
                         (Ref ((id ((name right) (id 24))))))))))))))
                (body
                 (Let
                  ((binding ((name foo) (id 33)))
                   (value
                    (TermApplication
                     ((func
                       (TypeApplication
                        ((tFunc (Ref ((id ((name funs) (id 31))))))
                         (args
                          ((Array
                            (Arr ((element (Literal IntLiteral)) (shape ())))))))))
                      (args
                       ((Scalar ((element (Literal (IntLiteral 1)))))
                        (Scalar ((element (Literal (IntLiteral 2))))))))))
                   (body
                    (Let
                     ((binding ((name bar) (id 34)))
                      (value
                       (TermApplication
                        ((func
                          (TypeApplication
                           ((tFunc (Ref ((id ((name funs) (id 31))))))
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
                      (body (Ref ((id ((name funs) (id 31))))))))))))))))))))))))
    Type:
    (Array
     (Arr
      ((element
        (Forall
         ((parameters (((binding ((name @t) (id 32))) (bound Array))))
          (body
           (Arr
            ((element
              (Func
               ((parameters
                 ((ArrayRef ((name @t) (id 32))) (ArrayRef ((name @t) (id 32)))))
                (return (ArrayRef ((name @t) (id 32)))))))
             (shape ())))))))
       (shape ((Add ((const 2) (refs ())))))))) |}];
  checkAndPrint
    {|
    (define (id{@t | } [x @t]) x)
    (define (use-id{@t | } [x @t]) (id{@t | } x))
    5
    |};
  [%expect
    {|
    Error: Expected an Arr type, got `@t`
    Error: Expected an Arr type, got `@t`
    Error: Expected an Arr type, got `@t` |}];
  checkAndPrint {| (reshape [1 2 3] [[1 2 3] [4 5 6]]) |};
  [%expect
    {|
    (Array
     (Frame
      ((dimensions (2))
       (elements
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
    Type:
    (Array
     (Arr
      ((element (Literal IntLiteral))
       (shape
        ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
         (Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| (reshape [1 2 3] [1 2 3]) |};
  [%expect
    {| Error: Array has shape `[3]`, which is not compatible with wanted shape `[1 2 3]` |}];
  checkAndPrint {| (i-fn (l) (fn ([x [int 4 l 2 2]]) (reshape [2 2 l 4] x))) |};
  [%expect
    {|
    (Atom
     (IndexLambda
      ((params (((binding ((name l) (id 20))) (bound Dim))))
       (body
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding ((name x) (id 21)))
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape
                    ((Add ((const 4) (refs ())))
                     (Add ((const 0) (refs ((((name l) (id 20)) 1)))))
                     (Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))))))
             (body (Ref ((id ((name x) (id 21)))))))))))))))
    Type:
    (Atom
     (Pi
      ((parameters (((binding ((name l) (id 20))) (bound Dim))))
       (body
        (Arr
         ((element
           (Func
            ((parameters
              ((Arr
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 4) (refs ())))
                   (Add ((const 0) (refs ((((name l) (id 20)) 1)))))
                   (Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))))
             (return
              (Arr
               ((element (Literal IntLiteral))
                (shape
                 ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))
                  (Add ((const 0) (refs ((((name l) (id 20)) 1)))))
                  (Add ((const 4) (refs ())))))))))))
          (shape ()))))))) |}];
  checkAndPrint {| (i-fn (@l) (fn ([x [int 4 @l 2 2]]) (reshape [2 2 @l 4] x))) |};
  [%expect
    {|
    (Atom
     (IndexLambda
      ((params (((binding ((name @l) (id 20))) (bound Shape))))
       (body
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding ((name x) (id 21)))
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape
                    ((Add ((const 4) (refs ()))) (ShapeRef ((name @l) (id 20)))
                     (Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))))))
             (body (Ref ((id ((name x) (id 21)))))))))))))))
    Type:
    (Atom
     (Pi
      ((parameters (((binding ((name @l) (id 20))) (bound Shape))))
       (body
        (Arr
         ((element
           (Func
            ((parameters
              ((Arr
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 4) (refs ()))) (ShapeRef ((name @l) (id 20)))
                   (Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))))
             (return
              (Arr
               ((element (Literal IntLiteral))
                (shape
                 ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))
                  (ShapeRef ((name @l) (id 20))) (Add ((const 4) (refs ())))))))))))
          (shape ()))))))) |}];
  checkAndPrint {| (i-fn (@l) (fn ([x [int 4 @l 2 2]]) (reshape [2 @l 4] x))) |};
  [%expect
    {| Error: Array has shape `[4 @l 2 2]`, which is not compatible with wanted shape `[2 @l 4]` |}]
;;
