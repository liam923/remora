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
      ((parameters (((binding @t.148) (bound Array)))) (body (ArrayRef @t.148))))) |}];
  checkAndPrint {| (Arr (Forall (@t) @t) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Forall
         ((parameters (((binding @t.148) (bound Array))))
          (body (ArrayRef @t.148)))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Forall (t) t) |};
  [%expect
    {|
    (Atom
     (Forall
      ((parameters (((binding t.148) (bound Atom))))
       (body (Arr ((element (AtomRef t.148)) (shape ()))))))) |}];
  checkAndPrint {| (Pi (@i) (Arr int @i)) |};
  [%expect
    {|
    (Atom
     (Pi
      ((parameters (((binding @i.148) (bound Shape))))
       (body (Arr ((element (Literal IntLiteral)) (shape ((ShapeRef @i.148))))))))) |}];
  checkAndPrint {| (Arr (Pi (@i) [int @i]) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Pi
         ((parameters (((binding @i.148) (bound Shape))))
          (body
           (Arr ((element (Literal IntLiteral)) (shape ((ShapeRef @i.148)))))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Pi (i) [int i i]) |};
  [%expect
    {|
    (Atom
     (Pi
      ((parameters (((binding i.148) (bound Dim))))
       (body
        (Arr
         ((element (Literal IntLiteral))
          (shape
           ((Add ((const 0) (refs ((i.148 1)))))
            (Add ((const 0) (refs ((i.148 1))))))))))))) |}];
  checkAndPrint {| (Sigma (@i) (Arr int @i)) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters (((binding @i.148) (bound Shape))))
       (body (Arr ((element (Literal IntLiteral)) (shape ((ShapeRef @i.148))))))))) |}];
  checkAndPrint {| (Arr (Sigma (@i) [int @i]) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding @i.148) (bound Shape))))
          (body
           (Arr ((element (Literal IntLiteral)) (shape ((ShapeRef @i.148)))))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Sigma (i) [int i i]) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters (((binding i.148) (bound Dim))))
       (body
        (Arr
         ((element (Literal IntLiteral))
          (shape
           ((Add ((const 0) (refs ((i.148 1)))))
            (Add ((const 0) (refs ((i.148 1))))))))))))) |}];
  checkAndPrint {| (Sigma (@i j) (Arr int [j @i 5 (+ j 10)])) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters
        (((binding @i.148) (bound Shape)) ((binding j.149) (bound Dim))))
       (body
        (Arr
         ((element (Literal IntLiteral))
          (shape
           ((Add ((const 0) (refs ((j.149 1))))) (ShapeRef @i.148)
            (Add ((const 5) (refs ()))) (Add ((const 10) (refs ((j.149 1))))))))))))) |}]
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
      ((func (Primitive ((name (Func Add)))))
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
      ((func (Primitive ((name (Func Add)))))
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
          ((func (Primitive ((name (Func Add)))))
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
      ((func (Primitive ((name (Func Add)))))
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
      ((binding foo.148) (value (Scalar ((element (Literal (IntLiteral 5))))))
       (body (Ref ((id foo.148)))))))
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
      ((binding add.148)
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding x.149)
                (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
               ((binding y.150)
                (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
             (body
              (TermApplication
               ((func (Primitive ((name (Func Add)))))
                (args ((Ref ((id x.149))) (Ref ((id y.150))))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id add.148))))
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
        (((binding x.148)
          (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
       (body (Ref ((id x.148)))))))
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
        (((binding x.148)
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
      ((binding foo.148)
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding x.149)
                (bound (Arr ((element (Literal BooleanLiteral)) (shape ())))))))
             (body (Ref ((id x.149))))))))))
       (body
        (TermApplication
         ((func (Ref ((id foo.148))))
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
      ((binding id.148)
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding @t.149) (bound Array))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params (((binding x.150) (bound (ArrayRef @t.149)))))
                   (body (Ref ((id x.150))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id id.148))))
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
      ((binding id.148)
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding t.149) (bound Atom))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding x.150)
                      (bound
                       (Arr
                        ((element (AtomRef t.149))
                         (shape ((Add ((const 2) (refs ())))))))))))
                   (body (Ref ((id x.150))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id id.148)))) (args ((Atom (Literal IntLiteral)))))))
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
      ((binding id.148)
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding @t.149) (bound Array))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params (((binding x.150) (bound (ArrayRef @t.149)))))
                   (body (Ref ((id x.150))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id id.148))))
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
      ((binding id.148)
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding @t.149) (bound Array))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params (((binding x.150) (bound (ArrayRef @t.149)))))
                   (body (Ref ((id x.150))))))))))))))))
       (body
        (TermApplication
         ((func
           (TypeApplication
            ((tFunc (Ref ((id id.148))))
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
      ((binding foo.148)
       (value
        (Scalar
         ((element
           (IndexLambda
            ((params (((binding @i.149) (bound Shape))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding x.150)
                      (bound
                       (Arr
                        ((element (Literal IntLiteral))
                         (shape ((Add ((const 1) (refs ()))) (ShapeRef @i.149)))))))))
                   (body (Ref ((id x.150))))))))))))))))
       (body
        (TermApplication
         ((func
           (IndexApplication
            ((iFunc (Ref ((id foo.148))))
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
      ((binding x.148) (value (Scalar ((element (Literal (IntLiteral 5))))))
       (body (Ref ((id x.148)))))))
    Type:
    (Array (Arr ((element (Literal IntLiteral)) (shape ()))))

    (Array
     (Let
      ((binding foo.148)
       (value
        (Scalar
         ((element
           (IndexLambda
            ((params (((binding i.149) (bound Dim))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding x.150)
                      (bound
                       (Arr
                        ((element (Literal IntLiteral))
                         (shape
                          ((Add ((const 1) (refs ())))
                           (Add ((const 0) (refs ((i.149 1)))))))))))))
                   (body (Ref ((id x.150))))))))))))))))
       (body
        (TermApplication
         ((func
           (IndexApplication
            ((iFunc (Ref ((id foo.148))))
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
      ((binding x.148)
       (value
        (Frame
         ((dimensions (5))
          (elements
           ((Scalar ((element (Literal (CharacterLiteral h)))))
            (Scalar ((element (Literal (CharacterLiteral e)))))
            (Scalar ((element (Literal (CharacterLiteral l)))))
            (Scalar ((element (Literal (CharacterLiteral l)))))
            (Scalar ((element (Literal (CharacterLiteral o))))))))))
       (body (Ref ((id x.148)))))))
    Type:
    (Array
     (Arr
      ((element (Literal CharacterLiteral))
       (shape ((Add ((const 5) (refs ())))))))) |}];
  checkAndPrint {| (define x : int "hello") x |};
  [%expect {|
    Error: Let expected a value of type `int`, got `[char 5]` |}];
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
      ((binding weekdays.148)
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
                    (shape ((Add ((const 0) (refs ((len.149 1)))))))))))))))
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
                    (shape ((Add ((const 0) (refs ((len.149 1)))))))))))))))
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
                    (shape ((Add ((const 0) (refs ((len.149 1)))))))))))))))
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
                    (shape ((Add ((const 0) (refs ((len.149 1)))))))))))))))
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
                    (shape ((Add ((const 0) (refs ((len.149 1))))))))))))))))))))
       (body
        (Unbox
         ((indexBindings ((len.150 Dim))) (valueBinding day.151)
          (box (Ref ((id weekdays.148))))
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
                          (((binding d.1) (bound Dim))
                           ((binding @cell-shape.2) (bound Shape))))
                         (body
                          (Scalar
                           ((element
                             (TypeLambda
                              ((params (((binding t.3) (bound Atom))))
                               (body
                                (Scalar
                                 ((element
                                   (TermLambda
                                    ((params
                                      (((binding arr.4)
                                        (bound
                                         (Arr
                                          ((element (AtomRef t.3))
                                           (shape
                                            ((Add ((const 0) (refs ((d.1 1)))))
                                             (ShapeRef @cell-shape.2)))))))))
                                     (body
                                      (ReifyIndex
                                       ((index
                                         (Dimension ((const 0) (refs ((d.1 1)))))))))))))))))))))))))))
                   (args
                    ((Dimension ((const 0) (refs ((len.150 1))))) (Shape ()))))))
                (args ((Atom (Literal CharacterLiteral)))))))
             (args ((Ref ((id day.151))))))))))))))
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
                  ((Add ((const 0) (refs ((r.148 1)))))
                   (Add ((const 0) (refs ((c.149 1)))))))))))))))
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
                  ((Add ((const 0) (refs ((r.148 1)))))
                   (Add ((const 0) (refs ((c.149 1)))))))))))))))
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
                  ((Add ((const 0) (refs ((r.148 1)))))
                   (Add ((const 0) (refs ((c.149 1))))))))))))))))))))
    Type:
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters
           (((binding r.148) (bound Dim)) ((binding c.149) (bound Dim))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape
              ((Add ((const 0) (refs ((r.148 1)))))
               (Add ((const 0) (refs ((c.149 1)))))))))))))
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
                 (shape ((Add ((const 0) (refs ((len.148 1))))))))))))))))))))
    Type:
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding len.148) (bound Dim))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ((len.148 1)))))))))))))
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
                ((element (Literal IntLiteral)) (shape ((ShapeRef @shape.148)))))))))))
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
                ((element (Literal IntLiteral)) (shape ((ShapeRef @shape.148)))))))))))
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
                ((element (Literal IntLiteral)) (shape ((ShapeRef @shape.148))))))))))))))))
    Type:
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding @shape.148) (bound Shape))))
          (body
           (Arr ((element (Literal IntLiteral)) (shape ((ShapeRef @shape.148)))))))))
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
           ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub)))))))))
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
          (elements
           ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub)))))))))
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
          (elements
           ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub)))))))))
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
      ((binding foo.148)
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding x.149)
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 2) (refs ())))))))))))
             (body (Ref ((id x.149))))))))))
       (body
        (TermApplication
         ((func (Ref ((id foo.148))))
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
      ((binding foo.148)
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding x.149)
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 2) (refs ())))))))))))
             (body (Scalar ((element (Literal (IntLiteral 5))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id foo.148))))
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
      ((binding foo.148)
       (value
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding x.149)
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 2) (refs ())))))))))))
             (body (Scalar ((element (Literal (IntLiteral 5))))))))))))
       (body
        (TermApplication
         ((func (Ref ((id foo.148))))
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
      ((binding left.148)
       (value
        (Scalar
         ((element
           (TypeLambda
            ((params (((binding @t.149) (bound Array))))
             (body
              (Scalar
               ((element
                 (TermLambda
                  ((params
                    (((binding l.150) (bound (ArrayRef @t.149)))
                     ((binding r.151) (bound (ArrayRef @t.149)))))
                   (body (Ref ((id l.150))))))))))))))))
       (body
        (Let
         ((binding right.152)
          (value
           (Scalar
            ((element
              (TypeLambda
               ((params (((binding @t.153) (bound Array))))
                (body
                 (Scalar
                  ((element
                    (TermLambda
                     ((params
                       (((binding l.154) (bound (ArrayRef @t.153)))
                        ((binding r.155) (bound (ArrayRef @t.153)))))
                      (body (Ref ((id r.155))))))))))))))))
          (body
           (Let
            ((binding id.156)
             (value
              (Scalar
               ((element
                 (TypeLambda
                  ((params (((binding @t.157) (bound Array))))
                   (body
                    (Scalar
                     ((element
                       (TermLambda
                        ((params (((binding x.158) (bound (ArrayRef @t.157)))))
                         (body (Ref ((id x.158))))))))))))))))
             (body
              (Let
               ((binding funs.159)
                (value
                 (TermApplication
                  ((func
                    (TypeApplication
                     ((tFunc (Ref ((id id.156))))
                      (args
                       ((Array
                         (Arr
                          ((element
                            (Forall
                             ((parameters (((binding @t.160) (bound Array))))
                              (body
                               (Arr
                                ((element
                                  (Func
                                   ((parameters
                                     ((ArrayRef @t.160) (ArrayRef @t.160)))
                                    (return (ArrayRef @t.160)))))
                                 (shape ())))))))
                           (shape ((Add ((const 2) (refs ())))))))))))))
                   (args
                    ((Frame
                      ((dimensions (2))
                       (elements ((Ref ((id left.148))) (Ref ((id right.152))))))))))))
                (body
                 (Let
                  ((binding foo.161)
                   (value
                    (TermApplication
                     ((func
                       (TypeApplication
                        ((tFunc (Ref ((id funs.159))))
                         (args
                          ((Array
                            (Arr ((element (Literal IntLiteral)) (shape ())))))))))
                      (args
                       ((Scalar ((element (Literal (IntLiteral 1)))))
                        (Scalar ((element (Literal (IntLiteral 2))))))))))
                   (body
                    (Let
                     ((binding bar.162)
                      (value
                       (TermApplication
                        ((func
                          (TypeApplication
                           ((tFunc (Ref ((id funs.159))))
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
                      (body (Ref ((id funs.159))))))))))))))))))))))
    Type:
    (Array
     (Arr
      ((element
        (Forall
         ((parameters (((binding @t.160) (bound Array))))
          (body
           (Arr
            ((element
              (Func
               ((parameters ((ArrayRef @t.160) (ArrayRef @t.160)))
                (return (ArrayRef @t.160)))))
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
     (ContiguousSubArray
      ((arrayArg
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
       (indexArg (Frame ((dimensions (0)) (elements ()))))
       (originalShape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))
       (resultShape
        ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
         (Add ((const 3) (refs ())))))
       (cellShape ()) (l ((const 0) (refs ()))))))
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
      ((params (((binding l.148) (bound Dim))))
       (body
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding x.149)
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape
                    ((Add ((const 4) (refs ())))
                     (Add ((const 0) (refs ((l.148 1)))))
                     (Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))))))
             (body
              (ContiguousSubArray
               ((arrayArg (Ref ((id x.149))))
                (indexArg (Frame ((dimensions (0)) (elements ()))))
                (originalShape
                 ((Add ((const 4) (refs ())))
                  (Add ((const 0) (refs ((l.148 1)))))
                  (Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))
                (resultShape
                 ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))
                  (Add ((const 0) (refs ((l.148 1)))))
                  (Add ((const 4) (refs ())))))
                (cellShape ()) (l ((const 0) (refs ())))))))))))))))
    Type:
    (Atom
     (Pi
      ((parameters (((binding l.148) (bound Dim))))
       (body
        (Arr
         ((element
           (Func
            ((parameters
              ((Arr
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 4) (refs ())))
                   (Add ((const 0) (refs ((l.148 1)))))
                   (Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))))
             (return
              (Arr
               ((element (Literal IntLiteral))
                (shape
                 ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))
                  (Add ((const 0) (refs ((l.148 1)))))
                  (Add ((const 4) (refs ())))))))))))
          (shape ()))))))) |}];
  checkAndPrint {| (i-fn (@l) (fn ([x [int 4 @l 2 2]]) (reshape [2 2 @l 4] x))) |};
  [%expect
    {|
    (Atom
     (IndexLambda
      ((params (((binding @l.148) (bound Shape))))
       (body
        (Scalar
         ((element
           (TermLambda
            ((params
              (((binding x.149)
                (bound
                 (Arr
                  ((element (Literal IntLiteral))
                   (shape
                    ((Add ((const 4) (refs ()))) (ShapeRef @l.148)
                     (Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))))))
             (body
              (ContiguousSubArray
               ((arrayArg (Ref ((id x.149))))
                (indexArg (Frame ((dimensions (0)) (elements ()))))
                (originalShape
                 ((Add ((const 4) (refs ()))) (ShapeRef @l.148)
                  (Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))
                (resultShape
                 ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))
                  (ShapeRef @l.148) (Add ((const 4) (refs ())))))
                (cellShape ()) (l ((const 0) (refs ())))))))))))))))
    Type:
    (Atom
     (Pi
      ((parameters (((binding @l.148) (bound Shape))))
       (body
        (Arr
         ((element
           (Func
            ((parameters
              ((Arr
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 4) (refs ()))) (ShapeRef @l.148)
                   (Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))))
             (return
              (Arr
               ((element (Literal IntLiteral))
                (shape
                 ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))
                  (ShapeRef @l.148) (Add ((const 4) (refs ())))))))))))
          (shape ()))))))) |}];
  checkAndPrint {| (i-fn (@l) (fn ([x [int 4 @l 2 2]]) (reshape [2 @l 4] x))) |};
  [%expect
    {| Error: Array has shape `[4 @l 2 2]`, which is not compatible with wanted shape `[2 @l 4]` |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (Array
     (Lift
      ((indexBinding i.148)
       (indexValue
        (Frame
         ((dimensions (3))
          (elements
           ((Scalar ((element (Literal (IntLiteral 1)))))
            (Scalar ((element (Literal (IntLiteral 2)))))
            (Scalar ((element (Literal (IntLiteral 3))))))))))
       (sort Dim) (frameShape ((Add ((const 3) (refs ())))))
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
                       (((binding @s.6) (bound Shape))
                        ((binding @cell-shape.7) (bound Shape))))
                      (body
                       (Scalar
                        ((element
                          (TypeLambda
                           ((params (((binding t.8) (bound Atom))))
                            (body
                             (Scalar
                              ((element
                                (TermLambda
                                 ((params
                                   (((binding v.9)
                                     (bound
                                      (Arr
                                       ((element (AtomRef t.8))
                                        (shape ((ShapeRef @cell-shape.7)))))))))
                                  (body
                                   (Let
                                    ((binding make.10)
                                     (value
                                      (Scalar
                                       ((element
                                         (TermLambda
                                          ((params
                                            (((binding foo.11)
                                              (bound
                                               (Arr
                                                ((element (Literal IntLiteral))
                                                 (shape ())))))
                                             ((binding v.12)
                                              (bound
                                               (Arr
                                                ((element (AtomRef t.8))
                                                 (shape
                                                  ((ShapeRef @cell-shape.7)))))))))
                                           (body (Ref ((id v.12))))))))))
                                     (body
                                      (TermApplication
                                       ((func (Ref ((id make.10))))
                                        (args
                                         ((IndexApplication
                                           ((iFunc
                                             (Primitive ((name (Val Iota)))))
                                            (args ((Shape ((ShapeRef @s.6)))))))
                                          (Ref ((id v.9)))))))))))))))))))))))))))))
                (args
                 ((Shape ((Add ((const 0) (refs ((i.148 1))))))) (Shape ()))))))
             (args ((Atom (Literal IntLiteral)))))))
          (args ((Scalar ((element (Literal (IntLiteral 5)))))))))))))
    Type:
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding i.148) (bound Dim))))
          (body
           (Arr
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ((i.148 1)))))))))))))
       (shape ((Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {|
    (lift [@i [1 2 3]]
      (replicate{int | @i []} 5))
    |};
  [%expect
    {|
    (Array
     (Lift
      ((indexBinding @i.148)
       (indexValue
        (Frame
         ((dimensions (3))
          (elements
           ((Scalar ((element (Literal (IntLiteral 1)))))
            (Scalar ((element (Literal (IntLiteral 2)))))
            (Scalar ((element (Literal (IntLiteral 3))))))))))
       (sort Shape) (frameShape ())
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
                       (((binding @s.6) (bound Shape))
                        ((binding @cell-shape.7) (bound Shape))))
                      (body
                       (Scalar
                        ((element
                          (TypeLambda
                           ((params (((binding t.8) (bound Atom))))
                            (body
                             (Scalar
                              ((element
                                (TermLambda
                                 ((params
                                   (((binding v.9)
                                     (bound
                                      (Arr
                                       ((element (AtomRef t.8))
                                        (shape ((ShapeRef @cell-shape.7)))))))))
                                  (body
                                   (Let
                                    ((binding make.10)
                                     (value
                                      (Scalar
                                       ((element
                                         (TermLambda
                                          ((params
                                            (((binding foo.11)
                                              (bound
                                               (Arr
                                                ((element (Literal IntLiteral))
                                                 (shape ())))))
                                             ((binding v.12)
                                              (bound
                                               (Arr
                                                ((element (AtomRef t.8))
                                                 (shape
                                                  ((ShapeRef @cell-shape.7)))))))))
                                           (body (Ref ((id v.12))))))))))
                                     (body
                                      (TermApplication
                                       ((func (Ref ((id make.10))))
                                        (args
                                         ((IndexApplication
                                           ((iFunc
                                             (Primitive ((name (Val Iota)))))
                                            (args ((Shape ((ShapeRef @s.6)))))))
                                          (Ref ((id v.9)))))))))))))))))))))))))))))
                (args ((Shape ((ShapeRef @i.148))) (Shape ()))))))
             (args ((Atom (Literal IntLiteral)))))))
          (args ((Scalar ((element (Literal (IntLiteral 5)))))))))))))
    Type:
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding @i.148) (bound Shape))))
          (body
           (Arr ((element (Literal IntLiteral)) (shape ((ShapeRef @i.148)))))))))
       (shape ())))) |}]
;;
