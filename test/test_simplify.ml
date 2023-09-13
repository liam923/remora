open! Base
open Remora

let%expect_test "check simplifying" =
  let pipeline =
    CompilerPipeline.(
      (module Parse.Stage (Source.UnitBuilder))
      @> (module TypeCheckStage.M (Source.UnitBuilder))
      @> (module Explicitize.Stage (Source.UnitBuilder))
      @> (module Inline.Stage (Source.UnitBuilder))
      @> (module Simplify.Stage (Source.UnitBuilder))
      @> (module Show.Stage (Nucleus) (Source.UnitBuilder))
      @> empty)
  in
  let checkAndPrint = TestPipeline.runAndPrint pipeline in
  checkAndPrint {| 5 |};
  [%expect
    {|
    (Scalar
     ((element (Literal (IntLiteral 5)))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| (+ 1 2) |};
  [%expect
    {|
    (Scalar
     ((element (Literal (IntLiteral 3)))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ((Add ((const 3) (refs ())))))
      (args
       (((binding ((name +arg1) (id 44)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (PrimitiveCall
        ((op Add)
         (args
          ((Ref
            ((id ((name +arg1) (id 44)))
             (type' ((element (Literal IntLiteral)) (shape ())))))
           (Scalar
            ((element (Literal (IntLiteral 4)))
             (type' ((element (Literal IntLiteral)) (shape ())))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint {|
    (t-fn (@t) "hello"){int| }
  |};
  [%expect
    {|
    (Frame
     ((dimensions (5))
      (elements
       ((Scalar
         ((element (Literal (CharacterLiteral h)))
          (type' ((element (Literal CharacterLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (CharacterLiteral e)))
          (type' ((element (Literal CharacterLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (CharacterLiteral l)))
          (type' ((element (Literal CharacterLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (CharacterLiteral l)))
          (type' ((element (Literal CharacterLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (CharacterLiteral o)))
          (type' ((element (Literal CharacterLiteral)) (shape ())))))))
      (type'
       ((element (Literal CharacterLiteral))
        (shape ((Add ((const 5) (refs ()))))))))) |}];
  checkAndPrint {|
    (define (id{@t| } [x @t]) x)
    (id{int| } 5)
  |};
  [%expect
    {|
    (Scalar
     ((element (Literal (IntLiteral 5)))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      (define id (t-fn (@t) (fn ([x @t]) x)))
      (id{(Forall (@t) (-> (@t) @t))| } id)
    |};
  [%expect
    {|
    (Scalar
     ((element (Literal UnitLiteral))
      (type' ((element (Literal UnitLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      (define id (t-fn (@t) (fn ([x @t]) x)))
      ((t-app (id{(Forall (@t) (-> (@t) @t))| } id) int) 5)
    |};
  [%expect
    {|
    (Scalar
     ((element (Literal (IntLiteral 5)))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      ((t-app (t-app (t-fn (@a) (t-fn (@b) (fn ([x int]) x))) int) int) 10)
    |};
  [%expect
    {|
    (Scalar
     ((element (Literal (IntLiteral 10)))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {|
      (length{int | 5 []} [1 2 3 4 5])
    |};
  [%expect
    {|
    (Scalar
     ((element (Literal (IntLiteral 5)))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| 
    (reduce{int | 4 [] []} + [1 2 3 4 5])
  |};
  [%expect
    {|
    (IntrinsicCall
     (Reduce
      (args
       (((firstBinding ((name reduceArg1) (id 46)))
         (secondBinding ((name reduceArg2) (id 47)))
         (value
          (Frame
           ((dimensions (5))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 4)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 5)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 5) (refs ())))))))))))))
      (zero ())
      (body
       (PrimitiveCall
        ((op Add)
         (args
          ((Ref
            ((id ((name reduceArg1) (id 46)))
             (type' ((element (Literal IntLiteral)) (shape ())))))
           (Ref
            ((id ((name reduceArg2) (id 47)))
             (type' ((element (Literal IntLiteral)) (shape ())))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (d ((const 5) (refs ()))) (itemPad ()) (cellShape ()) (associative true)
      (character Reduce) (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ((Add ((const 3) (refs ()))))) (args ())
      (body
       (Scalar
        ((element (Literal (IntLiteral 5)))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      (+
        (length{int | 5 []} [1 2 3 4 5])
        (length{char | 2 [2]} ["hi" "ih"]))
    |};
  [%expect
    {|
    (Scalar
     ((element (Literal (IntLiteral 7)))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      (t-app
        (reduce{(Forall (@t) int) | 1 [] []}
          (fn ([a (Forall (@a) int)] [b (Forall (@b) int)])
            (define sum (+ (t-app a char) (t-app b int)))
            (t-fn (@u) sum))
          [(t-fn (@x) 1) (t-fn (@y) 2)])
        int)
    |};
  [%expect
    {|
    (IntrinsicCall
     (Reduce
      (args
       (((firstBinding ((name reduce-arg1) (id 67)))
         (secondBinding ((name reduceArg2) (id 64)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ())))))))))))
        ((firstBinding ((name reduceArg1) (id 61)))
         (secondBinding ((name reduce-arg2) (id 68)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (zero ())
      (body
       (PrimitiveCall
        ((op Add)
         (args
          ((Ref
            ((id ((name reduceArg1) (id 61)))
             (type' ((element (Literal IntLiteral)) (shape ())))))
           (Ref
            ((id ((name reduceArg2) (id 64)))
             (type' ((element (Literal IntLiteral)) (shape ())))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (d ((const 2) (refs ()))) (itemPad ()) (cellShape ()) (associative true)
      (character Reduce) (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ [1 2 3] 4))
        (+ x (+ y y)))
      (foo [5 6 7])
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name y) (id 69)))
         (value
          (IntrinsicCall
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg1) (id 67)))
               (value
                (Frame
                 ((dimensions (3))
                  (elements
                   ((Scalar
                     ((element (Literal (IntLiteral 1)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 2)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 3)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (PrimitiveCall
              ((op Add)
               (args
                ((Ref
                  ((id ((name +arg1) (id 67)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Scalar
                  ((element (Literal (IntLiteral 4)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ())
         (args
          (((binding ((name hoistedExp) (id 76)))
            (value
             (IntrinsicCall
              (Map (frameShape ((Add ((const 3) (refs ())))))
               (args
                (((binding ((name +arg1) (id 71)))
                  (value
                   (Ref
                    ((id ((name y) (id 69)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))
                 ((binding ((name +arg2) (id 73)))
                  (value
                   (Ref
                    ((id ((name y) (id 69)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (PrimitiveCall
                 ((op Add)
                  (args
                   ((Ref
                     ((id ((name +arg1) (id 71)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Ref
                     ((id ((name +arg2) (id 73)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (IntrinsicCall
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name x) (id 62)))
               (value
                (Frame
                 ((dimensions (3))
                  (elements
                   ((Scalar
                     ((element (Literal (IntLiteral 5)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 6)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 7)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (IntrinsicCall
              (Map (frameShape ((Add ((const 3) (refs ())))))
               (args
                (((binding ((name +arg2) (id 75)))
                  (value
                   (Ref
                    ((id ((name hoistedExp) (id 76)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (PrimitiveCall
                 ((op Add)
                  (args
                   ((Ref
                     ((id ((name x) (id 62)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Ref
                     ((id ((name +arg2) (id 75)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ()))))))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))) (Add ((const 3) (refs ()))))))))))
         (type'
          ((element (Literal IntLiteral))
           (shape ((Add ((const 3) (refs ()))) (Add ((const 3) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 3) (refs ()))) (Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ [1 2 3] 4))
        (+ x y))
      (foo [5 6 7])
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name hoistedExp) (id 66)))
         (value
          (IntrinsicCall
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg1) (id 61)))
               (value
                (Frame
                 ((dimensions (3))
                  (elements
                   ((Scalar
                     ((element (Literal (IntLiteral 1)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 2)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 3)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (PrimitiveCall
              ((op Add)
               (args
                ((Ref
                  ((id ((name +arg1) (id 61)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Scalar
                  ((element (Literal (IntLiteral 4)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ((Add ((const 3) (refs ())))))
         (args
          (((binding ((name x) (id 57)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Scalar
                  ((element (Literal (IntLiteral 5)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Scalar
                  ((element (Literal (IntLiteral 6)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Scalar
                  ((element (Literal (IntLiteral 7)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (IntrinsicCall
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 65)))
               (value
                (Ref
                 ((id ((name hoistedExp) (id 66)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (PrimitiveCall
              ((op Add)
               (args
                ((Ref
                  ((id ((name x) (id 57)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name +arg2) (id 65)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (type'
          ((element (Literal IntLiteral))
           (shape ((Add ((const 3) (refs ()))) (Add ((const 3) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 3) (refs ()))) (Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ [1 2 3] 4))
        (+ x (+ y y)))
      (foo 5)
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name y) (id 67)))
         (value
          (IntrinsicCall
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg1) (id 65)))
               (value
                (Frame
                 ((dimensions (3))
                  (elements
                   ((Scalar
                     ((element (Literal (IntLiteral 1)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 2)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 3)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (PrimitiveCall
              ((op Add)
               (args
                ((Ref
                  ((id ((name +arg1) (id 65)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Scalar
                  ((element (Literal (IntLiteral 4)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ((Add ((const 3) (refs ())))))
         (args
          (((binding ((name +arg2) (id 73)))
            (value
             (IntrinsicCall
              (Map (frameShape ((Add ((const 3) (refs ())))))
               (args
                (((binding ((name +arg1) (id 69)))
                  (value
                   (Ref
                    ((id ((name y) (id 67)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))
                 ((binding ((name +arg2) (id 71)))
                  (value
                   (Ref
                    ((id ((name y) (id 67)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (PrimitiveCall
                 ((op Add)
                  (args
                   ((Ref
                     ((id ((name +arg1) (id 69)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Ref
                     ((id ((name +arg2) (id 71)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (PrimitiveCall
           ((op Add)
            (args
             ((Scalar
               ((element (Literal (IntLiteral 5)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Ref
               ((id ((name +arg2) (id 73)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ [1 2 3] x))
        (+ x (+ y y)))
      (foo [5 6 7])
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ((Add ((const 3) (refs ())))))
      (args
       (((binding ((name x) (id 62)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 5)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 6)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 7)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ())
         (args
          (((binding ((name y) (id 69)))
            (value
             (IntrinsicCall
              (Map (frameShape ((Add ((const 3) (refs ())))))
               (args
                (((binding ((name +arg1) (id 67)))
                  (value
                   (Frame
                    ((dimensions (3))
                     (elements
                      ((Scalar
                        ((element (Literal (IntLiteral 1)))
                         (type' ((element (Literal IntLiteral)) (shape ())))))
                       (Scalar
                        ((element (Literal (IntLiteral 2)))
                         (type' ((element (Literal IntLiteral)) (shape ())))))
                       (Scalar
                        ((element (Literal (IntLiteral 3)))
                         (type' ((element (Literal IntLiteral)) (shape ())))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (PrimitiveCall
                 ((op Add)
                  (args
                   ((Ref
                     ((id ((name +arg1) (id 67)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Ref
                     ((id ((name x) (id 62)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (IntrinsicCall
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 75)))
               (value
                (IntrinsicCall
                 (Map (frameShape ((Add ((const 3) (refs ())))))
                  (args
                   (((binding ((name +arg1) (id 71)))
                     (value
                      (Ref
                       ((id ((name y) (id 69)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name +arg2) (id 73)))
                     (value
                      (Ref
                       ((id ((name y) (id 69)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))))
                  (body
                   (PrimitiveCall
                    ((op Add)
                     (args
                      ((Ref
                        ((id ((name +arg1) (id 71)))
                         (type' ((element (Literal IntLiteral)) (shape ())))))
                       (Ref
                        ((id ((name +arg2) (id 73)))
                         (type' ((element (Literal IntLiteral)) (shape ())))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (PrimitiveCall
              ((op Add)
               (args
                ((Ref
                  ((id ((name x) (id 62)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name +arg2) (id 75)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 3) (refs ()))) (Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint
    {|
      (define (foo [x int])
        (define y (+ 3 4))
        (+ x (+ y y)))
      (foo [5 6 7])
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ((Add ((const 3) (refs ())))))
      (args
       (((binding ((name x) (id 58)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 5)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 6)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 7)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (PrimitiveCall
        ((op Add)
         (args
          ((Ref
            ((id ((name x) (id 58)))
             (type' ((element (Literal IntLiteral)) (shape ())))))
           (Scalar
            ((element (Literal (IntLiteral 14)))
             (type' ((element (Literal IntLiteral)) (shape ())))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint {| [[1 2] [3 4] [5 6]] |};
  [%expect
    {|
    (Frame
     ((dimensions (3 2))
      (elements
       ((Scalar
         ((element (Literal (IntLiteral 1)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 2)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 3)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 4)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 5)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 6)))
          (type' ((element (Literal IntLiteral)) (shape ())))))))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 3) (refs ()))) (Add ((const 2) (refs ()))))))))) |}];
  checkAndPrint {| (frame [0] int) |};
  [%expect
    {|
    (Frame
     ((dimensions (0)) (elements ())
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 0) (refs ()))))))))) |}];
  checkAndPrint {| [[1 2] (+ [3 4] [5 6])] |};
  [%expect
    {|
    (Frame
     ((dimensions (2))
      (elements
       ((Frame
         ((dimensions (2))
          (elements
           ((Scalar
             ((element (Literal (IntLiteral 1)))
              (type' ((element (Literal IntLiteral)) (shape ())))))
            (Scalar
             ((element (Literal (IntLiteral 2)))
              (type' ((element (Literal IntLiteral)) (shape ())))))))
          (type'
           ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))
        (IntrinsicCall
         (Map (frameShape ((Add ((const 2) (refs ())))))
          (args
           (((binding ((name +arg1) (id 45)))
             (value
              (Frame
               ((dimensions (2))
                (elements
                 ((Scalar
                   ((element (Literal (IntLiteral 3)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))
                  (Scalar
                   ((element (Literal (IntLiteral 4)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))))
                (type'
                 ((element (Literal IntLiteral))
                  (shape ((Add ((const 2) (refs ())))))))))))
            ((binding ((name +arg2) (id 47)))
             (value
              (Frame
               ((dimensions (2))
                (elements
                 ((Scalar
                   ((element (Literal (IntLiteral 5)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))
                  (Scalar
                   ((element (Literal (IntLiteral 6)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))))
                (type'
                 ((element (Literal IntLiteral))
                  (shape ((Add ((const 2) (refs ())))))))))))))
          (body
           (PrimitiveCall
            ((op Add)
             (args
              ((Ref
                ((id ((name +arg1) (id 45)))
                 (type' ((element (Literal IntLiteral)) (shape ())))))
               (Ref
                ((id ((name +arg2) (id 47)))
                 (type' ((element (Literal IntLiteral)) (shape ())))))))
             (type' ((element (Literal IntLiteral)) (shape ()))))))
          (type'
           ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ()))))))))) |}];
  checkAndPrint {| [(frame [0] int) (frame [0] int)] |};
  [%expect
    {|
    (Frame
     ((dimensions (2 0)) (elements ())
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 2) (refs ()))) (Add ((const 0) (refs ()))))))))) |}];
  checkAndPrint
    {| [[(+ [1 2] [3 4]) (+ [1 2] [3 4]) (+ [1 2] [3 4])] [[4 5] [6 7] [8 9]]] |};
  [%expect
    {|
      (Frame
       ((dimensions (2 3))
        (elements
         ((IntrinsicCall
           (Map (frameShape ((Add ((const 2) (refs ())))))
            (args
             (((binding ((name +arg1) (id 55)))
               (value
                (Frame
                 ((dimensions (2))
                  (elements
                   ((Scalar
                     ((element (Literal (IntLiteral 1)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 2)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 2) (refs ())))))))))))
              ((binding ((name +arg2) (id 57)))
               (value
                (Frame
                 ((dimensions (2))
                  (elements
                   ((Scalar
                     ((element (Literal (IntLiteral 3)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 4)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 2) (refs ())))))))))))))
            (body
             (PrimitiveCall
              ((op Add)
               (args
                ((Ref
                  ((id ((name +arg1) (id 55)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name +arg2) (id 57)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type'
             ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))
          (IntrinsicCall
           (Map (frameShape ((Add ((const 2) (refs ())))))
            (args
             (((binding ((name +arg1) (id 60)))
               (value
                (Frame
                 ((dimensions (2))
                  (elements
                   ((Scalar
                     ((element (Literal (IntLiteral 1)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 2)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 2) (refs ())))))))))))
              ((binding ((name +arg2) (id 62)))
               (value
                (Frame
                 ((dimensions (2))
                  (elements
                   ((Scalar
                     ((element (Literal (IntLiteral 3)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 4)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 2) (refs ())))))))))))))
            (body
             (PrimitiveCall
              ((op Add)
               (args
                ((Ref
                  ((id ((name +arg1) (id 60)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name +arg2) (id 62)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type'
             ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))
          (IntrinsicCall
           (Map (frameShape ((Add ((const 2) (refs ())))))
            (args
             (((binding ((name +arg1) (id 65)))
               (value
                (Frame
                 ((dimensions (2))
                  (elements
                   ((Scalar
                     ((element (Literal (IntLiteral 1)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 2)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 2) (refs ())))))))))))
              ((binding ((name +arg2) (id 67)))
               (value
                (Frame
                 ((dimensions (2))
                  (elements
                   ((Scalar
                     ((element (Literal (IntLiteral 3)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))
                    (Scalar
                     ((element (Literal (IntLiteral 4)))
                      (type' ((element (Literal IntLiteral)) (shape ())))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 2) (refs ())))))))))))))
            (body
             (PrimitiveCall
              ((op Add)
               (args
                ((Ref
                  ((id ((name +arg1) (id 65)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name +arg2) (id 67)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type'
             ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 4)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 5)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 6)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 7)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 8)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 9)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))))
        (type'
         ((element (Literal IntLiteral))
          (shape
           ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))
            (Add ((const 2) (refs ()))))))))) |}];
  checkAndPrint {| [[[1 2] [3 4] [5 6]] [[7 8] [9 10] [11 12]]] |};
  [%expect
    {|
    (Frame
     ((dimensions (2 3 2))
      (elements
       ((Scalar
         ((element (Literal (IntLiteral 1)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 2)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 3)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 4)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 5)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 6)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 7)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 8)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 9)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 10)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 11)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 12)))
          (type' ((element (Literal IntLiteral)) (shape ())))))))
      (type'
       ((element (Literal IntLiteral))
        (shape
         ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))
          (Add ((const 2) (refs ()))))))))) |}];
  checkAndPrint "(append{int | 3 2 []} [1 2 3] [4 5])";
  [%expect
    {|
    (Frame
     ((dimensions (5))
      (elements
       ((Scalar
         ((element (Literal (IntLiteral 1)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 2)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 3)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 4)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 5)))
          (type' ((element (Literal IntLiteral)) (shape ())))))))
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 5) (refs ()))))))))) |}];
  checkAndPrint "(append{int | 3 2 [1]} [[1] [2] [3]] [[4] [5]])";
  [%expect
    {|
    (Frame
     ((dimensions (5 1))
      (elements
       ((Scalar
         ((element (Literal (IntLiteral 1)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 2)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 3)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 4)))
          (type' ((element (Literal IntLiteral)) (shape ())))))
        (Scalar
         ((element (Literal (IntLiteral 5)))
          (type' ((element (Literal IntLiteral)) (shape ())))))))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 5) (refs ()))) (Add ((const 1) (refs ()))))))))) |}];
  checkAndPrint "[[1 1] [2 2] (+ [2 2] 1)]";
  [%expect
    {|
    (Frame
     ((dimensions (3))
      (elements
       ((Frame
         ((dimensions (2))
          (elements
           ((Scalar
             ((element (Literal (IntLiteral 1)))
              (type' ((element (Literal IntLiteral)) (shape ())))))
            (Scalar
             ((element (Literal (IntLiteral 1)))
              (type' ((element (Literal IntLiteral)) (shape ())))))))
          (type'
           ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))
        (Frame
         ((dimensions (2))
          (elements
           ((Scalar
             ((element (Literal (IntLiteral 2)))
              (type' ((element (Literal IntLiteral)) (shape ())))))
            (Scalar
             ((element (Literal (IntLiteral 2)))
              (type' ((element (Literal IntLiteral)) (shape ())))))))
          (type'
           ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))
        (IntrinsicCall
         (Map (frameShape ((Add ((const 2) (refs ())))))
          (args
           (((binding ((name +arg1) (id 44)))
             (value
              (Frame
               ((dimensions (2))
                (elements
                 ((Scalar
                   ((element (Literal (IntLiteral 2)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))
                  (Scalar
                   ((element (Literal (IntLiteral 2)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))))
                (type'
                 ((element (Literal IntLiteral))
                  (shape ((Add ((const 2) (refs ())))))))))))))
          (body
           (PrimitiveCall
            ((op Add)
             (args
              ((Ref
                ((id ((name +arg1) (id 44)))
                 (type' ((element (Literal IntLiteral)) (shape ())))))
               (Scalar
                ((element (Literal (IntLiteral 1)))
                 (type' ((element (Literal IntLiteral)) (shape ())))))))
             (type' ((element (Literal IntLiteral)) (shape ()))))))
          (type'
           ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 3) (refs ()))) (Add ((const 2) (refs ()))))))))) |}];
  checkAndPrint "(append{int | 3 2 [2]} [[1 1] [2 2] (+ [2 2] 1)] [[4 4] [5 5]])";
  [%expect
    {|
    (Frame
     ((dimensions (5))
      (elements
       ((Frame
         ((dimensions (2))
          (elements
           ((Scalar
             ((element (Literal (IntLiteral 1)))
              (type' ((element (Literal IntLiteral)) (shape ())))))
            (Scalar
             ((element (Literal (IntLiteral 1)))
              (type' ((element (Literal IntLiteral)) (shape ())))))))
          (type'
           ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))
        (Frame
         ((dimensions (2))
          (elements
           ((Scalar
             ((element (Literal (IntLiteral 2)))
              (type' ((element (Literal IntLiteral)) (shape ())))))
            (Scalar
             ((element (Literal (IntLiteral 2)))
              (type' ((element (Literal IntLiteral)) (shape ())))))))
          (type'
           ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))
        (IntrinsicCall
         (Map (frameShape ((Add ((const 2) (refs ())))))
          (args
           (((binding ((name +arg1) (id 48)))
             (value
              (Frame
               ((dimensions (2))
                (elements
                 ((Scalar
                   ((element (Literal (IntLiteral 2)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))
                  (Scalar
                   ((element (Literal (IntLiteral 2)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))))
                (type'
                 ((element (Literal IntLiteral))
                  (shape ((Add ((const 2) (refs ())))))))))))))
          (body
           (PrimitiveCall
            ((op Add)
             (args
              ((Ref
                ((id ((name +arg1) (id 48)))
                 (type' ((element (Literal IntLiteral)) (shape ())))))
               (Scalar
                ((element (Literal (IntLiteral 1)))
                 (type' ((element (Literal IntLiteral)) (shape ())))))))
             (type' ((element (Literal IntLiteral)) (shape ()))))))
          (type'
           ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))
        (Frame
         ((dimensions (2))
          (elements
           ((Scalar
             ((element (Literal (IntLiteral 4)))
              (type' ((element (Literal IntLiteral)) (shape ())))))
            (Scalar
             ((element (Literal (IntLiteral 4)))
              (type' ((element (Literal IntLiteral)) (shape ())))))))
          (type'
           ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))
        (Frame
         ((dimensions (2))
          (elements
           ((Scalar
             ((element (Literal (IntLiteral 5)))
              (type' ((element (Literal IntLiteral)) (shape ())))))
            (Scalar
             ((element (Literal (IntLiteral 5)))
              (type' ((element (Literal IntLiteral)) (shape ())))))))
          (type'
           ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 5) (refs ()))) (Add ((const 2) (refs ()))))))))) |}];
  checkAndPrint "iota{| [1 2 3]}";
  [%expect
    {|
    (IntrinsicCall
     (Iota
      (s
       ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
        (Add ((const 3) (refs ())))))
      (type'
       ((element (Literal IntLiteral))
        (shape
         ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
          (Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint "(index{int | [1 2 3] [4 5] 3} iota{| [1 2 3 4 5]} [0 1 0])";
  [%expect
    {|
    (IntrinsicCall
     (Index
      (arrayArg
       (IntrinsicCall
        (Iota
         (s
          ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
           (Add ((const 3) (refs ()))) (Add ((const 4) (refs ())))
           (Add ((const 5) (refs ())))))
         (type'
          ((element (Literal IntLiteral))
           (shape
            ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
             (Add ((const 3) (refs ()))) (Add ((const 4) (refs ())))
             (Add ((const 5) (refs ()))))))))))
      (indexArg
       (Frame
        ((dimensions (3))
         (elements
          ((Scalar
            ((element (Literal (IntLiteral 0)))
             (type' ((element (Literal IntLiteral)) (shape ())))))
           (Scalar
            ((element (Literal (IntLiteral 1)))
             (type' ((element (Literal IntLiteral)) (shape ())))))
           (Scalar
            ((element (Literal (IntLiteral 0)))
             (type' ((element (Literal IntLiteral)) (shape ())))))))
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))))
      (s
       ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
        (Add ((const 3) (refs ())))))
      (cellShape ((Add ((const 4) (refs ()))) (Add ((const 5) (refs ())))))
      (l ((const 3) (refs ())))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 4) (refs ()))) (Add ((const 5) (refs ()))))))))) |}]
;;
