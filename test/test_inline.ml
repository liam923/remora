open! Base
open Remora

let%expect_test "check inlining" =
  let pipeline =
    CompilerPipeline.(
      (module Parse.Stage (Source.UnitBuilder))
      @> (module TypeCheckStage.M (Source.UnitBuilder))
      @> (module Explicitize.Stage (Source.UnitBuilder))
      @> (module Inline.Stage (Source.UnitBuilder))
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
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 22)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name +arg1) (id 23)))
         (value
          (Scalar
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 24)))
         (value
          (Scalar
           ((element (Literal (IntLiteral 2)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (PrimitiveCall
        ((op Add)
         (args
          ((Ref
            ((id ((name +arg1) (id 23)))
             (type' ((element (Literal IntLiteral)) (shape ())))))
           (Ref
            ((id ((name +arg2) (id 24)))
             (type' ((element (Literal IntLiteral)) (shape ())))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 23)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name +arg1) (id 24)))
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
              (shape ((Add ((const 3) (refs ())))))))))))
        ((binding ((name +arg2) (id 26)))
         (value
          (Scalar
           ((element (Literal (IntLiteral 4)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ((Add ((const 3) (refs ())))))
         (args
          (((binding ((name +arg1) (id 25)))
            (value
             (Ref
              ((id ((name +arg1) (id 24)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (PrimitiveCall
           ((op Add)
            (args
             ((Ref
               ((id ((name +arg1) (id 25)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Ref
               ((id ((name +arg2) (id 26)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint {|
    (t-app (t-fn (@t) "hello") int)
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
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 25)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 26)))
            (value
             (Ref
              ((id ((name id) (id 25)))
               (type' ((element (Literal UnitLiteral)) (shape ())))))))
           ((binding ((name x) (id 27)))
            (value
             (Scalar
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name x) (id 27)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app id (Forall (@t) (-> (@t) @t))) id)
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 28)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name id) (id 26)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 27)))
            (value
             (Ref
              ((id ((name id) (id 26)))
               (type' ((element (Literal UnitLiteral)) (shape ())))))))
           ((binding ((name x) (id 29)))
            (value
             (Ref
              ((id ((name id) (id 28)))
               (type' ((element (Literal UnitLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name x) (id 29)))
            (type' ((element (Literal UnitLiteral)) (shape ()))))))
         (type' ((element (Literal UnitLiteral)) (shape ()))))))
      (type' ((element (Literal UnitLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app ((t-app id (Forall (@t) (-> (@t) @t))) id) int) 5)
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 28)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name id) (id 31)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 33)))
            (value
             (IntrinsicCall
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 29)))
                  (value
                   (Ref
                    ((id ((name id) (id 28)))
                     (type' ((element (Literal UnitLiteral)) (shape ())))))))
                 ((binding ((name x) (id 32)))
                  (value
                   (Ref
                    ((id ((name id) (id 31)))
                     (type' ((element (Literal UnitLiteral)) (shape ())))))))))
               (body
                (Ref
                 ((id ((name x) (id 32)))
                  (type' ((element (Literal UnitLiteral)) (shape ()))))))
               (type' ((element (Literal UnitLiteral)) (shape ())))))))
           ((binding ((name arg0) (id 34)))
            (value
             (Scalar
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name arg0) (id 34)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      ((t-app (t-app (t-fn (@a) (t-fn (@b) (fn ([x int]) x))) int) int) 10)
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 25)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name x) (id 26)))
         (value
          (Scalar
           ((element (Literal (IntLiteral 10)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (Ref
        ((id ((name x) (id 26)))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {|
      (length{int | 5 []} [1 2 3 4 5])
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 21)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name length-arg) (id 22)))
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
      (body
       (IntrinsicCall
        (Length
         (arg
          (Ref
           ((id ((name length-arg) (id 22)))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 5) (refs ()))))))))))
         (t (Literal IntLiteral)) (d ((const 5) (refs ()))) (cellShape ())
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| 
    (reduce{int | 4 [] []} + [1 2 3 4 5])
  |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 22)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name reduce-arg1) (id 23)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name reduce-arg2) (id 26)))
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
      (body
       (IntrinsicCall
        (Reduce
         (args
          (((firstBinding ((name reduceArg1) (id 27)))
            (secondBinding ((name reduceArg2) (id 28)))
            (value
             (Ref
              ((id ((name reduce-arg2) (id 26)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 5) (refs ())))))))))))))
         (body
          (PrimitiveCall
           ((op Add)
            (args
             ((Ref
               ((id ((name reduceArg1) (id 27)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Ref
               ((id ((name reduceArg2) (id 28)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (t (Literal IntLiteral)) (dSub1 ((const 4) (refs ()))) (itemPad ())
         (cellShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| ([+ -] 1 2) |};
  [%expect
    {|
    Error: Could not determine what function is being called in function call:
    ((func (Ref ((id ((name f) (id 22))))))
     (args (((id ((name +arg1) (id 19)))) ((id ((name +arg2) (id 20))))))
     (type' ((element (Literal IntLiteral)) (shape ())))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 26)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 27)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name id) (id 26)))
                   (type' ((element (Literal UnitLiteral)) (shape ())))))
                 (Ref
                  ((id ((name id) (id 26)))
                   (type' ((element (Literal UnitLiteral)) (shape ())))))
                 (Ref
                  ((id ((name id) (id 26)))
                   (type' ((element (Literal UnitLiteral)) (shape ())))))))
               (type'
                ((element (Literal UnitLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))
           ((binding ((name x) (id 29)))
            (value
             (Scalar
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (IntrinsicCall
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name f) (id 28)))
               (value
                (Ref
                 ((id ((name f) (id 27)))
                  (type'
                   ((element (Literal UnitLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (Ref
              ((id ((name x) (id 29)))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))))
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
    (IntrinsicCall
     (Map (frameShape ()) (args ())
      (body
       (IntrinsicCall
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 29)))
            (value
             (Scalar
              ((element (Literal UnitLiteral))
               (type' ((element (Literal UnitLiteral)) (shape ())))))))
           ((binding ((name +arg1) (id 32)))
            (value
             (IntrinsicCall
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 30)))
                  (value
                   (Scalar
                    ((element (Literal UnitLiteral))
                     (type' ((element (Literal UnitLiteral)) (shape ())))))))
                 ((binding ((name length-arg) (id 31)))
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
               (body
                (IntrinsicCall
                 (Length
                  (arg
                   (Ref
                    ((id ((name length-arg) (id 31)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 5) (refs ()))))))))))
                  (t (Literal IntLiteral)) (d ((const 5) (refs ())))
                  (cellShape ())
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name +arg2) (id 35)))
            (value
             (IntrinsicCall
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 33)))
                  (value
                   (Scalar
                    ((element (Literal UnitLiteral))
                     (type' ((element (Literal UnitLiteral)) (shape ())))))))
                 ((binding ((name length-arg) (id 34)))
                  (value
                   (Frame
                    ((dimensions (2))
                     (elements
                      ((Frame
                        ((dimensions (2))
                         (elements
                          ((Scalar
                            ((element (Literal (CharacterLiteral h)))
                             (type'
                              ((element (Literal CharacterLiteral)) (shape ())))))
                           (Scalar
                            ((element (Literal (CharacterLiteral i)))
                             (type'
                              ((element (Literal CharacterLiteral)) (shape ())))))))
                         (type'
                          ((element (Literal CharacterLiteral))
                           (shape ((Add ((const 2) (refs ())))))))))
                       (Frame
                        ((dimensions (2))
                         (elements
                          ((Scalar
                            ((element (Literal (CharacterLiteral i)))
                             (type'
                              ((element (Literal CharacterLiteral)) (shape ())))))
                           (Scalar
                            ((element (Literal (CharacterLiteral h)))
                             (type'
                              ((element (Literal CharacterLiteral)) (shape ())))))))
                         (type'
                          ((element (Literal CharacterLiteral))
                           (shape ((Add ((const 2) (refs ())))))))))))
                     (type'
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))))))))
               (body
                (IntrinsicCall
                 (Length
                  (arg
                   (Ref
                    ((id ((name length-arg) (id 34)))
                     (type'
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ()))))))))))
                  (t (Literal CharacterLiteral)) (d ((const 2) (refs ())))
                  (cellShape ((Add ((const 2) (refs ())))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (PrimitiveCall
           ((op Add)
            (args
             ((Ref
               ((id ((name +arg1) (id 32)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Ref
               ((id ((name +arg2) (id 35)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
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
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 34)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name reduce-arg1) (id 36)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name reduce-arg2) (id 44)))
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
        ((binding ((name reduce-arg2) (id 41)))
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
      (body
       (IntrinsicCall
        (Reduce
         (args
          (((firstBinding ((name reduce-arg1) (id 48)))
            (secondBinding ((name reduceArg2) (id 45)))
            (value
             (Ref
              ((id ((name reduce-arg2) (id 44)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((firstBinding ((name reduceArg1) (id 42)))
            (secondBinding ((name reduce-arg2) (id 49)))
            (value
             (Ref
              ((id ((name reduce-arg2) (id 41)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IntrinsicCall
           (Map (frameShape ())
            (args
             (((binding ((name sum) (id 47)))
               (value
                (IntrinsicCall
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 40)))
                     (value
                      (Scalar
                       ((element (Literal UnitLiteral))
                        (type' ((element (Literal UnitLiteral)) (shape ())))))))
                    ((binding ((name +arg1) (id 43)))
                     (value
                      (Ref
                       ((id ((name reduceArg1) (id 42)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name +arg2) (id 46)))
                     (value
                      (Ref
                       ((id ((name reduceArg2) (id 45)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (PrimitiveCall
                    ((op Add)
                     (args
                      ((Ref
                        ((id ((name +arg1) (id 43)))
                         (type' ((element (Literal IntLiteral)) (shape ())))))
                       (Ref
                        ((id ((name +arg2) (id 46)))
                         (type' ((element (Literal IntLiteral)) (shape ())))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (Ref
              ((id ((name sum) (id 47)))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (t (Literal IntLiteral)) (dSub1 ((const 1) (refs ()))) (itemPad ())
         (cellShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| (reduce{int | 2 [] []} [+ -] [1 2 3]) |};
  [%expect
    {|
    Error: Could not determine what function is being passed to reduce:
    ((func (Ref ((id ((name f) (id 21))))))
     (args
      (((id ((name reduce-arg1) (id 22)))) ((id ((name reduce-arg2) (id 20))))))
     (type' ((element (Literal IntLiteral)) (shape ())))) |}];
  checkAndPrint {| (length{(Forall (@x) int) | 2 []} [(t-fn (@x) 5) (t-fn (@x) 5)]) |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 24)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name length-arg) (id 25)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar
               ((element (Literal UnitLiteral))
                (type' ((element (Literal UnitLiteral)) (shape ())))))
              (Scalar
               ((element (Literal UnitLiteral))
                (type' ((element (Literal UnitLiteral)) (shape ())))))))
            (type'
             ((element (Literal UnitLiteral))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (IntrinsicCall
        (Length
         (arg
          (Ref
           ((id ((name length-arg) (id 25)))
            (type'
             ((element (Literal UnitLiteral))
              (shape ((Add ((const 2) (refs ()))))))))))
         (t (Literal UnitLiteral)) (d ((const 2) (refs ()))) (cellShape ())
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| (scan{int | 2 [] []} + [1 2 3]) |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 22)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name scan-arg1) (id 23)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name scan-arg2) (id 26)))
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
       (IntrinsicCall
        (Scan
         (args
          (((firstBinding ((name scan-arg1) (id 27)))
            (secondBinding ((name scan-arg2) (id 28)))
            (value
             (Ref
              ((id ((name scan-arg2) (id 26)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (PrimitiveCall
           ((op Add)
            (args
             ((Ref
               ((id ((name scan-arg1) (id 27)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Ref
               ((id ((name scan-arg2) (id 28)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (t (Literal IntLiteral)) (dSub1 ((const 2) (refs ()))) (itemPad ())
         (cellShape ())
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint {| (filter{int | 3 []} [1 2 3] [#t #t #f]) |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 22)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name filter-array) (id 23)))
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
              (shape ((Add ((const 3) (refs ())))))))))))
        ((binding ((name filter-flags) (id 24)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar
               ((element (Literal (BooleanLiteral true)))
                (type' ((element (Literal BooleanLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (BooleanLiteral true)))
                (type' ((element (Literal BooleanLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (BooleanLiteral false)))
                (type' ((element (Literal BooleanLiteral)) (shape ())))))))
            (type'
             ((element (Literal BooleanLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (IntrinsicCall
        (Filter
         (array
          (Ref
           ((id ((name filter-array) (id 23)))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (flags
          (Ref
           ((id ((name filter-flags) (id 24)))
            (type'
             ((element (Literal BooleanLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (t (Literal IntLiteral)) (d ((const 3) (refs ()))) (cellShape ())
         (type'
          ((element
            (Sigma
             ((parameters (((binding ((name d-out) (id 14))) (bound Dim))))
              (body
               ((element (Literal IntLiteral))
                (shape ((Add ((const 0) (refs ()))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding ((name d-out) (id 14))) (bound Dim))))
           (body
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ()))))))))))
        (shape ()))))) |}];
  checkAndPrint {| (append{int | 3 2 []} [1 2 3] [4 5]) |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 22)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name append-arg1) (id 23)))
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
              (shape ((Add ((const 3) (refs ())))))))))))
        ((binding ((name append-arg2) (id 24)))
         (value
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
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (IntrinsicCall
        (Append
         (arg1
          (Ref
           ((id ((name append-arg1) (id 23)))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (arg2
          (Ref
           ((id ((name append-arg2) (id 24)))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ()))))))))))
         (t (Literal IntLiteral)) (d1 ((const 3) (refs ())))
         (d2 ((const 2) (refs ()))) (cellShape ())
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 5) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 5) (refs ()))))))))) |}]
;;
