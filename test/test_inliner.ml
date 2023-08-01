open! Base
open Remora

let%expect_test "check inlining" =
  let pipeline =
    CompilerPipeline.(
      (module Parser.Stage (Source.UnitBuilder))
      @> (module TypeChecker.Stage (Source.UnitBuilder))
      @> (module Explicitize.Stage (Source.UnitBuilder))
      @> (module Inline.Stage (Source.UnitBuilder))
      @> (module Show.Stage (InlineNucleus) (Source.UnitBuilder))
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
     (Map
      (args
       (((binding ((name f) (id 10)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))
        ((binding ((name arg0) (id 11)))
         (value
          (Scalar
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name arg1) (id 12)))
         (value
          (Scalar
           ((element (Literal (IntLiteral 2)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (PrimitiveCall
        ((op Add)
         (args
          ((Ref
            ((id ((name arg0) (id 11)))
             (type' ((element (Literal IntLiteral)) (shape ())))))
           (Ref
            ((id ((name arg1) (id 12)))
             (type' ((element (Literal IntLiteral)) (shape ())))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect
    {|
    (IntrinsicCall
     (Map
      (args
       (((binding ((name f) (id 11)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))
        ((binding ((name arg0) (id 12)))
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
        ((binding ((name arg1) (id 14)))
         (value
          (Scalar
           ((element (Literal (IntLiteral 4)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (IntrinsicCall
        (Map
         (args
          (((binding ((name arg0) (id 13)))
            (value
             (Ref
              ((id ((name arg0) (id 12)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (PrimitiveCall
           ((op Add)
            (args
             ((Ref
               ((id ((name arg0) (id 13)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Ref
               ((id ((name arg1) (id 14)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ((Add ((const 3) (refs ())))))
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))))
      (frameShape ())
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
    (define id (t-fn (@t) (fn ([x @t]) x)))
    ((t-app id int) 5)
  |};
  [%expect
    {|
    (IntrinsicCall
     (Map
      (args
       (((binding ((name id) (id 13)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))))
      (body
       (IntrinsicCall
        (Map
         (args
          (((binding ((name f) (id 14)))
            (value
             (Ref
              ((id ((name id) (id 13)))
               (type' ((element (Literal Unit)) (shape ())))))))
           ((binding ((name arg0) (id 15)))
            (value
             (Scalar
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name arg0) (id 15)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      (define id (t-fn (@t) (fn ([x @t]) x)))
      ((t-app id (Forall (@t) (-> (@t) @t))) id)
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map
      (args
       (((binding ((name id) (id 16)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))
        ((binding ((name id) (id 14)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))))
      (body
       (IntrinsicCall
        (Map
         (args
          (((binding ((name f) (id 15)))
            (value
             (Ref
              ((id ((name id) (id 14)))
               (type' ((element (Literal Unit)) (shape ())))))))
           ((binding ((name arg0) (id 17)))
            (value
             (Ref
              ((id ((name id) (id 16)))
               (type' ((element (Literal Unit)) (shape ())))))))))
         (body
          (Ref
           ((id ((name arg0) (id 17)))
            (type' ((element (Literal Unit)) (shape ()))))))
         (frameShape ()) (type' ((element (Literal Unit)) (shape ()))))))
      (frameShape ()) (type' ((element (Literal Unit)) (shape ()))))) |}];
  checkAndPrint
    {|
      (define id (t-fn (@t) (fn ([x @t]) x)))
      ((t-app ((t-app id (Forall (@t) (-> (@t) @t))) id) int) 5)
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map
      (args
       (((binding ((name id) (id 16)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))
        ((binding ((name id) (id 19)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))))
      (body
       (IntrinsicCall
        (Map
         (args
          (((binding ((name f) (id 21)))
            (value
             (IntrinsicCall
              (Map
               (args
                (((binding ((name f) (id 17)))
                  (value
                   (Ref
                    ((id ((name id) (id 16)))
                     (type' ((element (Literal Unit)) (shape ())))))))
                 ((binding ((name arg0) (id 20)))
                  (value
                   (Ref
                    ((id ((name id) (id 19)))
                     (type' ((element (Literal Unit)) (shape ())))))))))
               (body
                (Ref
                 ((id ((name arg0) (id 20)))
                  (type' ((element (Literal Unit)) (shape ()))))))
               (frameShape ()) (type' ((element (Literal Unit)) (shape ())))))))
           ((binding ((name arg0) (id 22)))
            (value
             (Scalar
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name arg0) (id 22)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      ((t-app (t-app (t-fn (@a) (t-fn (@b) (fn ([x int]) x))) int) int) 10)
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map
      (args
       (((binding ((name f) (id 13)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))
        ((binding ((name arg0) (id 14)))
         (value
          (Scalar
           ((element (Literal (IntLiteral 10)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (Ref
        ((id ((name arg0) (id 14)))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {|
      ((t-app (i-app length 5 []) int) [1 2 3 4 5])
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map
      (args
       (((binding ((name f) (id 9)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))
        ((binding ((name arg0) (id 10)))
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
           ((id ((name arg0) (id 10)))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 5) (refs ()))))))))))
         (t (Literal IntLiteral)) (d ((const 5) (refs ()))) (cellShape ())
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| 
    ((t-app (i-app reduce 4 [] []) int) + [1 2 3 4 5])
  |};
  [%expect
    {|
    (IntrinsicCall
     (Map
      (args
       (((binding ((name f) (id 10)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))
        ((binding ((name arg0) (id 11)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))
        ((binding ((name arg1) (id 14)))
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
          (((firstBinding ((name reduceArg1) (id 15)))
            (secondBinding ((name reduceArg2) (id 16)))
            (value
             (Ref
              ((id ((name arg1) (id 14)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 5) (refs ())))))))))))))
         (body
          (PrimitiveCall
           ((op Add)
            (args
             ((Ref
               ((id ((name reduceArg1) (id 15)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Ref
               ((id ((name reduceArg2) (id 16)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (t (Literal IntLiteral)) (dSub1 ((const 4) (refs ()))) (itemPad ())
         (cellShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| ([+ -] 1 2) |};
  [%expect
    {|
    Error: Could not determine what function is being called in function call:
    ((func (Ref ((id ((name f) (id 10))))))
     (args (((id ((name arg0) (id 7)))) ((id ((name arg1) (id 8))))))
     (type' ((element (Literal IntLiteral)) (shape ())))) |}];
  checkAndPrint
    {|
      (define id (t-fn (@t) (fn ([x @t]) x)))
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map
      (args
       (((binding ((name id) (id 14)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))))
      (body
       (IntrinsicCall
        (Map
         (args
          (((binding ((name f) (id 15)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name id) (id 14)))
                   (type' ((element (Literal Unit)) (shape ())))))
                 (Ref
                  ((id ((name id) (id 14)))
                   (type' ((element (Literal Unit)) (shape ())))))
                 (Ref
                  ((id ((name id) (id 14)))
                   (type' ((element (Literal Unit)) (shape ())))))))
               (type'
                ((element (Literal Unit)) (shape ((Add ((const 3) (refs ())))))))))))
           ((binding ((name arg0) (id 17)))
            (value
             (Scalar
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (IntrinsicCall
           (Map
            (args
             (((binding ((name f) (id 16)))
               (value
                (Ref
                 ((id ((name f) (id 15)))
                  (type'
                   ((element (Literal Unit))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (Ref
              ((id ((name arg0) (id 17)))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (frameShape ((Add ((const 3) (refs ())))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (frameShape ())
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))))
      (frameShape ())
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint
    {|
      (define id (t-fn (@t) (fn ([x @t]) x)))
      (+
        ((t-app (i-app length 5 []) int) [1 2 3 4 5])
        ((t-app (i-app length 2 [2]) char) ["hi" "ih"]))
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map (args ())
      (body
       (IntrinsicCall
        (Map
         (args
          (((binding ((name f) (id 17)))
            (value
             (Scalar
              ((element (Literal Unit))
               (type' ((element (Literal Unit)) (shape ())))))))
           ((binding ((name arg0) (id 20)))
            (value
             (IntrinsicCall
              (Map
               (args
                (((binding ((name f) (id 18)))
                  (value
                   (Scalar
                    ((element (Literal Unit))
                     (type' ((element (Literal Unit)) (shape ())))))))
                 ((binding ((name arg0) (id 19)))
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
                    ((id ((name arg0) (id 19)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 5) (refs ()))))))))))
                  (t (Literal IntLiteral)) (d ((const 5) (refs ())))
                  (cellShape ())
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (frameShape ())
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name arg1) (id 23)))
            (value
             (IntrinsicCall
              (Map
               (args
                (((binding ((name f) (id 21)))
                  (value
                   (Scalar
                    ((element (Literal Unit))
                     (type' ((element (Literal Unit)) (shape ())))))))
                 ((binding ((name arg0) (id 22)))
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
                    ((id ((name arg0) (id 22)))
                     (type'
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ()))))))))))
                  (t (Literal CharacterLiteral)) (d ((const 2) (refs ())))
                  (cellShape ((Add ((const 2) (refs ())))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (frameShape ())
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (PrimitiveCall
           ((op Add)
            (args
             ((Ref
               ((id ((name arg0) (id 20)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Ref
               ((id ((name arg1) (id 23)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      (t-app
        ((t-app (i-app reduce 1 [] []) (Forall (@t) int))
          (fn ([a (Forall (@a) int)] [b (Forall (@b) int)])
            (define sum (+ (t-app a char) (t-app b int)))
            (t-fn (@u) sum))
          [(t-fn (@x) 1) (t-fn (@y) 2)])
        int)
    |};
  [%expect
    {|
    (IntrinsicCall
     (Map
      (args
       (((binding ((name f) (id 22)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))
        ((binding ((name arg0) (id 24)))
         (value
          (Scalar
           ((element (Literal Unit))
            (type' ((element (Literal Unit)) (shape ())))))))
        ((binding ((name arg1) (id 32)))
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
             ((element (Literal Unit)) (shape ((Add ((const 2) (refs ())))))))))))
        ((binding ((name arg1) (id 29)))
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
             ((element (Literal Unit)) (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (IntrinsicCall
        (Reduce
         (args
          (((firstBinding ((name reduceArg1) (id 36)))
            (secondBinding ((name reduceArg2) (id 33)))
            (value
             (Ref
              ((id ((name arg1) (id 32)))
               (type'
                ((element (Literal Unit)) (shape ((Add ((const 2) (refs ())))))))))))
           ((firstBinding ((name reduceArg1) (id 30)))
            (secondBinding ((name reduceArg2) (id 37)))
            (value
             (Ref
              ((id ((name arg1) (id 29)))
               (type'
                ((element (Literal Unit)) (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IntrinsicCall
           (Map
            (args
             (((binding ((name sum) (id 35)))
               (value
                (IntrinsicCall
                 (Map
                  (args
                   (((binding ((name f) (id 28)))
                     (value
                      (Scalar
                       ((element (Literal Unit))
                        (type' ((element (Literal Unit)) (shape ())))))))
                    ((binding ((name arg0) (id 31)))
                     (value
                      (Ref
                       ((id ((name reduceArg1) (id 30)))
                        (type' ((element (Literal Unit)) (shape ())))))))
                    ((binding ((name arg1) (id 34)))
                     (value
                      (Ref
                       ((id ((name reduceArg2) (id 33)))
                        (type' ((element (Literal Unit)) (shape ())))))))))
                  (body
                   (PrimitiveCall
                    ((op Add)
                     (args
                      ((Ref
                        ((id ((name arg0) (id 31)))
                         (type' ((element (Literal IntLiteral)) (shape ())))))
                       (Ref
                        ((id ((name arg1) (id 34)))
                         (type' ((element (Literal IntLiteral)) (shape ())))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (frameShape ())
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (Ref
              ((id ((name sum) (id 35)))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (frameShape ()) (type' ((element (Literal Unit)) (shape ()))))))
         (t (Literal Unit)) (dSub1 ((const 1) (refs ()))) (itemPad ())
         (cellShape ()) (type' ((element (Literal Unit)) (shape ()))))))
      (frameShape ()) (type' ((element (Literal Unit)) (shape ()))))) |}]
;;
