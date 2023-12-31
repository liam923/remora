open! Base
open Remora

let%expect_test "check explicitizing" =
  let pipeline =
    CompilerPipeline.(
      (module Parse.Stage (Source.UnitBuilder))
      @> (module TypeCheckStage.M (Source.UnitBuilder))
      @> (module Explicitize.Stage (Source.UnitBuilder))
      @> (module Show.Stage (Explicit) (Source.UnitBuilder))
      @> empty)
  in
  let checkAndPrint = TestPipeline.runAndPrint pipeline in
  checkAndPrint {| 5 |};
  [%expect {| (Scalar ((element (Literal (IntLiteral 5))))) |}];
  checkAndPrint {| (+ 1 2) |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name f) (id 41))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 39)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 40)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 41))))))
         (args (((id ((name +arg1) (id 39)))) ((id ((name +arg2) (id 40))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ())))))) |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name f) (id 41))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 39)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 40)))
         (value (Scalar ((element (Literal (IntLiteral 4)))))))))
      (body
       (Map
        ((args
          (((binding ((name +arg1) (id 42)))
            (value (Ref ((id ((name +arg1) (id 39)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 41))))))
            (args (((id ((name +arg1) (id 42)))) ((id ((name +arg2) (id 40))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ((Add ((const 3) (refs ())))))
         (type'
          (Arr
           ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))) |}];
  checkAndPrint {| (+ [1 2 3] [4 5 6]) |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name f) (id 41))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 39)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 40)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 4)))))
              (Scalar ((element (Literal (IntLiteral 5)))))
              (Scalar ((element (Literal (IntLiteral 6)))))))))))))
      (body
       (Map
        ((args
          (((binding ((name +arg1) (id 42)))
            (value (Ref ((id ((name +arg1) (id 39)))))))
           ((binding ((name +arg2) (id 43)))
            (value (Ref ((id ((name +arg2) (id 40)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 41))))))
            (args (((id ((name +arg1) (id 42)))) ((id ((name +arg2) (id 43))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ((Add ((const 3) (refs ())))))
         (type'
          (Arr
           ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))) |}];
  checkAndPrint {| (+ [[1 2]] [3]) |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name f) (id 41))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 39)))
         (value
          (Frame
           ((dimensions (1))
            (elements
             ((Frame
               ((dimensions (2))
                (elements
                 ((Scalar ((element (Literal (IntLiteral 1)))))
                  (Scalar ((element (Literal (IntLiteral 2)))))))))))))))
        ((binding ((name +arg2) (id 40)))
         (value
          (Frame
           ((dimensions (1))
            (elements ((Scalar ((element (Literal (IntLiteral 3)))))))))))))
      (body
       (Map
        ((args
          (((binding ((name +arg2) (id 42)))
            (value (Ref ((id ((name +arg2) (id 40)))))))
           ((binding ((name +arg1) (id 43)))
            (value (Ref ((id ((name +arg1) (id 39)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg1) (id 44)))
               (value (Ref ((id ((name +arg1) (id 43)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 41))))))
               (args
                (((id ((name +arg1) (id 44)))) ((id ((name +arg2) (id 42))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (frameShape ((Add ((const 2) (refs ())))))
            (type'
             (Arr
              ((element (Literal IntLiteral))
               (shape ((Add ((const 2) (refs ())))))))))))
         (frameShape ((Add ((const 1) (refs ())))))
         (type'
          (Arr
           ((element (Literal IntLiteral))
            (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal IntLiteral))
         (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))))) |}];
  checkAndPrint {| (+ 1 [2 3 4]) |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name f) (id 41))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 39)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 40)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))
              (Scalar ((element (Literal (IntLiteral 4)))))))))))))
      (body
       (Map
        ((args
          (((binding ((name +arg2) (id 42)))
            (value (Ref ((id ((name +arg2) (id 40)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 41))))))
            (args (((id ((name +arg1) (id 39)))) ((id ((name +arg2) (id 42))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ((Add ((const 3) (refs ())))))
         (type'
          (Arr
           ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))) |}];
  checkAndPrint {| (+ 1 [[[2 3 4] [5 6 7]]]) |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name f) (id 41))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 39)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 40)))
         (value
          (Frame
           ((dimensions (1))
            (elements
             ((Frame
               ((dimensions (2))
                (elements
                 ((Frame
                   ((dimensions (3))
                    (elements
                     ((Scalar ((element (Literal (IntLiteral 2)))))
                      (Scalar ((element (Literal (IntLiteral 3)))))
                      (Scalar ((element (Literal (IntLiteral 4)))))))))
                  (Frame
                   ((dimensions (3))
                    (elements
                     ((Scalar ((element (Literal (IntLiteral 5)))))
                      (Scalar ((element (Literal (IntLiteral 6)))))
                      (Scalar ((element (Literal (IntLiteral 7)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name +arg2) (id 42)))
            (value (Ref ((id ((name +arg2) (id 40)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 41))))))
            (args (((id ((name +arg1) (id 39)))) ((id ((name +arg2) (id 42))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape
          ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
           (Add ((const 3) (refs ())))))
         (type'
          (Arr
           ((element (Literal IntLiteral))
            (shape
             ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
              (Add ((const 3) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal IntLiteral))
         (shape
          ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
           (Add ((const 3) (refs ())))))))))) |}];
  checkAndPrint {|
    (define foo 1)
    foo
    |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name foo) (id 39)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body (Ref ((id ((name foo) (id 39)))))) (frameShape ())
      (type' (Arr ((element (Literal IntLiteral)) (shape ())))))) |}];
  checkAndPrint
    {|
    (define (foo [a int] [b [int 1 2 3]]) 0)
    (foo 0 [[[1 2 3] [4 5 6]]])
    |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name foo) (id 39)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name a) (id 40)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name b) (id 41)))
                  (bound
                   (Arr
                    ((element (Literal IntLiteral))
                     (shape
                      ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
                       (Add ((const 3) (refs ())))))))))))
               (body (Scalar ((element (Literal (IntLiteral 0)))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 44)))
            (value (Ref ((id ((name foo) (id 39)))))))
           ((binding ((name a) (id 42)))
            (value (Scalar ((element (Literal (IntLiteral 0)))))))
           ((binding ((name b) (id 43)))
            (value
             (Frame
              ((dimensions (1))
               (elements
                ((Frame
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
                         (Scalar ((element (Literal (IntLiteral 6)))))))))))))))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 44))))))
            (args (((id ((name a) (id 42)))) ((id ((name b) (id 43))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ())))))) |}];
  checkAndPrint
    {|
    (define (foo [a int] [b [int 1 2 3]]) 0)
    (foo [-1 0] [[[1 2 3] [4 5 6]]])
    |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name foo) (id 39)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name a) (id 40)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name b) (id 41)))
                  (bound
                   (Arr
                    ((element (Literal IntLiteral))
                     (shape
                      ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
                       (Add ((const 3) (refs ())))))))))))
               (body (Scalar ((element (Literal (IntLiteral 0)))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 44)))
            (value (Ref ((id ((name foo) (id 39)))))))
           ((binding ((name a) (id 42)))
            (value
             (Frame
              ((dimensions (2))
               (elements
                ((Scalar ((element (Literal (IntLiteral -1)))))
                 (Scalar ((element (Literal (IntLiteral 0)))))))))))
           ((binding ((name b) (id 43)))
            (value
             (Frame
              ((dimensions (1))
               (elements
                ((Frame
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
                         (Scalar ((element (Literal (IntLiteral 6)))))))))))))))))))))
         (body
          (Map
           ((args
             (((binding ((name a) (id 45)))
               (value (Ref ((id ((name a) (id 42)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 44))))))
               (args (((id ((name a) (id 45)))) ((id ((name b) (id 43))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (frameShape ((Add ((const 2) (refs ())))))
            (type'
             (Arr
              ((element (Literal IntLiteral))
               (shape ((Add ((const 2) (refs ())))))))))))
         (frameShape ())
         (type'
          (Arr
           ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal IntLiteral)) (shape ((Add ((const 2) (refs ())))))))))) |}];
  checkAndPrint {| ([+ - *] 0 1) |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name f) (id 41)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding ((name +arg1) (id 39)))
         (value (Scalar ((element (Literal (IntLiteral 0)))))))
        ((binding ((name +arg2) (id 40)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 42))) (value (Ref ((id ((name f) (id 41)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 42))))))
            (args (((id ((name +arg1) (id 39)))) ((id ((name +arg2) (id 40))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ((Add ((const 3) (refs ())))))
         (type'
          (Arr
           ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))) |}];
  checkAndPrint {| ([+ - *] [1 2 3] 1) |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name f) (id 41)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding ((name +arg1) (id 39)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 40)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 42))) (value (Ref ((id ((name f) (id 41)))))))
           ((binding ((name +arg1) (id 43)))
            (value (Ref ((id ((name +arg1) (id 39)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 42))))))
            (args (((id ((name +arg1) (id 43)))) ((id ((name +arg2) (id 40))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ((Add ((const 3) (refs ())))))
         (type'
          (Arr
           ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))) |}];
  checkAndPrint {| ([+ - *] [1 2 3] [4 5 6]) |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name f) (id 41)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding ((name +arg1) (id 39)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 40)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 4)))))
              (Scalar ((element (Literal (IntLiteral 5)))))
              (Scalar ((element (Literal (IntLiteral 6)))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 42))) (value (Ref ((id ((name f) (id 41)))))))
           ((binding ((name +arg1) (id 43)))
            (value (Ref ((id ((name +arg1) (id 39)))))))
           ((binding ((name +arg2) (id 44)))
            (value (Ref ((id ((name +arg2) (id 40)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 42))))))
            (args (((id ((name +arg1) (id 43)))) ((id ((name +arg2) (id 44))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ((Add ((const 3) (refs ())))))
         (type'
          (Arr
           ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))) |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name index-value) (id 42)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))))
      (body
       (IndexLet
        ((indexArgs
          (((indexBinding ((name i) (id 39)))
            (indexValue (Runtime (Ref ((id ((name index-value) (id 42)))))))
            (sort Dim))))
         (body
          (Scalar
           ((element
             (Box
              ((indices
                ((Dimension ((const 0) (refs ((((name i) (id 39)) 1)))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 41)))
                     (value
                      (TypeApplication
                       ((tFunc
                         (IndexApplication
                          ((iFunc (Primitive ((name (Func Replicate)))))
                           (args
                            ((Shape
                              ((Add ((const 0) (refs ((((name i) (id 39)) 1)))))))
                             (Shape ()))))))
                        (args ((Atom (Literal IntLiteral))))))))
                    ((binding ((name replicate-value) (id 40)))
                     (value (Scalar ((element (Literal (IntLiteral 5)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 41))))))
                     (args (((id ((name replicate-value) (id 40))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name i) (id 39)) 1))))))))))))
                  (frameShape ())
                  (type'
                   (Arr
                    ((element (Literal IntLiteral))
                     (shape ((Add ((const 0) (refs ((((name i) (id 39)) 1)))))))))))))
               (bodyType
                (Arr
                 ((element (Literal IntLiteral))
                  (shape ((Add ((const 0) (refs ((((name i) (id 39)) 1)))))))))))))))))))
      (frameShape ((Add ((const 3) (refs ())))))
      (type'
       (Arr
        ((element
          (Sigma
           ((parameters (((binding ((name i) (id 39))) (bound Dim))))
            (body
             (Arr
              ((element (Literal IntLiteral))
               (shape ((Add ((const 0) (refs ((((name i) (id 39)) 1)))))))))))))
         (shape ((Add ((const 3) (refs ())))))))))) |}]
;;
