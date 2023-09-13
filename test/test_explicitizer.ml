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
       (((binding ((name f) (id 40))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 38)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 39)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 40))))))
         (args (((id ((name +arg1) (id 38)))) ((id ((name +arg2) (id 39))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ())))))) |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name f) (id 40))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 38)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 39)))
         (value (Scalar ((element (Literal (IntLiteral 4)))))))))
      (body
       (Map
        ((args
          (((binding ((name +arg1) (id 41)))
            (value (Ref ((id ((name +arg1) (id 38)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 40))))))
            (args (((id ((name +arg1) (id 41)))) ((id ((name +arg2) (id 39))))))
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
       (((binding ((name f) (id 40))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 38)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 39)))
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
          (((binding ((name +arg1) (id 41)))
            (value (Ref ((id ((name +arg1) (id 38)))))))
           ((binding ((name +arg2) (id 42)))
            (value (Ref ((id ((name +arg2) (id 39)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 40))))))
            (args (((id ((name +arg1) (id 41)))) ((id ((name +arg2) (id 42))))))
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
       (((binding ((name f) (id 40))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 38)))
         (value
          (Frame
           ((dimensions (1))
            (elements
             ((Frame
               ((dimensions (2))
                (elements
                 ((Scalar ((element (Literal (IntLiteral 1)))))
                  (Scalar ((element (Literal (IntLiteral 2)))))))))))))))
        ((binding ((name +arg2) (id 39)))
         (value
          (Frame
           ((dimensions (1))
            (elements ((Scalar ((element (Literal (IntLiteral 3)))))))))))))
      (body
       (Map
        ((args
          (((binding ((name +arg2) (id 41)))
            (value (Ref ((id ((name +arg2) (id 39)))))))
           ((binding ((name +arg1) (id 42)))
            (value (Ref ((id ((name +arg1) (id 38)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg1) (id 43)))
               (value (Ref ((id ((name +arg1) (id 42)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 40))))))
               (args
                (((id ((name +arg1) (id 43)))) ((id ((name +arg2) (id 41))))))
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
       (((binding ((name f) (id 40))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 38)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 39)))
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
          (((binding ((name +arg2) (id 41)))
            (value (Ref ((id ((name +arg2) (id 39)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 40))))))
            (args (((id ((name +arg1) (id 38)))) ((id ((name +arg2) (id 41))))))
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
       (((binding ((name f) (id 40))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 38)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 39)))
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
          (((binding ((name +arg2) (id 41)))
            (value (Ref ((id ((name +arg2) (id 39)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 40))))))
            (args (((id ((name +arg1) (id 38)))) ((id ((name +arg2) (id 41))))))
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
       (((binding ((name foo) (id 38)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body (Ref ((id ((name foo) (id 38)))))) (frameShape ())
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
       (((binding ((name foo) (id 38)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name a) (id 39)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name b) (id 40)))
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
          (((binding ((name f) (id 43)))
            (value (Ref ((id ((name foo) (id 38)))))))
           ((binding ((name a) (id 41)))
            (value (Scalar ((element (Literal (IntLiteral 0)))))))
           ((binding ((name b) (id 42)))
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
           ((func (Ref ((id ((name f) (id 43))))))
            (args (((id ((name a) (id 41)))) ((id ((name b) (id 42))))))
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
       (((binding ((name foo) (id 38)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name a) (id 39)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name b) (id 40)))
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
          (((binding ((name f) (id 43)))
            (value (Ref ((id ((name foo) (id 38)))))))
           ((binding ((name a) (id 41)))
            (value
             (Frame
              ((dimensions (2))
               (elements
                ((Scalar ((element (Literal (IntLiteral -1)))))
                 (Scalar ((element (Literal (IntLiteral 0)))))))))))
           ((binding ((name b) (id 42)))
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
             (((binding ((name a) (id 44)))
               (value (Ref ((id ((name a) (id 41)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 43))))))
               (args (((id ((name a) (id 44)))) ((id ((name b) (id 42))))))
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
       (((binding ((name f) (id 40)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding ((name +arg1) (id 38)))
         (value (Scalar ((element (Literal (IntLiteral 0)))))))
        ((binding ((name +arg2) (id 39)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 41))) (value (Ref ((id ((name f) (id 40)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 41))))))
            (args (((id ((name +arg1) (id 38)))) ((id ((name +arg2) (id 39))))))
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
       (((binding ((name f) (id 40)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding ((name +arg1) (id 38)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 39)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 41))) (value (Ref ((id ((name f) (id 40)))))))
           ((binding ((name +arg1) (id 42)))
            (value (Ref ((id ((name +arg1) (id 38)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 41))))))
            (args (((id ((name +arg1) (id 42)))) ((id ((name +arg2) (id 39))))))
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
       (((binding ((name f) (id 40)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding ((name +arg1) (id 38)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 39)))
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
          (((binding ((name f) (id 41))) (value (Ref ((id ((name f) (id 40)))))))
           ((binding ((name +arg1) (id 42)))
            (value (Ref ((id ((name +arg1) (id 38)))))))
           ((binding ((name +arg2) (id 43)))
            (value (Ref ((id ((name +arg2) (id 39)))))))))
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
        ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ())))))))))) |}]
;;
