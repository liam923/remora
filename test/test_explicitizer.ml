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
       (((binding ((name f) (id 47))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 45)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 46)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 47))))))
         (args (((id ((name +arg1) (id 45)))) ((id ((name +arg2) (id 46))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ())))))) |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name f) (id 47))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 45)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 46)))
         (value (Scalar ((element (Literal (IntLiteral 4)))))))))
      (body
       (Map
        ((args
          (((binding ((name +arg1) (id 48)))
            (value (Ref ((id ((name +arg1) (id 45)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 47))))))
            (args (((id ((name +arg1) (id 48)))) ((id ((name +arg2) (id 46))))))
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
       (((binding ((name f) (id 47))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 45)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 46)))
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
          (((binding ((name +arg1) (id 48)))
            (value (Ref ((id ((name +arg1) (id 45)))))))
           ((binding ((name +arg2) (id 49)))
            (value (Ref ((id ((name +arg2) (id 46)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 47))))))
            (args (((id ((name +arg1) (id 48)))) ((id ((name +arg2) (id 49))))))
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
       (((binding ((name f) (id 47))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 45)))
         (value
          (Frame
           ((dimensions (1))
            (elements
             ((Frame
               ((dimensions (2))
                (elements
                 ((Scalar ((element (Literal (IntLiteral 1)))))
                  (Scalar ((element (Literal (IntLiteral 2)))))))))))))))
        ((binding ((name +arg2) (id 46)))
         (value
          (Frame
           ((dimensions (1))
            (elements ((Scalar ((element (Literal (IntLiteral 3)))))))))))))
      (body
       (Map
        ((args
          (((binding ((name +arg2) (id 48)))
            (value (Ref ((id ((name +arg2) (id 46)))))))
           ((binding ((name +arg1) (id 49)))
            (value (Ref ((id ((name +arg1) (id 45)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg1) (id 50)))
               (value (Ref ((id ((name +arg1) (id 49)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 47))))))
               (args
                (((id ((name +arg1) (id 50)))) ((id ((name +arg2) (id 48))))))
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
       (((binding ((name f) (id 47))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 45)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 46)))
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
          (((binding ((name +arg2) (id 48)))
            (value (Ref ((id ((name +arg2) (id 46)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 47))))))
            (args (((id ((name +arg1) (id 45)))) ((id ((name +arg2) (id 48))))))
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
       (((binding ((name f) (id 47))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 45)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 46)))
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
          (((binding ((name +arg2) (id 48)))
            (value (Ref ((id ((name +arg2) (id 46)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 47))))))
            (args (((id ((name +arg1) (id 45)))) ((id ((name +arg2) (id 48))))))
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
       (((binding ((name foo) (id 45)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body (Ref ((id ((name foo) (id 45)))))) (frameShape ())
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
       (((binding ((name foo) (id 45)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name a) (id 46)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name b) (id 47)))
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
          (((binding ((name f) (id 50)))
            (value (Ref ((id ((name foo) (id 45)))))))
           ((binding ((name a) (id 48)))
            (value (Scalar ((element (Literal (IntLiteral 0)))))))
           ((binding ((name b) (id 49)))
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
           ((func (Ref ((id ((name f) (id 50))))))
            (args (((id ((name a) (id 48)))) ((id ((name b) (id 49))))))
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
       (((binding ((name foo) (id 45)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name a) (id 46)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name b) (id 47)))
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
          (((binding ((name f) (id 50)))
            (value (Ref ((id ((name foo) (id 45)))))))
           ((binding ((name a) (id 48)))
            (value
             (Frame
              ((dimensions (2))
               (elements
                ((Scalar ((element (Literal (IntLiteral -1)))))
                 (Scalar ((element (Literal (IntLiteral 0)))))))))))
           ((binding ((name b) (id 49)))
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
             (((binding ((name a) (id 51)))
               (value (Ref ((id ((name a) (id 48)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 50))))))
               (args (((id ((name a) (id 51)))) ((id ((name b) (id 49))))))
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
       (((binding ((name f) (id 47)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding ((name +arg1) (id 45)))
         (value (Scalar ((element (Literal (IntLiteral 0)))))))
        ((binding ((name +arg2) (id 46)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 48))) (value (Ref ((id ((name f) (id 47)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 48))))))
            (args (((id ((name +arg1) (id 45)))) ((id ((name +arg2) (id 46))))))
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
       (((binding ((name f) (id 47)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding ((name +arg1) (id 45)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 46)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 48))) (value (Ref ((id ((name f) (id 47)))))))
           ((binding ((name +arg1) (id 49)))
            (value (Ref ((id ((name +arg1) (id 45)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 48))))))
            (args (((id ((name +arg1) (id 49)))) ((id ((name +arg2) (id 46))))))
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
       (((binding ((name f) (id 47)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding ((name +arg1) (id 45)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 46)))
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
          (((binding ((name f) (id 48))) (value (Ref ((id ((name f) (id 47)))))))
           ((binding ((name +arg1) (id 49)))
            (value (Ref ((id ((name +arg1) (id 45)))))))
           ((binding ((name +arg2) (id 50)))
            (value (Ref ((id ((name +arg2) (id 46)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 48))))))
            (args (((id ((name +arg1) (id 49)))) ((id ((name +arg2) (id 50))))))
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
       (((binding ((name index-value) (id 48)))
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
          (((indexBinding ((name i) (id 45)))
            (indexValue (Runtime (Ref ((id ((name index-value) (id 48)))))))
            (sort Dim))))
         (body
          (Scalar
           ((element
             (Box
              ((indices
                ((Dimension ((const 0) (refs ((((name i) (id 45)) 1)))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 47)))
                     (value
                      (TypeApplication
                       ((tFunc
                         (IndexApplication
                          ((iFunc (Primitive ((name (Func Replicate)))))
                           (args
                            ((Shape
                              ((Add ((const 0) (refs ((((name i) (id 45)) 1)))))))
                             (Shape ()))))))
                        (args ((Atom (Literal IntLiteral))))))))
                    ((binding ((name replicate-value) (id 46)))
                     (value (Scalar ((element (Literal (IntLiteral 5)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 47))))))
                     (args (((id ((name replicate-value) (id 46))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name i) (id 45)) 1))))))))))))
                  (frameShape ())
                  (type'
                   (Arr
                    ((element (Literal IntLiteral))
                     (shape ((Add ((const 0) (refs ((((name i) (id 45)) 1)))))))))))))
               (bodyType
                (Arr
                 ((element (Literal IntLiteral))
                  (shape ((Add ((const 0) (refs ((((name i) (id 45)) 1)))))))))))))))))))
      (frameShape ((Add ((const 3) (refs ())))))
      (type'
       (Arr
        ((element
          (Sigma
           ((parameters (((binding ((name i) (id 45))) (bound Dim))))
            (body
             (Arr
              ((element (Literal IntLiteral))
               (shape ((Add ((const 0) (refs ((((name i) (id 45)) 1)))))))))))))
         (shape ((Add ((const 3) (refs ())))))))))) |}]
;;
