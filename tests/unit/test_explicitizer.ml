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
       (((binding ((name f) (id 134))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 132)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 133)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 134))))))
         (args (((id ((name +arg1) (id 132)))) ((id ((name +arg2) (id 133))))))
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ())))))) |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect
    {|
    (Map
     ((args
       (((binding ((name f) (id 134))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 132)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 133)))
         (value (Scalar ((element (Literal (IntLiteral 4)))))))))
      (body
       (Map
        ((args
          (((binding ((name +arg1) (id 135)))
            (value (Ref ((id ((name +arg1) (id 132)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 134))))))
            (args
             (((id ((name +arg1) (id 135)))) ((id ((name +arg2) (id 133))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
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
       (((binding ((name f) (id 134))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 132)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 133)))
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
          (((binding ((name +arg1) (id 135)))
            (value (Ref ((id ((name +arg1) (id 132)))))))
           ((binding ((name +arg2) (id 136)))
            (value (Ref ((id ((name +arg2) (id 133)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 134))))))
            (args
             (((id ((name +arg1) (id 135)))) ((id ((name +arg2) (id 136))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
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
       (((binding ((name f) (id 134))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 132)))
         (value
          (Frame
           ((dimensions (1))
            (elements
             ((Frame
               ((dimensions (2))
                (elements
                 ((Scalar ((element (Literal (IntLiteral 1)))))
                  (Scalar ((element (Literal (IntLiteral 2)))))))))))))))
        ((binding ((name +arg2) (id 133)))
         (value
          (Frame
           ((dimensions (1))
            (elements ((Scalar ((element (Literal (IntLiteral 3)))))))))))))
      (body
       (Map
        ((args
          (((binding ((name +arg2) (id 135)))
            (value (Ref ((id ((name +arg2) (id 133)))))))
           ((binding ((name +arg1) (id 136)))
            (value (Ref ((id ((name +arg1) (id 132)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg1) (id 137)))
               (value (Ref ((id ((name +arg1) (id 136)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 134))))))
               (args
                (((id ((name +arg1) (id 137)))) ((id ((name +arg2) (id 135))))))
               (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
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
       (((binding ((name f) (id 134))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 132)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 133)))
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
          (((binding ((name +arg2) (id 135)))
            (value (Ref ((id ((name +arg2) (id 133)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 134))))))
            (args
             (((id ((name +arg1) (id 132)))) ((id ((name +arg2) (id 135))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
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
       (((binding ((name f) (id 134))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 132)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 133)))
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
          (((binding ((name +arg2) (id 135)))
            (value (Ref ((id ((name +arg2) (id 133)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 134))))))
            (args
             (((id ((name +arg1) (id 132)))) ((id ((name +arg2) (id 135))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
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
       (((binding ((name foo) (id 132)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body (Ref ((id ((name foo) (id 132)))))) (frameShape ())
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
       (((binding ((name foo) (id 132)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name a) (id 133)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name b) (id 134)))
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
          (((binding ((name f) (id 137)))
            (value (Ref ((id ((name foo) (id 132)))))))
           ((binding ((name a) (id 135)))
            (value (Scalar ((element (Literal (IntLiteral 0)))))))
           ((binding ((name b) (id 136)))
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
           ((func (Ref ((id ((name f) (id 137))))))
            (args (((id ((name a) (id 135)))) ((id ((name b) (id 136))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
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
       (((binding ((name foo) (id 132)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name a) (id 133)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name b) (id 134)))
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
          (((binding ((name f) (id 137)))
            (value (Ref ((id ((name foo) (id 132)))))))
           ((binding ((name a) (id 135)))
            (value
             (Frame
              ((dimensions (2))
               (elements
                ((Scalar ((element (Literal (IntLiteral -1)))))
                 (Scalar ((element (Literal (IntLiteral 0)))))))))))
           ((binding ((name b) (id 136)))
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
             (((binding ((name a) (id 138)))
               (value (Ref ((id ((name a) (id 135)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 137))))))
               (args (((id ((name a) (id 138)))) ((id ((name b) (id 136))))))
               (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
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
       (((binding ((name f) (id 134)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding ((name +arg1) (id 132)))
         (value (Scalar ((element (Literal (IntLiteral 0)))))))
        ((binding ((name +arg2) (id 133)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 135)))
            (value (Ref ((id ((name f) (id 134)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 135))))))
            (args
             (((id ((name +arg1) (id 132)))) ((id ((name +arg2) (id 133))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
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
       (((binding ((name f) (id 134)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding ((name +arg1) (id 132)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 133)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 135)))
            (value (Ref ((id ((name f) (id 134)))))))
           ((binding ((name +arg1) (id 136)))
            (value (Ref ((id ((name +arg1) (id 132)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 135))))))
            (args
             (((id ((name +arg1) (id 136)))) ((id ((name +arg2) (id 133))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
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
       (((binding ((name f) (id 134)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding ((name +arg1) (id 132)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding ((name +arg2) (id 133)))
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
          (((binding ((name f) (id 135)))
            (value (Ref ((id ((name f) (id 134)))))))
           ((binding ((name +arg1) (id 136)))
            (value (Ref ((id ((name +arg1) (id 132)))))))
           ((binding ((name +arg2) (id 137)))
            (value (Ref ((id ((name +arg2) (id 133)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 135))))))
            (args
             (((id ((name +arg1) (id 136)))) ((id ((name +arg2) (id 137))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
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
       (((binding ((name index-value) (id 139)))
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
          (((indexBinding ((name i) (id 132)))
            (indexValue (Runtime (Ref ((id ((name index-value) (id 139)))))))
            (sort Dim))))
         (body
          (Scalar
           ((element
             (Box
              ((indices
                ((Dimension ((const 0) (refs ((((name i) (id 132)) 1)))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 138)))
                     (value
                      (TypeApplication
                       ((tFunc
                         (IndexApplication
                          ((iFunc
                            (Scalar
                             ((element
                               (IndexLambda
                                ((params
                                  (((binding ((name @s) (id 6))) (bound Shape))
                                   ((binding ((name @cell-shape) (id 7)))
                                    (bound Shape))))
                                 (body
                                  (Scalar
                                   ((element
                                     (TypeLambda
                                      ((params
                                        (((binding ((name t) (id 8)))
                                          (bound Atom))))
                                       (body
                                        (Scalar
                                         ((element
                                           (TermLambda
                                            ((params
                                              (((binding ((name v) (id 9)))
                                                (bound
                                                 (Arr
                                                  ((element
                                                    (AtomRef ((name t) (id 8))))
                                                   (shape
                                                    ((ShapeRef
                                                      ((name @cell-shape) (id 7)))))))))))
                                             (body
                                              (Map
                                               ((args
                                                 (((binding
                                                    ((name make) (id 10)))
                                                   (value
                                                    (Scalar
                                                     ((element
                                                       (TermLambda
                                                        ((params
                                                          (((binding
                                                             ((name foo) (id 11)))
                                                            (bound
                                                             (Arr
                                                              ((element
                                                                (Literal
                                                                 IntLiteral))
                                                               (shape ())))))
                                                           ((binding
                                                             ((name v) (id 12)))
                                                            (bound
                                                             (Arr
                                                              ((element
                                                                (AtomRef
                                                                 ((name t)
                                                                  (id 8))))
                                                               (shape
                                                                ((ShapeRef
                                                                  ((name
                                                                    @cell-shape)
                                                                   (id 7)))))))))))
                                                         (body
                                                          (Ref
                                                           ((id
                                                             ((name v) (id 12)))))))))))))))
                                                (body
                                                 (Map
                                                  ((args
                                                    (((binding
                                                       ((name f) (id 136)))
                                                      (value
                                                       (Ref
                                                        ((id
                                                          ((name make) (id 10)))))))
                                                     ((binding
                                                       ((name foo) (id 134)))
                                                      (value
                                                       (IndexApplication
                                                        ((iFunc
                                                          (Primitive
                                                           ((name (Val Iota)))))
                                                         (args
                                                          ((Shape
                                                            ((ShapeRef
                                                              ((name @s) (id 6)))))))))))
                                                     ((binding
                                                       ((name v) (id 135)))
                                                      (value
                                                       (Ref
                                                        ((id ((name v) (id 9)))))))))
                                                   (body
                                                    (Map
                                                     ((args
                                                       (((binding
                                                          ((name foo) (id 137)))
                                                         (value
                                                          (Ref
                                                           ((id
                                                             ((name foo)
                                                              (id 134)))))))))
                                                      (body
                                                       (TermApplication
                                                        ((func
                                                          (Ref
                                                           ((id
                                                             ((name f) (id 136))))))
                                                         (args
                                                          (((id
                                                             ((name foo)
                                                              (id 137))))
                                                           ((id
                                                             ((name v) (id 135))))))
                                                         (type'
                                                          (Arr
                                                           ((element
                                                             (AtomRef
                                                              ((name t) (id 8))))
                                                            (shape
                                                             ((ShapeRef
                                                               ((name
                                                                 @cell-shape)
                                                                (id 7)))))))))))
                                                      (frameShape
                                                       ((ShapeRef
                                                         ((name @s) (id 6)))))
                                                      (type'
                                                       (Arr
                                                        ((element
                                                          (AtomRef
                                                           ((name t) (id 8))))
                                                         (shape
                                                          ((ShapeRef
                                                            ((name @s) (id 6)))
                                                           (ShapeRef
                                                            ((name @cell-shape)
                                                             (id 7)))))))))))
                                                   (frameShape ())
                                                   (type'
                                                    (Arr
                                                     ((element
                                                       (AtomRef
                                                        ((name t) (id 8))))
                                                      (shape
                                                       ((ShapeRef
                                                         ((name @s) (id 6)))
                                                        (ShapeRef
                                                         ((name @cell-shape)
                                                          (id 7)))))))))))
                                                (frameShape ())
                                                (type'
                                                 (Arr
                                                  ((element
                                                    (AtomRef ((name t) (id 8))))
                                                   (shape
                                                    ((ShapeRef
                                                      ((name @s) (id 6)))
                                                     (ShapeRef
                                                      ((name @cell-shape) (id 7)))))))))))))))))))))))))))))
                           (args
                            ((Shape
                              ((Add ((const 0) (refs ((((name i) (id 132)) 1)))))))
                             (Shape ()))))))
                        (args ((Atom (Literal IntLiteral))))))))
                    ((binding ((name v) (id 133)))
                     (value (Scalar ((element (Literal (IntLiteral 5)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 138))))))
                     (args (((id ((name v) (id 133))))))
                     (type'
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape
                         ((Add ((const 0) (refs ((((name i) (id 132)) 1)))))))))))))
                  (frameShape ())
                  (type'
                   (Arr
                    ((element (Literal IntLiteral))
                     (shape ((Add ((const 0) (refs ((((name i) (id 132)) 1)))))))))))))
               (bodyType
                (Arr
                 ((element (Literal IntLiteral))
                  (shape ((Add ((const 0) (refs ((((name i) (id 132)) 1)))))))))))))))))))
      (frameShape ((Add ((const 3) (refs ())))))
      (type'
       (Arr
        ((element
          (Sigma
           ((parameters (((binding ((name i) (id 132))) (bound Dim))))
            (body
             (Arr
              ((element (Literal IntLiteral))
               (shape ((Add ((const 0) (refs ((((name i) (id 132)) 1)))))))))))))
         (shape ((Add ((const 3) (refs ())))))))))) |}]
;;
