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
       (((binding f.150) (value (Primitive ((name (Func Add))))))
        ((binding +arg1.148)
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding +arg2.149)
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id f.150)))) (args (((id +arg1.148)) ((id +arg2.149))))
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ())))))) |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect
    {|
    (Map
     ((args
       (((binding f.150) (value (Primitive ((name (Func Add))))))
        ((binding +arg1.148)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding +arg2.149)
         (value (Scalar ((element (Literal (IntLiteral 4)))))))))
      (body
       (Map
        ((args (((binding +arg1.151) (value (Ref ((id +arg1.148)))))))
         (body
          (TermApplication
           ((func (Ref ((id f.150)))) (args (((id +arg1.151)) ((id +arg2.149))))
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
       (((binding f.150) (value (Primitive ((name (Func Add))))))
        ((binding +arg1.148)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding +arg2.149)
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
          (((binding +arg1.151) (value (Ref ((id +arg1.148)))))
           ((binding +arg2.152) (value (Ref ((id +arg2.149)))))))
         (body
          (TermApplication
           ((func (Ref ((id f.150)))) (args (((id +arg1.151)) ((id +arg2.152))))
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
       (((binding f.150) (value (Primitive ((name (Func Add))))))
        ((binding +arg1.148)
         (value
          (Frame
           ((dimensions (1))
            (elements
             ((Frame
               ((dimensions (2))
                (elements
                 ((Scalar ((element (Literal (IntLiteral 1)))))
                  (Scalar ((element (Literal (IntLiteral 2)))))))))))))))
        ((binding +arg2.149)
         (value
          (Frame
           ((dimensions (1))
            (elements ((Scalar ((element (Literal (IntLiteral 3)))))))))))))
      (body
       (Map
        ((args
          (((binding +arg2.151) (value (Ref ((id +arg2.149)))))
           ((binding +arg1.152) (value (Ref ((id +arg1.148)))))))
         (body
          (Map
           ((args (((binding +arg1.153) (value (Ref ((id +arg1.152)))))))
            (body
             (TermApplication
              ((func (Ref ((id f.150))))
               (args (((id +arg1.153)) ((id +arg2.151))))
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
       (((binding f.150) (value (Primitive ((name (Func Add))))))
        ((binding +arg1.148)
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding +arg2.149)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))
              (Scalar ((element (Literal (IntLiteral 4)))))))))))))
      (body
       (Map
        ((args (((binding +arg2.151) (value (Ref ((id +arg2.149)))))))
         (body
          (TermApplication
           ((func (Ref ((id f.150)))) (args (((id +arg1.148)) ((id +arg2.151))))
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
       (((binding f.150) (value (Primitive ((name (Func Add))))))
        ((binding +arg1.148)
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding +arg2.149)
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
        ((args (((binding +arg2.151) (value (Ref ((id +arg2.149)))))))
         (body
          (TermApplication
           ((func (Ref ((id f.150)))) (args (((id +arg1.148)) ((id +arg2.151))))
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
       (((binding foo.148) (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body (Ref ((id foo.148)))) (frameShape ())
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
       (((binding foo.148)
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding a.149)
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding b.150)
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
          (((binding f.153) (value (Ref ((id foo.148)))))
           ((binding a.151)
            (value (Scalar ((element (Literal (IntLiteral 0)))))))
           ((binding b.152)
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
           ((func (Ref ((id f.153)))) (args (((id a.151)) ((id b.152))))
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
       (((binding foo.148)
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding a.149)
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding b.150)
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
          (((binding f.153) (value (Ref ((id foo.148)))))
           ((binding a.151)
            (value
             (Frame
              ((dimensions (2))
               (elements
                ((Scalar ((element (Literal (IntLiteral -1)))))
                 (Scalar ((element (Literal (IntLiteral 0)))))))))))
           ((binding b.152)
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
           ((args (((binding a.154) (value (Ref ((id a.151)))))))
            (body
             (TermApplication
              ((func (Ref ((id f.153)))) (args (((id a.154)) ((id b.152))))
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
       (((binding f.150)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding +arg1.148)
         (value (Scalar ((element (Literal (IntLiteral 0)))))))
        ((binding +arg2.149)
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body
       (Map
        ((args (((binding f.151) (value (Ref ((id f.150)))))))
         (body
          (TermApplication
           ((func (Ref ((id f.151)))) (args (((id +arg1.148)) ((id +arg2.149))))
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
       (((binding f.150)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding +arg1.148)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding +arg2.149)
         (value (Scalar ((element (Literal (IntLiteral 1)))))))))
      (body
       (Map
        ((args
          (((binding f.151) (value (Ref ((id f.150)))))
           ((binding +arg1.152) (value (Ref ((id +arg1.148)))))))
         (body
          (TermApplication
           ((func (Ref ((id f.151)))) (args (((id +arg1.152)) ((id +arg2.149))))
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
       (((binding f.150)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Primitive ((name (Func Add)))) (Primitive ((name (Func Sub))))
              (Primitive ((name (Func Mul))))))))))
        ((binding +arg1.148)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))
              (Scalar ((element (Literal (IntLiteral 3)))))))))))
        ((binding +arg2.149)
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
          (((binding f.151) (value (Ref ((id f.150)))))
           ((binding +arg1.152) (value (Ref ((id +arg1.148)))))
           ((binding +arg2.153) (value (Ref ((id +arg2.149)))))))
         (body
          (TermApplication
           ((func (Ref ((id f.151)))) (args (((id +arg1.152)) ((id +arg2.153))))
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
       (((binding index-value.155)
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
          (((indexBinding i.148)
            (indexValue (Runtime (Ref ((id index-value.155))))) (sort Dim))))
         (body
          (Scalar
           ((element
             (Box
              ((indices ((Dimension ((const 0) (refs ((i.148 1)))))))
               (body
                (Map
                 ((args
                   (((binding f.154)
                     (value
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
                                                   (shape
                                                    ((ShapeRef @cell-shape.7)))))))))
                                             (body
                                              (Map
                                               ((args
                                                 (((binding make.10)
                                                   (value
                                                    (Scalar
                                                     ((element
                                                       (TermLambda
                                                        ((params
                                                          (((binding foo.11)
                                                            (bound
                                                             (Arr
                                                              ((element
                                                                (Literal
                                                                 IntLiteral))
                                                               (shape ())))))
                                                           ((binding v.12)
                                                            (bound
                                                             (Arr
                                                              ((element
                                                                (AtomRef t.8))
                                                               (shape
                                                                ((ShapeRef
                                                                  @cell-shape.7)))))))))
                                                         (body (Ref ((id v.12)))))))))))))
                                                (body
                                                 (Map
                                                  ((args
                                                    (((binding f.152)
                                                      (value
                                                       (Ref ((id make.10)))))
                                                     ((binding foo.150)
                                                      (value
                                                       (IndexApplication
                                                        ((iFunc
                                                          (Primitive
                                                           ((name (Val Iota)))))
                                                         (args
                                                          ((Shape
                                                            ((ShapeRef @s.6)))))))))
                                                     ((binding v.151)
                                                      (value (Ref ((id v.9)))))))
                                                   (body
                                                    (Map
                                                     ((args
                                                       (((binding foo.153)
                                                         (value
                                                          (Ref ((id foo.150)))))))
                                                      (body
                                                       (TermApplication
                                                        ((func
                                                          (Ref ((id f.152))))
                                                         (args
                                                          (((id foo.153))
                                                           ((id v.151))))
                                                         (type'
                                                          (Arr
                                                           ((element
                                                             (AtomRef t.8))
                                                            (shape
                                                             ((ShapeRef
                                                               @cell-shape.7)))))))))
                                                      (frameShape
                                                       ((ShapeRef @s.6)))
                                                      (type'
                                                       (Arr
                                                        ((element (AtomRef t.8))
                                                         (shape
                                                          ((ShapeRef @s.6)
                                                           (ShapeRef
                                                            @cell-shape.7)))))))))
                                                   (frameShape ())
                                                   (type'
                                                    (Arr
                                                     ((element (AtomRef t.8))
                                                      (shape
                                                       ((ShapeRef @s.6)
                                                        (ShapeRef @cell-shape.7)))))))))
                                                (frameShape ())
                                                (type'
                                                 (Arr
                                                  ((element (AtomRef t.8))
                                                   (shape
                                                    ((ShapeRef @s.6)
                                                     (ShapeRef @cell-shape.7)))))))))))))))))))))))))))
                           (args
                            ((Shape ((Add ((const 0) (refs ((i.148 1)))))))
                             (Shape ()))))))
                        (args ((Atom (Literal IntLiteral))))))))
                    ((binding v.149)
                     (value (Scalar ((element (Literal (IntLiteral 5)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id f.154)))) (args (((id v.149))))
                     (type'
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape ((Add ((const 0) (refs ((i.148 1)))))))))))))
                  (frameShape ())
                  (type'
                   (Arr
                    ((element (Literal IntLiteral))
                     (shape ((Add ((const 0) (refs ((i.148 1)))))))))))))
               (bodyType
                (Arr
                 ((element (Literal IntLiteral))
                  (shape ((Add ((const 0) (refs ((i.148 1)))))))))))))))))))
      (frameShape ((Add ((const 3) (refs ())))))
      (type'
       (Arr
        ((element
          (Sigma
           ((parameters (((binding i.148) (bound Dim))))
            (body
             (Arr
              ((element (Literal IntLiteral))
               (shape ((Add ((const 0) (refs ((i.148 1)))))))))))))
         (shape ((Add ((const 3) (refs ())))))))))) |}]
;;
