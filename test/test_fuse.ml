open! Base
open Remora

let%expect_test "check inlining" =
  let pipeline =
    CompilerPipeline.(
      (module Parse.Stage (Source.UnitBuilder))
      @> (module TypeCheckStage.M (Source.UnitBuilder))
      @> (module Explicitize.Stage (Source.UnitBuilder))
      @> (module Inline.Stage (Source.UnitBuilder))
      @> (module Simplify.Stage (Source.UnitBuilder))
      @> (module Nest.Stage (Source.UnitBuilder))
      @> (module Fuse.Stage (Source.UnitBuilder))
      @> (module Show.Stage (Nested) (Source.UnitBuilder))
      @> empty)
  in
  let fuseAndPrint = TestPipeline.runAndPrint pipeline in
  fuseAndPrint "(+ [1 2 3] (+ [4 5 6] [7 8 9]))";
  [%expect
    {|
    (Let
     ((args
       (((binding ((name +arg1) (id 61)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Literal (IntLiteral 1)) (Literal (IntLiteral 2))
              (Literal (IntLiteral 3))))))))))
      (body
       (Let
        ((args
          (((binding ((name +arg1) (id 62)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Literal (IntLiteral 4)) (Literal (IntLiteral 5))
                 (Literal (IntLiteral 6))))))))
           ((binding ((name +arg2) (id 63)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Literal (IntLiteral 7)) (Literal (IntLiteral 8))
                 (Literal (IntLiteral 9))))))))))
         (body
          (Let
           ((args
             (((binding ((name fused-block-result) (id 73)))
               (value
                (ConsumerBlock
                 ((frameShape (Add ((const 3) (refs ()))))
                  (mapArgs
                   (((binding ((name +arg1) (id 65)))
                     (ref ((id ((name +arg1) (id 62))))))
                    ((binding ((name +arg2) (id 66)))
                     (ref ((id ((name +arg2) (id 63))))))
                    ((binding ((name +arg1) (id 69)))
                     (ref ((id ((name +arg1) (id 61))))))))
                  (mapIotas ())
                  (mapBody
                   (Let
                    ((args
                      (((binding ((name fusion-target-map-result) (id 72)))
                        (value
                         (Let
                          ((args
                            (((binding ((name +arg1) (id 56)))
                              (value (Ref ((id ((name +arg1) (id 65)))))))
                             ((binding ((name +arg2) (id 58)))
                              (value (Ref ((id ((name +arg2) (id 66)))))))))
                           (body
                            (ScalarPrimitive
                             ((op Add)
                              (args
                               ((Ref ((id ((name +arg1) (id 56)))))
                                (Ref ((id ((name +arg2) (id 58))))))))))))))))
                     (body
                      (Values
                       ((elements
                         ((Ref ((id ((name fusion-target-map-result) (id 72)))))
                          (Let
                           ((args
                             (((binding ((name +arg2) (id 70)))
                               (value
                                (Ref
                                 ((id ((name fusion-target-map-result) (id 72)))))))))
                            (body
                             (Let
                              ((args
                                (((binding ((name +arg1) (id 53)))
                                  (value (Ref ((id ((name +arg1) (id 69)))))))
                                 ((binding ((name +arg2) (id 60)))
                                  (value (Ref ((id ((name +arg2) (id 70)))))))))
                               (body
                                (ScalarPrimitive
                                 ((op Add)
                                  (args
                                   ((Ref ((id ((name +arg1) (id 53)))))
                                    (Ref ((id ((name +arg2) (id 60)))))))))))))))))))))))
                  (mapBodyMatcher
                   (Unpack
                    ((Binding ((name map-result) (id 64)))
                     (Binding ((name map-result) (id 68))))))
                  (mapResults
                   (((name map-result) (id 64)) ((name map-result) (id 68))))
                  (consumer ())))))))
            (body
             (Let
              ((args
                (((binding ((name fusion-target-map-result) (id 74)))
                  (value
                   (Values
                    ((elements
                      ((TupleDeref
                        ((index 0)
                         (tuple
                          (TupleDeref
                           ((index 0)
                            (tuple
                             (Ref ((id ((name fused-block-result) (id 73)))))))))))))))))
                 ((binding ((name fusion-archer-map-result) (id 71)))
                  (value
                   (Values
                    ((elements
                      ((TupleDeref
                        ((index 1)
                         (tuple
                          (TupleDeref
                           ((index 0)
                            (tuple
                             (Ref ((id ((name fused-block-result) (id 73)))))))))))))))))))
               (body
                (Let
                 ((args
                   (((binding ((name +arg2) (id 67)))
                     (value
                      (TupleDeref
                       ((index 0)
                        (tuple
                         (TupleDeref
                          ((index 0)
                           (tuple
                            (Values
                             ((elements
                               ((Ref
                                 ((id ((name fusion-target-map-result) (id 74)))))
                                (Values ((elements ())))))))))))))))))
                  (body
                   (TupleDeref
                    ((index 0)
                     (tuple
                      (TupleDeref
                       ((index 0)
                        (tuple
                         (Values
                          ((elements
                            ((Ref
                              ((id ((name fusion-archer-map-result) (id 71)))))
                             (Values ((elements ())))))))))))))))))))))))))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (Let
     ((args ())
      (body
       (Let
        ((args
          (((binding ((name +arg2) (id 81)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Literal (IntLiteral 1)) (Literal (IntLiteral 2))
                 (Literal (IntLiteral 3))))))))))
         (body
          (Let
           ((args ())
            (body
             (Let
              ((args ())
               (body
                (Let
                 ((args
                   (((binding ((name +arg2) (id 92)))
                     (value
                      (Frame
                       ((dimensions (3))
                        (elements
                         ((Literal (IntLiteral 7)) (Literal (IntLiteral 8))
                          (Literal (IntLiteral 9))))))))))
                  (body
                   (Let
                    ((args
                      (((binding ((name +arg2) (id 86)))
                        (value
                         (Frame
                          ((dimensions (3))
                           (elements
                            ((Literal (IntLiteral 4)) (Literal (IntLiteral 5))
                             (Literal (IntLiteral 6))))))))))
                     (body
                      (Let
                       ((args
                         (((binding ((name fused-block-result) (id 110)))
                           (value
                            (ConsumerBlock
                             ((frameShape (Add ((const 3) (refs ()))))
                              (mapArgs
                               (((binding ((name +arg2) (id 83)))
                                 (ref ((id ((name +arg2) (id 81))))))
                                ((binding ((name +arg2) (id 95)))
                                 (ref ((id ((name +arg2) (id 92))))))
                                ((binding ((name +arg2) (id 89)))
                                 (ref ((id ((name +arg2) (id 86))))))))
                              (mapIotas ())
                              (mapBody
                               (Let
                                ((args
                                  (((binding
                                     ((name fusion-target-map-result) (id 109)))
                                    (value
                                     (Let
                                      ((args
                                        (((binding ((name +arg2) (id 66)))
                                          (value
                                           (Ref ((id ((name +arg2) (id 83)))))))))
                                       (body
                                        (ScalarPrimitive
                                         ((op Add)
                                          (args
                                           ((Literal (IntLiteral 1))
                                            (Ref ((id ((name +arg2) (id 66))))))))))))))))
                                 (body
                                  (Values
                                   ((elements
                                     ((Ref
                                       ((id
                                         ((name fusion-target-map-result)
                                          (id 109)))))
                                      (Let
                                       ((args
                                         (((binding ((name +arg1) (id 94)))
                                           (value
                                            (Ref
                                             ((id
                                               ((name fusion-target-map-result)
                                                (id 109)))))))
                                          ((binding ((name +arg1) (id 88)))
                                           (value
                                            (Ref
                                             ((id
                                               ((name fusion-target-map-result)
                                                (id 109)))))))))
                                        (body
                                         (Let
                                          ((args
                                            (((binding
                                               ((name fusion-target-map-result)
                                                (id 105)))
                                              (value
                                               (Let
                                                ((args
                                                  (((binding
                                                     ((name +arg1) (id 76)))
                                                    (value
                                                     (Ref
                                                      ((id
                                                        ((name +arg1) (id 94)))))))
                                                   ((binding
                                                     ((name +arg2) (id 78)))
                                                    (value
                                                     (Ref
                                                      ((id
                                                        ((name +arg2) (id 95)))))))))
                                                 (body
                                                  (ScalarPrimitive
                                                   ((op Add)
                                                    (args
                                                     ((Ref
                                                       ((id
                                                         ((name +arg1) (id 76)))))
                                                      (Ref
                                                       ((id
                                                         ((name +arg2) (id 78))))))))))))))))
                                           (body
                                            (Values
                                             ((elements
                                               ((Ref
                                                 ((id
                                                   ((name
                                                     fusion-target-map-result)
                                                    (id 105)))))
                                                (Let
                                                 ((args
                                                   (((binding
                                                      ((name +arg2) (id 99)))
                                                     (value
                                                      (Ref
                                                       ((id
                                                         ((name
                                                           fusion-target-map-result)
                                                          (id 105)))))))))
                                                  (body
                                                   (Let
                                                    ((args
                                                      (((binding
                                                         ((name
                                                           fusion-target-map-result)
                                                          (id 101)))
                                                        (value
                                                         (Let
                                                          ((args
                                                            (((binding
                                                               ((name +arg1)
                                                                (id 69)))
                                                              (value
                                                               (Ref
                                                                ((id
                                                                  ((name +arg1)
                                                                   (id 88)))))))
                                                             ((binding
                                                               ((name +arg2)
                                                                (id 71)))
                                                              (value
                                                               (Ref
                                                                ((id
                                                                  ((name +arg2)
                                                                   (id 89)))))))))
                                                           (body
                                                            (ScalarPrimitive
                                                             ((op Add)
                                                              (args
                                                               ((Ref
                                                                 ((id
                                                                   ((name +arg1)
                                                                    (id 69)))))
                                                                (Ref
                                                                 ((id
                                                                   ((name +arg2)
                                                                    (id 71))))))))))))))))
                                                     (body
                                                      (Values
                                                       ((elements
                                                         ((Ref
                                                           ((id
                                                             ((name
                                                               fusion-target-map-result)
                                                              (id 101)))))
                                                          (Let
                                                           ((args
                                                             (((binding
                                                                ((name +arg1)
                                                                 (id 98)))
                                                               (value
                                                                (Ref
                                                                 ((id
                                                                   ((name
                                                                     fusion-target-map-result)
                                                                    (id 101)))))))))
                                                            (body
                                                             (Let
                                                              ((args
                                                                (((binding
                                                                   ((name +arg1)
                                                                    (id 73)))
                                                                  (value
                                                                   (Ref
                                                                    ((id
                                                                      ((name
                                                                        +arg1)
                                                                       (id 98)))))))
                                                                 ((binding
                                                                   ((name +arg2)
                                                                    (id 80)))
                                                                  (value
                                                                   (Ref
                                                                    ((id
                                                                      ((name
                                                                        +arg2)
                                                                       (id 99)))))))))
                                                               (body
                                                                (ScalarPrimitive
                                                                 ((op Add)
                                                                  (args
                                                                   ((Ref
                                                                     ((id
                                                                       ((name
                                                                        +arg1)
                                                                        (id 73)))))
                                                                    (Ref
                                                                     ((id
                                                                       ((name
                                                                        +arg2)
                                                                        (id 80)))))))))))))))))))))))))))))))))))))))))))
                              (mapBodyMatcher
                               (Unpack
                                ((Binding ((name map-result) (id 82)))
                                 (Unpack
                                  ((Binding ((name map-result) (id 93)))
                                   (Unpack
                                    ((Binding ((name map-result) (id 87)))
                                     (Binding ((name map-result) (id 97))))))))))
                              (mapResults
                               (((name map-result) (id 82))
                                ((name map-result) (id 93))
                                ((name map-result) (id 87))
                                ((name map-result) (id 97))))
                              (consumer ())))))))
                        (body
                         (Let
                          ((args
                            (((binding
                               ((name fusion-target-map-result) (id 111)))
                              (value
                               (Values
                                ((elements
                                  ((TupleDeref
                                    ((index 0)
                                     (tuple
                                      (TupleDeref
                                       ((index 0)
                                        (tuple
                                         (Ref
                                          ((id
                                            ((name fused-block-result) (id 110)))))))))))))))))
                             ((binding
                               ((name fusion-archer-map-result) (id 108)))
                              (value
                               (Values
                                ((elements
                                  ((TupleDeref
                                    ((index 1)
                                     (tuple
                                      (TupleDeref
                                       ((index 0)
                                        (tuple
                                         (Ref
                                          ((id
                                            ((name fused-block-result) (id 110)))))))))))
                                   (TupleDeref
                                    ((index 2)
                                     (tuple
                                      (TupleDeref
                                       ((index 0)
                                        (tuple
                                         (Ref
                                          ((id
                                            ((name fused-block-result) (id 110)))))))))))
                                   (TupleDeref
                                    ((index 3)
                                     (tuple
                                      (TupleDeref
                                       ((index 0)
                                        (tuple
                                         (Ref
                                          ((id
                                            ((name fused-block-result) (id 110)))))))))))))))))))
                           (body
                            (Let
                             ((args
                               (((binding ((name x) (id 84)))
                                 (value
                                  (TupleDeref
                                   ((index 0)
                                    (tuple
                                     (TupleDeref
                                      ((index 0)
                                       (tuple
                                        (Values
                                         ((elements
                                           ((Ref
                                             ((id
                                               ((name fusion-target-map-result)
                                                (id 111)))))
                                            (Values ((elements ())))))))))))))))))
                              (body
                               (Let
                                ((args
                                  (((binding ((name x) (id 67)))
                                    (value (Ref ((id ((name x) (id 84)))))))))
                                 (body
                                  (Let
                                   ((args ())
                                    (body
                                     (Let
                                      ((args
                                        (((binding ((name +arg1) (id 91)))
                                          (value (Ref ((id ((name x) (id 67)))))))))
                                       (body
                                        (Let
                                         ((args
                                           (((binding ((name +arg1) (id 85)))
                                             (value
                                              (Ref ((id ((name x) (id 67)))))))))
                                          (body
                                           (Let
                                            ((args
                                              (((binding
                                                 ((name fused-block-result)
                                                  (id 106)))
                                                (value
                                                 (Values
                                                  ((elements
                                                    ((Ref
                                                      ((id
                                                        ((name
                                                          fusion-archer-map-result)
                                                         (id 108)))))
                                                     (Values ((elements ())))))))))))
                                             (body
                                              (Let
                                               ((args
                                                 (((binding
                                                    ((name
                                                      fusion-target-map-result)
                                                     (id 107)))
                                                   (value
                                                    (Values
                                                     ((elements
                                                       ((TupleDeref
                                                         ((index 0)
                                                          (tuple
                                                           (TupleDeref
                                                            ((index 0)
                                                             (tuple
                                                              (Ref
                                                               ((id
                                                                 ((name
                                                                   fused-block-result)
                                                                  (id 106)))))))))))))))))
                                                  ((binding
                                                    ((name
                                                      fusion-archer-map-result)
                                                     (id 104)))
                                                   (value
                                                    (Values
                                                     ((elements
                                                       ((TupleDeref
                                                         ((index 1)
                                                          (tuple
                                                           (TupleDeref
                                                            ((index 0)
                                                             (tuple
                                                              (Ref
                                                               ((id
                                                                 ((name
                                                                   fused-block-result)
                                                                  (id 106)))))))))))
                                                        (TupleDeref
                                                         ((index 2)
                                                          (tuple
                                                           (TupleDeref
                                                            ((index 0)
                                                             (tuple
                                                              (Ref
                                                               ((id
                                                                 ((name
                                                                   fused-block-result)
                                                                  (id 106)))))))))))))))))))
                                                (body
                                                 (Let
                                                  ((args
                                                    (((binding
                                                       ((name +arg2) (id 96)))
                                                      (value
                                                       (TupleDeref
                                                        ((index 0)
                                                         (tuple
                                                          (TupleDeref
                                                           ((index 0)
                                                            (tuple
                                                             (Values
                                                              ((elements
                                                                ((Ref
                                                                  ((id
                                                                    ((name
                                                                      fusion-target-map-result)
                                                                     (id 107)))))
                                                                 (Values
                                                                  ((elements ())))))))))))))))))
                                                   (body
                                                    (Let
                                                     ((args ())
                                                      (body
                                                       (Let
                                                        ((args
                                                          (((binding
                                                             ((name
                                                               fused-block-result)
                                                              (id 102)))
                                                            (value
                                                             (Values
                                                              ((elements
                                                                ((Ref
                                                                  ((id
                                                                    ((name
                                                                      fusion-archer-map-result)
                                                                     (id 104)))))
                                                                 (Values
                                                                  ((elements ())))))))))))
                                                         (body
                                                          (Let
                                                           ((args
                                                             (((binding
                                                                ((name
                                                                  fusion-target-map-result)
                                                                 (id 103)))
                                                               (value
                                                                (Values
                                                                 ((elements
                                                                   ((TupleDeref
                                                                     ((index 0)
                                                                      (tuple
                                                                       (TupleDeref
                                                                        ((index
                                                                        0)
                                                                        (tuple
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        fused-block-result)
                                                                        (id 102)))))))))))))))))
                                                              ((binding
                                                                ((name
                                                                  fusion-archer-map-result)
                                                                 (id 100)))
                                                               (value
                                                                (Values
                                                                 ((elements
                                                                   ((TupleDeref
                                                                     ((index 1)
                                                                      (tuple
                                                                       (TupleDeref
                                                                        ((index
                                                                        0)
                                                                        (tuple
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        fused-block-result)
                                                                        (id 102)))))))))))))))))))
                                                            (body
                                                             (Let
                                                              ((args
                                                                (((binding
                                                                   ((name +arg1)
                                                                    (id 90)))
                                                                  (value
                                                                   (TupleDeref
                                                                    ((index 0)
                                                                     (tuple
                                                                      (TupleDeref
                                                                       ((index 0)
                                                                        (tuple
                                                                        (Values
                                                                        ((elements
                                                                        ((Ref
                                                                        ((id
                                                                        ((name
                                                                        fusion-target-map-result)
                                                                        (id 103)))))
                                                                        (Values
                                                                        ((elements
                                                                        ())))))))))))))))))
                                                               (body
                                                                (TupleDeref
                                                                 ((index 0)
                                                                  (tuple
                                                                   (TupleDeref
                                                                    ((index 0)
                                                                     (tuple
                                                                      (Values
                                                                       ((elements
                                                                        ((Ref
                                                                        ((id
                                                                        ((name
                                                                        fusion-archer-map-result)
                                                                        (id 100)))))
                                                                        (Values
                                                                        ((elements
                                                                        ()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) |}];
  fuseAndPrint "(reduce{int | 2 [] []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (Let
     ((args ())
      (body
       (Let
        ((args
          (((binding ((name +arg2) (id 60)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Literal (IntLiteral 1)) (Literal (IntLiteral 2))
                 (Literal (IntLiteral 3))))))))))
         (body
          (Let
           ((args
             (((binding ((name fused-block-result) (id 68)))
               (value
                (ConsumerBlock
                 ((frameShape (Add ((const 3) (refs ()))))
                  (mapArgs
                   (((binding ((name +arg2) (id 62)))
                     (ref ((id ((name +arg2) (id 60))))))))
                  (mapIotas ())
                  (mapBody
                   (Let
                    ((args
                      (((binding ((name fusion-target-map-result) (id 66)))
                        (value
                         (Let
                          ((args
                            (((binding ((name +arg2) (id 55)))
                              (value (Ref ((id ((name +arg2) (id 62)))))))))
                           (body
                            (ScalarPrimitive
                             ((op Add)
                              (args
                               ((Literal (IntLiteral 1))
                                (Ref ((id ((name +arg2) (id 55))))))))))))))))
                     (body
                      (Values
                       ((elements
                         ((Ref ((id ((name fusion-target-map-result) (id 66)))))
                          (Let
                           ((args
                             (((binding ((name reduce-arg) (id 64)))
                               (value
                                (Ref
                                 ((id ((name fusion-target-map-result) (id 66)))))))))
                            (body
                             (Values
                              ((elements
                                ((Ref ((id ((name reduce-arg) (id 64))))))))))))))))))))
                  (mapBodyMatcher
                   (Unpack
                    ((Binding ((name map-result) (id 61)))
                     (Unpack ((Binding ((name reduce-arg) (id 59))))))))
                  (mapResults (((name map-result) (id 61))))
                  (consumer
                   ((Reduce
                     (arg
                      ((firstBinding ((name reduce-arg1) (id 57)))
                       (secondBinding ((name reduce-arg2) (id 58)))
                       (production
                        (ProductionTupleAtom
                         ((productionId ((name reduce-arg) (id 59))))))))
                     (zero ())
                     (body
                      (ScalarPrimitive
                       ((op Add)
                        (args
                         ((Ref ((id ((name reduce-arg1) (id 57)))))
                          (Ref ((id ((name reduce-arg2) (id 58))))))))))
                     (d ((const 3) (refs ()))) (itemPad ()) (associative true)
                     (character Reduce))))))))))
            (body
             (Let
              ((args
                (((binding ((name fusion-target-map-result) (id 69)))
                  (value
                   (Values
                    ((elements
                      ((TupleDeref
                        ((index 0)
                         (tuple
                          (TupleDeref
                           ((index 0)
                            (tuple
                             (Ref ((id ((name fused-block-result) (id 68)))))))))))))))))
                 ((binding ((name fusion-archer-map-result) (id 65)))
                  (value (Values ((elements ())))))
                 ((binding ((name fusion-archer-consumer-result) (id 67)))
                  (value
                   (TupleDeref
                    ((index 1)
                     (tuple (Ref ((id ((name fused-block-result) (id 68))))))))))))
               (body
                (Let
                 ((args
                   (((binding ((name reduce-arg) (id 63)))
                     (value
                      (TupleDeref
                       ((index 0)
                        (tuple
                         (TupleDeref
                          ((index 0)
                           (tuple
                            (Values
                             ((elements
                               ((Ref
                                 ((id ((name fusion-target-map-result) (id 69)))))
                                (Values ((elements ())))))))))))))))))
                  (body
                   (TupleDeref
                    ((index 1)
                     (tuple
                      (Values
                       ((elements
                         ((Ref ((id ((name fusion-archer-map-result) (id 65)))))
                          (Ref
                           ((id ((name fusion-archer-consumer-result) (id 67))))))))))))))))))))))))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 [] []} + x)
        (reduce{int | 2 [] []} + x))
    |};
  [%expect
    {|
    (Let
     ((args ())
      (body
       (Let
        ((args
          (((binding ((name +arg2) (id 77)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Literal (IntLiteral 1)) (Literal (IntLiteral 2))
                 (Literal (IntLiteral 3))))))))))
         (body
          (Let
           ((args ())
            (body
             (Let
              ((args ())
               (body
                (Let
                 ((args ())
                  (body
                   (Let
                    ((args ())
                     (body
                      (Let
                       ((args ())
                        (body
                         (Let
                          ((args ())
                           (body
                            (Let
                             ((args ())
                              (body
                               (Let
                                ((args
                                  (((binding ((name fused-block-result) (id 95)))
                                    (value
                                     (ConsumerBlock
                                      ((frameShape (Add ((const 3) (refs ()))))
                                       (mapArgs
                                        (((binding ((name +arg2) (id 79)))
                                          (ref ((id ((name +arg2) (id 77))))))))
                                       (mapIotas ())
                                       (mapBody
                                        (Let
                                         ((args
                                           (((binding
                                              ((name fusion-target-map-result)
                                               (id 93)))
                                             (value
                                              (Let
                                               ((args
                                                 (((binding
                                                    ((name
                                                      fusion-target-map-result)
                                                     (id 88)))
                                                   (value
                                                    (Let
                                                     ((args
                                                       (((binding
                                                          ((name +arg2) (id 63)))
                                                         (value
                                                          (Ref
                                                           ((id
                                                             ((name +arg2)
                                                              (id 79)))))))))
                                                      (body
                                                       (ScalarPrimitive
                                                        ((op Add)
                                                         (args
                                                          ((Literal
                                                            (IntLiteral 4))
                                                           (Ref
                                                            ((id
                                                              ((name +arg2)
                                                               (id 63))))))))))))))))
                                                (body
                                                 (Values
                                                  ((elements
                                                    ((Ref
                                                      ((id
                                                        ((name
                                                          fusion-target-map-result)
                                                         (id 88)))))
                                                     (Let
                                                      ((args
                                                        (((binding
                                                           ((name reduce-arg)
                                                            (id 83)))
                                                          (value
                                                           (Ref
                                                            ((id
                                                              ((name
                                                                fusion-target-map-result)
                                                               (id 88)))))))))
                                                       (body
                                                        (Values
                                                         ((elements
                                                           ((Ref
                                                             ((id
                                                               ((name reduce-arg)
                                                                (id 83)))))))))))))))))))))))
                                          (body
                                           (Values
                                            ((elements
                                              ((Ref
                                                ((id
                                                  ((name
                                                    fusion-target-map-result)
                                                   (id 93)))))
                                               (Let
                                                ((args
                                                  (((binding
                                                     ((name reduce-arg) (id 86)))
                                                    (value
                                                     (TupleDeref
                                                      ((index 0)
                                                       (tuple
                                                        (Ref
                                                         ((id
                                                           ((name
                                                             fusion-target-map-result)
                                                            (id 93))))))))))))
                                                 (body
                                                  (Values
                                                   ((elements
                                                     ((Ref
                                                       ((id
                                                         ((name reduce-arg)
                                                          (id 86))))))))))))))))))))
                                       (mapBodyMatcher
                                        (Unpack
                                         ((Unpack
                                           ((Binding ((name map-result) (id 78)))
                                            (Unpack
                                             ((Binding
                                               ((name reduce-arg) (id 81)))))))
                                          (Unpack
                                           ((Binding ((name reduce-arg) (id 84))))))))
                                       (mapResults (((name map-result) (id 78))))
                                       (consumer
                                        ((Reduce
                                          (arg
                                           ((firstBinding
                                             ((name fused-reduce-arg1) (id 97)))
                                            (secondBinding
                                             ((name fused-reduce-arg2) (id 98)))
                                            (production
                                             (ProductionTuple
                                              (elements
                                               ((ProductionTupleAtom
                                                 ((productionId
                                                   ((name reduce-arg) (id 81)))))
                                                (ProductionTupleAtom
                                                 ((productionId
                                                   ((name reduce-arg) (id 84)))))))))))
                                          (zero ())
                                          (body
                                           (Let
                                            ((args
                                              (((binding
                                                 ((name reduce-arg1) (id 66)))
                                                (value
                                                 (TupleDeref
                                                  ((index 0)
                                                   (tuple
                                                    (Ref
                                                     ((id
                                                       ((name fused-reduce-arg1)
                                                        (id 97))))))))))
                                               ((binding
                                                 ((name reduce-arg2) (id 67)))
                                                (value
                                                 (TupleDeref
                                                  ((index 0)
                                                   (tuple
                                                    (Ref
                                                     ((id
                                                       ((name fused-reduce-arg2)
                                                        (id 98))))))))))
                                               ((binding
                                                 ((name reduce-arg1) (id 74)))
                                                (value
                                                 (TupleDeref
                                                  ((index 1)
                                                   (tuple
                                                    (Ref
                                                     ((id
                                                       ((name fused-reduce-arg1)
                                                        (id 97))))))))))
                                               ((binding
                                                 ((name reduce-arg2) (id 75)))
                                                (value
                                                 (TupleDeref
                                                  ((index 1)
                                                   (tuple
                                                    (Ref
                                                     ((id
                                                       ((name fused-reduce-arg2)
                                                        (id 98))))))))))))
                                             (body
                                              (Values
                                               ((elements
                                                 ((ScalarPrimitive
                                                   ((op Add)
                                                    (args
                                                     ((Ref
                                                       ((id
                                                         ((name reduce-arg1)
                                                          (id 66)))))
                                                      (Ref
                                                       ((id
                                                         ((name reduce-arg2)
                                                          (id 67)))))))))
                                                  (ScalarPrimitive
                                                   ((op Add)
                                                    (args
                                                     ((Ref
                                                       ((id
                                                         ((name reduce-arg1)
                                                          (id 74)))))
                                                      (Ref
                                                       ((id
                                                         ((name reduce-arg2)
                                                          (id 75)))))))))))))))))
                                          (d ((const 3) (refs ()))) (itemPad ())
                                          (associative true) (character Reduce))))))))))
                                 (body
                                  (Let
                                   ((args
                                     (((binding
                                        ((name fusion-target-map-result) (id 96)))
                                       (value
                                        (Values
                                         ((elements
                                           ((TupleDeref
                                             ((index 0)
                                              (tuple
                                               (TupleDeref
                                                ((index 0)
                                                 (tuple
                                                  (Ref
                                                   ((id
                                                     ((name fused-block-result)
                                                      (id 95)))))))))))))))))
                                      ((binding
                                        ((name fusion-archer-map-result) (id 92)))
                                       (value (Values ((elements ())))))
                                      ((binding
                                        ((name fusion-archer-consumer-result)
                                         (id 94)))
                                       (value
                                        (TupleDeref
                                         ((index 1)
                                          (tuple
                                           (TupleDeref
                                            ((index 1)
                                             (tuple
                                              (Ref
                                               ((id
                                                 ((name fused-block-result)
                                                  (id 95)))))))))))))))
                                    (body
                                     (Let
                                      ((args
                                        (((binding
                                           ((name fused-block-result) (id 90)))
                                          (value
                                           (Values
                                            ((elements
                                              ((Ref
                                                ((id
                                                  ((name
                                                    fusion-target-map-result)
                                                   (id 96)))))
                                               (TupleDeref
                                                ((index 0)
                                                 (tuple
                                                  (TupleDeref
                                                   ((index 1)
                                                    (tuple
                                                     (Ref
                                                      ((id
                                                        ((name
                                                          fused-block-result)
                                                         (id 95)))))))))))))))))))
                                       (body
                                        (Let
                                         ((args
                                           (((binding
                                              ((name fusion-target-map-result)
                                               (id 91)))
                                             (value
                                              (Values
                                               ((elements
                                                 ((TupleDeref
                                                   ((index 0)
                                                    (tuple
                                                     (TupleDeref
                                                      ((index 0)
                                                       (tuple
                                                        (Ref
                                                         ((id
                                                           ((name
                                                             fused-block-result)
                                                            (id 90)))))))))))))))))
                                            ((binding
                                              ((name fusion-archer-map-result)
                                               (id 87)))
                                             (value (Values ((elements ())))))
                                            ((binding
                                              ((name
                                                fusion-archer-consumer-result)
                                               (id 89)))
                                             (value
                                              (TupleDeref
                                               ((index 1)
                                                (tuple
                                                 (Ref
                                                  ((id
                                                    ((name fused-block-result)
                                                     (id 90))))))))))))
                                          (body
                                           (Let
                                            ((args
                                              (((binding ((name x) (id 80)))
                                                (value
                                                 (TupleDeref
                                                  ((index 0)
                                                   (tuple
                                                    (TupleDeref
                                                     ((index 0)
                                                      (tuple
                                                       (Values
                                                        ((elements
                                                          ((Ref
                                                            ((id
                                                              ((name
                                                                fusion-target-map-result)
                                                               (id 91)))))
                                                           (Values
                                                            ((elements ())))))))))))))))))
                                             (body
                                              (Let
                                               ((args
                                                 (((binding ((name x) (id 64)))
                                                   (value
                                                    (Ref
                                                     ((id ((name x) (id 80)))))))))
                                                (body
                                                 (ScalarPrimitive
                                                  ((op Add)
                                                   (args
                                                    ((Let
                                                      ((args
                                                        (((binding
                                                           ((name reduce-arg)
                                                            (id 82)))
                                                          (value
                                                           (Ref
                                                            ((id
                                                              ((name x) (id 64)))))))))
                                                       (body
                                                        (TupleDeref
                                                         ((index 1)
                                                          (tuple
                                                           (Values
                                                            ((elements
                                                              ((Ref
                                                                ((id
                                                                  ((name
                                                                    fusion-archer-map-result)
                                                                   (id 87)))))
                                                               (Ref
                                                                ((id
                                                                  ((name
                                                                    fusion-archer-consumer-result)
                                                                   (id 89)))))))))))))))
                                                     (Let
                                                      ((args
                                                        (((binding
                                                           ((name reduce-arg)
                                                            (id 85)))
                                                          (value
                                                           (Ref
                                                            ((id
                                                              ((name x) (id 64)))))))))
                                                       (body
                                                        (TupleDeref
                                                         ((index 1)
                                                          (tuple
                                                           (Values
                                                            ((elements
                                                              ((Ref
                                                                ((id
                                                                  ((name
                                                                    fusion-archer-map-result)
                                                                   (id 92)))))
                                                               (Ref
                                                                ((id
                                                                  ((name
                                                                    fusion-archer-consumer-result)
                                                                   (id 94)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) |}]
;;
