open! Base
open Remora

let%expect_test "check fusing" =
  let pipeline =
    CompilerPipeline.(
      (module Parse.Stage (Source.UnitBuilder))
      @> (module TypeCheckStage.M (Source.UnitBuilder))
      @> (module Explicitize.Stage (Source.UnitBuilder))
      @> (module Inline.Stage (Source.UnitBuilder))
      @> (module Simplify.Stage (Source.UnitBuilder))
      @> (module Nest.Stage (Source.UnitBuilder))
      @> (module Fuse.Stage (Source.UnitBuilder))
      @> (module SimplifyNested.Stage (Source.UnitBuilder))
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
           ((dimension 3)
            (elements
             ((Literal (IntLiteral 1)) (Literal (IntLiteral 2))
              (Literal (IntLiteral 3))))))))))
      (body
       (Let
        ((args
          (((binding ((name +arg1) (id 62)))
            (value
             (Frame
              ((dimension 3)
               (elements
                ((Literal (IntLiteral 4)) (Literal (IntLiteral 5))
                 (Literal (IntLiteral 6))))))))
           ((binding ((name +arg2) (id 63)))
            (value
             (Frame
              ((dimension 3)
               (elements
                ((Literal (IntLiteral 7)) (Literal (IntLiteral 8))
                 (Literal (IntLiteral 9))))))))))
         (body
          (TupleDeref
           ((index 1)
            (tuple
             (TupleDeref
              ((index 0)
               (tuple
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
                         (ScalarPrimitive
                          ((op Add)
                           (args
                            ((Ref ((id ((name +arg1) (id 65)))))
                             (Ref ((id ((name +arg2) (id 66)))))))))))))
                     (body
                      (Values
                       ((elements
                         ((Ref ((id ((name fusion-target-map-result) (id 72)))))
                          (ScalarPrimitive
                           ((op Add)
                            (args
                             ((Ref ((id ((name +arg1) (id 69)))))
                              (Ref
                               ((id ((name fusion-target-map-result) (id 72)))))))))))))))))
                  (mapBodyMatcher
                   (Unpack
                    ((Binding ((name map-result) (id 64)))
                     (Binding ((name map-result) (id 68))))))
                  (mapResults
                   (((name map-result) (id 64)) ((name map-result) (id 68))))
                  (consumer ()))))))))))))))) |}];
  fuseAndPrint {|
    (define x (+ 1 [1 2 3]))
    (+ (+ x [4 5 6]) (+ x [7 8 9]))
  |};
  [%expect
    {|
    (Let
     ((args
       (((binding ((name +arg2) (id 81)))
         (value
          (Frame
           ((dimension 3)
            (elements
             ((Literal (IntLiteral 1)) (Literal (IntLiteral 2))
              (Literal (IntLiteral 3))))))))))
      (body
       (Let
        ((args
          (((binding ((name +arg2) (id 92)))
            (value
             (Frame
              ((dimension 3)
               (elements
                ((Literal (IntLiteral 7)) (Literal (IntLiteral 8))
                 (Literal (IntLiteral 9))))))))))
         (body
          (Let
           ((args
             (((binding ((name +arg2) (id 86)))
               (value
                (Frame
                 ((dimension 3)
                  (elements
                   ((Literal (IntLiteral 4)) (Literal (IntLiteral 5))
                    (Literal (IntLiteral 6))))))))))
            (body
             (TupleDeref
              ((index 3)
               (tuple
                (TupleDeref
                 ((index 0)
                  (tuple
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
                         (((binding ((name fusion-target-map-result) (id 109)))
                           (value
                            (ScalarPrimitive
                             ((op Add)
                              (args
                               ((Literal (IntLiteral 1))
                                (Ref ((id ((name +arg2) (id 83)))))))))))))
                        (body
                         (Values
                          ((elements
                            ((Ref
                              ((id ((name fusion-target-map-result) (id 109)))))
                             (Let
                              ((args
                                (((binding
                                   ((name fusion-target-map-result) (id 105)))
                                  (value
                                   (ScalarPrimitive
                                    ((op Add)
                                     (args
                                      ((Ref
                                        ((id
                                          ((name fusion-target-map-result)
                                           (id 109)))))
                                       (Ref ((id ((name +arg2) (id 95)))))))))))))
                               (body
                                (Values
                                 ((elements
                                   ((Ref
                                     ((id
                                       ((name fusion-target-map-result) (id 105)))))
                                    (Let
                                     ((args
                                       (((binding
                                          ((name fusion-target-map-result)
                                           (id 101)))
                                         (value
                                          (ScalarPrimitive
                                           ((op Add)
                                            (args
                                             ((Ref
                                               ((id
                                                 ((name fusion-target-map-result)
                                                  (id 109)))))
                                              (Ref ((id ((name +arg2) (id 89)))))))))))))
                                      (body
                                       (Values
                                        ((elements
                                          ((Ref
                                            ((id
                                              ((name fusion-target-map-result)
                                               (id 101)))))
                                           (ScalarPrimitive
                                            ((op Add)
                                             (args
                                              ((Ref
                                                ((id
                                                  ((name
                                                    fusion-target-map-result)
                                                   (id 101)))))
                                               (Ref
                                                ((id
                                                  ((name
                                                    fusion-target-map-result)
                                                   (id 105)))))))))))))))))))))))))))))))
                     (mapBodyMatcher
                      (Unpack
                       ((Binding ((name map-result) (id 82)))
                        (Unpack
                         ((Binding ((name map-result) (id 93)))
                          (Unpack
                           ((Binding ((name map-result) (id 87)))
                            (Binding ((name map-result) (id 97))))))))))
                     (mapResults
                      (((name map-result) (id 82)) ((name map-result) (id 93))
                       ((name map-result) (id 87)) ((name map-result) (id 97))))
                     (consumer ())))))))))))))))))) |}];
  fuseAndPrint "(reduce{int | 2 [] []} + (+ 1 [1 2 3]))";
  [%expect
    {|
    (Let
     ((args
       (((binding ((name +arg2) (id 60)))
         (value
          (Frame
           ((dimension 3)
            (elements
             ((Literal (IntLiteral 1)) (Literal (IntLiteral 2))
              (Literal (IntLiteral 3))))))))))
      (body
       (TupleDeref
        ((index 1)
         (tuple
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
                   (ScalarPrimitive
                    ((op Add)
                     (args
                      ((Literal (IntLiteral 1))
                       (Ref ((id ((name +arg2) (id 62)))))))))))))
               (body
                (Values
                 ((elements
                   ((Ref ((id ((name fusion-target-map-result) (id 66)))))
                    (Values
                     ((elements
                       ((Ref ((id ((name fusion-target-map-result) (id 66)))))))))))))))))
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
               (character Reduce)))))))))))) |}];
  fuseAndPrint
    {|
      (define x (+ 4 [1 2 3]))
      (+
        (reduce{int | 2 [] []} + (+ 5 x))
        (reduce{int | 2 [] []} + (+ 6 x)))
    |};
  [%expect
    {|
    (Let
     ((args
       (((binding ((name +arg2) (id 93)))
         (value
          (Frame
           ((dimension 3)
            (elements
             ((Literal (IntLiteral 1)) (Literal (IntLiteral 2))
              (Literal (IntLiteral 3))))))))))
      (body
       (Let
        ((args
          (((binding ((name fused-block-result) (id 127)))
            (value
             (ConsumerBlock
              ((frameShape (Add ((const 3) (refs ()))))
               (mapArgs
                (((binding ((name +arg2) (id 95)))
                  (ref ((id ((name +arg2) (id 93))))))))
               (mapIotas ())
               (mapBody
                (Let
                 ((args
                   (((binding ((name fusion-target-map-result) (id 125)))
                     (value
                      (Let
                       ((args
                         (((binding ((name fusion-target-map-result) (id 120)))
                           (value
                            (ScalarPrimitive
                             ((op Add)
                              (args
                               ((Literal (IntLiteral 4))
                                (Ref ((id ((name +arg2) (id 95)))))))))))))
                        (body
                         (Values
                          ((elements
                            ((Ref
                              ((id ((name fusion-target-map-result) (id 120)))))
                             (Let
                              ((args
                                (((binding
                                   ((name fusion-target-map-result) (id 110)))
                                  (value
                                   (ScalarPrimitive
                                    ((op Add)
                                     (args
                                      ((Literal (IntLiteral 5))
                                       (Ref
                                        ((id
                                          ((name fusion-target-map-result)
                                           (id 120)))))))))))))
                               (body
                                (Values
                                 ((elements
                                   ((Ref
                                     ((id
                                       ((name fusion-target-map-result) (id 110)))))
                                    (Values
                                     ((elements
                                       ((Ref
                                         ((id
                                           ((name fusion-target-map-result)
                                            (id 110)))))))))))))))))))))))))))
                  (body
                   (Values
                    ((elements
                      ((Ref ((id ((name fusion-target-map-result) (id 125)))))
                       (Let
                        ((args
                          (((binding ((name fusion-target-map-result) (id 115)))
                            (value
                             (ScalarPrimitive
                              ((op Add)
                               (args
                                ((Literal (IntLiteral 6))
                                 (TupleDeref
                                  ((index 0)
                                   (tuple
                                    (Ref
                                     ((id
                                       ((name fusion-target-map-result) (id 125))))))))))))))))
                         (body
                          (Values
                           ((elements
                             ((Ref
                               ((id ((name fusion-target-map-result) (id 115)))))
                              (Values
                               ((elements
                                 ((Ref
                                   ((id
                                     ((name fusion-target-map-result) (id 115))))))))))))))))))))))))
               (mapBodyMatcher
                (Unpack
                 ((Unpack
                   ((Binding ((name map-result) (id 94)))
                    (Unpack
                     ((Binding ((name map-result) (id 99)))
                      (Unpack ((Binding ((name reduce-arg) (id 97)))))))))
                  (Unpack
                   ((Binding ((name map-result) (id 105)))
                    (Unpack ((Binding ((name reduce-arg) (id 103))))))))))
               (mapResults
                (((name map-result) (id 94)) ((name map-result) (id 99))
                 ((name map-result) (id 105))))
               (consumer
                ((Reduce
                  (arg
                   ((firstBinding ((name fused-reduce-arg1) (id 129)))
                    (secondBinding ((name fused-reduce-arg2) (id 130)))
                    (production
                     (ProductionTuple
                      (elements
                       ((ProductionTupleAtom
                         ((productionId ((name reduce-arg) (id 97)))))
                        (ProductionTupleAtom
                         ((productionId ((name reduce-arg) (id 103)))))))))))
                  (zero ())
                  (body
                   (Values
                    ((elements
                      ((ScalarPrimitive
                        ((op Add)
                         (args
                          ((TupleDeref
                            ((index 0)
                             (tuple
                              (Ref ((id ((name fused-reduce-arg1) (id 129))))))))
                           (TupleDeref
                            ((index 0)
                             (tuple
                              (Ref ((id ((name fused-reduce-arg2) (id 130))))))))))))
                       (ScalarPrimitive
                        ((op Add)
                         (args
                          ((TupleDeref
                            ((index 1)
                             (tuple
                              (Ref ((id ((name fused-reduce-arg1) (id 129))))))))
                           (TupleDeref
                            ((index 1)
                             (tuple
                              (Ref ((id ((name fused-reduce-arg2) (id 130)))))))))))))))))
                  (d ((const 3) (refs ()))) (itemPad ()) (associative true)
                  (character Reduce))))))))))
         (body
          (ScalarPrimitive
           ((op Add)
            (args
             ((TupleDeref
               ((index 0)
                (tuple
                 (TupleDeref
                  ((index 1)
                   (tuple (Ref ((id ((name fused-block-result) (id 127)))))))))))
              (TupleDeref
               ((index 1)
                (tuple
                 (TupleDeref
                  ((index 1)
                   (tuple (Ref ((id ((name fused-block-result) (id 127))))))))))))))))))))) |}]
;;
