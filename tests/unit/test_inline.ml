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
    (AtomAsArray
     ((element (Literal (IntLiteral 5)))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| (+ 1 2) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding f.151)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding +arg1.152)
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding +arg2.153)
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 2)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (AtomAsArray
        ((element
          (AtomicPrimitive
           ((op Add)
            (args
             ((ArrayAsAtom
               ((array
                 (Ref
                  ((id +arg1.152)
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id +arg2.153)
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))))
            (type' (Literal IntLiteral)))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding f.152)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding +arg1.153)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))
        ((binding +arg2.155)
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 4)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 3) (refs ())))))
         (args
          (((binding +arg1.154)
            (value
             (Ref
              ((id +arg1.153)
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (AtomAsArray
           ((element
             (AtomicPrimitive
              ((op Add)
               (args
                ((ArrayAsAtom
                  ((array
                    (Ref
                     ((id +arg1.154)
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id +arg2.155)
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))))
               (type' (Literal IntLiteral)))))
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
       ((AtomAsArray
         ((element (Literal (CharacterLiteral h)))
          (type' ((element (Literal CharacterLiteral)) (shape ())))))
        (AtomAsArray
         ((element (Literal (CharacterLiteral e)))
          (type' ((element (Literal CharacterLiteral)) (shape ())))))
        (AtomAsArray
         ((element (Literal (CharacterLiteral l)))
          (type' ((element (Literal CharacterLiteral)) (shape ())))))
        (AtomAsArray
         ((element (Literal (CharacterLiteral l)))
          (type' ((element (Literal CharacterLiteral)) (shape ())))))
        (AtomAsArray
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
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding id.154)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.155)
            (value (Ref ((id id.154) (type' ((element (Tuple ())) (shape ())))))))
           ((binding x.157)
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref ((id x.157) (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app id (Forall (@t) (-> (@t) @t))) id)
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding id.158)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding id.155)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.156)
            (value (Ref ((id id.155) (type' ((element (Tuple ())) (shape ())))))))
           ((binding x.159)
            (value (Ref ((id id.158) (type' ((element (Tuple ())) (shape ())))))))))
         (body (Ref ((id x.159) (type' ((element (Tuple ())) (shape ()))))))
         (type' ((element (Tuple ())) (shape ()))))))
      (type' ((element (Tuple ())) (shape ()))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app ((t-app id (Forall (@t) (-> (@t) @t))) id) int) 5)
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding id.157)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding id.161)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.163)
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding f.158)
                  (value
                   (Ref ((id id.157) (type' ((element (Tuple ())) (shape ())))))))
                 ((binding x.162)
                  (value
                   (Ref ((id id.161) (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (Ref ((id x.162) (type' ((element (Tuple ())) (shape ()))))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding arg0.165)
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id arg0.165) (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      ((t-app (t-app (t-fn (@a) (t-fn (@b) (fn ([x int]) x))) int) int) 10)
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding f.154)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding x.156)
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 10)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (Ref ((id x.156) (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {|
      (length{int | 5 []} [1 2 3 4 5])
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding f.151)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ReifyIndex
        ((index (Dimension ((const 5) (refs ()))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| 
    (reduce{int | 4 [] []} + [1 2 3 4 5])
  |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding f.182)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding op.268)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding arr.278)
         (value
          (Frame
           ((dimensions (5))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 4)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 5)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 5) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.228)
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding f.269)
            (value (Ref ((id op.268) (type' ((element (Tuple ())) (shape ())))))))
           ((binding init.315)
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding f.306)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding arr.312)
                  (value
                   (Ref
                    ((id arr.278)
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 5) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.311)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding contiguous-subarray-array.313)
                     (value
                      (Ref
                       ((id arr.312)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ())))))))))))
                    ((binding contiguous-subarray-index.314)
                     (value
                      (Frame
                       ((dimensions (1))
                        (elements
                         ((AtomAsArray
                           ((element (Literal (IntLiteral 0)))
                            (type' ((element (Literal IntLiteral)) (shape ())))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (ContiguousSubArray
                     (arrayArg
                      (Ref
                       ((id contiguous-subarray-array.313)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id contiguous-subarray-index.314)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 5) (refs ())))))
                     (resultShape ()) (cellShape ()) (l ((const 1) (refs ())))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding arr.282)
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding f.272)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding arr.279)
                  (value
                   (Ref
                    ((id arr.278)
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 5) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.277)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding contiguous-subarray-array.280)
                     (value
                      (Ref
                       ((id arr.279)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ())))))))))))
                    ((binding contiguous-subarray-index.281)
                     (value
                      (Frame
                       ((dimensions (1))
                        (elements
                         ((AtomAsArray
                           ((element (Literal (IntLiteral 1)))
                            (type' ((element (Literal IntLiteral)) (shape ())))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (ContiguousSubArray
                     (arrayArg
                      (Ref
                       ((id contiguous-subarray-array.280)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id contiguous-subarray-index.281)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 5) (refs ())))))
                     (resultShape ((Add ((const 4) (refs ()))))) (cellShape ())
                     (l ((const 1) (refs ())))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 4) (refs ()))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 4) (refs ()))))))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 4) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding init.316)
               (value
                (Ref
                 ((id init.315)
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding up-ranked-f.256)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.254)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding reduce-f-arg.257)
                     (value
                      (Ref
                       ((id up-ranked-f.256)
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding reduce-zero-arg.319)
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.291)
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding v.317)
                           (value
                            (Ref
                             ((id init.316)
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding make.301)
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding f.302)
                                 (value
                                  (Ref
                                   ((id make.301)
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding v.318)
                                 (value
                                  (Ref
                                   ((id v.317)
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ()) (args ())
                                 (body
                                  (Ref
                                   ((id v.318)
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding reduce-array-arg.283)
                     (value
                      (Ref
                       ((id arr.282)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 4) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Reduce
                     (arg
                      ((firstBinding reduce-arg1.284)
                       (secondBinding reduce-arg2.287)
                       (value
                        (Ref
                         ((id reduce-array-arg.283)
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 4) (refs ()))))))))))))
                     (zero
                      (Ref
                       ((id reduce-zero-arg.319)
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.270)
                           (value
                            (Ref
                             ((id f.269)
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding arg0.285)
                           (value
                            (Ref
                             ((id reduce-arg1.284)
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding arg1.288)
                           (value
                            (Ref
                             ((id reduce-arg2.287)
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding arg0.286)
                              (value
                               (Ref
                                ((id arg0.285)
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding arg1.289)
                              (value
                               (Ref
                                ((id arg1.288)
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))))
                           (body
                            (AtomAsArray
                             ((element
                               (AtomicPrimitive
                                ((op Add)
                                 (args
                                  ((ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id arg0.286)
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))
                                   (ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id arg1.289)
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))))
                                 (type' (Literal IntLiteral)))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (d ((const 4) (refs ()))) (cellShape ()) (character Reduce)
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| ([+ -] 1 2) |};
  [%expect
    {|
    Error: Could not determine what function is being called in function call:
    ((func (Ref ((id f.151)))) (args (((id +arg1.148)) ((id +arg2.149))))
     (type' (Arr ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
      (define (id{@t| } [x @t]) x)
      ((t-app [id id id] int) 5)
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding id.155)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.156)
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref ((id id.155) (type' ((element (Tuple ())) (shape ())))))
                 (Ref ((id id.155) (type' ((element (Tuple ())) (shape ())))))
                 (Ref ((id id.155) (type' ((element (Tuple ())) (shape ())))))))
               (type'
                ((element (Tuple ())) (shape ((Add ((const 3) (refs ())))))))))))
           ((binding x.159)
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding f.157)
               (value
                (Ref
                 ((id f.156)
                  (type'
                   ((element (Tuple ())) (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (Ref
              ((id x.159) (type' ((element (Literal IntLiteral)) (shape ()))))))
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
    (ArrayPrimitive
     (Map (frameShape ()) (args ())
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.158)
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding +arg1.162)
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding f.160)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ReifyIndex
                 ((index (Dimension ((const 5) (refs ()))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding +arg2.166)
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding f.164)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ReifyIndex
                 ((index (Dimension ((const 2) (refs ()))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (AtomAsArray
           ((element
             (AtomicPrimitive
              ((op Add)
               (args
                ((ArrayAsAtom
                  ((array
                    (Ref
                     ((id +arg1.162)
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id +arg2.166)
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))))
               (type' (Literal IntLiteral)))))
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
    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding f.225)
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding d-1.24) (bound Dim))
                       ((binding @cell-shape.25) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding t.26) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding arr.226)
                                    (bound
                                     (Arr
                                      ((element (AtomRef t.26))
                                       (shape
                                        ((Add ((const 1) (refs ((d-1.24 1)))))
                                         (ShapeRef @cell-shape.25)))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding f.227)
                                       (value
                                        (TypeApplication
                                         ((tFunc
                                           (IndexApplication
                                            ((iFunc
                                              (Primitive
                                               ((name (Func ContiguousSubArray)))))
                                             (args
                                              ((Shape
                                                ((Add
                                                  ((const 1) (refs ((d-1.24 1)))))))
                                               (Shape ())
                                               (Shape
                                                ((ShapeRef @cell-shape.25)))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args ((Atom (AtomRef t.26))))))))
                                      ((binding contiguous-subarray-array.228)
                                       (value (Ref ((id arr.226)))))
                                      ((binding contiguous-subarray-index.229)
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id f.227))))
                                       (args
                                        (((id contiguous-subarray-array.228))
                                         ((id contiguous-subarray-index.229))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef t.26))
                                          (shape ((ShapeRef @cell-shape.25)))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef t.26))
                                       (shape ((ShapeRef @cell-shape.25)))))))))))))))))))))))))))
               (args ((Dimension ((const 1) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding @t.148) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding arr.230) (value (Ref ((id arr.161)))))))
      (body
       (TermApplication
        ((func (Ref ((id f.225)))) (args (((id arr.230))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding @t.148) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ())))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding @t.148) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding f.232)
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding d-1.28) (bound Dim))
                       ((binding @cell-shape.29) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding t.30) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding arr.233)
                                    (bound
                                     (Arr
                                      ((element (AtomRef t.30))
                                       (shape
                                        ((Add ((const 1) (refs ((d-1.28 1)))))
                                         (ShapeRef @cell-shape.29)))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding f.234)
                                       (value
                                        (TypeApplication
                                         ((tFunc
                                           (IndexApplication
                                            ((iFunc
                                              (Primitive
                                               ((name (Func ContiguousSubArray)))))
                                             (args
                                              ((Shape
                                                ((Add
                                                  ((const 1) (refs ((d-1.28 1)))))))
                                               (Shape
                                                ((Add
                                                  ((const 0) (refs ((d-1.28 1)))))))
                                               (Shape
                                                ((ShapeRef @cell-shape.29)))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args ((Atom (AtomRef t.30))))))))
                                      ((binding contiguous-subarray-array.235)
                                       (value (Ref ((id arr.233)))))
                                      ((binding contiguous-subarray-index.236)
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id f.234))))
                                       (args
                                        (((id contiguous-subarray-array.235))
                                         ((id contiguous-subarray-index.236))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef t.30))
                                          (shape
                                           ((Add ((const 0) (refs ((d-1.28 1)))))
                                            (ShapeRef @cell-shape.29)))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef t.30))
                                       (shape
                                        ((Add ((const 0) (refs ((d-1.28 1)))))
                                         (ShapeRef @cell-shape.29)))))))))))))))))))))))))))
               (args ((Dimension ((const 1) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding @t.148) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding arr.237) (value (Ref ((id arr.161)))))))
      (body
       (TermApplication
        ((func (Ref ((id f.232)))) (args (((id arr.237))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding @t.148) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ((Add ((const 1) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding @t.148) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ((Add ((const 1) (refs ())))))))))) |}];
  checkAndPrint {| (reduce{int | 2 [] []} [+ -] [1 2 3]) |};
  [%expect
    {|
    Error: Could not determine what function is being called in function call:
    ((func (Ref ((id f.264)))) (args (((id arg0.267)) ((id arg1.268))))
     (type' (Arr ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
    (define abc (t-fn (@t) 1))
    (reduce{(Forall (@t) int) | 2 [] []}
      (fn ([x (Forall (@t) int)] [y (Forall (@t) int)])
        x)
      [abc abc abc]){int | }
    |};
  [%expect
    {|
    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding f.220)
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding d-1.24) (bound Dim))
                       ((binding @cell-shape.25) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding t.26) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding arr.221)
                                    (bound
                                     (Arr
                                      ((element (AtomRef t.26))
                                       (shape
                                        ((Add ((const 1) (refs ((d-1.24 1)))))
                                         (ShapeRef @cell-shape.25)))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding f.222)
                                       (value
                                        (TypeApplication
                                         ((tFunc
                                           (IndexApplication
                                            ((iFunc
                                              (Primitive
                                               ((name (Func ContiguousSubArray)))))
                                             (args
                                              ((Shape
                                                ((Add
                                                  ((const 1) (refs ((d-1.24 1)))))))
                                               (Shape ())
                                               (Shape
                                                ((ShapeRef @cell-shape.25)))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args ((Atom (AtomRef t.26))))))))
                                      ((binding contiguous-subarray-array.223)
                                       (value (Ref ((id arr.221)))))
                                      ((binding contiguous-subarray-index.224)
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id f.222))))
                                       (args
                                        (((id contiguous-subarray-array.223))
                                         ((id contiguous-subarray-index.224))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef t.26))
                                          (shape ((ShapeRef @cell-shape.25)))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef t.26))
                                       (shape ((ShapeRef @cell-shape.25)))))))))))))))))))))))))))
               (args ((Dimension ((const 2) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding @t.150) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding arr.225) (value (Ref ((id arr.156)))))))
      (body
       (TermApplication
        ((func (Ref ((id f.220)))) (args (((id arr.225))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding @t.150) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ())))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding @t.150) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding f.227)
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding d-1.28) (bound Dim))
                       ((binding @cell-shape.29) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding t.30) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding arr.228)
                                    (bound
                                     (Arr
                                      ((element (AtomRef t.30))
                                       (shape
                                        ((Add ((const 1) (refs ((d-1.28 1)))))
                                         (ShapeRef @cell-shape.29)))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding f.229)
                                       (value
                                        (TypeApplication
                                         ((tFunc
                                           (IndexApplication
                                            ((iFunc
                                              (Primitive
                                               ((name (Func ContiguousSubArray)))))
                                             (args
                                              ((Shape
                                                ((Add
                                                  ((const 1) (refs ((d-1.28 1)))))))
                                               (Shape
                                                ((Add
                                                  ((const 0) (refs ((d-1.28 1)))))))
                                               (Shape
                                                ((ShapeRef @cell-shape.29)))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args ((Atom (AtomRef t.30))))))))
                                      ((binding contiguous-subarray-array.230)
                                       (value (Ref ((id arr.228)))))
                                      ((binding contiguous-subarray-index.231)
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id f.229))))
                                       (args
                                        (((id contiguous-subarray-array.230))
                                         ((id contiguous-subarray-index.231))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef t.30))
                                          (shape
                                           ((Add ((const 0) (refs ((d-1.28 1)))))
                                            (ShapeRef @cell-shape.29)))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef t.30))
                                       (shape
                                        ((Add ((const 0) (refs ((d-1.28 1)))))
                                         (ShapeRef @cell-shape.29)))))))))))))))))))))))))))
               (args ((Dimension ((const 2) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding @t.150) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding arr.232) (value (Ref ((id arr.156)))))))
      (body
       (TermApplication
        ((func (Ref ((id f.227)))) (args (((id arr.232))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding @t.150) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ((Add ((const 2) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding @t.150) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ((Add ((const 2) (refs ())))))))))) |}];
  checkAndPrint
    {|
    (define abc (t-fn (@t) 1))
    (reduce{(Forall (@t) int) | 2 [] []}
      (fn ([x (Forall (@t) int)] [y (Forall (@t) int)])
        x)
      [abc abc abc])
    |};
  [%expect
    {|
    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding f.220)
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding d-1.24) (bound Dim))
                       ((binding @cell-shape.25) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding t.26) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding arr.221)
                                    (bound
                                     (Arr
                                      ((element (AtomRef t.26))
                                       (shape
                                        ((Add ((const 1) (refs ((d-1.24 1)))))
                                         (ShapeRef @cell-shape.25)))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding f.222)
                                       (value
                                        (TypeApplication
                                         ((tFunc
                                           (IndexApplication
                                            ((iFunc
                                              (Primitive
                                               ((name (Func ContiguousSubArray)))))
                                             (args
                                              ((Shape
                                                ((Add
                                                  ((const 1) (refs ((d-1.24 1)))))))
                                               (Shape ())
                                               (Shape
                                                ((ShapeRef @cell-shape.25)))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args ((Atom (AtomRef t.26))))))))
                                      ((binding contiguous-subarray-array.223)
                                       (value (Ref ((id arr.221)))))
                                      ((binding contiguous-subarray-index.224)
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id f.222))))
                                       (args
                                        (((id contiguous-subarray-array.223))
                                         ((id contiguous-subarray-index.224))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef t.26))
                                          (shape ((ShapeRef @cell-shape.25)))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef t.26))
                                       (shape ((ShapeRef @cell-shape.25)))))))))))))))))))))))))))
               (args ((Dimension ((const 2) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding @t.150) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding arr.225) (value (Ref ((id arr.156)))))))
      (body
       (TermApplication
        ((func (Ref ((id f.220)))) (args (((id arr.225))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding @t.150) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ())))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding @t.150) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding f.227)
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding d-1.28) (bound Dim))
                       ((binding @cell-shape.29) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding t.30) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding arr.228)
                                    (bound
                                     (Arr
                                      ((element (AtomRef t.30))
                                       (shape
                                        ((Add ((const 1) (refs ((d-1.28 1)))))
                                         (ShapeRef @cell-shape.29)))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding f.229)
                                       (value
                                        (TypeApplication
                                         ((tFunc
                                           (IndexApplication
                                            ((iFunc
                                              (Primitive
                                               ((name (Func ContiguousSubArray)))))
                                             (args
                                              ((Shape
                                                ((Add
                                                  ((const 1) (refs ((d-1.28 1)))))))
                                               (Shape
                                                ((Add
                                                  ((const 0) (refs ((d-1.28 1)))))))
                                               (Shape
                                                ((ShapeRef @cell-shape.29)))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args ((Atom (AtomRef t.30))))))))
                                      ((binding contiguous-subarray-array.230)
                                       (value (Ref ((id arr.228)))))
                                      ((binding contiguous-subarray-index.231)
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id f.229))))
                                       (args
                                        (((id contiguous-subarray-array.230))
                                         ((id contiguous-subarray-index.231))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef t.30))
                                          (shape
                                           ((Add ((const 0) (refs ((d-1.28 1)))))
                                            (ShapeRef @cell-shape.29)))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef t.30))
                                       (shape
                                        ((Add ((const 0) (refs ((d-1.28 1)))))
                                         (ShapeRef @cell-shape.29)))))))))))))))))))))))))))
               (args ((Dimension ((const 2) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding @t.150) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding arr.232) (value (Ref ((id arr.156)))))))
      (body
       (TermApplication
        ((func (Ref ((id f.227)))) (args (((id arr.232))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding @t.150) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ((Add ((const 2) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding @t.150) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ((Add ((const 2) (refs ())))))))))) |}];
  checkAndPrint
    {|
    (define abc (t-fn (@t) 1))
    (reduce{(Forall (@t) int) | 2 [] []}
      (fn ([x (Forall (@t) int)] [y (Forall (@t) int)])
        abc)
      [abc abc abc])
    |};
  [%expect
    {|
    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding f.220)
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding d-1.24) (bound Dim))
                       ((binding @cell-shape.25) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding t.26) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding arr.221)
                                    (bound
                                     (Arr
                                      ((element (AtomRef t.26))
                                       (shape
                                        ((Add ((const 1) (refs ((d-1.24 1)))))
                                         (ShapeRef @cell-shape.25)))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding f.222)
                                       (value
                                        (TypeApplication
                                         ((tFunc
                                           (IndexApplication
                                            ((iFunc
                                              (Primitive
                                               ((name (Func ContiguousSubArray)))))
                                             (args
                                              ((Shape
                                                ((Add
                                                  ((const 1) (refs ((d-1.24 1)))))))
                                               (Shape ())
                                               (Shape
                                                ((ShapeRef @cell-shape.25)))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args ((Atom (AtomRef t.26))))))))
                                      ((binding contiguous-subarray-array.223)
                                       (value (Ref ((id arr.221)))))
                                      ((binding contiguous-subarray-index.224)
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id f.222))))
                                       (args
                                        (((id contiguous-subarray-array.223))
                                         ((id contiguous-subarray-index.224))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef t.26))
                                          (shape ((ShapeRef @cell-shape.25)))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef t.26))
                                       (shape ((ShapeRef @cell-shape.25)))))))))))))))))))))))))))
               (args ((Dimension ((const 2) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding @t.150) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding arr.225) (value (Ref ((id arr.156)))))))
      (body
       (TermApplication
        ((func (Ref ((id f.220)))) (args (((id arr.225))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding @t.150) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ())))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding @t.150) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding f.227)
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding d-1.28) (bound Dim))
                       ((binding @cell-shape.29) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding t.30) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding arr.228)
                                    (bound
                                     (Arr
                                      ((element (AtomRef t.30))
                                       (shape
                                        ((Add ((const 1) (refs ((d-1.28 1)))))
                                         (ShapeRef @cell-shape.29)))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding f.229)
                                       (value
                                        (TypeApplication
                                         ((tFunc
                                           (IndexApplication
                                            ((iFunc
                                              (Primitive
                                               ((name (Func ContiguousSubArray)))))
                                             (args
                                              ((Shape
                                                ((Add
                                                  ((const 1) (refs ((d-1.28 1)))))))
                                               (Shape
                                                ((Add
                                                  ((const 0) (refs ((d-1.28 1)))))))
                                               (Shape
                                                ((ShapeRef @cell-shape.29)))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args ((Atom (AtomRef t.30))))))))
                                      ((binding contiguous-subarray-array.230)
                                       (value (Ref ((id arr.228)))))
                                      ((binding contiguous-subarray-index.231)
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id f.229))))
                                       (args
                                        (((id contiguous-subarray-array.230))
                                         ((id contiguous-subarray-index.231))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef t.30))
                                          (shape
                                           ((Add ((const 0) (refs ((d-1.28 1)))))
                                            (ShapeRef @cell-shape.29)))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef t.30))
                                       (shape
                                        ((Add ((const 0) (refs ((d-1.28 1)))))
                                         (ShapeRef @cell-shape.29)))))))))))))))))))))))))))
               (args ((Dimension ((const 2) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding @t.150) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding arr.232) (value (Ref ((id arr.156)))))))
      (body
       (TermApplication
        ((func (Ref ((id f.227)))) (args (((id arr.232))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding @t.150) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ((Add ((const 2) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding @t.150) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ((Add ((const 2) (refs ())))))))))) |}];
  checkAndPrint {| (reduce{int | 2 [] []} + [1 2 3]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding f.182)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding op.268)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding arr.278)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.228)
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding f.269)
            (value (Ref ((id op.268) (type' ((element (Tuple ())) (shape ())))))))
           ((binding init.315)
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding f.306)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding arr.312)
                  (value
                   (Ref
                    ((id arr.278)
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.311)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding contiguous-subarray-array.313)
                     (value
                      (Ref
                       ((id arr.312)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding contiguous-subarray-index.314)
                     (value
                      (Frame
                       ((dimensions (1))
                        (elements
                         ((AtomAsArray
                           ((element (Literal (IntLiteral 0)))
                            (type' ((element (Literal IntLiteral)) (shape ())))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (ContiguousSubArray
                     (arrayArg
                      (Ref
                       ((id contiguous-subarray-array.313)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id contiguous-subarray-index.314)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 3) (refs ())))))
                     (resultShape ()) (cellShape ()) (l ((const 1) (refs ())))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding arr.282)
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding f.272)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding arr.279)
                  (value
                   (Ref
                    ((id arr.278)
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.277)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding contiguous-subarray-array.280)
                     (value
                      (Ref
                       ((id arr.279)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding contiguous-subarray-index.281)
                     (value
                      (Frame
                       ((dimensions (1))
                        (elements
                         ((AtomAsArray
                           ((element (Literal (IntLiteral 1)))
                            (type' ((element (Literal IntLiteral)) (shape ())))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (ContiguousSubArray
                     (arrayArg
                      (Ref
                       ((id contiguous-subarray-array.280)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id contiguous-subarray-index.281)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 3) (refs ())))))
                     (resultShape ((Add ((const 2) (refs ()))))) (cellShape ())
                     (l ((const 1) (refs ())))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 2) (refs ()))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 2) (refs ()))))))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding init.316)
               (value
                (Ref
                 ((id init.315)
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding up-ranked-f.256)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.254)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding reduce-f-arg.257)
                     (value
                      (Ref
                       ((id up-ranked-f.256)
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding reduce-zero-arg.319)
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.291)
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding v.317)
                           (value
                            (Ref
                             ((id init.316)
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding make.301)
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding f.302)
                                 (value
                                  (Ref
                                   ((id make.301)
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding v.318)
                                 (value
                                  (Ref
                                   ((id v.317)
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ()) (args ())
                                 (body
                                  (Ref
                                   ((id v.318)
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding reduce-array-arg.283)
                     (value
                      (Ref
                       ((id arr.282)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 2) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Reduce
                     (arg
                      ((firstBinding reduce-arg1.284)
                       (secondBinding reduce-arg2.287)
                       (value
                        (Ref
                         ((id reduce-array-arg.283)
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 2) (refs ()))))))))))))
                     (zero
                      (Ref
                       ((id reduce-zero-arg.319)
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.270)
                           (value
                            (Ref
                             ((id f.269)
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding arg0.285)
                           (value
                            (Ref
                             ((id reduce-arg1.284)
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding arg1.288)
                           (value
                            (Ref
                             ((id reduce-arg2.287)
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding arg0.286)
                              (value
                               (Ref
                                ((id arg0.285)
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding arg1.289)
                              (value
                               (Ref
                                ((id arg1.288)
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))))
                           (body
                            (AtomAsArray
                             ((element
                               (AtomicPrimitive
                                ((op Add)
                                 (args
                                  ((ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id arg0.286)
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))
                                   (ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id arg1.289)
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))))
                                 (type' (Literal IntLiteral)))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (d ((const 2) (refs ()))) (cellShape ()) (character Reduce)
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| (fold{int int | 5 []} + 0 [1 2 3 4 5]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding f.152)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding fold-f-arg.153)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding fold-zero-arg.156)
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 0)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding fold-array-arg.158)
         (value
          (Frame
           ((dimensions (5))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 4)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 5)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 5) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Fold
         (zeroArg
          ((binding fold-zero-arg.157)
           (value
            (Ref
             ((id fold-zero-arg.156)
              (type' ((element (Literal IntLiteral)) (shape ()))))))))
         (arrayArgs
          (((binding fold-array-arg.159)
            (value
             (Ref
              ((id fold-array-arg.158)
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 5) (refs ())))))))))))))
         (body
          (AtomAsArray
           ((element
             (AtomicPrimitive
              ((op Add)
               (args
                ((ArrayAsAtom
                  ((array
                    (Ref
                     ((id fold-zero-arg.157)
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id fold-array-arg.159)
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))))
               (type' (Literal IntLiteral)))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (reverse false) (d ((const 5) (refs ()))) (cellShape ())
         (character Fold) (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
    (define abc (t-fn (@t) 1))
    (fold{int (Forall (@t) int) | 3 []}
      (fn ([x (Forall (@t) int)] [y int])
        x)
        abc [1 2 3]){int | }
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding abc.166)
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.158)
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding fold-f-arg.160)
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding fold-zero-arg.167)
            (value
             (Ref
              ((id abc.166) (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Fold
            (zeroArg
             ((binding fold-zero-arg.168)
              (value
               (Ref
                ((id fold-zero-arg.167)
                 (type' ((element (Literal IntLiteral)) (shape ()))))))))
            (arrayArgs ())
            (body
             (Ref
              ((id fold-zero-arg.168)
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (reverse false) (d ((const 3) (refs ()))) (cellShape ())
            (character Fold) (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
    (define abc (t-fn (@t) 1))
    (fold{(Forall (@t) int) int | 3 []}
      (fn ([x int] [y (Forall (@t) int)])
        (define a y{int | })
        (define b y{bool | })
        (+ a b))
      10 [abc abc abc])
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding abc.177)
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding abc.182)
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.163)
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding fold-f-arg.165)
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding fold-zero-arg.187)
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding fold-array-arg.178)
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id abc.177)
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id abc.177)
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id abc.177)
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))
           ((binding fold-array-arg.183)
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id abc.182)
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id abc.182)
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id abc.182)
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Fold
            (zeroArg
             ((binding fold-zero-arg.188)
              (value
               (Ref
                ((id fold-zero-arg.187)
                 (type' ((element (Literal IntLiteral)) (shape ()))))))))
            (arrayArgs
             (((binding fold-array-arg.179)
               (value
                (Ref
                 ((id fold-array-arg.178)
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))
              ((binding fold-array-arg.184)
               (value
                (Ref
                 ((id fold-array-arg.183)
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding a.180)
                  (value
                   (Ref
                    ((id fold-array-arg.179)
                     (type' ((element (Literal IntLiteral)) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding b.185)
                     (value
                      (Ref
                       ((id fold-array-arg.184)
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding f.176)
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding +arg1.181)
                        (value
                         (Ref
                          ((id a.180)
                           (type' ((element (Literal IntLiteral)) (shape ())))))))
                       ((binding +arg2.186)
                        (value
                         (Ref
                          ((id b.185)
                           (type' ((element (Literal IntLiteral)) (shape ())))))))))
                     (body
                      (AtomAsArray
                       ((element
                         (AtomicPrimitive
                          ((op Add)
                           (args
                            ((ArrayAsAtom
                              ((array
                                (Ref
                                 ((id +arg1.181)
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id +arg2.186)
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))))
                           (type' (Literal IntLiteral)))))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (reverse false) (d ((const 3) (refs ()))) (cellShape ())
            (character Fold) (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
    (define abc (t-fn (@t) 1))
    (fold{int (Forall (@t) int) | 3 []}
      (fn ([x (Forall (@t) int)] [y int])
        (define a x{int | })
        x)
      abc [1 2 3]){int | }
    |};
  [%expect
    {|
    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding a.154)
         (value
          (TypeApplication
           ((tFunc (Ref ((id x.152))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))
      (body (Ref ((id x.152)))) (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding @t.151) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ())))))) |}];
  checkAndPrint {| (length{(Forall (@x) int) | 2 []} [(t-fn (@x) 5) (t-fn (@x) 5)]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding f.154)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ReifyIndex
        ((index (Dimension ((const 2) (refs ()))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| (iscan{int | 2 [] []} + [1 2 3]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding f.182)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding op.268)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding arr.278)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.228)
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding f.269)
            (value (Ref ((id op.268) (type' ((element (Tuple ())) (shape ())))))))
           ((binding init.315)
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding f.306)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding arr.312)
                  (value
                   (Ref
                    ((id arr.278)
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.311)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding contiguous-subarray-array.313)
                     (value
                      (Ref
                       ((id arr.312)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding contiguous-subarray-index.314)
                     (value
                      (Frame
                       ((dimensions (1))
                        (elements
                         ((AtomAsArray
                           ((element (Literal (IntLiteral 0)))
                            (type' ((element (Literal IntLiteral)) (shape ())))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (ContiguousSubArray
                     (arrayArg
                      (Ref
                       ((id contiguous-subarray-array.313)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id contiguous-subarray-index.314)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 3) (refs ())))))
                     (resultShape ()) (cellShape ()) (l ((const 1) (refs ())))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding arr.282)
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding f.272)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding arr.279)
                  (value
                   (Ref
                    ((id arr.278)
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.277)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding contiguous-subarray-array.280)
                     (value
                      (Ref
                       ((id arr.279)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding contiguous-subarray-index.281)
                     (value
                      (Frame
                       ((dimensions (1))
                        (elements
                         ((AtomAsArray
                           ((element (Literal (IntLiteral 1)))
                            (type' ((element (Literal IntLiteral)) (shape ())))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (ContiguousSubArray
                     (arrayArg
                      (Ref
                       ((id contiguous-subarray-array.280)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id contiguous-subarray-index.281)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 3) (refs ())))))
                     (resultShape ((Add ((const 2) (refs ()))))) (cellShape ())
                     (l ((const 1) (refs ())))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 2) (refs ()))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 2) (refs ()))))))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding init.316)
               (value
                (Ref
                 ((id init.315)
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding up-ranked-f.256)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.254)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding scan-f-arg.257)
                     (value
                      (Ref
                       ((id up-ranked-f.256)
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding scan-zero-arg.319)
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.291)
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding v.317)
                           (value
                            (Ref
                             ((id init.316)
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding make.301)
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding f.302)
                                 (value
                                  (Ref
                                   ((id make.301)
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding v.318)
                                 (value
                                  (Ref
                                   ((id v.317)
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ()) (args ())
                                 (body
                                  (Ref
                                   ((id v.318)
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding scan-array-arg.283)
                     (value
                      (Ref
                       ((id arr.282)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 2) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Reduce
                     (arg
                      ((firstBinding reduce-arg1.284)
                       (secondBinding reduce-arg2.287)
                       (value
                        (Ref
                         ((id scan-array-arg.283)
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 2) (refs ()))))))))))))
                     (zero
                      (Ref
                       ((id scan-zero-arg.319)
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.270)
                           (value
                            (Ref
                             ((id f.269)
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding arg0.285)
                           (value
                            (Ref
                             ((id reduce-arg1.284)
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding arg1.288)
                           (value
                            (Ref
                             ((id reduce-arg2.287)
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding arg0.286)
                              (value
                               (Ref
                                ((id arg0.285)
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding arg1.289)
                              (value
                               (Ref
                                ((id arg1.288)
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))))
                           (body
                            (AtomAsArray
                             ((element
                               (AtomicPrimitive
                                ((op Add)
                                 (args
                                  ((ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id arg0.286)
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))
                                   (ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id arg1.289)
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))))
                                 (type' (Literal IntLiteral)))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (d ((const 2) (refs ()))) (cellShape ()) (character Scan)
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ()))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ()))))))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ()))))))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint {| (filter{int | 3 []} [1 2 3] [#t #t #f]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding f.216)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding arr.307)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))
        ((binding flags.310)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((AtomAsArray
               ((element (Literal (BooleanLiteral true)))
                (type' ((element (Literal BooleanLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (BooleanLiteral true)))
                (type' ((element (Literal BooleanLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (BooleanLiteral false)))
                (type' ((element (Literal BooleanLiteral)) (shape ())))))))
            (type'
             ((element (Literal BooleanLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding locs-raw.443)
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding f.313)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding -arg1.440)
                  (value
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding f.315)
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding op.402)
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding init.432)
                        (value
                         (AtomAsArray
                          ((element (Literal (IntLiteral 0)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))
                       ((binding arr.408)
                        (value
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding f.405)
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding boolToIntArg.406)
                              (value
                               (Ref
                                ((id flags.310)
                                 (type'
                                  ((element (Literal BooleanLiteral))
                                   (shape ((Add ((const 3) (refs ())))))))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ((Add ((const 3) (refs ())))))
                              (args
                               (((binding boolToIntArg.407)
                                 (value
                                  (Ref
                                   ((id boolToIntArg.406)
                                    (type'
                                     ((element (Literal BooleanLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))))
                              (body
                               (AtomAsArray
                                ((element
                                  (AtomicPrimitive
                                   ((op BoolToInt)
                                    (args
                                     ((ArrayAsAtom
                                       ((array
                                         (Ref
                                          ((id boolToIntArg.407)
                                           (type'
                                            ((element (Literal BooleanLiteral))
                                             (shape ()))))))
                                        (type' (Literal BooleanLiteral))))))
                                    (type' (Literal IntLiteral)))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 3) (refs ()))))))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.355)
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding arr.437)
                           (value
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding f.362)
                                 (value
                                  (AtomAsArray
                                   ((element (Values ((elements ()) (type' ()))))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding f.403)
                                 (value
                                  (Ref
                                   ((id op.402)
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding init.433)
                                 (value
                                  (Ref
                                   ((id init.432)
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))
                                ((binding arr.409)
                                 (value
                                  (Ref
                                   ((id arr.408)
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding up-ranked-f.390)
                                    (value
                                     (AtomAsArray
                                      ((element
                                        (Values ((elements ()) (type' ()))))
                                       (type' ((element (Tuple ())) (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding f.388)
                                       (value
                                        (AtomAsArray
                                         ((element
                                           (Values ((elements ()) (type' ()))))
                                          (type'
                                           ((element (Tuple ())) (shape ())))))))
                                      ((binding scan-f-arg.391)
                                       (value
                                        (Ref
                                         ((id up-ranked-f.390)
                                          (type'
                                           ((element (Tuple ())) (shape ())))))))
                                      ((binding scan-zero-arg.436)
                                       (value
                                        (ArrayPrimitive
                                         (Map (frameShape ())
                                          (args
                                           (((binding f.418)
                                             (value
                                              (AtomAsArray
                                               ((element
                                                 (Values
                                                  ((elements ()) (type' ()))))
                                                (type'
                                                 ((element (Tuple ()))
                                                  (shape ())))))))
                                            ((binding v.434)
                                             (value
                                              (Ref
                                               ((id init.433)
                                                (type'
                                                 ((element (Literal IntLiteral))
                                                  (shape ())))))))))
                                          (body
                                           (ArrayPrimitive
                                            (Map (frameShape ())
                                             (args
                                              (((binding make.428)
                                                (value
                                                 (AtomAsArray
                                                  ((element
                                                    (Values
                                                     ((elements ()) (type' ()))))
                                                   (type'
                                                    ((element (Tuple ()))
                                                     (shape ())))))))))
                                             (body
                                              (ArrayPrimitive
                                               (Map (frameShape ())
                                                (args
                                                 (((binding f.429)
                                                   (value
                                                    (Ref
                                                     ((id make.428)
                                                      (type'
                                                       ((element (Tuple ()))
                                                        (shape ())))))))
                                                  ((binding v.435)
                                                   (value
                                                    (Ref
                                                     ((id v.434)
                                                      (type'
                                                       ((element
                                                         (Literal IntLiteral))
                                                        (shape ())))))))))
                                                (body
                                                 (ArrayPrimitive
                                                  (Map (frameShape ()) (args ())
                                                   (body
                                                    (Ref
                                                     ((id v.435)
                                                      (type'
                                                       ((element
                                                         (Literal IntLiteral))
                                                        (shape ()))))))
                                                   (type'
                                                    ((element
                                                      (Literal IntLiteral))
                                                     (shape ()))))))
                                                (type'
                                                 ((element (Literal IntLiteral))
                                                  (shape ()))))))
                                             (type'
                                              ((element (Literal IntLiteral))
                                               (shape ()))))))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))
                                      ((binding scan-array-arg.410)
                                       (value
                                        (Ref
                                         ((id arr.409)
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ((Add ((const 3) (refs ())))))))))))))
                                    (body
                                     (ArrayPrimitive
                                      (Reduce
                                       (arg
                                        ((firstBinding reduce-arg1.411)
                                         (secondBinding reduce-arg2.414)
                                         (value
                                          (Ref
                                           ((id scan-array-arg.410)
                                            (type'
                                             ((element (Literal IntLiteral))
                                              (shape
                                               ((Add ((const 3) (refs ()))))))))))))
                                       (zero
                                        (Ref
                                         ((id scan-zero-arg.436)
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ()))))))
                                       (body
                                        (ArrayPrimitive
                                         (Map (frameShape ())
                                          (args
                                           (((binding f.404)
                                             (value
                                              (Ref
                                               ((id f.403)
                                                (type'
                                                 ((element (Tuple ()))
                                                  (shape ())))))))
                                            ((binding arg0.412)
                                             (value
                                              (Ref
                                               ((id reduce-arg1.411)
                                                (type'
                                                 ((element (Literal IntLiteral))
                                                  (shape ())))))))
                                            ((binding arg1.415)
                                             (value
                                              (Ref
                                               ((id reduce-arg2.414)
                                                (type'
                                                 ((element (Literal IntLiteral))
                                                  (shape ())))))))))
                                          (body
                                           (ArrayPrimitive
                                            (Map (frameShape ())
                                             (args
                                              (((binding arg0.413)
                                                (value
                                                 (Ref
                                                  ((id arg0.412)
                                                   (type'
                                                    ((element
                                                      (Literal IntLiteral))
                                                     (shape ())))))))
                                               ((binding arg1.416)
                                                (value
                                                 (Ref
                                                  ((id arg1.415)
                                                   (type'
                                                    ((element
                                                      (Literal IntLiteral))
                                                     (shape ())))))))))
                                             (body
                                              (AtomAsArray
                                               ((element
                                                 (AtomicPrimitive
                                                  ((op Add)
                                                   (args
                                                    ((ArrayAsAtom
                                                      ((array
                                                        (Ref
                                                         ((id arg0.413)
                                                          (type'
                                                           ((element
                                                             (Literal IntLiteral))
                                                            (shape ()))))))
                                                       (type'
                                                        (Literal IntLiteral))))
                                                     (ArrayAsAtom
                                                      ((array
                                                        (Ref
                                                         ((id arg1.416)
                                                          (type'
                                                           ((element
                                                             (Literal IntLiteral))
                                                            (shape ()))))))
                                                       (type'
                                                        (Literal IntLiteral))))))
                                                   (type' (Literal IntLiteral)))))
                                                (type'
                                                 ((element (Literal IntLiteral))
                                                  (shape ()))))))
                                             (type'
                                              ((element (Literal IntLiteral))
                                               (shape ()))))))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ()))))))
                                       (d ((const 3) (refs ()))) (cellShape ())
                                       (character Scan)
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ((Add ((const 4) (refs ()))))))))))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 4) (refs ()))))))))))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 4) (refs ()))))))))))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 4) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding f.360)
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding contiguous-subarray-array.438)
                              (value
                               (Ref
                                ((id arr.437)
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 4) (refs ())))))))))))
                             ((binding contiguous-subarray-index.439)
                              (value
                               (Frame
                                ((dimensions (1))
                                 (elements
                                  ((AtomAsArray
                                    ((element (Literal (IntLiteral 1)))
                                     (type'
                                      ((element (Literal IntLiteral)) (shape ())))))))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 1) (refs ())))))))))))))
                           (body
                            (ArrayPrimitive
                             (ContiguousSubArray
                              (arrayArg
                               (Ref
                                ((id contiguous-subarray-array.438)
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 4) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id contiguous-subarray-index.439)
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 1) (refs ()))))))))))
                              (originalShape ((Add ((const 4) (refs ())))))
                              (resultShape ((Add ((const 3) (refs ())))))
                              (cellShape ()) (l ((const 1) (refs ())))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 3) (refs ()))))))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 3) (refs ()))))))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))
                 ((binding -arg2.442)
                  (value
                   (AtomAsArray
                    ((element (Literal (IntLiteral 1)))
                     (type' ((element (Literal IntLiteral)) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ((Add ((const 3) (refs ())))))
                  (args
                   (((binding -arg1.441)
                     (value
                      (Ref
                       ((id -arg1.440)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))))
                  (body
                   (AtomAsArray
                    ((element
                      (AtomicPrimitive
                       ((op Sub)
                        (args
                         ((ArrayAsAtom
                           ((array
                             (Ref
                              ((id -arg1.441)
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id -arg2.442)
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))))
                        (type' (Literal IntLiteral)))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ()))))))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding locs.465)
               (value
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.309)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding if-cond.311)
                     (value
                      (Ref
                       ((id flags.310)
                        (type'
                         ((element (Literal BooleanLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding then-branch.444)
                     (value
                      (Ref
                       ((id locs-raw.443)
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding else-branch.463)
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.447)
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding v.461)
                           (value
                            (AtomAsArray
                             ((element (Literal (IntLiteral -1)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding make.457)
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding f.458)
                                 (value
                                  (Ref
                                   ((id make.457)
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding v.462)
                                 (value
                                  (Ref
                                   ((id v.461)
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ((Add ((const 3) (refs ())))))
                                 (args ())
                                 (body
                                  (Ref
                                   ((id v.462)
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 3) (refs ()))))))))))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 3) (refs ()))))))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 3) (refs ()))))))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ((Add ((const 3) (refs ())))))
                     (args
                      (((binding if-cond.312)
                        (value
                         (Ref
                          ((id if-cond.311)
                           (type'
                            ((element (Literal BooleanLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))
                       ((binding then-branch.445)
                        (value
                         (Ref
                          ((id then-branch.444)
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))
                       ((binding else-branch.464)
                        (value
                         (Ref
                          ((id else-branch.463)
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))))
                     (body
                      (AtomAsArray
                       ((element
                         (AtomicPrimitive
                          ((op If)
                           (args
                            ((ArrayAsAtom
                              ((array
                                (Ref
                                 ((id if-cond.312)
                                  (type'
                                   ((element (Literal BooleanLiteral))
                                    (shape ()))))))
                               (type' (Literal BooleanLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id then-branch.445)
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id else-branch.464)
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))))
                           (type' (Literal IntLiteral)))))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ()))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding result-size.485)
                  (value
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding f.467)
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding +arg1.468)
                        (value
                         (AtomAsArray
                          ((element (Literal (IntLiteral 1)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))
                       ((binding +arg2.484)
                        (value
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding f.470)
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding arr.477)
                              (value
                               (Ref
                                ((id locs-raw.443)
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 3) (refs ())))))))))))
                             ((binding index.482)
                              (value
                               (Frame
                                ((dimensions (1))
                                 (elements
                                  ((ArrayPrimitive
                                    (Map (frameShape ())
                                     (args
                                      (((binding f.479)
                                        (value
                                         (AtomAsArray
                                          ((element
                                            (Values ((elements ()) (type' ()))))
                                           (type'
                                            ((element (Tuple ())) (shape ())))))))
                                       ((binding -arg1.480)
                                        (value
                                         (ReifyIndex
                                          ((index
                                            (Dimension ((const 3) (refs ()))))
                                           (type'
                                            ((element (Literal IntLiteral))
                                             (shape ())))))))
                                       ((binding -arg2.481)
                                        (value
                                         (AtomAsArray
                                          ((element (Literal (IntLiteral 1)))
                                           (type'
                                            ((element (Literal IntLiteral))
                                             (shape ())))))))))
                                     (body
                                      (AtomAsArray
                                       ((element
                                         (AtomicPrimitive
                                          ((op Sub)
                                           (args
                                            ((ArrayAsAtom
                                              ((array
                                                (Ref
                                                 ((id -arg1.480)
                                                  (type'
                                                   ((element
                                                     (Literal IntLiteral))
                                                    (shape ()))))))
                                               (type' (Literal IntLiteral))))
                                             (ArrayAsAtom
                                              ((array
                                                (Ref
                                                 ((id -arg2.481)
                                                  (type'
                                                   ((element
                                                     (Literal IntLiteral))
                                                    (shape ()))))))
                                               (type' (Literal IntLiteral))))))
                                           (type' (Literal IntLiteral)))))
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type'
                                      ((element (Literal IntLiteral)) (shape ())))))))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 1) (refs ())))))))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding f.476)
                                 (value
                                  (AtomAsArray
                                   ((element (Values ((elements ()) (type' ()))))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding contiguous-subarray-array.478)
                                 (value
                                  (Ref
                                   ((id arr.477)
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))
                                ((binding contiguous-subarray-index.483)
                                 (value
                                  (Ref
                                   ((id index.482)
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 1) (refs ())))))))))))))
                              (body
                               (ArrayPrimitive
                                (ContiguousSubArray
                                 (arrayArg
                                  (Ref
                                   ((id contiguous-subarray-array.478)
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ()))))))))))
                                 (indexArg
                                  (Ref
                                   ((id contiguous-subarray-index.483)
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 1) (refs ()))))))))))
                                 (originalShape ((Add ((const 3) (refs ())))))
                                 (resultShape ()) (cellShape ())
                                 (l ((const 1) (refs ())))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))))
                     (body
                      (AtomAsArray
                       ((element
                         (AtomicPrimitive
                          ((op Add)
                           (args
                            ((ArrayAsAtom
                              ((array
                                (Ref
                                 ((id +arg1.468)
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id +arg2.484)
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))))
                           (type' (Literal IntLiteral)))))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (type' ((element (Literal IntLiteral)) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding index-value.486)
                     (value
                      (Ref
                       ((id result-size.485)
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (IndexLet
                    ((indexArgs
                      (((indexBinding d.129)
                        (indexValue
                         (Runtime
                          (Ref
                           ((id index-value.486)
                            (type' ((element (Literal IntLiteral)) (shape ())))))))
                        (sort Dim))))
                     (body
                      (AtomAsArray
                       ((element
                         (Box
                          ((indices ((Dimension ((const 0) (refs ((d.129 1)))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding f.306)
                                 (value
                                  (AtomAsArray
                                   ((element (Values ((elements ()) (type' ()))))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding scatter-values.308)
                                 (value
                                  (Ref
                                   ((id arr.307)
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))
                                ((binding scatter-indices.466)
                                 (value
                                  (Ref
                                   ((id locs.465)
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))))
                              (body
                               (ArrayPrimitive
                                (Scatter
                                 (valuesArg
                                  (Ref
                                   ((id scatter-values.308)
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ()))))))))))
                                 (indicesArg
                                  (Ref
                                   ((id scatter-indices.466)
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ()))))))))))
                                 (dIn ((const 3) (refs ())))
                                 (dOut ((const 0) (refs ((d.129 1)))))
                                 (cellShape ())
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 0) (refs ((d.129 1))))))))))))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 0) (refs ((d.129 1))))))))))))
                           (bodyType
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 0) (refs ((d.129 1)))))))))
                           (type'
                            ((parameters (((binding d.129) (bound Dim))))
                             (body
                              ((element (Literal IntLiteral))
                               (shape ((Add ((const 0) (refs ((d.129 1))))))))))))))
                        (type'
                         ((element
                           (Sigma
                            ((parameters (((binding d.129) (bound Dim))))
                             (body
                              ((element (Literal IntLiteral))
                               (shape ((Add ((const 0) (refs ((d.129 1))))))))))))
                          (shape ()))))))
                     (type'
                      ((element
                        (Sigma
                         ((parameters (((binding d.129) (bound Dim))))
                          (body
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 0) (refs ((d.129 1))))))))))))
                       (shape ()))))))
                  (type'
                   ((element
                     (Sigma
                      ((parameters (((binding d.129) (bound Dim))))
                       (body
                        ((element (Literal IntLiteral))
                         (shape ((Add ((const 0) (refs ((d.129 1))))))))))))
                    (shape ()))))))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding d.129) (bound Dim))))
                    (body
                     ((element (Literal IntLiteral))
                      (shape ((Add ((const 0) (refs ((d.129 1))))))))))))
                 (shape ()))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding d.129) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((d.129 1))))))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding d.129) (bound Dim))))
              (body
               ((element (Literal IntLiteral))
                (shape ((Add ((const 0) (refs ((d.129 1))))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding d.129) (bound Dim))))
           (body
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ((d.129 1))))))))))))
        (shape ()))))) |}];
  checkAndPrint {| (append{int | 3 2 []} [1 2 3] [4 5]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding f.151)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding append-arg1.152)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))
        ((binding append-arg2.153)
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 4)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 5)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Append
         (arg1
          (Ref
           ((id append-arg1.152)
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (arg2
          (Ref
           ((id append-arg2.153)
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ()))))))))))
         (d1 ((const 3) (refs ()))) (d2 ((const 2) (refs ()))) (cellShape ())
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 5) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 5) (refs ()))))))))) |}];
  checkAndPrint
    {|
      (define foo
        (let [x 5]
          (fn () x)))
      (foo) |};
  [%expect {| Error: Lambda captures variable x.149, which escapes its definition |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ((Add ((const 3) (refs ())))))
      (args
       (((binding index-value.173)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (IndexLet
        ((indexArgs
          (((indexBinding i.148)
            (indexValue
             (Runtime
              (Ref
               ((id index-value.173)
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (sort Dim))))
         (body
          (AtomAsArray
           ((element
             (Box
              ((indices ((Dimension ((const 0) (refs ((i.148 1)))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.157)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding v.171)
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 5)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding make.167)
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.168)
                           (value
                            (Ref
                             ((id make.167)
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding v.172)
                           (value
                            (Ref
                             ((id v.171)
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map
                           (frameShape ((Add ((const 0) (refs ((i.148 1)))))))
                           (args ())
                           (body
                            (Ref
                             ((id v.172)
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 0) (refs ((i.148 1))))))))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 0) (refs ((i.148 1))))))))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 0) (refs ((i.148 1))))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 0) (refs ((i.148 1))))))))))))
               (bodyType
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 0) (refs ((i.148 1)))))))))
               (type'
                ((parameters (((binding i.148) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((i.148 1))))))))))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding i.148) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((i.148 1))))))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding i.148) (bound Dim))))
              (body
               ((element (Literal IntLiteral))
                (shape ((Add ((const 0) (refs ((i.148 1))))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding i.148) (bound Dim))))
           (body
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ((i.148 1))))))))))))
        (shape ((Add ((const 3) (refs ()))))))))) |}];
  checkAndPrint {|
    (lift [@i [1 2 3]]
      (replicate{int | @i []} 5))
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding index-value.173)
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (IndexLet
        ((indexArgs
          (((indexBinding @i.148)
            (indexValue
             (Runtime
              (Ref
               ((id index-value.173)
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (sort Shape))))
         (body
          (AtomAsArray
           ((element
             (Box
              ((indices ((Shape ((ShapeRef @i.148)))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.157)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding v.171)
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 5)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding make.167)
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.168)
                           (value
                            (Ref
                             ((id make.167)
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding v.172)
                           (value
                            (Ref
                             ((id v.171)
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ((ShapeRef @i.148))) (args ())
                           (body
                            (Ref
                             ((id v.172)
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((ShapeRef @i.148))))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((ShapeRef @i.148))))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((ShapeRef @i.148))))))))
                  (type'
                   ((element (Literal IntLiteral)) (shape ((ShapeRef @i.148))))))))
               (bodyType
                ((element (Literal IntLiteral)) (shape ((ShapeRef @i.148)))))
               (type'
                ((parameters (((binding @i.148) (bound Shape))))
                 (body
                  ((element (Literal IntLiteral)) (shape ((ShapeRef @i.148))))))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding @i.148) (bound Shape))))
                 (body
                  ((element (Literal IntLiteral)) (shape ((ShapeRef @i.148))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding @i.148) (bound Shape))))
              (body ((element (Literal IntLiteral)) (shape ((ShapeRef @i.148))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding @i.148) (bound Shape))))
           (body ((element (Literal IntLiteral)) (shape ((ShapeRef @i.148))))))))
        (shape ()))))) |}]
;;
