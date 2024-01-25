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
       (((binding ((name f) (id 135)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 136)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 137)))
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
                  ((id ((name +arg1) (id 136)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id ((name +arg2) (id 137)))
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
       (((binding ((name f) (id 136)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 137)))
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
        ((binding ((name +arg2) (id 139)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 4)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 3) (refs ())))))
         (args
          (((binding ((name +arg1) (id 138)))
            (value
             (Ref
              ((id ((name +arg1) (id 137)))
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
                     ((id ((name +arg1) (id 138)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name +arg2) (id 139)))
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
       (((binding ((name id) (id 138)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 139)))
            (value
             (Ref
              ((id ((name id) (id 138)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 141)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name x) (id 141)))
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
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 142)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name id) (id 139)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 140)))
            (value
             (Ref
              ((id ((name id) (id 139)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 143)))
            (value
             (Ref
              ((id ((name id) (id 142)))
               (type' ((element (Tuple ())) (shape ())))))))))
         (body
          (Ref
           ((id ((name x) (id 143))) (type' ((element (Tuple ())) (shape ()))))))
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
       (((binding ((name id) (id 141)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name id) (id 145)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 147)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 142)))
                  (value
                   (Ref
                    ((id ((name id) (id 141)))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name x) (id 146)))
                  (value
                   (Ref
                    ((id ((name id) (id 145)))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (Ref
                 ((id ((name x) (id 146)))
                  (type' ((element (Tuple ())) (shape ()))))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name arg0) (id 149)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name arg0) (id 149)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
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
       (((binding ((name f) (id 138)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name x) (id 140)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 10)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (Ref
        ((id ((name x) (id 140)))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {|
      (length{int | 5 []} [1 2 3 4 5])
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 135)))
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
       (((binding ((name f) (id 166)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name op) (id 252)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name arr) (id 262)))
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
          (((binding ((name f) (id 212)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name f) (id 253)))
            (value
             (Ref
              ((id ((name op) (id 252)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name init) (id 299)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 290)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 296)))
                  (value
                   (Ref
                    ((id ((name arr) (id 262)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 5) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 295)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 297)))
                     (value
                      (Ref
                       ((id ((name arr) (id 296)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 298)))
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
                       ((id ((name contiguous-subarray-array) (id 297)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 298)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 5) (refs ())))))
                     (resultShape ()) (cellShape ()) (l ((const 1) (refs ())))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name arr) (id 266)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 256)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 263)))
                  (value
                   (Ref
                    ((id ((name arr) (id 262)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 5) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 261)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 264)))
                     (value
                      (Ref
                       ((id ((name arr) (id 263)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 265)))
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
                       ((id ((name contiguous-subarray-array) (id 264)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 265)))
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
             (((binding ((name init) (id 300)))
               (value
                (Ref
                 ((id ((name init) (id 299)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name up-ranked-f) (id 240)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 238)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-f-arg) (id 241)))
                     (value
                      (Ref
                       ((id ((name up-ranked-f) (id 240)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-zero-arg) (id 303)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 275)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 301)))
                           (value
                            (Ref
                             ((id ((name init) (id 300)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name make) (id 285)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 286)))
                                 (value
                                  (Ref
                                   ((id ((name make) (id 285)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name v) (id 302)))
                                 (value
                                  (Ref
                                   ((id ((name v) (id 301)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ()) (args ())
                                 (body
                                  (Ref
                                   ((id ((name v) (id 302)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name reduce-array-arg) (id 267)))
                     (value
                      (Ref
                       ((id ((name arr) (id 266)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 4) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Reduce
                     (arg
                      ((firstBinding ((name reduce-arg1) (id 268)))
                       (secondBinding ((name reduce-arg2) (id 271)))
                       (value
                        (Ref
                         ((id ((name reduce-array-arg) (id 267)))
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 4) (refs ()))))))))))))
                     (zero
                      (Ref
                       ((id ((name reduce-zero-arg) (id 303)))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 254)))
                           (value
                            (Ref
                             ((id ((name f) (id 253)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arg0) (id 269)))
                           (value
                            (Ref
                             ((id ((name reduce-arg1) (id 268)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding ((name arg1) (id 272)))
                           (value
                            (Ref
                             ((id ((name reduce-arg2) (id 271)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name arg0) (id 270)))
                              (value
                               (Ref
                                ((id ((name arg0) (id 269)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding ((name arg1) (id 273)))
                              (value
                               (Ref
                                ((id ((name arg1) (id 272)))
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
                                       ((id ((name arg0) (id 270)))
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))
                                   (ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id ((name arg1) (id 273)))
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
    ((func (Ref ((id ((name f) (id 135))))))
     (args (((id ((name +arg1) (id 132)))) ((id ((name +arg2) (id 133))))))
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
       (((binding ((name id) (id 139)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 140)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name id) (id 139)))
                   (type' ((element (Tuple ())) (shape ())))))
                 (Ref
                  ((id ((name id) (id 139)))
                   (type' ((element (Tuple ())) (shape ())))))
                 (Ref
                  ((id ((name id) (id 139)))
                   (type' ((element (Tuple ())) (shape ())))))))
               (type'
                ((element (Tuple ())) (shape ((Add ((const 3) (refs ())))))))))))
           ((binding ((name x) (id 143)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name f) (id 141)))
               (value
                (Ref
                 ((id ((name f) (id 140)))
                  (type'
                   ((element (Tuple ())) (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (Ref
              ((id ((name x) (id 143)))
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
    (ArrayPrimitive
     (Map (frameShape ()) (args ())
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 142)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name +arg1) (id 146)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 144)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ReifyIndex
                 ((index (Dimension ((const 5) (refs ()))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name +arg2) (id 150)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 148)))
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
                     ((id ((name +arg1) (id 146)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name +arg2) (id 150)))
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
       (((binding ((name f) (id 209)))
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding ((name d-1) (id 24))) (bound Dim))
                       ((binding ((name @cell-shape) (id 25))) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding ((name t) (id 26))) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding ((name arr) (id 210)))
                                    (bound
                                     (Arr
                                      ((element (AtomRef ((name t) (id 26))))
                                       (shape
                                        ((Add
                                          ((const 1)
                                           (refs ((((name d-1) (id 24)) 1)))))
                                         (ShapeRef ((name @cell-shape) (id 25)))))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding ((name f) (id 211)))
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
                                                  ((const 1)
                                                   (refs
                                                    ((((name d-1) (id 24)) 1)))))))
                                               (Shape ())
                                               (Shape
                                                ((ShapeRef
                                                  ((name @cell-shape) (id 25)))))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args
                                           ((Atom (AtomRef ((name t) (id 26))))))))))
                                      ((binding
                                        ((name contiguous-subarray-array)
                                         (id 212)))
                                       (value (Ref ((id ((name arr) (id 210)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 213)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 211))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 212))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 213))))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef ((name t) (id 26))))
                                          (shape
                                           ((ShapeRef
                                             ((name @cell-shape) (id 25)))))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef ((name t) (id 26))))
                                       (shape
                                        ((ShapeRef ((name @cell-shape) (id 25)))))))))))))))))))))))))))))
               (args ((Dimension ((const 1) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding ((name @t) (id 132))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 214)))
         (value (Ref ((id ((name arr) (id 145)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 209))))))
         (args (((id ((name arr) (id 214))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 132))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ())))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 132))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding ((name f) (id 216)))
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding ((name d-1) (id 28))) (bound Dim))
                       ((binding ((name @cell-shape) (id 29))) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding ((name t) (id 30))) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding ((name arr) (id 217)))
                                    (bound
                                     (Arr
                                      ((element (AtomRef ((name t) (id 30))))
                                       (shape
                                        ((Add
                                          ((const 1)
                                           (refs ((((name d-1) (id 28)) 1)))))
                                         (ShapeRef ((name @cell-shape) (id 29)))))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding ((name f) (id 218)))
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
                                                  ((const 1)
                                                   (refs
                                                    ((((name d-1) (id 28)) 1)))))))
                                               (Shape
                                                ((Add
                                                  ((const 0)
                                                   (refs
                                                    ((((name d-1) (id 28)) 1)))))))
                                               (Shape
                                                ((ShapeRef
                                                  ((name @cell-shape) (id 29)))))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args
                                           ((Atom (AtomRef ((name t) (id 30))))))))))
                                      ((binding
                                        ((name contiguous-subarray-array)
                                         (id 219)))
                                       (value (Ref ((id ((name arr) (id 217)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 220)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 218))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 219))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 220))))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef ((name t) (id 30))))
                                          (shape
                                           ((Add
                                             ((const 0)
                                              (refs ((((name d-1) (id 28)) 1)))))
                                            (ShapeRef
                                             ((name @cell-shape) (id 29)))))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef ((name t) (id 30))))
                                       (shape
                                        ((Add
                                          ((const 0)
                                           (refs ((((name d-1) (id 28)) 1)))))
                                         (ShapeRef ((name @cell-shape) (id 29)))))))))))))))))))))))))))))
               (args ((Dimension ((const 1) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding ((name @t) (id 132))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 221)))
         (value (Ref ((id ((name arr) (id 145)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 216))))))
         (args (((id ((name arr) (id 221))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 132))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ((Add ((const 1) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 132))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ((Add ((const 1) (refs ())))))))))) |}];
  checkAndPrint {| (reduce{int | 2 [] []} [+ -] [1 2 3]) |};
  [%expect
    {|
    Error: Could not determine what function is being called in function call:
    ((func (Ref ((id ((name f) (id 248))))))
     (args (((id ((name arg0) (id 251)))) ((id ((name arg1) (id 252))))))
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
       (((binding ((name f) (id 204)))
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding ((name d-1) (id 24))) (bound Dim))
                       ((binding ((name @cell-shape) (id 25))) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding ((name t) (id 26))) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding ((name arr) (id 205)))
                                    (bound
                                     (Arr
                                      ((element (AtomRef ((name t) (id 26))))
                                       (shape
                                        ((Add
                                          ((const 1)
                                           (refs ((((name d-1) (id 24)) 1)))))
                                         (ShapeRef ((name @cell-shape) (id 25)))))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding ((name f) (id 206)))
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
                                                  ((const 1)
                                                   (refs
                                                    ((((name d-1) (id 24)) 1)))))))
                                               (Shape ())
                                               (Shape
                                                ((ShapeRef
                                                  ((name @cell-shape) (id 25)))))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args
                                           ((Atom (AtomRef ((name t) (id 26))))))))))
                                      ((binding
                                        ((name contiguous-subarray-array)
                                         (id 207)))
                                       (value (Ref ((id ((name arr) (id 205)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 208)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 206))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 207))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 208))))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef ((name t) (id 26))))
                                          (shape
                                           ((ShapeRef
                                             ((name @cell-shape) (id 25)))))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef ((name t) (id 26))))
                                       (shape
                                        ((ShapeRef ((name @cell-shape) (id 25)))))))))))))))))))))))))))))
               (args ((Dimension ((const 2) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding ((name @t) (id 134))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 209)))
         (value (Ref ((id ((name arr) (id 140)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 204))))))
         (args (((id ((name arr) (id 209))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 134))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ())))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 134))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding ((name f) (id 211)))
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding ((name d-1) (id 28))) (bound Dim))
                       ((binding ((name @cell-shape) (id 29))) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding ((name t) (id 30))) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding ((name arr) (id 212)))
                                    (bound
                                     (Arr
                                      ((element (AtomRef ((name t) (id 30))))
                                       (shape
                                        ((Add
                                          ((const 1)
                                           (refs ((((name d-1) (id 28)) 1)))))
                                         (ShapeRef ((name @cell-shape) (id 29)))))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding ((name f) (id 213)))
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
                                                  ((const 1)
                                                   (refs
                                                    ((((name d-1) (id 28)) 1)))))))
                                               (Shape
                                                ((Add
                                                  ((const 0)
                                                   (refs
                                                    ((((name d-1) (id 28)) 1)))))))
                                               (Shape
                                                ((ShapeRef
                                                  ((name @cell-shape) (id 29)))))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args
                                           ((Atom (AtomRef ((name t) (id 30))))))))))
                                      ((binding
                                        ((name contiguous-subarray-array)
                                         (id 214)))
                                       (value (Ref ((id ((name arr) (id 212)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 215)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 213))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 214))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 215))))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef ((name t) (id 30))))
                                          (shape
                                           ((Add
                                             ((const 0)
                                              (refs ((((name d-1) (id 28)) 1)))))
                                            (ShapeRef
                                             ((name @cell-shape) (id 29)))))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef ((name t) (id 30))))
                                       (shape
                                        ((Add
                                          ((const 0)
                                           (refs ((((name d-1) (id 28)) 1)))))
                                         (ShapeRef ((name @cell-shape) (id 29)))))))))))))))))))))))))))))
               (args ((Dimension ((const 2) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding ((name @t) (id 134))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 216)))
         (value (Ref ((id ((name arr) (id 140)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 211))))))
         (args (((id ((name arr) (id 216))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 134))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ((Add ((const 2) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 134))) (bound Array))))
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
       (((binding ((name f) (id 204)))
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding ((name d-1) (id 24))) (bound Dim))
                       ((binding ((name @cell-shape) (id 25))) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding ((name t) (id 26))) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding ((name arr) (id 205)))
                                    (bound
                                     (Arr
                                      ((element (AtomRef ((name t) (id 26))))
                                       (shape
                                        ((Add
                                          ((const 1)
                                           (refs ((((name d-1) (id 24)) 1)))))
                                         (ShapeRef ((name @cell-shape) (id 25)))))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding ((name f) (id 206)))
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
                                                  ((const 1)
                                                   (refs
                                                    ((((name d-1) (id 24)) 1)))))))
                                               (Shape ())
                                               (Shape
                                                ((ShapeRef
                                                  ((name @cell-shape) (id 25)))))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args
                                           ((Atom (AtomRef ((name t) (id 26))))))))))
                                      ((binding
                                        ((name contiguous-subarray-array)
                                         (id 207)))
                                       (value (Ref ((id ((name arr) (id 205)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 208)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 206))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 207))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 208))))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef ((name t) (id 26))))
                                          (shape
                                           ((ShapeRef
                                             ((name @cell-shape) (id 25)))))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef ((name t) (id 26))))
                                       (shape
                                        ((ShapeRef ((name @cell-shape) (id 25)))))))))))))))))))))))))))))
               (args ((Dimension ((const 2) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding ((name @t) (id 134))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 209)))
         (value (Ref ((id ((name arr) (id 140)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 204))))))
         (args (((id ((name arr) (id 209))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 134))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ())))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 134))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding ((name f) (id 211)))
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding ((name d-1) (id 28))) (bound Dim))
                       ((binding ((name @cell-shape) (id 29))) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding ((name t) (id 30))) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding ((name arr) (id 212)))
                                    (bound
                                     (Arr
                                      ((element (AtomRef ((name t) (id 30))))
                                       (shape
                                        ((Add
                                          ((const 1)
                                           (refs ((((name d-1) (id 28)) 1)))))
                                         (ShapeRef ((name @cell-shape) (id 29)))))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding ((name f) (id 213)))
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
                                                  ((const 1)
                                                   (refs
                                                    ((((name d-1) (id 28)) 1)))))))
                                               (Shape
                                                ((Add
                                                  ((const 0)
                                                   (refs
                                                    ((((name d-1) (id 28)) 1)))))))
                                               (Shape
                                                ((ShapeRef
                                                  ((name @cell-shape) (id 29)))))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args
                                           ((Atom (AtomRef ((name t) (id 30))))))))))
                                      ((binding
                                        ((name contiguous-subarray-array)
                                         (id 214)))
                                       (value (Ref ((id ((name arr) (id 212)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 215)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 213))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 214))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 215))))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef ((name t) (id 30))))
                                          (shape
                                           ((Add
                                             ((const 0)
                                              (refs ((((name d-1) (id 28)) 1)))))
                                            (ShapeRef
                                             ((name @cell-shape) (id 29)))))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef ((name t) (id 30))))
                                       (shape
                                        ((Add
                                          ((const 0)
                                           (refs ((((name d-1) (id 28)) 1)))))
                                         (ShapeRef ((name @cell-shape) (id 29)))))))))))))))))))))))))))))
               (args ((Dimension ((const 2) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding ((name @t) (id 134))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 216)))
         (value (Ref ((id ((name arr) (id 140)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 211))))))
         (args (((id ((name arr) (id 216))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 134))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ((Add ((const 2) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 134))) (bound Array))))
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
       (((binding ((name f) (id 204)))
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding ((name d-1) (id 24))) (bound Dim))
                       ((binding ((name @cell-shape) (id 25))) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding ((name t) (id 26))) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding ((name arr) (id 205)))
                                    (bound
                                     (Arr
                                      ((element (AtomRef ((name t) (id 26))))
                                       (shape
                                        ((Add
                                          ((const 1)
                                           (refs ((((name d-1) (id 24)) 1)))))
                                         (ShapeRef ((name @cell-shape) (id 25)))))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding ((name f) (id 206)))
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
                                                  ((const 1)
                                                   (refs
                                                    ((((name d-1) (id 24)) 1)))))))
                                               (Shape ())
                                               (Shape
                                                ((ShapeRef
                                                  ((name @cell-shape) (id 25)))))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args
                                           ((Atom (AtomRef ((name t) (id 26))))))))))
                                      ((binding
                                        ((name contiguous-subarray-array)
                                         (id 207)))
                                       (value (Ref ((id ((name arr) (id 205)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 208)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 206))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 207))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 208))))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef ((name t) (id 26))))
                                          (shape
                                           ((ShapeRef
                                             ((name @cell-shape) (id 25)))))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef ((name t) (id 26))))
                                       (shape
                                        ((ShapeRef ((name @cell-shape) (id 25)))))))))))))))))))))))))))))
               (args ((Dimension ((const 2) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding ((name @t) (id 134))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 209)))
         (value (Ref ((id ((name arr) (id 140)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 204))))))
         (args (((id ((name arr) (id 209))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 134))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ())))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 134))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding ((name f) (id 211)))
         (value
          (TypeApplication
           ((tFunc
             (IndexApplication
              ((iFunc
                (Scalar
                 ((element
                   (IndexLambda
                    ((params
                      (((binding ((name d-1) (id 28))) (bound Dim))
                       ((binding ((name @cell-shape) (id 29))) (bound Shape))))
                     (body
                      (Scalar
                       ((element
                         (TypeLambda
                          ((params (((binding ((name t) (id 30))) (bound Atom))))
                           (body
                            (Scalar
                             ((element
                               (TermLambda
                                ((params
                                  (((binding ((name arr) (id 212)))
                                    (bound
                                     (Arr
                                      ((element (AtomRef ((name t) (id 30))))
                                       (shape
                                        ((Add
                                          ((const 1)
                                           (refs ((((name d-1) (id 28)) 1)))))
                                         (ShapeRef ((name @cell-shape) (id 29)))))))))))
                                 (body
                                  (Map
                                   ((args
                                     (((binding ((name f) (id 213)))
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
                                                  ((const 1)
                                                   (refs
                                                    ((((name d-1) (id 28)) 1)))))))
                                               (Shape
                                                ((Add
                                                  ((const 0)
                                                   (refs
                                                    ((((name d-1) (id 28)) 1)))))))
                                               (Shape
                                                ((ShapeRef
                                                  ((name @cell-shape) (id 29)))))
                                               (Dimension ((const 1) (refs ()))))))))
                                          (args
                                           ((Atom (AtomRef ((name t) (id 30))))))))))
                                      ((binding
                                        ((name contiguous-subarray-array)
                                         (id 214)))
                                       (value (Ref ((id ((name arr) (id 212)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 215)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 213))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 214))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 215))))))
                                       (type'
                                        (Arr
                                         ((element (AtomRef ((name t) (id 30))))
                                          (shape
                                           ((Add
                                             ((const 0)
                                              (refs ((((name d-1) (id 28)) 1)))))
                                            (ShapeRef
                                             ((name @cell-shape) (id 29)))))))))))
                                    (frameShape ())
                                    (type'
                                     (Arr
                                      ((element (AtomRef ((name t) (id 30))))
                                       (shape
                                        ((Add
                                          ((const 0)
                                           (refs ((((name d-1) (id 28)) 1)))))
                                         (ShapeRef ((name @cell-shape) (id 29)))))))))))))))))))))))))))))
               (args ((Dimension ((const 2) (refs ()))) (Shape ()))))))
            (args
             ((Atom
               (Forall
                ((parameters (((binding ((name @t) (id 134))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 216)))
         (value (Ref ((id ((name arr) (id 140)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 211))))))
         (args (((id ((name arr) (id 216))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 134))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ((Add ((const 2) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 134))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ((Add ((const 2) (refs ())))))))))) |}];
  checkAndPrint {| (reduce{int | 2 [] []} + [1 2 3]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 166)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name op) (id 252)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name arr) (id 262)))
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
          (((binding ((name f) (id 212)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name f) (id 253)))
            (value
             (Ref
              ((id ((name op) (id 252)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name init) (id 299)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 290)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 296)))
                  (value
                   (Ref
                    ((id ((name arr) (id 262)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 295)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 297)))
                     (value
                      (Ref
                       ((id ((name arr) (id 296)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 298)))
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
                       ((id ((name contiguous-subarray-array) (id 297)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 298)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 3) (refs ())))))
                     (resultShape ()) (cellShape ()) (l ((const 1) (refs ())))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name arr) (id 266)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 256)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 263)))
                  (value
                   (Ref
                    ((id ((name arr) (id 262)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 261)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 264)))
                     (value
                      (Ref
                       ((id ((name arr) (id 263)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 265)))
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
                       ((id ((name contiguous-subarray-array) (id 264)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 265)))
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
             (((binding ((name init) (id 300)))
               (value
                (Ref
                 ((id ((name init) (id 299)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name up-ranked-f) (id 240)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 238)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-f-arg) (id 241)))
                     (value
                      (Ref
                       ((id ((name up-ranked-f) (id 240)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-zero-arg) (id 303)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 275)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 301)))
                           (value
                            (Ref
                             ((id ((name init) (id 300)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name make) (id 285)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 286)))
                                 (value
                                  (Ref
                                   ((id ((name make) (id 285)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name v) (id 302)))
                                 (value
                                  (Ref
                                   ((id ((name v) (id 301)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ()) (args ())
                                 (body
                                  (Ref
                                   ((id ((name v) (id 302)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name reduce-array-arg) (id 267)))
                     (value
                      (Ref
                       ((id ((name arr) (id 266)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 2) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Reduce
                     (arg
                      ((firstBinding ((name reduce-arg1) (id 268)))
                       (secondBinding ((name reduce-arg2) (id 271)))
                       (value
                        (Ref
                         ((id ((name reduce-array-arg) (id 267)))
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 2) (refs ()))))))))))))
                     (zero
                      (Ref
                       ((id ((name reduce-zero-arg) (id 303)))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 254)))
                           (value
                            (Ref
                             ((id ((name f) (id 253)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arg0) (id 269)))
                           (value
                            (Ref
                             ((id ((name reduce-arg1) (id 268)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding ((name arg1) (id 272)))
                           (value
                            (Ref
                             ((id ((name reduce-arg2) (id 271)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name arg0) (id 270)))
                              (value
                               (Ref
                                ((id ((name arg0) (id 269)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding ((name arg1) (id 273)))
                              (value
                               (Ref
                                ((id ((name arg1) (id 272)))
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
                                       ((id ((name arg0) (id 270)))
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))
                                   (ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id ((name arg1) (id 273)))
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
       (((binding ((name f) (id 136)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name fold-f-arg) (id 137)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name fold-zero-arg) (id 140)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 0)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name fold-array-arg) (id 142)))
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
          ((binding ((name fold-zero-arg) (id 141)))
           (value
            (Ref
             ((id ((name fold-zero-arg) (id 140)))
              (type' ((element (Literal IntLiteral)) (shape ()))))))))
         (arrayArgs
          (((binding ((name fold-array-arg) (id 143)))
            (value
             (Ref
              ((id ((name fold-array-arg) (id 142)))
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
                     ((id ((name fold-zero-arg) (id 141)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name fold-array-arg) (id 143)))
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
       (((binding ((name abc) (id 150)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 142)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-f-arg) (id 144)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-zero-arg) (id 151)))
            (value
             (Ref
              ((id ((name abc) (id 150)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Fold
            (zeroArg
             ((binding ((name fold-zero-arg) (id 152)))
              (value
               (Ref
                ((id ((name fold-zero-arg) (id 151)))
                 (type' ((element (Literal IntLiteral)) (shape ()))))))))
            (arrayArgs ())
            (body
             (Ref
              ((id ((name fold-zero-arg) (id 152)))
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
       (((binding ((name abc) (id 161)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name abc) (id 166)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 147)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-f-arg) (id 149)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-zero-arg) (id 171)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name fold-array-arg) (id 162)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name abc) (id 161)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 161)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 161)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))
           ((binding ((name fold-array-arg) (id 167)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name abc) (id 166)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 166)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 166)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Fold
            (zeroArg
             ((binding ((name fold-zero-arg) (id 172)))
              (value
               (Ref
                ((id ((name fold-zero-arg) (id 171)))
                 (type' ((element (Literal IntLiteral)) (shape ()))))))))
            (arrayArgs
             (((binding ((name fold-array-arg) (id 163)))
               (value
                (Ref
                 ((id ((name fold-array-arg) (id 162)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))
              ((binding ((name fold-array-arg) (id 168)))
               (value
                (Ref
                 ((id ((name fold-array-arg) (id 167)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name a) (id 164)))
                  (value
                   (Ref
                    ((id ((name fold-array-arg) (id 163)))
                     (type' ((element (Literal IntLiteral)) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name b) (id 169)))
                     (value
                      (Ref
                       ((id ((name fold-array-arg) (id 168)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name f) (id 160)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name +arg1) (id 165)))
                        (value
                         (Ref
                          ((id ((name a) (id 164)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))
                       ((binding ((name +arg2) (id 170)))
                        (value
                         (Ref
                          ((id ((name b) (id 169)))
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
                                 ((id ((name +arg1) (id 165)))
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name +arg2) (id 170)))
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
       (((binding ((name a) (id 138)))
         (value
          (TypeApplication
           ((tFunc (Ref ((id ((name x) (id 136))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))
      (body (Ref ((id ((name x) (id 136)))))) (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 135))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ())))))) |}];
  checkAndPrint {| (length{(Forall (@x) int) | 2 []} [(t-fn (@x) 5) (t-fn (@x) 5)]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 138)))
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
       (((binding ((name f) (id 166)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name op) (id 252)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name arr) (id 262)))
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
          (((binding ((name f) (id 212)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name f) (id 253)))
            (value
             (Ref
              ((id ((name op) (id 252)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name init) (id 299)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 290)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 296)))
                  (value
                   (Ref
                    ((id ((name arr) (id 262)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 295)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 297)))
                     (value
                      (Ref
                       ((id ((name arr) (id 296)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 298)))
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
                       ((id ((name contiguous-subarray-array) (id 297)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 298)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 3) (refs ())))))
                     (resultShape ()) (cellShape ()) (l ((const 1) (refs ())))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name arr) (id 266)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 256)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 263)))
                  (value
                   (Ref
                    ((id ((name arr) (id 262)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 261)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 264)))
                     (value
                      (Ref
                       ((id ((name arr) (id 263)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 265)))
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
                       ((id ((name contiguous-subarray-array) (id 264)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 265)))
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
             (((binding ((name init) (id 300)))
               (value
                (Ref
                 ((id ((name init) (id 299)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name up-ranked-f) (id 240)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 238)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name scan-f-arg) (id 241)))
                     (value
                      (Ref
                       ((id ((name up-ranked-f) (id 240)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name scan-zero-arg) (id 303)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 275)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 301)))
                           (value
                            (Ref
                             ((id ((name init) (id 300)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name make) (id 285)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 286)))
                                 (value
                                  (Ref
                                   ((id ((name make) (id 285)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name v) (id 302)))
                                 (value
                                  (Ref
                                   ((id ((name v) (id 301)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ()) (args ())
                                 (body
                                  (Ref
                                   ((id ((name v) (id 302)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name scan-array-arg) (id 267)))
                     (value
                      (Ref
                       ((id ((name arr) (id 266)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 2) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Reduce
                     (arg
                      ((firstBinding ((name reduce-arg1) (id 268)))
                       (secondBinding ((name reduce-arg2) (id 271)))
                       (value
                        (Ref
                         ((id ((name scan-array-arg) (id 267)))
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 2) (refs ()))))))))))))
                     (zero
                      (Ref
                       ((id ((name scan-zero-arg) (id 303)))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 254)))
                           (value
                            (Ref
                             ((id ((name f) (id 253)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arg0) (id 269)))
                           (value
                            (Ref
                             ((id ((name reduce-arg1) (id 268)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding ((name arg1) (id 272)))
                           (value
                            (Ref
                             ((id ((name reduce-arg2) (id 271)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name arg0) (id 270)))
                              (value
                               (Ref
                                ((id ((name arg0) (id 269)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding ((name arg1) (id 273)))
                              (value
                               (Ref
                                ((id ((name arg1) (id 272)))
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
                                       ((id ((name arg0) (id 270)))
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))
                                   (ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id ((name arg1) (id 273)))
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
       (((binding ((name f) (id 190)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name arr) (id 271)))
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
        ((binding ((name flags) (id 274)))
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
          (((binding ((name locs-raw) (id 403)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 278)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name op) (id 365)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name init) (id 395)))
                  (value
                   (AtomAsArray
                    ((element (Literal (IntLiteral 0)))
                     (type' ((element (Literal IntLiteral)) (shape ())))))))
                 ((binding ((name arr) (id 371)))
                  (value
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name f) (id 368)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name boolToIntArg) (id 369)))
                        (value
                         (Ref
                          ((id ((name flags) (id 274)))
                           (type'
                            ((element (Literal BooleanLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ((Add ((const 3) (refs ())))))
                        (args
                         (((binding ((name boolToIntArg) (id 370)))
                           (value
                            (Ref
                             ((id ((name boolToIntArg) (id 369)))
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
                                    ((id ((name boolToIntArg) (id 370)))
                                     (type'
                                      ((element (Literal BooleanLiteral))
                                       (shape ()))))))
                                  (type' (Literal BooleanLiteral))))))
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
                   (((binding ((name f) (id 318)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name arr) (id 400)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 325)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name f) (id 366)))
                           (value
                            (Ref
                             ((id ((name op) (id 365)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name init) (id 396)))
                           (value
                            (Ref
                             ((id ((name init) (id 395)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding ((name arr) (id 372)))
                           (value
                            (Ref
                             ((id ((name arr) (id 371)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 3) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name up-ranked-f) (id 353)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 351)))
                                 (value
                                  (AtomAsArray
                                   ((element (Values ((elements ()) (type' ()))))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name scan-f-arg) (id 354)))
                                 (value
                                  (Ref
                                   ((id ((name up-ranked-f) (id 353)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name scan-zero-arg) (id 399)))
                                 (value
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name f) (id 381)))
                                       (value
                                        (AtomAsArray
                                         ((element
                                           (Values ((elements ()) (type' ()))))
                                          (type'
                                           ((element (Tuple ())) (shape ())))))))
                                      ((binding ((name v) (id 397)))
                                       (value
                                        (Ref
                                         ((id ((name init) (id 396)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))))
                                    (body
                                     (ArrayPrimitive
                                      (Map (frameShape ())
                                       (args
                                        (((binding ((name make) (id 391)))
                                          (value
                                           (AtomAsArray
                                            ((element
                                              (Values ((elements ()) (type' ()))))
                                             (type'
                                              ((element (Tuple ())) (shape ())))))))))
                                       (body
                                        (ArrayPrimitive
                                         (Map (frameShape ())
                                          (args
                                           (((binding ((name f) (id 392)))
                                             (value
                                              (Ref
                                               ((id ((name make) (id 391)))
                                                (type'
                                                 ((element (Tuple ()))
                                                  (shape ())))))))
                                            ((binding ((name v) (id 398)))
                                             (value
                                              (Ref
                                               ((id ((name v) (id 397)))
                                                (type'
                                                 ((element (Literal IntLiteral))
                                                  (shape ())))))))))
                                          (body
                                           (ArrayPrimitive
                                            (Map (frameShape ()) (args ())
                                             (body
                                              (Ref
                                               ((id ((name v) (id 398)))
                                                (type'
                                                 ((element (Literal IntLiteral))
                                                  (shape ()))))))
                                             (type'
                                              ((element (Literal IntLiteral))
                                               (shape ()))))))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ()))))))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ()))))))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))
                                ((binding ((name scan-array-arg) (id 373)))
                                 (value
                                  (Ref
                                   ((id ((name arr) (id 372)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))))
                              (body
                               (ArrayPrimitive
                                (Reduce
                                 (arg
                                  ((firstBinding ((name reduce-arg1) (id 374)))
                                   (secondBinding ((name reduce-arg2) (id 377)))
                                   (value
                                    (Ref
                                     ((id ((name scan-array-arg) (id 373)))
                                      (type'
                                       ((element (Literal IntLiteral))
                                        (shape ((Add ((const 3) (refs ()))))))))))))
                                 (zero
                                  (Ref
                                   ((id ((name scan-zero-arg) (id 399)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name f) (id 367)))
                                       (value
                                        (Ref
                                         ((id ((name f) (id 366)))
                                          (type'
                                           ((element (Tuple ())) (shape ())))))))
                                      ((binding ((name arg0) (id 375)))
                                       (value
                                        (Ref
                                         ((id ((name reduce-arg1) (id 374)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))
                                      ((binding ((name arg1) (id 378)))
                                       (value
                                        (Ref
                                         ((id ((name reduce-arg2) (id 377)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))))
                                    (body
                                     (ArrayPrimitive
                                      (Map (frameShape ())
                                       (args
                                        (((binding ((name arg0) (id 376)))
                                          (value
                                           (Ref
                                            ((id ((name arg0) (id 375)))
                                             (type'
                                              ((element (Literal IntLiteral))
                                               (shape ())))))))
                                         ((binding ((name arg1) (id 379)))
                                          (value
                                           (Ref
                                            ((id ((name arg1) (id 378)))
                                             (type'
                                              ((element (Literal IntLiteral))
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
                                                   ((id ((name arg0) (id 376)))
                                                    (type'
                                                     ((element
                                                       (Literal IntLiteral))
                                                      (shape ()))))))
                                                 (type' (Literal IntLiteral))))
                                               (ArrayAsAtom
                                                ((array
                                                  (Ref
                                                   ((id ((name arg1) (id 379)))
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
                                        ((element (Literal IntLiteral))
                                         (shape ()))))))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
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
                      (((binding ((name f) (id 323)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name contiguous-subarray-array) (id 401)))
                        (value
                         (Ref
                          ((id ((name arr) (id 400)))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 4) (refs ())))))))))))
                       ((binding ((name contiguous-subarray-index) (id 402)))
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
                          ((id ((name contiguous-subarray-array) (id 401)))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 4) (refs ()))))))))))
                        (indexArg
                         (Ref
                          ((id ((name contiguous-subarray-index) (id 402)))
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
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding ((name locs) (id 425)))
               (value
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 273)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name if-cond) (id 275)))
                     (value
                      (Ref
                       ((id ((name flags) (id 274)))
                        (type'
                         ((element (Literal BooleanLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name then-branch) (id 404)))
                     (value
                      (Ref
                       ((id ((name locs-raw) (id 403)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name else-branch) (id 423)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 407)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 421)))
                           (value
                            (AtomAsArray
                             ((element (Literal (IntLiteral -1)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name make) (id 417)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 418)))
                                 (value
                                  (Ref
                                   ((id ((name make) (id 417)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name v) (id 422)))
                                 (value
                                  (Ref
                                   ((id ((name v) (id 421)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ((Add ((const 3) (refs ())))))
                                 (args ())
                                 (body
                                  (Ref
                                   ((id ((name v) (id 422)))
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
                      (((binding ((name if-cond) (id 276)))
                        (value
                         (Ref
                          ((id ((name if-cond) (id 275)))
                           (type'
                            ((element (Literal BooleanLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))
                       ((binding ((name then-branch) (id 405)))
                        (value
                         (Ref
                          ((id ((name then-branch) (id 404)))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))
                       ((binding ((name else-branch) (id 424)))
                        (value
                         (Ref
                          ((id ((name else-branch) (id 423)))
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
                                 ((id ((name if-cond) (id 276)))
                                  (type'
                                   ((element (Literal BooleanLiteral))
                                    (shape ()))))))
                               (type' (Literal BooleanLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name then-branch) (id 405)))
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name else-branch) (id 424)))
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
                (((binding ((name result-size) (id 439)))
                  (value
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name f) (id 428)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name arr) (id 435)))
                        (value
                         (Ref
                          ((id ((name locs-raw) (id 403)))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))
                       ((binding ((name index) (id 437)))
                        (value
                         (Frame
                          ((dimensions (1))
                           (elements
                            ((ReifyIndex
                              ((index (Dimension ((const 3) (refs ()))))
                               (type'
                                ((element (Literal IntLiteral)) (shape ())))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 1) (refs ())))))))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 434)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name contiguous-subarray-array) (id 436)))
                           (value
                            (Ref
                             ((id ((name arr) (id 435)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 3) (refs ())))))))))))
                          ((binding ((name contiguous-subarray-index) (id 438)))
                           (value
                            (Ref
                             ((id ((name index) (id 437)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 1) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (ContiguousSubArray
                           (arrayArg
                            (Ref
                             ((id ((name contiguous-subarray-array) (id 436)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 3) (refs ()))))))))))
                           (indexArg
                            (Ref
                             ((id ((name contiguous-subarray-index) (id 438)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 1) (refs ()))))))))))
                           (originalShape ((Add ((const 3) (refs ())))))
                           (resultShape ()) (cellShape ())
                           (l ((const 1) (refs ())))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (type' ((element (Literal IntLiteral)) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name index-value) (id 440)))
                     (value
                      (Ref
                       ((id ((name result-size) (id 439)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (IndexLet
                    ((indexArgs
                      (((indexBinding ((name d) (id 129)))
                        (indexValue
                         (Runtime
                          (Ref
                           ((id ((name index-value) (id 440)))
                            (type' ((element (Literal IntLiteral)) (shape ())))))))
                        (sort Dim))))
                     (body
                      (AtomAsArray
                       ((element
                         (Box
                          ((indices
                            ((Dimension
                              ((const 0) (refs ((((name d) (id 129)) 1)))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 270)))
                                 (value
                                  (AtomAsArray
                                   ((element (Values ((elements ()) (type' ()))))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name scatter-values) (id 272)))
                                 (value
                                  (Ref
                                   ((id ((name arr) (id 271)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))
                                ((binding ((name scatter-indices) (id 426)))
                                 (value
                                  (Ref
                                   ((id ((name locs) (id 425)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))))
                              (body
                               (ArrayPrimitive
                                (Scatter
                                 (valuesArg
                                  (Ref
                                   ((id ((name scatter-values) (id 272)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ()))))))))))
                                 (indicesArg
                                  (Ref
                                   ((id ((name scatter-indices) (id 426)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ()))))))))))
                                 (dIn ((const 3) (refs ())))
                                 (dOut
                                  ((const 0) (refs ((((name d) (id 129)) 1)))))
                                 (cellShape ())
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape
                                    ((Add
                                      ((const 0)
                                       (refs ((((name d) (id 129)) 1))))))))))))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape
                                 ((Add
                                   ((const 0) (refs ((((name d) (id 129)) 1))))))))))))
                           (bodyType
                            ((element (Literal IntLiteral))
                             (shape
                              ((Add ((const 0) (refs ((((name d) (id 129)) 1)))))))))
                           (type'
                            ((parameters
                              (((binding ((name d) (id 129))) (bound Dim))))
                             (body
                              ((element (Literal IntLiteral))
                               (shape
                                ((Add
                                  ((const 0) (refs ((((name d) (id 129)) 1))))))))))))))
                        (type'
                         ((element
                           (Sigma
                            ((parameters
                              (((binding ((name d) (id 129))) (bound Dim))))
                             (body
                              ((element (Literal IntLiteral))
                               (shape
                                ((Add
                                  ((const 0) (refs ((((name d) (id 129)) 1))))))))))))
                          (shape ()))))))
                     (type'
                      ((element
                        (Sigma
                         ((parameters
                           (((binding ((name d) (id 129))) (bound Dim))))
                          (body
                           ((element (Literal IntLiteral))
                            (shape
                             ((Add ((const 0) (refs ((((name d) (id 129)) 1))))))))))))
                       (shape ()))))))
                  (type'
                   ((element
                     (Sigma
                      ((parameters (((binding ((name d) (id 129))) (bound Dim))))
                       (body
                        ((element (Literal IntLiteral))
                         (shape
                          ((Add ((const 0) (refs ((((name d) (id 129)) 1))))))))))))
                    (shape ()))))))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name d) (id 129))) (bound Dim))))
                    (body
                     ((element (Literal IntLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name d) (id 129)) 1))))))))))))
                 (shape ()))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name d) (id 129))) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((((name d) (id 129)) 1))))))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding ((name d) (id 129))) (bound Dim))))
              (body
               ((element (Literal IntLiteral))
                (shape ((Add ((const 0) (refs ((((name d) (id 129)) 1))))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding ((name d) (id 129))) (bound Dim))))
           (body
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ((((name d) (id 129)) 1))))))))))))
        (shape ()))))) |}];
  checkAndPrint {| (append{int | 3 2 []} [1 2 3] [4 5]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 135)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name append-arg1) (id 136)))
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
        ((binding ((name append-arg2) (id 137)))
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
           ((id ((name append-arg1) (id 136)))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (arg2
          (Ref
           ((id ((name append-arg2) (id 137)))
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
  [%expect {| Error: Lambda captures variable x.133, which escapes its definition |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ((Add ((const 3) (refs ())))))
      (args
       (((binding ((name index-value) (id 157)))
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
          (((indexBinding ((name i) (id 132)))
            (indexValue
             (Runtime
              (Ref
               ((id ((name index-value) (id 157)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (sort Dim))))
         (body
          (AtomAsArray
           ((element
             (Box
              ((indices
                ((Dimension ((const 0) (refs ((((name i) (id 132)) 1)))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 141)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name v) (id 155)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 5)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name make) (id 151)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 152)))
                           (value
                            (Ref
                             ((id ((name make) (id 151)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 156)))
                           (value
                            (Ref
                             ((id ((name v) (id 155)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map
                           (frameShape
                            ((Add ((const 0) (refs ((((name i) (id 132)) 1)))))))
                           (args ())
                           (body
                            (Ref
                             ((id ((name v) (id 156)))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape
                              ((Add ((const 0) (refs ((((name i) (id 132)) 1))))))))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape
                           ((Add ((const 0) (refs ((((name i) (id 132)) 1))))))))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name i) (id 132)) 1))))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 0) (refs ((((name i) (id 132)) 1))))))))))))
               (bodyType
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 0) (refs ((((name i) (id 132)) 1)))))))))
               (type'
                ((parameters (((binding ((name i) (id 132))) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((((name i) (id 132)) 1))))))))))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name i) (id 132))) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((((name i) (id 132)) 1))))))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding ((name i) (id 132))) (bound Dim))))
              (body
               ((element (Literal IntLiteral))
                (shape ((Add ((const 0) (refs ((((name i) (id 132)) 1))))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding ((name i) (id 132))) (bound Dim))))
           (body
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ((((name i) (id 132)) 1))))))))))))
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
       (((binding ((name index-value) (id 157)))
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
          (((indexBinding ((name @i) (id 132)))
            (indexValue
             (Runtime
              (Ref
               ((id ((name index-value) (id 157)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (sort Shape))))
         (body
          (AtomAsArray
           ((element
             (Box
              ((indices ((Shape ((ShapeRef ((name @i) (id 132)))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 141)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name v) (id 155)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 5)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name make) (id 151)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 152)))
                           (value
                            (Ref
                             ((id ((name make) (id 151)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 156)))
                           (value
                            (Ref
                             ((id ((name v) (id 155)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ((ShapeRef ((name @i) (id 132)))))
                           (args ())
                           (body
                            (Ref
                             ((id ((name v) (id 156)))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((ShapeRef ((name @i) (id 132))))))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((ShapeRef ((name @i) (id 132))))))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((ShapeRef ((name @i) (id 132))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((ShapeRef ((name @i) (id 132))))))))))
               (bodyType
                ((element (Literal IntLiteral))
                 (shape ((ShapeRef ((name @i) (id 132)))))))
               (type'
                ((parameters (((binding ((name @i) (id 132))) (bound Shape))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((ShapeRef ((name @i) (id 132))))))))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name @i) (id 132))) (bound Shape))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((ShapeRef ((name @i) (id 132))))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding ((name @i) (id 132))) (bound Shape))))
              (body
               ((element (Literal IntLiteral))
                (shape ((ShapeRef ((name @i) (id 132))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding ((name @i) (id 132))) (bound Shape))))
           (body
            ((element (Literal IntLiteral))
             (shape ((ShapeRef ((name @i) (id 132))))))))))
        (shape ()))))) |}]
;;
