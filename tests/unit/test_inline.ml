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
       (((binding ((name f) (id 151)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 152)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 153)))
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
                  ((id ((name +arg1) (id 152)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id ((name +arg2) (id 153)))
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
       (((binding ((name f) (id 152)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 153)))
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
        ((binding ((name +arg2) (id 155)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 4)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 3) (refs ())))))
         (args
          (((binding ((name +arg1) (id 154)))
            (value
             (Ref
              ((id ((name +arg1) (id 153)))
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
                     ((id ((name +arg1) (id 154)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name +arg2) (id 155)))
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
       (((binding ((name id) (id 154)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 155)))
            (value
             (Ref
              ((id ((name id) (id 154)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 157)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name x) (id 157)))
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
       (((binding ((name id) (id 158)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name id) (id 155)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 156)))
            (value
             (Ref
              ((id ((name id) (id 155)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 159)))
            (value
             (Ref
              ((id ((name id) (id 158)))
               (type' ((element (Tuple ())) (shape ())))))))))
         (body
          (Ref
           ((id ((name x) (id 159))) (type' ((element (Tuple ())) (shape ()))))))
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
       (((binding ((name id) (id 157)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name id) (id 161)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 163)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 158)))
                  (value
                   (Ref
                    ((id ((name id) (id 157)))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name x) (id 162)))
                  (value
                   (Ref
                    ((id ((name id) (id 161)))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (Ref
                 ((id ((name x) (id 162)))
                  (type' ((element (Tuple ())) (shape ()))))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name arg0) (id 165)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name arg0) (id 165)))
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
       (((binding ((name f) (id 154)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name x) (id 156)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 10)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (Ref
        ((id ((name x) (id 156)))
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
       (((binding ((name f) (id 151)))
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
       (((binding ((name f) (id 182)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name op) (id 268)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name arr) (id 278)))
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
          (((binding ((name f) (id 228)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name f) (id 269)))
            (value
             (Ref
              ((id ((name op) (id 268)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name init) (id 315)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 306)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 312)))
                  (value
                   (Ref
                    ((id ((name arr) (id 278)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 5) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 311)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 313)))
                     (value
                      (Ref
                       ((id ((name arr) (id 312)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 314)))
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
                       ((id ((name contiguous-subarray-array) (id 313)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 314)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 5) (refs ())))))
                     (resultShape ()) (cellShape ()) (l ((const 1) (refs ())))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name arr) (id 282)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 272)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 279)))
                  (value
                   (Ref
                    ((id ((name arr) (id 278)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 5) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 277)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 280)))
                     (value
                      (Ref
                       ((id ((name arr) (id 279)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 281)))
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
                       ((id ((name contiguous-subarray-array) (id 280)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 281)))
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
             (((binding ((name init) (id 316)))
               (value
                (Ref
                 ((id ((name init) (id 315)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name up-ranked-f) (id 256)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 254)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-f-arg) (id 257)))
                     (value
                      (Ref
                       ((id ((name up-ranked-f) (id 256)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-zero-arg) (id 319)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 291)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 317)))
                           (value
                            (Ref
                             ((id ((name init) (id 316)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name make) (id 301)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 302)))
                                 (value
                                  (Ref
                                   ((id ((name make) (id 301)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name v) (id 318)))
                                 (value
                                  (Ref
                                   ((id ((name v) (id 317)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ()) (args ())
                                 (body
                                  (Ref
                                   ((id ((name v) (id 318)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name reduce-array-arg) (id 283)))
                     (value
                      (Ref
                       ((id ((name arr) (id 282)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 4) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Reduce
                     (arg
                      ((firstBinding ((name reduce-arg1) (id 284)))
                       (secondBinding ((name reduce-arg2) (id 287)))
                       (value
                        (Ref
                         ((id ((name reduce-array-arg) (id 283)))
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 4) (refs ()))))))))))))
                     (zero
                      (Ref
                       ((id ((name reduce-zero-arg) (id 319)))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 270)))
                           (value
                            (Ref
                             ((id ((name f) (id 269)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arg0) (id 285)))
                           (value
                            (Ref
                             ((id ((name reduce-arg1) (id 284)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding ((name arg1) (id 288)))
                           (value
                            (Ref
                             ((id ((name reduce-arg2) (id 287)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name arg0) (id 286)))
                              (value
                               (Ref
                                ((id ((name arg0) (id 285)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding ((name arg1) (id 289)))
                              (value
                               (Ref
                                ((id ((name arg1) (id 288)))
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
                                       ((id ((name arg0) (id 286)))
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))
                                   (ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id ((name arg1) (id 289)))
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
    ((func (Ref ((id ((name f) (id 151))))))
     (args (((id ((name +arg1) (id 148)))) ((id ((name +arg2) (id 149))))))
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
       (((binding ((name id) (id 155)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 156)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name id) (id 155)))
                   (type' ((element (Tuple ())) (shape ())))))
                 (Ref
                  ((id ((name id) (id 155)))
                   (type' ((element (Tuple ())) (shape ())))))
                 (Ref
                  ((id ((name id) (id 155)))
                   (type' ((element (Tuple ())) (shape ())))))))
               (type'
                ((element (Tuple ())) (shape ((Add ((const 3) (refs ())))))))))))
           ((binding ((name x) (id 159)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name f) (id 157)))
               (value
                (Ref
                 ((id ((name f) (id 156)))
                  (type'
                   ((element (Tuple ())) (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (Ref
              ((id ((name x) (id 159)))
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
          (((binding ((name f) (id 158)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name +arg1) (id 162)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 160)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ReifyIndex
                 ((index (Dimension ((const 5) (refs ()))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name +arg2) (id 166)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 164)))
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
                     ((id ((name +arg1) (id 162)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name +arg2) (id 166)))
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
       (((binding ((name f) (id 225)))
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
                                  (((binding ((name arr) (id 226)))
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
                                     (((binding ((name f) (id 227)))
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
                                         (id 228)))
                                       (value (Ref ((id ((name arr) (id 226)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 229)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 227))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 228))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 229))))))
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
                ((parameters (((binding ((name @t) (id 148))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 230)))
         (value (Ref ((id ((name arr) (id 161)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 225))))))
         (args (((id ((name arr) (id 230))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 148))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ())))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 148))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding ((name f) (id 232)))
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
                                  (((binding ((name arr) (id 233)))
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
                                     (((binding ((name f) (id 234)))
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
                                         (id 235)))
                                       (value (Ref ((id ((name arr) (id 233)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 236)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 234))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 235))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 236))))))
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
                ((parameters (((binding ((name @t) (id 148))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 237)))
         (value (Ref ((id ((name arr) (id 161)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 232))))))
         (args (((id ((name arr) (id 237))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 148))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ((Add ((const 1) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 148))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ((Add ((const 1) (refs ())))))))))) |}];
  checkAndPrint {| (reduce{int | 2 [] []} [+ -] [1 2 3]) |};
  [%expect
    {|
    Error: Could not determine what function is being called in function call:
    ((func (Ref ((id ((name f) (id 264))))))
     (args (((id ((name arg0) (id 267)))) ((id ((name arg1) (id 268))))))
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
       (((binding ((name f) (id 220)))
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
                                  (((binding ((name arr) (id 221)))
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
                                     (((binding ((name f) (id 222)))
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
                                         (id 223)))
                                       (value (Ref ((id ((name arr) (id 221)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 224)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 222))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 223))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 224))))))
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
                ((parameters (((binding ((name @t) (id 150))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 225)))
         (value (Ref ((id ((name arr) (id 156)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 220))))))
         (args (((id ((name arr) (id 225))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 150))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ())))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 150))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding ((name f) (id 227)))
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
                                  (((binding ((name arr) (id 228)))
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
                                     (((binding ((name f) (id 229)))
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
                                         (id 230)))
                                       (value (Ref ((id ((name arr) (id 228)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 231)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 229))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 230))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 231))))))
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
                ((parameters (((binding ((name @t) (id 150))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 232)))
         (value (Ref ((id ((name arr) (id 156)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 227))))))
         (args (((id ((name arr) (id 232))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 150))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ((Add ((const 2) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 150))) (bound Array))))
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
       (((binding ((name f) (id 220)))
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
                                  (((binding ((name arr) (id 221)))
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
                                     (((binding ((name f) (id 222)))
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
                                         (id 223)))
                                       (value (Ref ((id ((name arr) (id 221)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 224)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 222))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 223))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 224))))))
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
                ((parameters (((binding ((name @t) (id 150))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 225)))
         (value (Ref ((id ((name arr) (id 156)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 220))))))
         (args (((id ((name arr) (id 225))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 150))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ())))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 150))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding ((name f) (id 227)))
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
                                  (((binding ((name arr) (id 228)))
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
                                     (((binding ((name f) (id 229)))
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
                                         (id 230)))
                                       (value (Ref ((id ((name arr) (id 228)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 231)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 229))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 230))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 231))))))
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
                ((parameters (((binding ((name @t) (id 150))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 232)))
         (value (Ref ((id ((name arr) (id 156)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 227))))))
         (args (((id ((name arr) (id 232))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 150))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ((Add ((const 2) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 150))) (bound Array))))
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
       (((binding ((name f) (id 220)))
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
                                  (((binding ((name arr) (id 221)))
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
                                     (((binding ((name f) (id 222)))
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
                                         (id 223)))
                                       (value (Ref ((id ((name arr) (id 221)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 224)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 222))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 223))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 224))))))
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
                ((parameters (((binding ((name @t) (id 150))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 225)))
         (value (Ref ((id ((name arr) (id 156)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 220))))))
         (args (((id ((name arr) (id 225))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 150))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ())))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 150))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding ((name f) (id 227)))
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
                                  (((binding ((name arr) (id 228)))
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
                                     (((binding ((name f) (id 229)))
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
                                         (id 230)))
                                       (value (Ref ((id ((name arr) (id 228)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 231)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 229))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 230))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 231))))))
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
                ((parameters (((binding ((name @t) (id 150))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 232)))
         (value (Ref ((id ((name arr) (id 156)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 227))))))
         (args (((id ((name arr) (id 232))))))
         (type'
          (Arr
           ((element
             (Forall
              ((parameters (((binding ((name @t) (id 150))) (bound Array))))
               (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (shape ((Add ((const 2) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 150))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ((Add ((const 2) (refs ())))))))))) |}];
  checkAndPrint {| (reduce{int | 2 [] []} + [1 2 3]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 182)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name op) (id 268)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name arr) (id 278)))
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
          (((binding ((name f) (id 228)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name f) (id 269)))
            (value
             (Ref
              ((id ((name op) (id 268)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name init) (id 315)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 306)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 312)))
                  (value
                   (Ref
                    ((id ((name arr) (id 278)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 311)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 313)))
                     (value
                      (Ref
                       ((id ((name arr) (id 312)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 314)))
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
                       ((id ((name contiguous-subarray-array) (id 313)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 314)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 3) (refs ())))))
                     (resultShape ()) (cellShape ()) (l ((const 1) (refs ())))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name arr) (id 282)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 272)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 279)))
                  (value
                   (Ref
                    ((id ((name arr) (id 278)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 277)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 280)))
                     (value
                      (Ref
                       ((id ((name arr) (id 279)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 281)))
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
                       ((id ((name contiguous-subarray-array) (id 280)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 281)))
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
             (((binding ((name init) (id 316)))
               (value
                (Ref
                 ((id ((name init) (id 315)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name up-ranked-f) (id 256)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 254)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-f-arg) (id 257)))
                     (value
                      (Ref
                       ((id ((name up-ranked-f) (id 256)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-zero-arg) (id 319)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 291)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 317)))
                           (value
                            (Ref
                             ((id ((name init) (id 316)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name make) (id 301)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 302)))
                                 (value
                                  (Ref
                                   ((id ((name make) (id 301)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name v) (id 318)))
                                 (value
                                  (Ref
                                   ((id ((name v) (id 317)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ()) (args ())
                                 (body
                                  (Ref
                                   ((id ((name v) (id 318)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name reduce-array-arg) (id 283)))
                     (value
                      (Ref
                       ((id ((name arr) (id 282)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 2) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Reduce
                     (arg
                      ((firstBinding ((name reduce-arg1) (id 284)))
                       (secondBinding ((name reduce-arg2) (id 287)))
                       (value
                        (Ref
                         ((id ((name reduce-array-arg) (id 283)))
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 2) (refs ()))))))))))))
                     (zero
                      (Ref
                       ((id ((name reduce-zero-arg) (id 319)))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 270)))
                           (value
                            (Ref
                             ((id ((name f) (id 269)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arg0) (id 285)))
                           (value
                            (Ref
                             ((id ((name reduce-arg1) (id 284)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding ((name arg1) (id 288)))
                           (value
                            (Ref
                             ((id ((name reduce-arg2) (id 287)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name arg0) (id 286)))
                              (value
                               (Ref
                                ((id ((name arg0) (id 285)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding ((name arg1) (id 289)))
                              (value
                               (Ref
                                ((id ((name arg1) (id 288)))
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
                                       ((id ((name arg0) (id 286)))
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))
                                   (ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id ((name arg1) (id 289)))
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
       (((binding ((name f) (id 152)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name fold-f-arg) (id 153)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name fold-zero-arg) (id 156)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 0)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name fold-array-arg) (id 158)))
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
          ((binding ((name fold-zero-arg) (id 157)))
           (value
            (Ref
             ((id ((name fold-zero-arg) (id 156)))
              (type' ((element (Literal IntLiteral)) (shape ()))))))))
         (arrayArgs
          (((binding ((name fold-array-arg) (id 159)))
            (value
             (Ref
              ((id ((name fold-array-arg) (id 158)))
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
                     ((id ((name fold-zero-arg) (id 157)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name fold-array-arg) (id 159)))
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
       (((binding ((name abc) (id 166)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 158)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-f-arg) (id 160)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-zero-arg) (id 167)))
            (value
             (Ref
              ((id ((name abc) (id 166)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Fold
            (zeroArg
             ((binding ((name fold-zero-arg) (id 168)))
              (value
               (Ref
                ((id ((name fold-zero-arg) (id 167)))
                 (type' ((element (Literal IntLiteral)) (shape ()))))))))
            (arrayArgs ())
            (body
             (Ref
              ((id ((name fold-zero-arg) (id 168)))
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
       (((binding ((name abc) (id 177)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name abc) (id 182)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 163)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-f-arg) (id 165)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-zero-arg) (id 187)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name fold-array-arg) (id 178)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name abc) (id 177)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 177)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 177)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))
           ((binding ((name fold-array-arg) (id 183)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name abc) (id 182)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 182)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 182)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Fold
            (zeroArg
             ((binding ((name fold-zero-arg) (id 188)))
              (value
               (Ref
                ((id ((name fold-zero-arg) (id 187)))
                 (type' ((element (Literal IntLiteral)) (shape ()))))))))
            (arrayArgs
             (((binding ((name fold-array-arg) (id 179)))
               (value
                (Ref
                 ((id ((name fold-array-arg) (id 178)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))
              ((binding ((name fold-array-arg) (id 184)))
               (value
                (Ref
                 ((id ((name fold-array-arg) (id 183)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name a) (id 180)))
                  (value
                   (Ref
                    ((id ((name fold-array-arg) (id 179)))
                     (type' ((element (Literal IntLiteral)) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name b) (id 185)))
                     (value
                      (Ref
                       ((id ((name fold-array-arg) (id 184)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name f) (id 176)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name +arg1) (id 181)))
                        (value
                         (Ref
                          ((id ((name a) (id 180)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))
                       ((binding ((name +arg2) (id 186)))
                        (value
                         (Ref
                          ((id ((name b) (id 185)))
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
                                 ((id ((name +arg1) (id 181)))
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name +arg2) (id 186)))
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
       (((binding ((name a) (id 154)))
         (value
          (TypeApplication
           ((tFunc (Ref ((id ((name x) (id 152))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))
      (body (Ref ((id ((name x) (id 152)))))) (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 151))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ())))))) |}];
  checkAndPrint {| (length{(Forall (@x) int) | 2 []} [(t-fn (@x) 5) (t-fn (@x) 5)]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 154)))
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
       (((binding ((name f) (id 182)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name op) (id 268)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name arr) (id 278)))
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
          (((binding ((name f) (id 228)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name f) (id 269)))
            (value
             (Ref
              ((id ((name op) (id 268)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name init) (id 315)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 306)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 312)))
                  (value
                   (Ref
                    ((id ((name arr) (id 278)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 311)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 313)))
                     (value
                      (Ref
                       ((id ((name arr) (id 312)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 314)))
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
                       ((id ((name contiguous-subarray-array) (id 313)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 314)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 3) (refs ())))))
                     (resultShape ()) (cellShape ()) (l ((const 1) (refs ())))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name arr) (id 282)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 272)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 279)))
                  (value
                   (Ref
                    ((id ((name arr) (id 278)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 277)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 280)))
                     (value
                      (Ref
                       ((id ((name arr) (id 279)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 281)))
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
                       ((id ((name contiguous-subarray-array) (id 280)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 281)))
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
             (((binding ((name init) (id 316)))
               (value
                (Ref
                 ((id ((name init) (id 315)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name up-ranked-f) (id 256)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 254)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name scan-f-arg) (id 257)))
                     (value
                      (Ref
                       ((id ((name up-ranked-f) (id 256)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name scan-zero-arg) (id 319)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 291)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 317)))
                           (value
                            (Ref
                             ((id ((name init) (id 316)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name make) (id 301)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 302)))
                                 (value
                                  (Ref
                                   ((id ((name make) (id 301)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name v) (id 318)))
                                 (value
                                  (Ref
                                   ((id ((name v) (id 317)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ()) (args ())
                                 (body
                                  (Ref
                                   ((id ((name v) (id 318)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name scan-array-arg) (id 283)))
                     (value
                      (Ref
                       ((id ((name arr) (id 282)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 2) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Reduce
                     (arg
                      ((firstBinding ((name reduce-arg1) (id 284)))
                       (secondBinding ((name reduce-arg2) (id 287)))
                       (value
                        (Ref
                         ((id ((name scan-array-arg) (id 283)))
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 2) (refs ()))))))))))))
                     (zero
                      (Ref
                       ((id ((name scan-zero-arg) (id 319)))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 270)))
                           (value
                            (Ref
                             ((id ((name f) (id 269)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arg0) (id 285)))
                           (value
                            (Ref
                             ((id ((name reduce-arg1) (id 284)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding ((name arg1) (id 288)))
                           (value
                            (Ref
                             ((id ((name reduce-arg2) (id 287)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name arg0) (id 286)))
                              (value
                               (Ref
                                ((id ((name arg0) (id 285)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding ((name arg1) (id 289)))
                              (value
                               (Ref
                                ((id ((name arg1) (id 288)))
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
                                       ((id ((name arg0) (id 286)))
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))
                                   (ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id ((name arg1) (id 289)))
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
       (((binding ((name f) (id 216)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name arr) (id 307)))
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
        ((binding ((name flags) (id 310)))
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
          (((binding ((name locs-raw) (id 443)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 313)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name -arg1) (id 440)))
                  (value
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name f) (id 315)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name op) (id 402)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name init) (id 432)))
                        (value
                         (AtomAsArray
                          ((element (Literal (IntLiteral 0)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))
                       ((binding ((name arr) (id 408)))
                        (value
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 405)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding ((name boolToIntArg) (id 406)))
                              (value
                               (Ref
                                ((id ((name flags) (id 310)))
                                 (type'
                                  ((element (Literal BooleanLiteral))
                                   (shape ((Add ((const 3) (refs ())))))))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ((Add ((const 3) (refs ())))))
                              (args
                               (((binding ((name boolToIntArg) (id 407)))
                                 (value
                                  (Ref
                                   ((id ((name boolToIntArg) (id 406)))
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
                                          ((id ((name boolToIntArg) (id 407)))
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
                         (((binding ((name f) (id 355)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 437)))
                           (value
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 362)))
                                 (value
                                  (AtomAsArray
                                   ((element (Values ((elements ()) (type' ()))))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name f) (id 403)))
                                 (value
                                  (Ref
                                   ((id ((name op) (id 402)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name init) (id 433)))
                                 (value
                                  (Ref
                                   ((id ((name init) (id 432)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))
                                ((binding ((name arr) (id 409)))
                                 (value
                                  (Ref
                                   ((id ((name arr) (id 408)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding ((name up-ranked-f) (id 390)))
                                    (value
                                     (AtomAsArray
                                      ((element
                                        (Values ((elements ()) (type' ()))))
                                       (type' ((element (Tuple ())) (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name f) (id 388)))
                                       (value
                                        (AtomAsArray
                                         ((element
                                           (Values ((elements ()) (type' ()))))
                                          (type'
                                           ((element (Tuple ())) (shape ())))))))
                                      ((binding ((name scan-f-arg) (id 391)))
                                       (value
                                        (Ref
                                         ((id ((name up-ranked-f) (id 390)))
                                          (type'
                                           ((element (Tuple ())) (shape ())))))))
                                      ((binding ((name scan-zero-arg) (id 436)))
                                       (value
                                        (ArrayPrimitive
                                         (Map (frameShape ())
                                          (args
                                           (((binding ((name f) (id 418)))
                                             (value
                                              (AtomAsArray
                                               ((element
                                                 (Values
                                                  ((elements ()) (type' ()))))
                                                (type'
                                                 ((element (Tuple ()))
                                                  (shape ())))))))
                                            ((binding ((name v) (id 434)))
                                             (value
                                              (Ref
                                               ((id ((name init) (id 433)))
                                                (type'
                                                 ((element (Literal IntLiteral))
                                                  (shape ())))))))))
                                          (body
                                           (ArrayPrimitive
                                            (Map (frameShape ())
                                             (args
                                              (((binding ((name make) (id 428)))
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
                                                 (((binding ((name f) (id 429)))
                                                   (value
                                                    (Ref
                                                     ((id ((name make) (id 428)))
                                                      (type'
                                                       ((element (Tuple ()))
                                                        (shape ())))))))
                                                  ((binding ((name v) (id 435)))
                                                   (value
                                                    (Ref
                                                     ((id ((name v) (id 434)))
                                                      (type'
                                                       ((element
                                                         (Literal IntLiteral))
                                                        (shape ())))))))))
                                                (body
                                                 (ArrayPrimitive
                                                  (Map (frameShape ()) (args ())
                                                   (body
                                                    (Ref
                                                     ((id ((name v) (id 435)))
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
                                      ((binding ((name scan-array-arg) (id 410)))
                                       (value
                                        (Ref
                                         ((id ((name arr) (id 409)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ((Add ((const 3) (refs ())))))))))))))
                                    (body
                                     (ArrayPrimitive
                                      (Reduce
                                       (arg
                                        ((firstBinding
                                          ((name reduce-arg1) (id 411)))
                                         (secondBinding
                                          ((name reduce-arg2) (id 414)))
                                         (value
                                          (Ref
                                           ((id ((name scan-array-arg) (id 410)))
                                            (type'
                                             ((element (Literal IntLiteral))
                                              (shape
                                               ((Add ((const 3) (refs ()))))))))))))
                                       (zero
                                        (Ref
                                         ((id ((name scan-zero-arg) (id 436)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ()))))))
                                       (body
                                        (ArrayPrimitive
                                         (Map (frameShape ())
                                          (args
                                           (((binding ((name f) (id 404)))
                                             (value
                                              (Ref
                                               ((id ((name f) (id 403)))
                                                (type'
                                                 ((element (Tuple ()))
                                                  (shape ())))))))
                                            ((binding ((name arg0) (id 412)))
                                             (value
                                              (Ref
                                               ((id
                                                 ((name reduce-arg1) (id 411)))
                                                (type'
                                                 ((element (Literal IntLiteral))
                                                  (shape ())))))))
                                            ((binding ((name arg1) (id 415)))
                                             (value
                                              (Ref
                                               ((id
                                                 ((name reduce-arg2) (id 414)))
                                                (type'
                                                 ((element (Literal IntLiteral))
                                                  (shape ())))))))))
                                          (body
                                           (ArrayPrimitive
                                            (Map (frameShape ())
                                             (args
                                              (((binding ((name arg0) (id 413)))
                                                (value
                                                 (Ref
                                                  ((id ((name arg0) (id 412)))
                                                   (type'
                                                    ((element
                                                      (Literal IntLiteral))
                                                     (shape ())))))))
                                               ((binding ((name arg1) (id 416)))
                                                (value
                                                 (Ref
                                                  ((id ((name arg1) (id 415)))
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
                                                         ((id
                                                           ((name arg0) (id 413)))
                                                          (type'
                                                           ((element
                                                             (Literal IntLiteral))
                                                            (shape ()))))))
                                                       (type'
                                                        (Literal IntLiteral))))
                                                     (ArrayAsAtom
                                                      ((array
                                                        (Ref
                                                         ((id
                                                           ((name arg1) (id 416)))
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
                            (((binding ((name f) (id 360)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding
                               ((name contiguous-subarray-array) (id 438)))
                              (value
                               (Ref
                                ((id ((name arr) (id 437)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 4) (refs ())))))))))))
                             ((binding
                               ((name contiguous-subarray-index) (id 439)))
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
                                ((id ((name contiguous-subarray-array) (id 438)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 4) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id ((name contiguous-subarray-index) (id 439)))
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
                 ((binding ((name -arg2) (id 442)))
                  (value
                   (AtomAsArray
                    ((element (Literal (IntLiteral 1)))
                     (type' ((element (Literal IntLiteral)) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ((Add ((const 3) (refs ())))))
                  (args
                   (((binding ((name -arg1) (id 441)))
                     (value
                      (Ref
                       ((id ((name -arg1) (id 440)))
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
                              ((id ((name -arg1) (id 441)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name -arg2) (id 442)))
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
             (((binding ((name locs) (id 465)))
               (value
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 309)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name if-cond) (id 311)))
                     (value
                      (Ref
                       ((id ((name flags) (id 310)))
                        (type'
                         ((element (Literal BooleanLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name then-branch) (id 444)))
                     (value
                      (Ref
                       ((id ((name locs-raw) (id 443)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name else-branch) (id 463)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 447)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 461)))
                           (value
                            (AtomAsArray
                             ((element (Literal (IntLiteral -1)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name make) (id 457)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 458)))
                                 (value
                                  (Ref
                                   ((id ((name make) (id 457)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name v) (id 462)))
                                 (value
                                  (Ref
                                   ((id ((name v) (id 461)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ((Add ((const 3) (refs ())))))
                                 (args ())
                                 (body
                                  (Ref
                                   ((id ((name v) (id 462)))
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
                      (((binding ((name if-cond) (id 312)))
                        (value
                         (Ref
                          ((id ((name if-cond) (id 311)))
                           (type'
                            ((element (Literal BooleanLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))
                       ((binding ((name then-branch) (id 445)))
                        (value
                         (Ref
                          ((id ((name then-branch) (id 444)))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))
                       ((binding ((name else-branch) (id 464)))
                        (value
                         (Ref
                          ((id ((name else-branch) (id 463)))
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
                                 ((id ((name if-cond) (id 312)))
                                  (type'
                                   ((element (Literal BooleanLiteral))
                                    (shape ()))))))
                               (type' (Literal BooleanLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name then-branch) (id 445)))
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name else-branch) (id 464)))
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
                (((binding ((name result-size) (id 485)))
                  (value
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name f) (id 467)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name +arg1) (id 468)))
                        (value
                         (AtomAsArray
                          ((element (Literal (IntLiteral 1)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))
                       ((binding ((name +arg2) (id 484)))
                        (value
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 470)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding ((name arr) (id 477)))
                              (value
                               (Ref
                                ((id ((name locs-raw) (id 443)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 3) (refs ())))))))))))
                             ((binding ((name index) (id 482)))
                              (value
                               (Frame
                                ((dimensions (1))
                                 (elements
                                  ((ArrayPrimitive
                                    (Map (frameShape ())
                                     (args
                                      (((binding ((name f) (id 479)))
                                        (value
                                         (AtomAsArray
                                          ((element
                                            (Values ((elements ()) (type' ()))))
                                           (type'
                                            ((element (Tuple ())) (shape ())))))))
                                       ((binding ((name -arg1) (id 480)))
                                        (value
                                         (ReifyIndex
                                          ((index
                                            (Dimension ((const 3) (refs ()))))
                                           (type'
                                            ((element (Literal IntLiteral))
                                             (shape ())))))))
                                       ((binding ((name -arg2) (id 481)))
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
                                                 ((id ((name -arg1) (id 480)))
                                                  (type'
                                                   ((element
                                                     (Literal IntLiteral))
                                                    (shape ()))))))
                                               (type' (Literal IntLiteral))))
                                             (ArrayAsAtom
                                              ((array
                                                (Ref
                                                 ((id ((name -arg2) (id 481)))
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
                               (((binding ((name f) (id 476)))
                                 (value
                                  (AtomAsArray
                                   ((element (Values ((elements ()) (type' ()))))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding
                                  ((name contiguous-subarray-array) (id 478)))
                                 (value
                                  (Ref
                                   ((id ((name arr) (id 477)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))
                                ((binding
                                  ((name contiguous-subarray-index) (id 483)))
                                 (value
                                  (Ref
                                   ((id ((name index) (id 482)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 1) (refs ())))))))))))))
                              (body
                               (ArrayPrimitive
                                (ContiguousSubArray
                                 (arrayArg
                                  (Ref
                                   ((id
                                     ((name contiguous-subarray-array) (id 478)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ()))))))))))
                                 (indexArg
                                  (Ref
                                   ((id
                                     ((name contiguous-subarray-index) (id 483)))
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
                                 ((id ((name +arg1) (id 468)))
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name +arg2) (id 484)))
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
                   (((binding ((name index-value) (id 486)))
                     (value
                      (Ref
                       ((id ((name result-size) (id 485)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (IndexLet
                    ((indexArgs
                      (((indexBinding ((name d) (id 129)))
                        (indexValue
                         (Runtime
                          (Ref
                           ((id ((name index-value) (id 486)))
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
                               (((binding ((name f) (id 306)))
                                 (value
                                  (AtomAsArray
                                   ((element (Values ((elements ()) (type' ()))))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name scatter-values) (id 308)))
                                 (value
                                  (Ref
                                   ((id ((name arr) (id 307)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))
                                ((binding ((name scatter-indices) (id 466)))
                                 (value
                                  (Ref
                                   ((id ((name locs) (id 465)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))))
                              (body
                               (ArrayPrimitive
                                (Scatter
                                 (valuesArg
                                  (Ref
                                   ((id ((name scatter-values) (id 308)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ()))))))))))
                                 (indicesArg
                                  (Ref
                                   ((id ((name scatter-indices) (id 466)))
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
       (((binding ((name f) (id 151)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name append-arg1) (id 152)))
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
        ((binding ((name append-arg2) (id 153)))
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
           ((id ((name append-arg1) (id 152)))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (arg2
          (Ref
           ((id ((name append-arg2) (id 153)))
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
       (((binding ((name index-value) (id 173)))
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
          (((indexBinding ((name i) (id 148)))
            (indexValue
             (Runtime
              (Ref
               ((id ((name index-value) (id 173)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (sort Dim))))
         (body
          (AtomAsArray
           ((element
             (Box
              ((indices
                ((Dimension ((const 0) (refs ((((name i) (id 148)) 1)))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 157)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name v) (id 171)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 5)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name make) (id 167)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 168)))
                           (value
                            (Ref
                             ((id ((name make) (id 167)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 172)))
                           (value
                            (Ref
                             ((id ((name v) (id 171)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map
                           (frameShape
                            ((Add ((const 0) (refs ((((name i) (id 148)) 1)))))))
                           (args ())
                           (body
                            (Ref
                             ((id ((name v) (id 172)))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape
                              ((Add ((const 0) (refs ((((name i) (id 148)) 1))))))))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape
                           ((Add ((const 0) (refs ((((name i) (id 148)) 1))))))))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name i) (id 148)) 1))))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 0) (refs ((((name i) (id 148)) 1))))))))))))
               (bodyType
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 0) (refs ((((name i) (id 148)) 1)))))))))
               (type'
                ((parameters (((binding ((name i) (id 148))) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((((name i) (id 148)) 1))))))))))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name i) (id 148))) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((((name i) (id 148)) 1))))))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding ((name i) (id 148))) (bound Dim))))
              (body
               ((element (Literal IntLiteral))
                (shape ((Add ((const 0) (refs ((((name i) (id 148)) 1))))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding ((name i) (id 148))) (bound Dim))))
           (body
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ((((name i) (id 148)) 1))))))))))))
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
       (((binding ((name index-value) (id 173)))
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
          (((indexBinding ((name @i) (id 148)))
            (indexValue
             (Runtime
              (Ref
               ((id ((name index-value) (id 173)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (sort Shape))))
         (body
          (AtomAsArray
           ((element
             (Box
              ((indices ((Shape ((ShapeRef ((name @i) (id 148)))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 157)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name v) (id 171)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 5)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name make) (id 167)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 168)))
                           (value
                            (Ref
                             ((id ((name make) (id 167)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 172)))
                           (value
                            (Ref
                             ((id ((name v) (id 171)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ((ShapeRef ((name @i) (id 148)))))
                           (args ())
                           (body
                            (Ref
                             ((id ((name v) (id 172)))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((ShapeRef ((name @i) (id 148))))))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((ShapeRef ((name @i) (id 148))))))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((ShapeRef ((name @i) (id 148))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((ShapeRef ((name @i) (id 148))))))))))
               (bodyType
                ((element (Literal IntLiteral))
                 (shape ((ShapeRef ((name @i) (id 148)))))))
               (type'
                ((parameters (((binding ((name @i) (id 148))) (bound Shape))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((ShapeRef ((name @i) (id 148))))))))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name @i) (id 148))) (bound Shape))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((ShapeRef ((name @i) (id 148))))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding ((name @i) (id 148))) (bound Shape))))
              (body
               ((element (Literal IntLiteral))
                (shape ((ShapeRef ((name @i) (id 148))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding ((name @i) (id 148))) (bound Shape))))
           (body
            ((element (Literal IntLiteral))
             (shape ((ShapeRef ((name @i) (id 148))))))))))
        (shape ()))))) |}]
;;
