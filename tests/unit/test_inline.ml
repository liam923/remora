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
       (((binding ((name f) (id 114)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 115)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 116)))
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
                  ((id ((name +arg1) (id 115)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id ((name +arg2) (id 116)))
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
       (((binding ((name f) (id 115)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 116)))
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
        ((binding ((name +arg2) (id 118)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 4)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 3) (refs ())))))
         (args
          (((binding ((name +arg1) (id 117)))
            (value
             (Ref
              ((id ((name +arg1) (id 116)))
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
                     ((id ((name +arg1) (id 117)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name +arg2) (id 118)))
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
       (((binding ((name id) (id 117)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 118)))
            (value
             (Ref
              ((id ((name id) (id 117)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 120)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name x) (id 120)))
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
       (((binding ((name id) (id 121)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name id) (id 118)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 119)))
            (value
             (Ref
              ((id ((name id) (id 118)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 122)))
            (value
             (Ref
              ((id ((name id) (id 121)))
               (type' ((element (Tuple ())) (shape ())))))))))
         (body
          (Ref
           ((id ((name x) (id 122))) (type' ((element (Tuple ())) (shape ()))))))
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
       (((binding ((name id) (id 120)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name id) (id 124)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 126)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 121)))
                  (value
                   (Ref
                    ((id ((name id) (id 120)))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name x) (id 125)))
                  (value
                   (Ref
                    ((id ((name id) (id 124)))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (Ref
                 ((id ((name x) (id 125)))
                  (type' ((element (Tuple ())) (shape ()))))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name arg0) (id 128)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name arg0) (id 128)))
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
       (((binding ((name f) (id 117)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name x) (id 119)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 10)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (Ref
        ((id ((name x) (id 119)))
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
       (((binding ((name f) (id 114)))
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
       (((binding ((name f) (id 145)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name op) (id 231)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name arr) (id 241)))
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
          (((binding ((name f) (id 191)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name f) (id 232)))
            (value
             (Ref
              ((id ((name op) (id 231)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name init) (id 278)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 269)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 275)))
                  (value
                   (Ref
                    ((id ((name arr) (id 241)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 5) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 274)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 276)))
                     (value
                      (Ref
                       ((id ((name arr) (id 275)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 277)))
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
                       ((id ((name contiguous-subarray-array) (id 276)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 277)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 5) (refs ())))))
                     (resultShape ()) (cellShape ()) (l ((const 1) (refs ())))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name arr) (id 245)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 235)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 242)))
                  (value
                   (Ref
                    ((id ((name arr) (id 241)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 5) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 240)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 243)))
                     (value
                      (Ref
                       ((id ((name arr) (id 242)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 244)))
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
                       ((id ((name contiguous-subarray-array) (id 243)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 5) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 244)))
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
             (((binding ((name init) (id 279)))
               (value
                (Ref
                 ((id ((name init) (id 278)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name up-ranked-f) (id 219)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 217)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-f-arg) (id 220)))
                     (value
                      (Ref
                       ((id ((name up-ranked-f) (id 219)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-zero-arg) (id 282)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 254)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 280)))
                           (value
                            (Ref
                             ((id ((name init) (id 279)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name make) (id 264)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 265)))
                                 (value
                                  (Ref
                                   ((id ((name make) (id 264)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name v) (id 281)))
                                 (value
                                  (Ref
                                   ((id ((name v) (id 280)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ()) (args ())
                                 (body
                                  (Ref
                                   ((id ((name v) (id 281)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name reduce-array-arg) (id 246)))
                     (value
                      (Ref
                       ((id ((name arr) (id 245)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 4) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Reduce
                     (arg
                      ((firstBinding ((name reduce-arg1) (id 247)))
                       (secondBinding ((name reduce-arg2) (id 250)))
                       (value
                        (Ref
                         ((id ((name reduce-array-arg) (id 246)))
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 4) (refs ()))))))))))))
                     (zero
                      (Ref
                       ((id ((name reduce-zero-arg) (id 282)))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 233)))
                           (value
                            (Ref
                             ((id ((name f) (id 232)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arg0) (id 248)))
                           (value
                            (Ref
                             ((id ((name reduce-arg1) (id 247)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding ((name arg1) (id 251)))
                           (value
                            (Ref
                             ((id ((name reduce-arg2) (id 250)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name arg0) (id 249)))
                              (value
                               (Ref
                                ((id ((name arg0) (id 248)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding ((name arg1) (id 252)))
                              (value
                               (Ref
                                ((id ((name arg1) (id 251)))
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
                                       ((id ((name arg0) (id 249)))
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))
                                   (ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id ((name arg1) (id 252)))
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
    ((func (Ref ((id ((name f) (id 114))))))
     (args (((id ((name +arg1) (id 111)))) ((id ((name +arg2) (id 112))))))
     (type' ((element (Literal IntLiteral)) (shape ())))) |}];
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
       (((binding ((name id) (id 118)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 119)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name id) (id 118)))
                   (type' ((element (Tuple ())) (shape ())))))
                 (Ref
                  ((id ((name id) (id 118)))
                   (type' ((element (Tuple ())) (shape ())))))
                 (Ref
                  ((id ((name id) (id 118)))
                   (type' ((element (Tuple ())) (shape ())))))))
               (type'
                ((element (Tuple ())) (shape ((Add ((const 3) (refs ())))))))))))
           ((binding ((name x) (id 122)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name f) (id 120)))
               (value
                (Ref
                 ((id ((name f) (id 119)))
                  (type'
                   ((element (Tuple ())) (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (Ref
              ((id ((name x) (id 122)))
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
          (((binding ((name f) (id 121)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name +arg1) (id 125)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 123)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ReifyIndex
                 ((index (Dimension ((const 5) (refs ()))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name +arg2) (id 129)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 127)))
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
                     ((id ((name +arg1) (id 125)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name +arg2) (id 129)))
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
       (((binding ((name f) (id 188)))
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
                                  (((binding ((name arr) (id 189)))
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
                                     (((binding ((name f) (id 190)))
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
                                         (id 191)))
                                       (value (Ref ((id ((name arr) (id 189)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 192)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 190))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 191))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 192))))))
                                       (type'
                                        ((element (AtomRef ((name t) (id 26))))
                                         (shape
                                          ((ShapeRef
                                            ((name @cell-shape) (id 25))))))))))
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
                ((parameters (((binding ((name @t) (id 111))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 193)))
         (value (Ref ((id ((name arr) (id 124)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 188))))))
         (args (((id ((name arr) (id 193))))))
         (type'
          ((element
            (Forall
             ((parameters (((binding ((name @t) (id 111))) (bound Array))))
              (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
           (shape ()))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 111))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding ((name f) (id 195)))
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
                                  (((binding ((name arr) (id 196)))
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
                                     (((binding ((name f) (id 197)))
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
                                         (id 198)))
                                       (value (Ref ((id ((name arr) (id 196)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 199)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 197))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 198))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 199))))))
                                       (type'
                                        ((element (AtomRef ((name t) (id 30))))
                                         (shape
                                          ((Add
                                            ((const 0)
                                             (refs ((((name d-1) (id 28)) 1)))))
                                           (ShapeRef
                                            ((name @cell-shape) (id 29))))))))))
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
                ((parameters (((binding ((name @t) (id 111))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 200)))
         (value (Ref ((id ((name arr) (id 124)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 195))))))
         (args (((id ((name arr) (id 200))))))
         (type'
          ((element
            (Forall
             ((parameters (((binding ((name @t) (id 111))) (bound Array))))
              (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
           (shape ((Add ((const 1) (refs ()))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 111))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ((Add ((const 1) (refs ())))))))))) |}];
  checkAndPrint {| (reduce{int | 2 [] []} [+ -] [1 2 3]) |};
  [%expect
    {|
    Error: Could not determine what function is being called in function call:
    ((func (Ref ((id ((name f) (id 227))))))
     (args (((id ((name arg0) (id 230)))) ((id ((name arg1) (id 231))))))
     (type' ((element (Literal IntLiteral)) (shape ())))) |}];
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
       (((binding ((name f) (id 183)))
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
                                  (((binding ((name arr) (id 184)))
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
                                     (((binding ((name f) (id 185)))
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
                                         (id 186)))
                                       (value (Ref ((id ((name arr) (id 184)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 187)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 185))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 186))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 187))))))
                                       (type'
                                        ((element (AtomRef ((name t) (id 26))))
                                         (shape
                                          ((ShapeRef
                                            ((name @cell-shape) (id 25))))))))))
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
                ((parameters (((binding ((name @t) (id 113))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 188)))
         (value (Ref ((id ((name arr) (id 119)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 183))))))
         (args (((id ((name arr) (id 188))))))
         (type'
          ((element
            (Forall
             ((parameters (((binding ((name @t) (id 113))) (bound Array))))
              (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
           (shape ()))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 113))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding ((name f) (id 190)))
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
                                  (((binding ((name arr) (id 191)))
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
                                     (((binding ((name f) (id 192)))
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
                                         (id 193)))
                                       (value (Ref ((id ((name arr) (id 191)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 194)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 192))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 193))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 194))))))
                                       (type'
                                        ((element (AtomRef ((name t) (id 30))))
                                         (shape
                                          ((Add
                                            ((const 0)
                                             (refs ((((name d-1) (id 28)) 1)))))
                                           (ShapeRef
                                            ((name @cell-shape) (id 29))))))))))
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
                ((parameters (((binding ((name @t) (id 113))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 195)))
         (value (Ref ((id ((name arr) (id 119)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 190))))))
         (args (((id ((name arr) (id 195))))))
         (type'
          ((element
            (Forall
             ((parameters (((binding ((name @t) (id 113))) (bound Array))))
              (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
           (shape ((Add ((const 2) (refs ()))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 113))) (bound Array))))
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
       (((binding ((name f) (id 183)))
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
                                  (((binding ((name arr) (id 184)))
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
                                     (((binding ((name f) (id 185)))
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
                                         (id 186)))
                                       (value (Ref ((id ((name arr) (id 184)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 187)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 185))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 186))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 187))))))
                                       (type'
                                        ((element (AtomRef ((name t) (id 26))))
                                         (shape
                                          ((ShapeRef
                                            ((name @cell-shape) (id 25))))))))))
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
                ((parameters (((binding ((name @t) (id 113))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 188)))
         (value (Ref ((id ((name arr) (id 119)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 183))))))
         (args (((id ((name arr) (id 188))))))
         (type'
          ((element
            (Forall
             ((parameters (((binding ((name @t) (id 113))) (bound Array))))
              (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
           (shape ()))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 113))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding ((name f) (id 190)))
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
                                  (((binding ((name arr) (id 191)))
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
                                     (((binding ((name f) (id 192)))
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
                                         (id 193)))
                                       (value (Ref ((id ((name arr) (id 191)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 194)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 192))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 193))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 194))))))
                                       (type'
                                        ((element (AtomRef ((name t) (id 30))))
                                         (shape
                                          ((Add
                                            ((const 0)
                                             (refs ((((name d-1) (id 28)) 1)))))
                                           (ShapeRef
                                            ((name @cell-shape) (id 29))))))))))
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
                ((parameters (((binding ((name @t) (id 113))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 195)))
         (value (Ref ((id ((name arr) (id 119)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 190))))))
         (args (((id ((name arr) (id 195))))))
         (type'
          ((element
            (Forall
             ((parameters (((binding ((name @t) (id 113))) (bound Array))))
              (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
           (shape ((Add ((const 2) (refs ()))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 113))) (bound Array))))
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
       (((binding ((name f) (id 183)))
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
                                  (((binding ((name arr) (id 184)))
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
                                     (((binding ((name f) (id 185)))
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
                                         (id 186)))
                                       (value (Ref ((id ((name arr) (id 184)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 187)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 0)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 185))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 186))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 187))))))
                                       (type'
                                        ((element (AtomRef ((name t) (id 26))))
                                         (shape
                                          ((ShapeRef
                                            ((name @cell-shape) (id 25))))))))))
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
                ((parameters (((binding ((name @t) (id 113))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 188)))
         (value (Ref ((id ((name arr) (id 119)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 183))))))
         (args (((id ((name arr) (id 188))))))
         (type'
          ((element
            (Forall
             ((parameters (((binding ((name @t) (id 113))) (bound Array))))
              (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
           (shape ()))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 113))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ()))))))

    Error: Polymorphic variables and function arguments must be a value type, got not-value:
    (Map
     ((args
       (((binding ((name f) (id 190)))
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
                                  (((binding ((name arr) (id 191)))
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
                                     (((binding ((name f) (id 192)))
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
                                         (id 193)))
                                       (value (Ref ((id ((name arr) (id 191)))))))
                                      ((binding
                                        ((name contiguous-subarray-index)
                                         (id 194)))
                                       (value
                                        (Frame
                                         ((dimensions (1))
                                          (elements
                                           ((Scalar
                                             ((element (Literal (IntLiteral 1)))))))))))))
                                    (body
                                     (TermApplication
                                      ((func (Ref ((id ((name f) (id 192))))))
                                       (args
                                        (((id
                                           ((name contiguous-subarray-array)
                                            (id 193))))
                                         ((id
                                           ((name contiguous-subarray-index)
                                            (id 194))))))
                                       (type'
                                        ((element (AtomRef ((name t) (id 30))))
                                         (shape
                                          ((Add
                                            ((const 0)
                                             (refs ((((name d-1) (id 28)) 1)))))
                                           (ShapeRef
                                            ((name @cell-shape) (id 29))))))))))
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
                ((parameters (((binding ((name @t) (id 113))) (bound Array))))
                 (body (Arr ((element (Literal IntLiteral)) (shape ())))))))))))))
        ((binding ((name arr) (id 195)))
         (value (Ref ((id ((name arr) (id 119)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 190))))))
         (args (((id ((name arr) (id 195))))))
         (type'
          ((element
            (Forall
             ((parameters (((binding ((name @t) (id 113))) (bound Array))))
              (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
           (shape ((Add ((const 2) (refs ()))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 113))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ((Add ((const 2) (refs ())))))))))) |}];
  checkAndPrint {| (reduce{int | 2 [] []} + [1 2 3]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 145)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name op) (id 231)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name arr) (id 241)))
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
          (((binding ((name f) (id 191)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name f) (id 232)))
            (value
             (Ref
              ((id ((name op) (id 231)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name init) (id 278)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 269)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 275)))
                  (value
                   (Ref
                    ((id ((name arr) (id 241)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 274)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 276)))
                     (value
                      (Ref
                       ((id ((name arr) (id 275)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 277)))
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
                       ((id ((name contiguous-subarray-array) (id 276)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 277)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 3) (refs ())))))
                     (resultShape ()) (cellShape ()) (l ((const 1) (refs ())))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name arr) (id 245)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 235)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 242)))
                  (value
                   (Ref
                    ((id ((name arr) (id 241)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 240)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 243)))
                     (value
                      (Ref
                       ((id ((name arr) (id 242)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 244)))
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
                       ((id ((name contiguous-subarray-array) (id 243)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 244)))
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
             (((binding ((name init) (id 279)))
               (value
                (Ref
                 ((id ((name init) (id 278)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name up-ranked-f) (id 219)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 217)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-f-arg) (id 220)))
                     (value
                      (Ref
                       ((id ((name up-ranked-f) (id 219)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-zero-arg) (id 282)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 254)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 280)))
                           (value
                            (Ref
                             ((id ((name init) (id 279)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name make) (id 264)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 265)))
                                 (value
                                  (Ref
                                   ((id ((name make) (id 264)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name v) (id 281)))
                                 (value
                                  (Ref
                                   ((id ((name v) (id 280)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ()) (args ())
                                 (body
                                  (Ref
                                   ((id ((name v) (id 281)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name reduce-array-arg) (id 246)))
                     (value
                      (Ref
                       ((id ((name arr) (id 245)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 2) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Reduce
                     (arg
                      ((firstBinding ((name reduce-arg1) (id 247)))
                       (secondBinding ((name reduce-arg2) (id 250)))
                       (value
                        (Ref
                         ((id ((name reduce-array-arg) (id 246)))
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 2) (refs ()))))))))))))
                     (zero
                      (Ref
                       ((id ((name reduce-zero-arg) (id 282)))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 233)))
                           (value
                            (Ref
                             ((id ((name f) (id 232)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arg0) (id 248)))
                           (value
                            (Ref
                             ((id ((name reduce-arg1) (id 247)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding ((name arg1) (id 251)))
                           (value
                            (Ref
                             ((id ((name reduce-arg2) (id 250)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name arg0) (id 249)))
                              (value
                               (Ref
                                ((id ((name arg0) (id 248)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding ((name arg1) (id 252)))
                              (value
                               (Ref
                                ((id ((name arg1) (id 251)))
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
                                       ((id ((name arg0) (id 249)))
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))
                                   (ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id ((name arg1) (id 252)))
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
       (((binding ((name f) (id 115)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name fold-f-arg) (id 116)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name fold-zero-arg) (id 119)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 0)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name fold-array-arg) (id 121)))
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
          ((binding ((name fold-zero-arg) (id 120)))
           (value
            (Ref
             ((id ((name fold-zero-arg) (id 119)))
              (type' ((element (Literal IntLiteral)) (shape ()))))))))
         (arrayArgs
          (((binding ((name fold-array-arg) (id 122)))
            (value
             (Ref
              ((id ((name fold-array-arg) (id 121)))
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
                     ((id ((name fold-zero-arg) (id 120)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name fold-array-arg) (id 122)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))))
               (type' (Literal IntLiteral)))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (d ((const 5) (refs ()))) (cellShape ()) (character Fold)
         (type' ((element (Literal IntLiteral)) (shape ()))))))
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
       (((binding ((name abc) (id 129)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 121)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-f-arg) (id 123)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-zero-arg) (id 130)))
            (value
             (Ref
              ((id ((name abc) (id 129)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Fold
            (zeroArg
             ((binding ((name fold-zero-arg) (id 131)))
              (value
               (Ref
                ((id ((name fold-zero-arg) (id 130)))
                 (type' ((element (Literal IntLiteral)) (shape ()))))))))
            (arrayArgs ())
            (body
             (Ref
              ((id ((name fold-zero-arg) (id 131)))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (d ((const 3) (refs ()))) (cellShape ()) (character Fold)
            (type' ((element (Literal IntLiteral)) (shape ()))))))
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
       (((binding ((name abc) (id 140)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name abc) (id 145)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 126)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-f-arg) (id 128)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-zero-arg) (id 150)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name fold-array-arg) (id 141)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name abc) (id 140)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 140)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 140)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))
           ((binding ((name fold-array-arg) (id 146)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name abc) (id 145)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 145)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 145)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Fold
            (zeroArg
             ((binding ((name fold-zero-arg) (id 151)))
              (value
               (Ref
                ((id ((name fold-zero-arg) (id 150)))
                 (type' ((element (Literal IntLiteral)) (shape ()))))))))
            (arrayArgs
             (((binding ((name fold-array-arg) (id 142)))
               (value
                (Ref
                 ((id ((name fold-array-arg) (id 141)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))
              ((binding ((name fold-array-arg) (id 147)))
               (value
                (Ref
                 ((id ((name fold-array-arg) (id 146)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name a) (id 143)))
                  (value
                   (Ref
                    ((id ((name fold-array-arg) (id 142)))
                     (type' ((element (Literal IntLiteral)) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name b) (id 148)))
                     (value
                      (Ref
                       ((id ((name fold-array-arg) (id 147)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name f) (id 139)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name +arg1) (id 144)))
                        (value
                         (Ref
                          ((id ((name a) (id 143)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))
                       ((binding ((name +arg2) (id 149)))
                        (value
                         (Ref
                          ((id ((name b) (id 148)))
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
                                 ((id ((name +arg1) (id 144)))
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name +arg2) (id 149)))
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))))
                           (type' (Literal IntLiteral)))))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (d ((const 3) (refs ()))) (cellShape ()) (character Fold)
            (type' ((element (Literal IntLiteral)) (shape ()))))))
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
       (((binding ((name a) (id 117)))
         (value
          (TypeApplication
           ((tFunc (Ref ((id ((name x) (id 115))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))
      (body (Ref ((id ((name x) (id 115)))))) (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 114))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ())))))) |}];
  checkAndPrint {| (length{(Forall (@x) int) | 2 []} [(t-fn (@x) 5) (t-fn (@x) 5)]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 117)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ReifyIndex
        ((index (Dimension ((const 2) (refs ()))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| (scan{int | 2 [] []} + [1 2 3]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 145)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name op) (id 231)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name arr) (id 241)))
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
          (((binding ((name f) (id 191)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name f) (id 232)))
            (value
             (Ref
              ((id ((name op) (id 231)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name init) (id 278)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 269)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 275)))
                  (value
                   (Ref
                    ((id ((name arr) (id 241)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 274)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 276)))
                     (value
                      (Ref
                       ((id ((name arr) (id 275)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 277)))
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
                       ((id ((name contiguous-subarray-array) (id 276)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 277)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 1) (refs ()))))))))))
                     (originalShape ((Add ((const 3) (refs ())))))
                     (resultShape ()) (cellShape ()) (l ((const 1) (refs ())))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name arr) (id 245)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 235)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 242)))
                  (value
                   (Ref
                    ((id ((name arr) (id 241)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 3) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 240)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name contiguous-subarray-array) (id 243)))
                     (value
                      (Ref
                       ((id ((name arr) (id 242)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name contiguous-subarray-index) (id 244)))
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
                       ((id ((name contiguous-subarray-array) (id 243)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ()))))))))))
                     (indexArg
                      (Ref
                       ((id ((name contiguous-subarray-index) (id 244)))
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
             (((binding ((name init) (id 279)))
               (value
                (Ref
                 ((id ((name init) (id 278)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name up-ranked-f) (id 219)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 217)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name scan-f-arg) (id 220)))
                     (value
                      (Ref
                       ((id ((name up-ranked-f) (id 219)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name scan-zero-arg) (id 282)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 254)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 280)))
                           (value
                            (Ref
                             ((id ((name init) (id 279)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name make) (id 264)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 265)))
                                 (value
                                  (Ref
                                   ((id ((name make) (id 264)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name v) (id 281)))
                                 (value
                                  (Ref
                                   ((id ((name v) (id 280)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ()) (args ())
                                 (body
                                  (Ref
                                   ((id ((name v) (id 281)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name scan-array-arg) (id 246)))
                     (value
                      (Ref
                       ((id ((name arr) (id 245)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 2) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Reduce
                     (arg
                      ((firstBinding ((name reduce-arg1) (id 247)))
                       (secondBinding ((name reduce-arg2) (id 250)))
                       (value
                        (Ref
                         ((id ((name scan-array-arg) (id 246)))
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 2) (refs ()))))))))))))
                     (zero
                      (Ref
                       ((id ((name scan-zero-arg) (id 282)))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 233)))
                           (value
                            (Ref
                             ((id ((name f) (id 232)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arg0) (id 248)))
                           (value
                            (Ref
                             ((id ((name reduce-arg1) (id 247)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding ((name arg1) (id 251)))
                           (value
                            (Ref
                             ((id ((name reduce-arg2) (id 250)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name arg0) (id 249)))
                              (value
                               (Ref
                                ((id ((name arg0) (id 248)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding ((name arg1) (id 252)))
                              (value
                               (Ref
                                ((id ((name arg1) (id 251)))
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
                                       ((id ((name arg0) (id 249)))
                                        (type'
                                         ((element (Literal IntLiteral))
                                          (shape ()))))))
                                     (type' (Literal IntLiteral))))
                                   (ArrayAsAtom
                                    ((array
                                      (Ref
                                       ((id ((name arg1) (id 252)))
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
       (((binding ((name f) (id 169)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name arr) (id 250)))
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
        ((binding ((name flags) (id 253)))
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
          (((binding ((name locs-raw) (id 382)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 257)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name op) (id 344)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name zero) (id 374)))
                  (value
                   (AtomAsArray
                    ((element (Literal (IntLiteral 0)))
                     (type' ((element (Literal IntLiteral)) (shape ())))))))
                 ((binding ((name arr) (id 350)))
                  (value
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name f) (id 347)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name boolToIntArg) (id 348)))
                        (value
                         (Ref
                          ((id ((name flags) (id 253)))
                           (type'
                            ((element (Literal BooleanLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ((Add ((const 3) (refs ())))))
                        (args
                         (((binding ((name boolToIntArg) (id 349)))
                           (value
                            (Ref
                             ((id ((name boolToIntArg) (id 348)))
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
                                    ((id ((name boolToIntArg) (id 349)))
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
                   (((binding ((name f) (id 297)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name arr) (id 379)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 304)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name f) (id 345)))
                           (value
                            (Ref
                             ((id ((name op) (id 344)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name init) (id 375)))
                           (value
                            (Ref
                             ((id ((name zero) (id 374)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))
                          ((binding ((name arr) (id 351)))
                           (value
                            (Ref
                             ((id ((name arr) (id 350)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 3) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name up-ranked-f) (id 332)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 330)))
                                 (value
                                  (AtomAsArray
                                   ((element (Values ((elements ()) (type' ()))))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name scan-f-arg) (id 333)))
                                 (value
                                  (Ref
                                   ((id ((name up-ranked-f) (id 332)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name scan-zero-arg) (id 378)))
                                 (value
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name f) (id 360)))
                                       (value
                                        (AtomAsArray
                                         ((element
                                           (Values ((elements ()) (type' ()))))
                                          (type'
                                           ((element (Tuple ())) (shape ())))))))
                                      ((binding ((name v) (id 376)))
                                       (value
                                        (Ref
                                         ((id ((name init) (id 375)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))))
                                    (body
                                     (ArrayPrimitive
                                      (Map (frameShape ())
                                       (args
                                        (((binding ((name make) (id 370)))
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
                                           (((binding ((name f) (id 371)))
                                             (value
                                              (Ref
                                               ((id ((name make) (id 370)))
                                                (type'
                                                 ((element (Tuple ()))
                                                  (shape ())))))))
                                            ((binding ((name v) (id 377)))
                                             (value
                                              (Ref
                                               ((id ((name v) (id 376)))
                                                (type'
                                                 ((element (Literal IntLiteral))
                                                  (shape ())))))))))
                                          (body
                                           (ArrayPrimitive
                                            (Map (frameShape ()) (args ())
                                             (body
                                              (Ref
                                               ((id ((name v) (id 377)))
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
                                ((binding ((name scan-array-arg) (id 352)))
                                 (value
                                  (Ref
                                   ((id ((name arr) (id 351)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))))
                              (body
                               (ArrayPrimitive
                                (Reduce
                                 (arg
                                  ((firstBinding ((name reduce-arg1) (id 353)))
                                   (secondBinding ((name reduce-arg2) (id 356)))
                                   (value
                                    (Ref
                                     ((id ((name scan-array-arg) (id 352)))
                                      (type'
                                       ((element (Literal IntLiteral))
                                        (shape ((Add ((const 3) (refs ()))))))))))))
                                 (zero
                                  (Ref
                                   ((id ((name scan-zero-arg) (id 378)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name f) (id 346)))
                                       (value
                                        (Ref
                                         ((id ((name f) (id 345)))
                                          (type'
                                           ((element (Tuple ())) (shape ())))))))
                                      ((binding ((name arg0) (id 354)))
                                       (value
                                        (Ref
                                         ((id ((name reduce-arg1) (id 353)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))
                                      ((binding ((name arg1) (id 357)))
                                       (value
                                        (Ref
                                         ((id ((name reduce-arg2) (id 356)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))))
                                    (body
                                     (ArrayPrimitive
                                      (Map (frameShape ())
                                       (args
                                        (((binding ((name arg0) (id 355)))
                                          (value
                                           (Ref
                                            ((id ((name arg0) (id 354)))
                                             (type'
                                              ((element (Literal IntLiteral))
                                               (shape ())))))))
                                         ((binding ((name arg1) (id 358)))
                                          (value
                                           (Ref
                                            ((id ((name arg1) (id 357)))
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
                                                   ((id ((name arg0) (id 355)))
                                                    (type'
                                                     ((element
                                                       (Literal IntLiteral))
                                                      (shape ()))))))
                                                 (type' (Literal IntLiteral))))
                                               (ArrayAsAtom
                                                ((array
                                                  (Ref
                                                   ((id ((name arg1) (id 358)))
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
                      (((binding ((name f) (id 302)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name contiguous-subarray-array) (id 380)))
                        (value
                         (Ref
                          ((id ((name arr) (id 379)))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 4) (refs ())))))))))))
                       ((binding ((name contiguous-subarray-index) (id 381)))
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
                          ((id ((name contiguous-subarray-array) (id 380)))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 4) (refs ()))))))))))
                        (indexArg
                         (Ref
                          ((id ((name contiguous-subarray-index) (id 381)))
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
             (((binding ((name locs) (id 404)))
               (value
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 252)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name if-cond) (id 254)))
                     (value
                      (Ref
                       ((id ((name flags) (id 253)))
                        (type'
                         ((element (Literal BooleanLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name then-branch) (id 383)))
                     (value
                      (Ref
                       ((id ((name locs-raw) (id 382)))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 3) (refs ())))))))))))
                    ((binding ((name else-branch) (id 402)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 386)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 400)))
                           (value
                            (AtomAsArray
                             ((element (Literal (IntLiteral -1)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name make) (id 396)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 397)))
                                 (value
                                  (Ref
                                   ((id ((name make) (id 396)))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name v) (id 401)))
                                 (value
                                  (Ref
                                   ((id ((name v) (id 400)))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ())))))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ((Add ((const 3) (refs ())))))
                                 (args ())
                                 (body
                                  (Ref
                                   ((id ((name v) (id 401)))
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
                      (((binding ((name if-cond) (id 255)))
                        (value
                         (Ref
                          ((id ((name if-cond) (id 254)))
                           (type'
                            ((element (Literal BooleanLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))
                       ((binding ((name then-branch) (id 384)))
                        (value
                         (Ref
                          ((id ((name then-branch) (id 383)))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))
                       ((binding ((name else-branch) (id 403)))
                        (value
                         (Ref
                          ((id ((name else-branch) (id 402)))
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
                                 ((id ((name if-cond) (id 255)))
                                  (type'
                                   ((element (Literal BooleanLiteral))
                                    (shape ()))))))
                               (type' (Literal BooleanLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name then-branch) (id 384)))
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name else-branch) (id 403)))
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
                (((binding ((name result-size) (id 418)))
                  (value
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name f) (id 407)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name arr) (id 414)))
                        (value
                         (Ref
                          ((id ((name locs-raw) (id 382)))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 3) (refs ())))))))))))
                       ((binding ((name index) (id 416)))
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
                         (((binding ((name f) (id 413)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name contiguous-subarray-array) (id 415)))
                           (value
                            (Ref
                             ((id ((name arr) (id 414)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 3) (refs ())))))))))))
                          ((binding ((name contiguous-subarray-index) (id 417)))
                           (value
                            (Ref
                             ((id ((name index) (id 416)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 1) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (ContiguousSubArray
                           (arrayArg
                            (Ref
                             ((id ((name contiguous-subarray-array) (id 415)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 3) (refs ()))))))))))
                           (indexArg
                            (Ref
                             ((id ((name contiguous-subarray-index) (id 417)))
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
                   (((binding ((name index-value) (id 419)))
                     (value
                      (Ref
                       ((id ((name result-size) (id 418)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (IndexLet
                    ((indexArgs
                      (((indexBinding ((name d) (id 103)))
                        (indexValue
                         (Runtime
                          (Ref
                           ((id ((name index-value) (id 419)))
                            (type' ((element (Literal IntLiteral)) (shape ())))))))
                        (sort Dim))))
                     (body
                      (AtomAsArray
                       ((element
                         (Box
                          ((indices
                            ((Dimension
                              ((const 0) (refs ((((name d) (id 103)) 1)))))))
                           (body
                            (ArrayPrimitive
                             (Map (frameShape ())
                              (args
                               (((binding ((name f) (id 249)))
                                 (value
                                  (AtomAsArray
                                   ((element (Values ((elements ()) (type' ()))))
                                    (type' ((element (Tuple ())) (shape ())))))))
                                ((binding ((name scatter-values) (id 251)))
                                 (value
                                  (Ref
                                   ((id ((name arr) (id 250)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))
                                ((binding ((name scatter-indices) (id 405)))
                                 (value
                                  (Ref
                                   ((id ((name locs) (id 404)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ())))))))))))))
                              (body
                               (ArrayPrimitive
                                (Scatter
                                 (valuesArg
                                  (Ref
                                   ((id ((name scatter-values) (id 251)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ()))))))))))
                                 (indicesArg
                                  (Ref
                                   ((id ((name scatter-indices) (id 405)))
                                    (type'
                                     ((element (Literal IntLiteral))
                                      (shape ((Add ((const 3) (refs ()))))))))))
                                 (dIn ((const 3) (refs ())))
                                 (dOut
                                  ((const 0) (refs ((((name d) (id 103)) 1)))))
                                 (cellShape ())
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape
                                    ((Add
                                      ((const 0)
                                       (refs ((((name d) (id 103)) 1))))))))))))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape
                                 ((Add
                                   ((const 0) (refs ((((name d) (id 103)) 1))))))))))))
                           (bodyType
                            ((element (Literal IntLiteral))
                             (shape
                              ((Add ((const 0) (refs ((((name d) (id 103)) 1)))))))))
                           (type'
                            ((parameters
                              (((binding ((name d) (id 103))) (bound Dim))))
                             (body
                              ((element (Literal IntLiteral))
                               (shape
                                ((Add
                                  ((const 0) (refs ((((name d) (id 103)) 1))))))))))))))
                        (type'
                         ((element
                           (Sigma
                            ((parameters
                              (((binding ((name d) (id 103))) (bound Dim))))
                             (body
                              ((element (Literal IntLiteral))
                               (shape
                                ((Add
                                  ((const 0) (refs ((((name d) (id 103)) 1))))))))))))
                          (shape ()))))))
                     (type'
                      ((element
                        (Sigma
                         ((parameters
                           (((binding ((name d) (id 103))) (bound Dim))))
                          (body
                           ((element (Literal IntLiteral))
                            (shape
                             ((Add ((const 0) (refs ((((name d) (id 103)) 1))))))))))))
                       (shape ()))))))
                  (type'
                   ((element
                     (Sigma
                      ((parameters (((binding ((name d) (id 103))) (bound Dim))))
                       (body
                        ((element (Literal IntLiteral))
                         (shape
                          ((Add ((const 0) (refs ((((name d) (id 103)) 1))))))))))))
                    (shape ()))))))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name d) (id 103))) (bound Dim))))
                    (body
                     ((element (Literal IntLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name d) (id 103)) 1))))))))))))
                 (shape ()))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name d) (id 103))) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((((name d) (id 103)) 1))))))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding ((name d) (id 103))) (bound Dim))))
              (body
               ((element (Literal IntLiteral))
                (shape ((Add ((const 0) (refs ((((name d) (id 103)) 1))))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding ((name d) (id 103))) (bound Dim))))
           (body
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ((((name d) (id 103)) 1))))))))))))
        (shape ()))))) |}];
  checkAndPrint {| (append{int | 3 2 []} [1 2 3] [4 5]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 114)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name append-arg1) (id 115)))
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
        ((binding ((name append-arg2) (id 116)))
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
           ((id ((name append-arg1) (id 115)))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (arg2
          (Ref
           ((id ((name append-arg2) (id 116)))
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
  [%expect {| Error: Lambda captures variable x.112, which escapes its definition |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ((Add ((const 3) (refs ())))))
      (args
       (((binding ((name index-value) (id 136)))
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
          (((indexBinding ((name i) (id 111)))
            (indexValue
             (Runtime
              (Ref
               ((id ((name index-value) (id 136)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (sort Dim))))
         (body
          (AtomAsArray
           ((element
             (Box
              ((indices
                ((Dimension ((const 0) (refs ((((name i) (id 111)) 1)))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 120)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name v) (id 134)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 5)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name make) (id 130)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 131)))
                           (value
                            (Ref
                             ((id ((name make) (id 130)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 135)))
                           (value
                            (Ref
                             ((id ((name v) (id 134)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map
                           (frameShape
                            ((Add ((const 0) (refs ((((name i) (id 111)) 1)))))))
                           (args ())
                           (body
                            (Ref
                             ((id ((name v) (id 135)))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape
                              ((Add ((const 0) (refs ((((name i) (id 111)) 1))))))))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape
                           ((Add ((const 0) (refs ((((name i) (id 111)) 1))))))))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name i) (id 111)) 1))))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 0) (refs ((((name i) (id 111)) 1))))))))))))
               (bodyType
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 0) (refs ((((name i) (id 111)) 1)))))))))
               (type'
                ((parameters (((binding ((name i) (id 111))) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((((name i) (id 111)) 1))))))))))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name i) (id 111))) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((((name i) (id 111)) 1))))))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding ((name i) (id 111))) (bound Dim))))
              (body
               ((element (Literal IntLiteral))
                (shape ((Add ((const 0) (refs ((((name i) (id 111)) 1))))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding ((name i) (id 111))) (bound Dim))))
           (body
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ((((name i) (id 111)) 1))))))))))))
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
       (((binding ((name index-value) (id 136)))
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
          (((indexBinding ((name @i) (id 111)))
            (indexValue
             (Runtime
              (Ref
               ((id ((name index-value) (id 136)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (sort Shape))))
         (body
          (AtomAsArray
           ((element
             (Box
              ((indices ((Shape ((ShapeRef ((name @i) (id 111)))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 120)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name v) (id 134)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 5)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name make) (id 130)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 131)))
                           (value
                            (Ref
                             ((id ((name make) (id 130)))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name v) (id 135)))
                           (value
                            (Ref
                             ((id ((name v) (id 134)))
                              (type' ((element (Literal IntLiteral)) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ((ShapeRef ((name @i) (id 111)))))
                           (args ())
                           (body
                            (Ref
                             ((id ((name v) (id 135)))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((ShapeRef ((name @i) (id 111))))))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((ShapeRef ((name @i) (id 111))))))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((ShapeRef ((name @i) (id 111))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((ShapeRef ((name @i) (id 111))))))))))
               (bodyType
                ((element (Literal IntLiteral))
                 (shape ((ShapeRef ((name @i) (id 111)))))))
               (type'
                ((parameters (((binding ((name @i) (id 111))) (bound Shape))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((ShapeRef ((name @i) (id 111))))))))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name @i) (id 111))) (bound Shape))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((ShapeRef ((name @i) (id 111))))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding ((name @i) (id 111))) (bound Shape))))
              (body
               ((element (Literal IntLiteral))
                (shape ((ShapeRef ((name @i) (id 111))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding ((name @i) (id 111))) (bound Shape))))
           (body
            ((element (Literal IntLiteral))
             (shape ((ShapeRef ((name @i) (id 111))))))))))
        (shape ()))))) |}]
;;
