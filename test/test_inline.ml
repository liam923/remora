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
       (((binding ((name f) (id 48)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 49)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 50)))
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
                  ((id ((name +arg1) (id 49)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id ((name +arg2) (id 50)))
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
       (((binding ((name f) (id 49)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 50)))
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
        ((binding ((name +arg2) (id 52)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 4)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 3) (refs ())))))
         (args
          (((binding ((name +arg1) (id 51)))
            (value
             (Ref
              ((id ((name +arg1) (id 50)))
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
                     ((id ((name +arg1) (id 51)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name +arg2) (id 52)))
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
       (((binding ((name id) (id 51)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 52)))
            (value
             (Ref
              ((id ((name id) (id 51)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 53)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name x) (id 53)))
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
       (((binding ((name id) (id 54)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name id) (id 52)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 53)))
            (value
             (Ref
              ((id ((name id) (id 52)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 55)))
            (value
             (Ref
              ((id ((name id) (id 54)))
               (type' ((element (Tuple ())) (shape ())))))))))
         (body
          (Ref
           ((id ((name x) (id 55))) (type' ((element (Tuple ())) (shape ()))))))
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
       (((binding ((name id) (id 54)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name id) (id 57)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 59)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 55)))
                  (value
                   (Ref
                    ((id ((name id) (id 54)))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name x) (id 58)))
                  (value
                   (Ref
                    ((id ((name id) (id 57)))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (Ref
                 ((id ((name x) (id 58)))
                  (type' ((element (Tuple ())) (shape ()))))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name arg0) (id 60)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name arg0) (id 60)))
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
       (((binding ((name f) (id 51)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name x) (id 52)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 10)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (Ref
        ((id ((name x) (id 52)))
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
       (((binding ((name f) (id 48)))
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
       (((binding ((name f) (id 48)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name reduce-f-arg) (id 49)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name reduce-array-arg) (id 52)))
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
        (Reduce
         (arg
          ((firstBinding ((name reduce-arg1) (id 53)))
           (secondBinding ((name reduce-arg2) (id 54)))
           (value
            (Ref
             ((id ((name reduce-array-arg) (id 52)))
              (type'
               ((element (Literal IntLiteral))
                (shape ((Add ((const 5) (refs ()))))))))))))
         (zero ())
         (body
          (AtomAsArray
           ((element
             (AtomicPrimitive
              ((op Add)
               (args
                ((ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name reduce-arg1) (id 53)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name reduce-arg2) (id 54)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))))
               (type' (Literal IntLiteral)))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (d ((const 5) (refs ()))) (itemPad ()) (cellShape ()) (associative true)
         (character Reduce) (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| ([+ -] 1 2) |};
  [%expect
    {|
    Error: Could not determine what function is being called in function call:
    ((func (Ref ((id ((name f) (id 48))))))
     (args (((id ((name +arg1) (id 45)))) ((id ((name +arg2) (id 46))))))
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
       (((binding ((name id) (id 52)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 53)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name id) (id 52)))
                   (type' ((element (Tuple ())) (shape ())))))
                 (Ref
                  ((id ((name id) (id 52)))
                   (type' ((element (Tuple ())) (shape ())))))
                 (Ref
                  ((id ((name id) (id 52)))
                   (type' ((element (Tuple ())) (shape ())))))))
               (type'
                ((element (Tuple ())) (shape ((Add ((const 3) (refs ())))))))))))
           ((binding ((name x) (id 55)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name f) (id 54)))
               (value
                (Ref
                 ((id ((name f) (id 53)))
                  (type'
                   ((element (Tuple ())) (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (Ref
              ((id ((name x) (id 55)))
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
          (((binding ((name f) (id 55)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name +arg1) (id 58)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 57)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))))
               (body
                (ReifyIndex
                 ((index (Dimension ((const 5) (refs ()))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name +arg2) (id 61)))
            (value
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 60)))
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
                     ((id ((name +arg1) (id 58)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name +arg2) (id 61)))
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
       (((binding ((name sum) (id 50)))
         (value
          (Map
           ((args
             (((binding ((name f) (id 56)))
               (value (Primitive ((name (Func Add))))))
              ((binding ((name +arg1) (id 54)))
               (value
                (TypeApplication
                 ((tFunc (Ref ((id ((name a) (id 48))))))
                  (args
                   ((Array
                     (Arr ((element (Literal CharacterLiteral)) (shape ()))))))))))
              ((binding ((name +arg2) (id 55)))
               (value
                (TypeApplication
                 ((tFunc (Ref ((id ((name b) (id 49))))))
                  (args
                   ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 56))))))
               (args
                (((id ((name +arg1) (id 54)))) ((id ((name +arg2) (id 55))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (frameShape ())
            (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
      (body
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @u) (id 51))) (bound Array))))
            (body (Ref ((id ((name sum) (id 50))))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @u) (id 51))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ())))))) |}];
  checkAndPrint {| (reduce{int | 2 [] []} [+ -] [1 2 3]) |};
  [%expect
    {|
    Error: Could not determine what function is being passed to reduce:
    ((func (Ref ((id ((name f) (id 47))))))
     (args
      (((id ((name reduce-f-arg) (id 48))))
       ((id ((name reduce-array-arg) (id 46))))))
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
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name abc) (id 61)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 55)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name reduce-f-arg) (id 57)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name reduce-array-arg) (id 62)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name abc) (id 61)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 61)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 61)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Reduce
            (arg
             ((firstBinding ((name reduce-arg1) (id 63)))
              (secondBinding ((name reduce-arg2) (id 64)))
              (value
               (Ref
                ((id ((name reduce-array-arg) (id 62)))
                 (type'
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 3) (refs ()))))))))))))
            (zero ())
            (body
             (Ref
              ((id ((name reduce-arg1) (id 63)))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (d ((const 3) (refs ()))) (itemPad ()) (cellShape ())
            (associative true) (character Reduce)
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
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
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name abc) (id 61)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 55)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name reduce-f-arg) (id 57)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name reduce-array-arg) (id 62)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name abc) (id 61)))
                   (type' ((element (Tuple ())) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 61)))
                   (type' ((element (Tuple ())) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 61)))
                   (type' ((element (Tuple ())) (shape ())))))))
               (type'
                ((element (Tuple ())) (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Reduce
            (arg
             ((firstBinding ((name reduce-arg1) (id 63)))
              (secondBinding ((name reduce-arg2) (id 64)))
              (value
               (Ref
                ((id ((name reduce-array-arg) (id 62)))
                 (type'
                  ((element (Tuple ())) (shape ((Add ((const 3) (refs ()))))))))))))
            (zero ())
            (body
             (Ref
              ((id ((name reduce-arg1) (id 63)))
               (type' ((element (Tuple ())) (shape ()))))))
            (d ((const 3) (refs ()))) (itemPad ()) (cellShape ())
            (associative true) (character Reduce)
            (type' ((element (Tuple ())) (shape ()))))))
         (type' ((element (Tuple ())) (shape ()))))))
      (type' ((element (Tuple ())) (shape ()))))) |}];
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
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name abc) (id 61)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 55)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name reduce-f-arg) (id 57)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name reduce-array-arg) (id 62)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name abc) (id 61)))
                   (type' ((element (Tuple ())) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 61)))
                   (type' ((element (Tuple ())) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 61)))
                   (type' ((element (Tuple ())) (shape ())))))))
               (type'
                ((element (Tuple ())) (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Reduce
            (arg
             ((firstBinding ((name reduce-arg1) (id 63)))
              (secondBinding ((name reduce-arg2) (id 64)))
              (value
               (Ref
                ((id ((name reduce-array-arg) (id 62)))
                 (type'
                  ((element (Tuple ())) (shape ((Add ((const 3) (refs ()))))))))))))
            (zero ())
            (body
             (Ref
              ((id ((name abc) (id 61)))
               (type' ((element (Tuple ())) (shape ()))))))
            (d ((const 3) (refs ()))) (itemPad ()) (cellShape ())
            (associative true) (character Reduce)
            (type' ((element (Tuple ())) (shape ()))))))
         (type' ((element (Tuple ())) (shape ()))))))
      (type' ((element (Tuple ())) (shape ()))))) |}];
  checkAndPrint {| (reduce{int | 2 [] []} + [1 2 3]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 48)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name reduce-f-arg) (id 49)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name reduce-array-arg) (id 52)))
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
        (Reduce
         (arg
          ((firstBinding ((name reduce-arg1) (id 53)))
           (secondBinding ((name reduce-arg2) (id 54)))
           (value
            (Ref
             ((id ((name reduce-array-arg) (id 52)))
              (type'
               ((element (Literal IntLiteral))
                (shape ((Add ((const 3) (refs ()))))))))))))
         (zero ())
         (body
          (AtomAsArray
           ((element
             (AtomicPrimitive
              ((op Add)
               (args
                ((ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name reduce-arg1) (id 53)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name reduce-arg2) (id 54)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))))
               (type' (Literal IntLiteral)))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (d ((const 3) (refs ()))) (itemPad ()) (cellShape ()) (associative true)
         (character Reduce) (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint {| (fold{int int | 5 [] []} + 0 [1 2 3 4 5]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 49)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name fold-f-arg) (id 50)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name fold-zero-arg) (id 53)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 0)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name fold-array-arg) (id 55)))
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
          ((binding ((name fold-zero-arg) (id 54)))
           (value
            (Ref
             ((id ((name fold-zero-arg) (id 53)))
              (type' ((element (Literal IntLiteral)) (shape ()))))))))
         (arrayArgs
          (((binding ((name fold-array-arg) (id 56)))
            (value
             (Ref
              ((id ((name fold-array-arg) (id 55)))
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
                     ((id ((name fold-zero-arg) (id 54)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name fold-array-arg) (id 56)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))))
               (type' (Literal IntLiteral)))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (d ((const 5) (refs ()))) (itemPad ()) (cellShape ()) (character Fold)
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
    (define abc (t-fn (@t) 1))
    (fold{int (Forall (@t) int) | 3 [] []}
      (fn ([x (Forall (@t) int)] [y int])
        x)
        abc [1 2 3]){int | }
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name abc) (id 61)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 55)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-f-arg) (id 57)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-zero-arg) (id 62)))
            (value
             (Ref
              ((id ((name abc) (id 61)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Fold
            (zeroArg
             ((binding ((name fold-zero-arg) (id 63)))
              (value
               (Ref
                ((id ((name fold-zero-arg) (id 62)))
                 (type' ((element (Literal IntLiteral)) (shape ()))))))))
            (arrayArgs ())
            (body
             (Ref
              ((id ((name fold-zero-arg) (id 63)))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (d ((const 3) (refs ()))) (itemPad ()) (cellShape ())
            (character Fold) (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
    (define abc (t-fn (@t) 1))
    (fold{(Forall (@t) int) int | 3 [] []}
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
       (((binding ((name abc) (id 67)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name abc) (id 72)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 60)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-f-arg) (id 62)))
            (value
             (AtomAsArray
              ((element (Values ((elements ()) (type' ()))))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name fold-zero-arg) (id 77)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name fold-array-arg) (id 68)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name abc) (id 67)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 67)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 67)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))
           ((binding ((name fold-array-arg) (id 73)))
            (value
             (Frame
              ((dimensions (3))
               (elements
                ((Ref
                  ((id ((name abc) (id 72)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 72)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name abc) (id 72)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Fold
            (zeroArg
             ((binding ((name fold-zero-arg) (id 78)))
              (value
               (Ref
                ((id ((name fold-zero-arg) (id 77)))
                 (type' ((element (Literal IntLiteral)) (shape ()))))))))
            (arrayArgs
             (((binding ((name fold-array-arg) (id 69)))
               (value
                (Ref
                 ((id ((name fold-array-arg) (id 68)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))
              ((binding ((name fold-array-arg) (id 74)))
               (value
                (Ref
                 ((id ((name fold-array-arg) (id 73)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name a) (id 70)))
                  (value
                   (Ref
                    ((id ((name fold-array-arg) (id 69)))
                     (type' ((element (Literal IntLiteral)) (shape ())))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name b) (id 75)))
                     (value
                      (Ref
                       ((id ((name fold-array-arg) (id 74)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name f) (id 66)))
                        (value
                         (AtomAsArray
                          ((element (Values ((elements ()) (type' ()))))
                           (type' ((element (Tuple ())) (shape ())))))))
                       ((binding ((name +arg1) (id 71)))
                        (value
                         (Ref
                          ((id ((name a) (id 70)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))
                       ((binding ((name +arg2) (id 76)))
                        (value
                         (Ref
                          ((id ((name b) (id 75)))
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
                                 ((id ((name +arg1) (id 71)))
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name +arg2) (id 76)))
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))))
                           (type' (Literal IntLiteral)))))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (d ((const 3) (refs ()))) (itemPad ()) (cellShape ())
            (character Fold) (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}];
  checkAndPrint
    {|
    (define abc (t-fn (@t) 1))
    (fold{int (Forall (@t) int) | 3 [] []}
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
       (((binding ((name a) (id 51)))
         (value
          (TypeApplication
           ((tFunc (Ref ((id ((name x) (id 49))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))
      (body (Ref ((id ((name x) (id 49)))))) (frameShape ())
      (type'
       (Arr
        ((element
          (Forall
           ((parameters (((binding ((name @t) (id 48))) (bound Array))))
            (body (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (shape ())))))) |}];
  checkAndPrint {| (length{(Forall (@x) int) | 2 []} [(t-fn (@x) 5) (t-fn (@x) 5)]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 51)))
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
       (((binding ((name f) (id 48)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name reduce-f-arg) (id 49)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name reduce-array-arg) (id 52)))
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
        (Reduce
         (arg
          ((firstBinding ((name reduce-arg1) (id 53)))
           (secondBinding ((name reduce-arg2) (id 54)))
           (value
            (Ref
             ((id ((name reduce-array-arg) (id 52)))
              (type'
               ((element (Literal IntLiteral))
                (shape ((Add ((const 3) (refs ()))))))))))))
         (zero ())
         (body
          (AtomAsArray
           ((element
             (AtomicPrimitive
              ((op Add)
               (args
                ((ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name reduce-arg1) (id 53)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name reduce-arg2) (id 54)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))))
               (type' (Literal IntLiteral)))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (d ((const 3) (refs ()))) (itemPad ()) (cellShape ()) (associative true)
         (character Scan)
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))) |}];
  (* checkAndPrint {| (filter{int | 3 []} [1 2 3] [#t #t #f]) |};
  [%expect
    {|
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 33)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name filter-array) (id 34)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))
        ((binding ((name filter-flags) (id 35)))
         (value
          (Frame
           ((dimensions (3))
            (elements
             ((Scalar
               ((element (Literal (BooleanLiteral true)))
                (type' ((element (Literal BooleanLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (BooleanLiteral true)))
                (type' ((element (Literal BooleanLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (BooleanLiteral false)))
                (type' ((element (Literal BooleanLiteral)) (shape ())))))))
            (type'
             ((element (Literal BooleanLiteral))
              (shape ((Add ((const 3) (refs ())))))))))))))
      (body
       (IntrinsicCall
        (Filter
         (array
          (Ref
           ((id ((name filter-array) (id 34)))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (flags
          (Ref
           ((id ((name filter-flags) (id 35)))
            (type'
             ((element (Literal BooleanLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (d ((const 3) (refs ()))) (cellShape ())
         (type'
          ((element
            (Sigma
             ((parameters (((binding ((name d-out) (id 20))) (bound Dim))))
              (body
               ((element (Literal IntLiteral))
                (shape ((Add ((const 0) (refs ()))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding ((name d-out) (id 20))) (bound Dim))))
           (body
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ()))))))))))
        (shape ()))))) |}]; *)
  checkAndPrint {| (append{int | 3 2 []} [1 2 3] [4 5]) |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 48)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name append-arg1) (id 49)))
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
        ((binding ((name append-arg2) (id 50)))
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
           ((id ((name append-arg1) (id 49)))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (arg2
          (Ref
           ((id ((name append-arg2) (id 50)))
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
  [%expect {| Error: Lambda captures variable x, which escapes its definition |}];
  checkAndPrint {|
    (lift [i [1 2 3]]
      (replicate{int | [i] []} 5))
    |};
  [%expect
    {|
    (ArrayPrimitive
     (Map (frameShape ((Add ((const 3) (refs ())))))
      (args
       (((binding ((name index-value) (id 52)))
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
          (((indexBinding ((name i) (id 45)))
            (indexValue
             (Runtime
              (Ref
               ((id ((name index-value) (id 52)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (sort Dim))))
         (body
          (AtomAsArray
           ((element
             (Box
              ((indices
                ((Dimension ((const 0) (refs ((((name i) (id 45)) 1)))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 49)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name replicate-value) (id 50)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 5)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name replicated-value) (id 51)))
                        (value
                         (Ref
                          ((id ((name replicate-value) (id 50)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map
                        (frameShape
                         ((Add ((const 0) (refs ((((name i) (id 45)) 1)))))))
                        (args ())
                        (body
                         (Ref
                          ((id ((name replicated-value) (id 51)))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape
                           ((Add ((const 0) (refs ((((name i) (id 45)) 1))))))))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name i) (id 45)) 1))))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 0) (refs ((((name i) (id 45)) 1))))))))))))
               (bodyType
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 0) (refs ((((name i) (id 45)) 1)))))))))
               (type'
                ((parameters (((binding ((name i) (id 45))) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((((name i) (id 45)) 1))))))))))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name i) (id 45))) (bound Dim))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((Add ((const 0) (refs ((((name i) (id 45)) 1))))))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding ((name i) (id 45))) (bound Dim))))
              (body
               ((element (Literal IntLiteral))
                (shape ((Add ((const 0) (refs ((((name i) (id 45)) 1))))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding ((name i) (id 45))) (bound Dim))))
           (body
            ((element (Literal IntLiteral))
             (shape ((Add ((const 0) (refs ((((name i) (id 45)) 1))))))))))))
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
       (((binding ((name index-value) (id 52)))
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
          (((indexBinding ((name @i) (id 45)))
            (indexValue
             (Runtime
              (Ref
               ((id ((name index-value) (id 52)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (sort Shape))))
         (body
          (AtomAsArray
           ((element
             (Box
              ((indices ((Shape ((ShapeRef ((name @i) (id 45)))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 49)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name replicate-value) (id 50)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 5)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name replicated-value) (id 51)))
                        (value
                         (Ref
                          ((id ((name replicate-value) (id 50)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ((ShapeRef ((name @i) (id 45)))))
                        (args ())
                        (body
                         (Ref
                          ((id ((name replicated-value) (id 51)))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((ShapeRef ((name @i) (id 45))))))))))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((ShapeRef ((name @i) (id 45))))))))))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((ShapeRef ((name @i) (id 45))))))))))
               (bodyType
                ((element (Literal IntLiteral))
                 (shape ((ShapeRef ((name @i) (id 45)))))))
               (type'
                ((parameters (((binding ((name @i) (id 45))) (bound Shape))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((ShapeRef ((name @i) (id 45))))))))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name @i) (id 45))) (bound Shape))))
                 (body
                  ((element (Literal IntLiteral))
                   (shape ((ShapeRef ((name @i) (id 45))))))))))
              (shape ()))))))
         (type'
          ((element
            (Sigma
             ((parameters (((binding ((name @i) (id 45))) (bound Shape))))
              (body
               ((element (Literal IntLiteral))
                (shape ((ShapeRef ((name @i) (id 45))))))))))
           (shape ()))))))
      (type'
       ((element
         (Sigma
          ((parameters (((binding ((name @i) (id 45))) (bound Shape))))
           (body
            ((element (Literal IntLiteral))
             (shape ((ShapeRef ((name @i) (id 45))))))))))
        (shape ()))))) |}]
;;
