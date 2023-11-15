open! Base
open Remora

(* A collection of programs, whose intermediate results after stages are
   print out *)

module PrintResult
    (ShowResult : Show.Basic)
    (Stage : CompilerPipeline.Stage
               with type state = CompilerState.state
               with type output = ShowResult.t
               with type error = (unit option, string) Source.annotate) =
struct
  type input = Stage.input
  type output = Stage.output
  type error = Stage.error
  type state = Stage.state

  let name = [%string "Print %{Stage.name}"]

  let run input =
    CompilerPipeline.S.makeF ~f:(fun state ->
      match CompilerPipeline.S.run (Stage.run input) state with
      | MResult.MOk (_, result) as ok ->
        Stdio.print_endline [%string "Result of stage %{Stage.name}:"];
        Stdio.print_endline ([%sexp_of: ShowResult.t] result |> Sexp.to_string_hum);
        ok
      | MResult.Errors errs ->
        Stdio.prerr_endline [%string "Errors in stage %{Stage.name}:"];
        NeList.iter errs ~f:(fun (err : (unit option, string) Source.annotate) ->
          Stdio.prerr_endline [%string "Error: %{err.elem}"]);
        MResult.Errors errs)
  ;;
end

let printStages input =
  let pipeline =
    CompilerPipeline.(
      (module Parse.Stage (Source.UnitBuilder))
      @> (module PrintResult (Typed) (TypeCheckStage.M (Source.UnitBuilder)))
      @> (module PrintResult (Explicit) (Explicitize.Stage (Source.UnitBuilder)))
      @> (module PrintResult (Nucleus) (Inline.Stage (Source.UnitBuilder)))
      @> (module PrintResult (Nucleus) (Simplify.Stage (Source.UnitBuilder)))
      @> (module PrintResult (Nested) (Nest.Stage (Source.UnitBuilder)))
      @> (module PrintResult (Nested) (Fuse.Stage (Source.UnitBuilder)))
      @> (module PrintResult (Nested) (SimplifyNested.Stage (Source.UnitBuilder)))
      @> (module PrintResult (Corn) (Kernelize.Stage (Source.UnitBuilder)))
      @> empty)
  in
  match
    CompilerPipeline.S.runA (CompilerPipeline.make pipeline input) CompilerState.initial
  with
  | MOk _ -> ()
  | Errors errs ->
    NeList.iter errs ~f:(fun err -> Stdio.prerr_endline [%string "Error: %{err.elem}"])
;;

let%expect_test "simple addition" =
  printStages {|
    (+ 1 2)
  |};
  [%expect
    {|
    Result of stage Type Check:
    (TermApplication
     ((func (Primitive ((name (Func Add)))))
      (args
       ((Scalar ((element (Literal (IntLiteral 1)))))
        (Scalar ((element (Literal (IntLiteral 2)))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name f) (id 43))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 41)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 42)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 43))))))
         (args (((id ((name +arg1) (id 41)))) ((id ((name +arg2) (id 42))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 44)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 45)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 46)))
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
                  ((id ((name +arg1) (id 45)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id ((name +arg2) (id 46)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))))
            (type' (Literal IntLiteral)))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Simplify:
    (AtomAsArray
     ((element (Literal (IntLiteral 3)))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    3
    Result of stage Fuse:
    3
    Result of stage Simplify Nested:
    3
    Result of stage Kernelize:
    3 |}]
;;

let%expect_test "simple function definition and call" =
  printStages {|
    (define (add [x int] [y int]) (+ x y))
    (add 5 10)
  |};
  [%expect
    {|
    Result of stage Type Check:
    (Let
     ((binding ((name add) (id 41)))
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding ((name x) (id 42)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding ((name y) (id 43)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args
                ((Ref ((id ((name x) (id 42))))) (Ref ((id ((name y) (id 43))))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name add) (id 41))))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name add) (id 41)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name x) (id 42)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name y) (id 43)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 46)))
                     (value (Primitive ((name (Func Add))))))
                    ((binding ((name +arg1) (id 44)))
                     (value (Ref ((id ((name x) (id 42)))))))
                    ((binding ((name +arg2) (id 45)))
                     (value (Ref ((id ((name y) (id 43)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 46))))))
                     (args
                      (((id ((name +arg1) (id 44))))
                       ((id ((name +arg2) (id 45))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 49)))
            (value (Ref ((id ((name add) (id 41)))))))
           ((binding ((name x) (id 47)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding ((name y) (id 48)))
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 49))))))
            (args (((id ((name x) (id 47)))) ((id ((name y) (id 48))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name add) (id 51)))
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
              ((id ((name add) (id 51)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 54)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name y) (id 56)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 53)))
               (value
                (AtomAsArray
                 ((element (Values ((elements ()) (type' ()))))
                  (type' ((element (Tuple ())) (shape ())))))))
              ((binding ((name +arg1) (id 55)))
               (value
                (Ref
                 ((id ((name x) (id 54)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name +arg2) (id 57)))
               (value
                (Ref
                 ((id ((name y) (id 56)))
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
                        ((id ((name +arg1) (id 55)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 57)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))))
                  (type' (Literal IntLiteral)))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Simplify:
    (AtomAsArray
     ((element (Literal (IntLiteral 15)))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    15
    Result of stage Fuse:
    15
    Result of stage Simplify Nested:
    15
    Result of stage Kernelize:
    15 |}]
;;

let%expect_test "polymorphic function definition and call" =
  printStages {|
    (define (id{@t | } [e @t]) e)
    (id{int | } 5)
  |};
  [%expect
    {|
    Result of stage Type Check:
    (Let
     ((binding ((name id) (id 41)))
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @t) (id 42))) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name e) (id 43)))
                     (bound (ArrayRef ((name @t) (id 42)))))))
                  (body (Ref ((id ((name e) (id 43))))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id ((name id) (id 41))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name id) (id 41)))
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding ((name @t) (id 42))) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name e) (id 43)))
                        (bound (ArrayRef ((name @t) (id 42)))))))
                     (body (Ref ((id ((name e) (id 43)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 45)))
            (value
             (TypeApplication
              ((tFunc (Ref ((id ((name id) (id 41))))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding ((name e) (id 44)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 45))))))
            (args (((id ((name e) (id 44))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 47)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 48)))
            (value
             (Ref
              ((id ((name id) (id 47)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name e) (id 49)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name e) (id 49)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Simplify:
    (AtomAsArray
     ((element (Literal (IntLiteral 5)))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    5
    Result of stage Fuse:
    5
    Result of stage Simplify Nested:
    5
    Result of stage Kernelize:
    5 |}]
;;

let%expect_test "function call with implicit map" =
  printStages {| (+ [1 2] [[3 4 5] [6 7 8]]) |};
  [%expect
    {|
    Result of stage Type Check:
    (TermApplication
     ((func (Primitive ((name (Func Add)))))
      (args
       ((Frame
         ((dimensions (2))
          (elements
           ((Scalar ((element (Literal (IntLiteral 1)))))
            (Scalar ((element (Literal (IntLiteral 2)))))))))
        (Frame
         ((dimensions (2))
          (elements
           ((Frame
             ((dimensions (3))
              (elements
               ((Scalar ((element (Literal (IntLiteral 3)))))
                (Scalar ((element (Literal (IntLiteral 4)))))
                (Scalar ((element (Literal (IntLiteral 5)))))))))
            (Frame
             ((dimensions (3))
              (elements
               ((Scalar ((element (Literal (IntLiteral 6)))))
                (Scalar ((element (Literal (IntLiteral 7)))))
                (Scalar ((element (Literal (IntLiteral 8)))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name f) (id 43))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 41)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding ((name +arg2) (id 42)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Frame
               ((dimensions (3))
                (elements
                 ((Scalar ((element (Literal (IntLiteral 3)))))
                  (Scalar ((element (Literal (IntLiteral 4)))))
                  (Scalar ((element (Literal (IntLiteral 5)))))))))
              (Frame
               ((dimensions (3))
                (elements
                 ((Scalar ((element (Literal (IntLiteral 6)))))
                  (Scalar ((element (Literal (IntLiteral 7)))))
                  (Scalar ((element (Literal (IntLiteral 8)))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name +arg1) (id 44)))
            (value (Ref ((id ((name +arg1) (id 41)))))))
           ((binding ((name +arg2) (id 45)))
            (value (Ref ((id ((name +arg2) (id 42)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg2) (id 46)))
               (value (Ref ((id ((name +arg2) (id 45)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 43))))))
               (args
                (((id ((name +arg1) (id 44)))) ((id ((name +arg2) (id 46))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (frameShape ((Add ((const 3) (refs ())))))
            (type'
             (Arr
              ((element (Literal IntLiteral))
               (shape ((Add ((const 3) (refs ())))))))))))
         (frameShape ((Add ((const 2) (refs ())))))
         (type'
          (Arr
           ((element (Literal IntLiteral))
            (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal IntLiteral))
         (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ()))))))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 47)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 48)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ())))))))))))
        ((binding ((name +arg2) (id 50)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Frame
               ((dimensions (3))
                (elements
                 ((AtomAsArray
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
                  (shape ((Add ((const 3) (refs ())))))))))
              (Frame
               ((dimensions (3))
                (elements
                 ((AtomAsArray
                   ((element (Literal (IntLiteral 6)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))
                  (AtomAsArray
                   ((element (Literal (IntLiteral 7)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))
                  (AtomAsArray
                   ((element (Literal (IntLiteral 8)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))))
                (type'
                 ((element (Literal IntLiteral))
                  (shape ((Add ((const 3) (refs ())))))))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name +arg1) (id 49)))
            (value
             (Ref
              ((id ((name +arg1) (id 48)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding ((name +arg2) (id 51)))
            (value
             (Ref
              ((id ((name +arg2) (id 50)))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 52)))
               (value
                (Ref
                 ((id ((name +arg2) (id 51)))
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
                        ((id ((name +arg1) (id 49)))
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
             ((element (Literal IntLiteral))
              (shape ((Add ((const 3) (refs ()))))))))))
         (type'
          ((element (Literal IntLiteral))
           (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))
    Result of stage Simplify:
    (ArrayPrimitive
     (Map (frameShape ((Add ((const 2) (refs ())))))
      (args
       (((binding ((name +arg1) (id 49)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ())))))))))))
        ((binding ((name +arg2) (id 51)))
         (value
          (Frame
           ((dimensions (2 3))
            (elements
             ((AtomAsArray
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 4)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 5)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 6)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 7)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (AtomAsArray
               ((element (Literal (IntLiteral 8)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 3) (refs ())))))
         (args
          (((binding ((name +arg2) (id 52)))
            (value
             (Ref
              ((id ((name +arg2) (id 51)))
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
                     ((id ((name +arg1) (id 49)))
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
       ((element (Literal IntLiteral))
        (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))
    Result of stage Nest:
    (let ((+arg1.53 (frame 1 2)) (+arg2.54 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.56 +arg1.53) (+arg2.57 +arg2.54))
         (let ((+arg1.49 +arg1.56) (+arg2.51 +arg2.57))
          (let ((+arg2.58 +arg2.51))
           (#0
            (#0
             (loop-block (frame-shape 3)
              (map ((+arg2.60 +arg2.58))
               (let ((+arg2.52 +arg2.60)) (+ +arg1.49 +arg2.52)))
              (body-matcher map-result.59) (map-result (map-result.59))
              (consumer (values))))))))
        (body-matcher map-result.55) (map-result (map-result.55))
        (consumer (values))))))
    Result of stage Fuse:
    (let ((+arg1.53 (frame 1 2)) (+arg2.54 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.56 +arg1.53) (+arg2.57 +arg2.54))
         (let ((+arg1.49 +arg1.56) (+arg2.51 +arg2.57))
          (let ((+arg2.58 +arg2.51))
           (#0
            (#0
             (loop-block (frame-shape 3)
              (map ((+arg2.60 +arg2.58))
               (let ((+arg2.52 +arg2.60)) (+ +arg1.49 +arg2.52)))
              (body-matcher map-result.59) (map-result (map-result.59))
              (consumer (values))))))))
        (body-matcher map-result.55) (map-result (map-result.55))
        (consumer (values))))))
    Result of stage Simplify Nested:
    (let ((+arg1.53 (frame 1 2)) (+arg2.54 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.56 +arg1.53) (+arg2.57 +arg2.54))
         (let ((+arg2.58 +arg2.57))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.60 +arg2.58)) (+ +arg1.56 +arg2.60))
             (body-matcher map-result.59) (map-result (map-result.59))
             (consumer (values)))))))
        (body-matcher map-result.55) (map-result (map-result.55))
        (consumer (values))))))
    Result of stage Kernelize:
    (let ((+arg1.53 (frame 1 2)) (+arg2.54 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.56 +arg1.53) (+arg2.57 +arg2.54))
         (let ((+arg2.58 +arg2.57))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.60 +arg2.58)) (+ +arg1.56 +arg2.60))
             (body-matcher map-result.59) (map-result (map-result.59))
             (consumer (values)))))))
        (body-matcher map-result.55) (map-result (map-result.55))
        (consumer (values)))))) |}]
;;

let%expect_test "box and unbox" =
  printStages
    {|
    (define words
      (boxes (len) [char len] [2]
        ((3) "hey" )
        ((2) "hi" )))

    (unbox words (word len)
      (= 3 (length{char | len []} word)))
    |};
  [%expect
    {|
    Result of stage Type Check:
    (Let
     ((binding ((name words) (id 41)))
      (value
       (Frame
        ((dimensions (2))
         (elements
          ((Scalar
            ((element
              (Box
               ((indices ((Dimension ((const 3) (refs ())))))
                (body
                 (Frame
                  ((dimensions (3))
                   (elements
                    ((Scalar ((element (Literal (CharacterLiteral h)))))
                     (Scalar ((element (Literal (CharacterLiteral e)))))
                     (Scalar ((element (Literal (CharacterLiteral y))))))))))
                (bodyType
                 (Arr
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 42)) 1)))))))))))))))
           (Scalar
            ((element
              (Box
               ((indices ((Dimension ((const 2) (refs ())))))
                (body
                 (Frame
                  ((dimensions (2))
                   (elements
                    ((Scalar ((element (Literal (CharacterLiteral h)))))
                     (Scalar ((element (Literal (CharacterLiteral i))))))))))
                (bodyType
                 (Arr
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings ((((name len) (id 43)) Dim)))
         (valueBinding ((name word) (id 44)))
         (box (Ref ((id ((name words) (id 41))))))
         (body
          (TermApplication
           ((func (Primitive ((name (Func Equal)))))
            (args
             ((Scalar ((element (Literal (IntLiteral 3)))))
              (TermApplication
               ((func
                 (TypeApplication
                  ((tFunc
                    (IndexApplication
                     ((iFunc
                       (Scalar
                        ((element
                          (IndexLambda
                           ((params
                             (((binding ((name d) (id 0))) (bound Dim))
                              ((binding ((name @cell-shape) (id 1)))
                               (bound Shape))))
                            (body
                             (Scalar
                              ((element
                                (TypeLambda
                                 ((params
                                   (((binding ((name t) (id 2))) (bound Atom))))
                                  (body
                                   (Scalar
                                    ((element
                                      (TermLambda
                                       ((params
                                         (((binding ((name arr) (id 3)))
                                           (bound
                                            (Arr
                                             ((element
                                               (AtomRef ((name t) (id 2))))
                                              (shape
                                               ((Add
                                                 ((const 0)
                                                  (refs ((((name d) (id 0)) 1)))))
                                                (ShapeRef
                                                 ((name @cell-shape) (id 1)))))))))))
                                        (body
                                         (ReifyIndex
                                          ((index
                                            (Dimension
                                             ((const 0)
                                              (refs ((((name d) (id 0)) 1)))))))))))))))))))))))))))
                      (args
                       ((Dimension ((const 0) (refs ((((name len) (id 43)) 1)))))
                        (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id ((name word) (id 44)))))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name words) (id 41)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar
               ((element
                 (Box
                  ((indices ((Dimension ((const 3) (refs ())))))
                   (body
                    (Frame
                     ((dimensions (3))
                      (elements
                       ((Scalar ((element (Literal (CharacterLiteral h)))))
                        (Scalar ((element (Literal (CharacterLiteral e)))))
                        (Scalar ((element (Literal (CharacterLiteral y))))))))))
                   (bodyType
                    (Arr
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 42)) 1)))))))))))))))
              (Scalar
               ((element
                 (Box
                  ((indices ((Dimension ((const 2) (refs ())))))
                   (body
                    (Frame
                     ((dimensions (2))
                      (elements
                       ((Scalar ((element (Literal (CharacterLiteral h)))))
                        (Scalar ((element (Literal (CharacterLiteral i))))))))))
                   (bodyType
                    (Arr
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 42)) 1)))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name box) (id 50)))
            (value (Ref ((id ((name words) (id 41)))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 43)))
               (indexValue
                (FromBox (box (Ref ((id ((name box) (id 50)))))) (i 0)))
               (sort Dim))))
            (body
             (Map
              ((args
                (((binding ((name word) (id 44)))
                  (value (BoxValue ((box (Ref ((id ((name box) (id 50))))))))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 49)))
                     (value (Primitive ((name (Func Equal))))))
                    ((binding ((name =arg1) (id 45)))
                     (value (Scalar ((element (Literal (IntLiteral 3)))))))
                    ((binding ((name =arg2) (id 48)))
                     (value
                      (Map
                       ((args
                         (((binding ((name f) (id 47)))
                           (value
                            (TypeApplication
                             ((tFunc
                               (IndexApplication
                                ((iFunc
                                  (Scalar
                                   ((element
                                     (IndexLambda
                                      ((params
                                        (((binding ((name d) (id 0)))
                                          (bound Dim))
                                         ((binding ((name @cell-shape) (id 1)))
                                          (bound Shape))))
                                       (body
                                        (Scalar
                                         ((element
                                           (TypeLambda
                                            ((params
                                              (((binding ((name t) (id 2)))
                                                (bound Atom))))
                                             (body
                                              (Scalar
                                               ((element
                                                 (TermLambda
                                                  ((params
                                                    (((binding
                                                       ((name arr) (id 3)))
                                                      (bound
                                                       (Arr
                                                        ((element
                                                          (AtomRef
                                                           ((name t) (id 2))))
                                                         (shape
                                                          ((Add
                                                            ((const 0)
                                                             (refs
                                                              ((((name d) (id 0))
                                                                1)))))
                                                           (ShapeRef
                                                            ((name @cell-shape)
                                                             (id 1)))))))))))
                                                   (body
                                                    (ReifyIndex
                                                     ((index
                                                       (Dimension
                                                        ((const 0)
                                                         (refs
                                                          ((((name d) (id 0)) 1)))))))))))))))))))))))))))
                                 (args
                                  ((Dimension
                                    ((const 0) (refs ((((name len) (id 43)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal CharacterLiteral))))))))
                          ((binding ((name arr) (id 46)))
                           (value (Ref ((id ((name word) (id 44)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 47))))))
                           (args (((id ((name arr) (id 46))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 49))))))
                     (args
                      (((id ((name =arg1) (id 45))))
                       ((id ((name =arg2) (id 48))))))
                     (type' ((element (Literal BooleanLiteral)) (shape ()))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal BooleanLiteral)) (shape ())))))))
               (frameShape ())
               (type' (Arr ((element (Literal BooleanLiteral)) (shape ()))))))))))
         (frameShape ((Add ((const 2) (refs ())))))
         (type'
          (Arr
           ((element (Literal BooleanLiteral))
            (shape ((Add ((const 2) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal BooleanLiteral))
         (shape ((Add ((const 2) (refs ()))))))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name words) (id 56)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((AtomAsArray
               ((element
                 (Box
                  ((indices ((Dimension ((const 3) (refs ())))))
                   (body
                    (Frame
                     ((dimensions (3))
                      (elements
                       ((AtomAsArray
                         ((element (Literal (CharacterLiteral h)))
                          (type'
                           ((element (Literal CharacterLiteral)) (shape ())))))
                        (AtomAsArray
                         ((element (Literal (CharacterLiteral e)))
                          (type'
                           ((element (Literal CharacterLiteral)) (shape ())))))
                        (AtomAsArray
                         ((element (Literal (CharacterLiteral y)))
                          (type'
                           ((element (Literal CharacterLiteral)) (shape ())))))))
                      (type'
                       ((element (Literal CharacterLiteral))
                        (shape ((Add ((const 3) (refs ()))))))))))
                   (bodyType
                    ((element (Literal CharacterLiteral))
                     (shape
                      ((Add ((const 0) (refs ((((name len) (id 42)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 42))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 42))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))
                  (shape ())))))
              (AtomAsArray
               ((element
                 (Box
                  ((indices ((Dimension ((const 2) (refs ())))))
                   (body
                    (Frame
                     ((dimensions (2))
                      (elements
                       ((AtomAsArray
                         ((element (Literal (CharacterLiteral h)))
                          (type'
                           ((element (Literal CharacterLiteral)) (shape ())))))
                        (AtomAsArray
                         ((element (Literal (CharacterLiteral i)))
                          (type'
                           ((element (Literal CharacterLiteral)) (shape ())))))))
                      (type'
                       ((element (Literal CharacterLiteral))
                        (shape ((Add ((const 2) (refs ()))))))))))
                   (bodyType
                    ((element (Literal CharacterLiteral))
                     (shape
                      ((Add ((const 0) (refs ((((name len) (id 42)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 42))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 42))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 42))) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name box) (id 57)))
            (value
             (Ref
              ((id ((name words) (id 56)))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 42))) (bound Dim))))
                    (body
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 43)))
               (indexValue
                (FromBox
                 (box
                  (Ref
                   ((id ((name box) (id 57)))
                    (type'
                     ((element
                       (Sigma
                        ((parameters
                          (((binding ((name len) (id 42))) (bound Dim))))
                         (body
                          ((element (Literal CharacterLiteral))
                           (shape
                            ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))
                      (shape ()))))))
                 (i 0)))
               (sort Dim))))
            (body
             (ArrayPrimitive
              (Map (frameShape ()) (args ())
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 51)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name =arg1) (id 52)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 3)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name =arg2) (id 55)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 54)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ReifyIndex
                          ((index
                            (Dimension
                             ((const 0) (refs ((((name len) (id 43)) 1))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))))
                  (body
                   (AtomAsArray
                    ((element
                      (AtomicPrimitive
                       ((op Equal)
                        (args
                         ((ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name =arg1) (id 52)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name =arg2) (id 55)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))))
                        (type' (Literal BooleanLiteral)))))
                     (type' ((element (Literal BooleanLiteral)) (shape ()))))))
                  (type' ((element (Literal BooleanLiteral)) (shape ()))))))
               (type' ((element (Literal BooleanLiteral)) (shape ()))))))
            (type' ((element (Literal BooleanLiteral)) (shape ()))))))
         (type'
          ((element (Literal BooleanLiteral))
           (shape ((Add ((const 2) (refs ()))))))))))
      (type'
       ((element (Literal BooleanLiteral)) (shape ((Add ((const 2) (refs ())))))))))
    Result of stage Simplify:
    (ArrayPrimitive
     (Map (frameShape ((Add ((const 2) (refs ())))))
      (args
       (((binding ((name box) (id 57)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((AtomAsArray
               ((element
                 (Box
                  ((indices ((Dimension ((const 3) (refs ())))))
                   (body
                    (Frame
                     ((dimensions (3))
                      (elements
                       ((AtomAsArray
                         ((element (Literal (CharacterLiteral h)))
                          (type'
                           ((element (Literal CharacterLiteral)) (shape ())))))
                        (AtomAsArray
                         ((element (Literal (CharacterLiteral e)))
                          (type'
                           ((element (Literal CharacterLiteral)) (shape ())))))
                        (AtomAsArray
                         ((element (Literal (CharacterLiteral y)))
                          (type'
                           ((element (Literal CharacterLiteral)) (shape ())))))))
                      (type'
                       ((element (Literal CharacterLiteral))
                        (shape ((Add ((const 3) (refs ()))))))))))
                   (bodyType
                    ((element (Literal CharacterLiteral))
                     (shape
                      ((Add ((const 0) (refs ((((name len) (id 42)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 42))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 42))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))
                  (shape ())))))
              (AtomAsArray
               ((element
                 (Box
                  ((indices ((Dimension ((const 2) (refs ())))))
                   (body
                    (Frame
                     ((dimensions (2))
                      (elements
                       ((AtomAsArray
                         ((element (Literal (CharacterLiteral h)))
                          (type'
                           ((element (Literal CharacterLiteral)) (shape ())))))
                        (AtomAsArray
                         ((element (Literal (CharacterLiteral i)))
                          (type'
                           ((element (Literal CharacterLiteral)) (shape ())))))))
                      (type'
                       ((element (Literal CharacterLiteral))
                        (shape ((Add ((const 2) (refs ()))))))))))
                   (bodyType
                    ((element (Literal CharacterLiteral))
                     (shape
                      ((Add ((const 0) (refs ((((name len) (id 42)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 42))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 42))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 42))) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (IndexLet
        ((indexArgs
          (((indexBinding ((name len) (id 43)))
            (indexValue
             (FromBox
              (box
               (Ref
                ((id ((name box) (id 57)))
                 (type'
                  ((element
                    (Sigma
                     ((parameters (((binding ((name len) (id 42))) (bound Dim))))
                      (body
                       ((element (Literal CharacterLiteral))
                        (shape
                         ((Add ((const 0) (refs ((((name len) (id 42)) 1))))))))))))
                   (shape ()))))))
              (i 0)))
            (sort Dim))))
         (body
          (AtomAsArray
           ((element
             (AtomicPrimitive
              ((op Equal)
               (args
                ((Literal (IntLiteral 3))
                 (ArrayAsAtom
                  ((array
                    (ReifyIndex
                     ((index
                       (Dimension ((const 0) (refs ((((name len) (id 43)) 1))))))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))))
               (type' (Literal BooleanLiteral)))))
            (type' ((element (Literal BooleanLiteral)) (shape ()))))))
         (type' ((element (Literal BooleanLiteral)) (shape ()))))))
      (type'
       ((element (Literal BooleanLiteral)) (shape ((Add ((const 2) (refs ())))))))))
    Result of stage Nest:
    (let
     ((box.58 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.60 box.58))
         (let ((box.57 box.60))
          (index-let ((len.43 box-index-0 box.57)) (= 3 (reify-index len.43)))))
        (body-matcher map-result.59) (map-result (map-result.59))
        (consumer (values))))))
    Result of stage Fuse:
    (let
     ((box.58 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.60 box.58))
         (let ((box.57 box.60))
          (index-let ((len.43 box-index-0 box.57)) (= 3 (reify-index len.43)))))
        (body-matcher map-result.59) (map-result (map-result.59))
        (consumer (values))))))
    Result of stage Simplify Nested:
    (let
     ((box.58 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.60 box.58))
         (index-let ((len.43 box-index-0 box.60)) (= 3 (reify-index len.43))))
        (body-matcher map-result.59) (map-result (map-result.59))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((box.58 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.60 box.58))
         (index-let ((len.43 box-index-0 box.60)) (= 3 (reify-index len.43))))
        (body-matcher map-result.59) (map-result (map-result.59))
        (consumer (values)))))) |}]
;;

let%expect_test "sum rows" =
  printStages
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 [] []} + row))
    (sum-row{ | 9} iota{ | [1000000 10]})
    |};
  [%expect
    {|
    Result of stage Type Check:
    (Let
     ((binding ((name sum-row) (id 41)))
      (value
       (Scalar
        ((element
          (IndexLambda
           ((params (((binding ((name d-1) (id 42))) (bound Dim))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name row) (id 43)))
                     (bound
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape
                         ((Add ((const 1) (refs ((((name d-1) (id 42)) 1)))))))))))))
                  (body
                   (TermApplication
                    ((func
                      (TypeApplication
                       ((tFunc
                         (IndexApplication
                          ((iFunc
                            (Primitive
                             ((name
                               (Func
                                (Reduce (associative true) (explicitZero false)
                                 (character Reduce)))))))
                           (args
                            ((Dimension
                              ((const 0) (refs ((((name d-1) (id 42)) 1)))))
                             (Shape ()) (Shape ()))))))
                        (args ((Atom (Literal IntLiteral)))))))
                     (args
                      ((Primitive ((name (Func Add))))
                       (Ref ((id ((name row) (id 43))))))))))))))))))))))
      (body
       (TermApplication
        ((func
          (IndexApplication
           ((iFunc (Ref ((id ((name sum-row) (id 41))))))
            (args ((Dimension ((const 9) (refs ()))))))))
         (args
          ((IndexApplication
            ((iFunc (Primitive ((name (Val Iota)))))
             (args
              ((Shape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ()))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name sum-row) (id 41)))
         (value
          (Scalar
           ((element
             (IndexLambda
              ((params (((binding ((name d-1) (id 42))) (bound Dim))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name row) (id 43)))
                        (bound
                         (Arr
                          ((element (Literal IntLiteral))
                           (shape
                            ((Add ((const 1) (refs ((((name d-1) (id 42)) 1)))))))))))))
                     (body
                      (Map
                       ((args
                         (((binding ((name f) (id 46)))
                           (value
                            (TypeApplication
                             ((tFunc
                               (IndexApplication
                                ((iFunc
                                  (Primitive
                                   ((name
                                     (Func
                                      (Reduce (associative true)
                                       (explicitZero false) (character Reduce)))))))
                                 (args
                                  ((Dimension
                                    ((const 0) (refs ((((name d-1) (id 42)) 1)))))
                                   (Shape ()) (Shape ()))))))
                              (args ((Atom (Literal IntLiteral))))))))
                          ((binding ((name reduce-f-arg) (id 44)))
                           (value (Primitive ((name (Func Add))))))
                          ((binding ((name reduce-array-arg) (id 45)))
                           (value (Ref ((id ((name row) (id 43)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 46))))))
                           (args
                            (((id ((name reduce-f-arg) (id 44))))
                             ((id ((name reduce-array-arg) (id 45))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 48)))
            (value
             (IndexApplication
              ((iFunc (Ref ((id ((name sum-row) (id 41))))))
               (args ((Dimension ((const 9) (refs ())))))))))
           ((binding ((name row) (id 47)))
            (value
             (IndexApplication
              ((iFunc (Primitive ((name (Val Iota)))))
               (args
                ((Shape
                  ((Add ((const 1000000) (refs ())))
                   (Add ((const 10) (refs ())))))))))))))
         (body
          (Map
           ((args
             (((binding ((name row) (id 49)))
               (value (Ref ((id ((name row) (id 47)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 48))))))
               (args (((id ((name row) (id 49))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (frameShape ((Add ((const 1000000) (refs ())))))
            (type'
             (Arr
              ((element (Literal IntLiteral))
               (shape ((Add ((const 1000000) (refs ())))))))))))
         (frameShape ())
         (type'
          (Arr
           ((element (Literal IntLiteral))
            (shape ((Add ((const 1000000) (refs ())))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal IntLiteral))
         (shape ((Add ((const 1000000) (refs ()))))))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name sum-row) (id 51)))
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
              ((id ((name sum-row) (id 51)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name row) (id 58)))
            (value
             (ArrayPrimitive
              (Map
               (frameShape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
               (args ()) (iotaVar (((name iota) (id 57))))
               (body
                (Ref
                 ((id ((name iota) (id 57)))
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 1000000) (refs ())))
                   (Add ((const 10) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 1000000) (refs ())))))
            (args
             (((binding ((name row) (id 59)))
               (value
                (Ref
                 ((id ((name row) (id 58)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape
                     ((Add ((const 1000000) (refs ())))
                      (Add ((const 10) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 53)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name reduce-f-arg) (id 54)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name reduce-array-arg) (id 60)))
                  (value
                   (Ref
                    ((id ((name row) (id 59)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 10) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Reduce
                  (arg
                   ((firstBinding ((name reduce-arg1) (id 61)))
                    (secondBinding ((name reduce-arg2) (id 62)))
                    (value
                     (Ref
                      ((id ((name reduce-array-arg) (id 60)))
                       (type'
                        ((element (Literal IntLiteral))
                         (shape ((Add ((const 10) (refs ()))))))))))))
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
                              ((id ((name reduce-arg1) (id 61)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name reduce-arg2) (id 62)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))))
                        (type' (Literal IntLiteral)))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (d ((const 10) (refs ()))) (itemPad ()) (cellShape ())
                  (associative true) (character Reduce)
                  (type' ((element (Literal IntLiteral)) (shape ()))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 1000000) (refs ()))))))))))
         (type'
          ((element (Literal IntLiteral))
           (shape ((Add ((const 1000000) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 1000000) (refs ())))))))))
    Result of stage Simplify:
    (ArrayPrimitive
     (Map (frameShape ((Add ((const 1000000) (refs ())))))
      (args
       (((binding ((name row) (id 59)))
         (value
          (ArrayPrimitive
           (Map
            (frameShape
             ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
            (args ()) (iotaVar (((name iota) (id 57))))
            (body
             (Ref
              ((id ((name iota) (id 57)))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type'
             ((element (Literal IntLiteral))
              (shape
               ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Reduce
         (arg
          ((firstBinding ((name reduce-arg1) (id 61)))
           (secondBinding ((name reduce-arg2) (id 62)))
           (value
            (Ref
             ((id ((name row) (id 59)))
              (type'
               ((element (Literal IntLiteral))
                (shape ((Add ((const 10) (refs ()))))))))))))
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
                     ((id ((name reduce-arg1) (id 61)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))
                 (ArrayAsAtom
                  ((array
                    (Ref
                     ((id ((name reduce-arg2) (id 62)))
                      (type' ((element (Literal IntLiteral)) (shape ()))))))
                   (type' (Literal IntLiteral))))))
               (type' (Literal IntLiteral)))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (d ((const 10) (refs ()))) (itemPad ()) (cellShape ())
         (associative true) (character Reduce)
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 1000000) (refs ())))))))))
    Result of stage Nest:
    (let
     ((row.67
       (let ()
        (#0
         (#0
          (loop-block (frame-shape 1000000)
           (map () (iota iota.64)
            (#0
             (#0
              (loop-block (frame-shape 10)
               (map () (iota (iota.66 : iota.64))
                (let ((iota.57 iota.66)) iota.57))
               (body-matcher map-result.65) (map-result (map-result.65))
               (consumer (values))))))
           (body-matcher map-result.63) (map-result (map-result.63))
           (consumer (values))))))))
     (#0
      (#0
       (loop-block (frame-shape 1000000)
        (map ((row.69 row.67))
         (let ((row.59 row.69))
          (let ((reduce-arg.71 row.59))
           (#1
            (loop-block (frame-shape 10)
             (map ((reduce-arg.72 reduce-arg.71)) (values reduce-arg.72))
             (body-matcher (reduce-arg.70)) (map-result ())
             (consumer
              (reduce (shape) (reduce-arg1.61 reduce-arg2.62 reduce-arg.70)
               (+ reduce-arg1.61 reduce-arg2.62))))))))
        (body-matcher map-result.68) (map-result (map-result.68))
        (consumer (values))))))
    Result of stage Fuse:
    (let ()
     (let ()
      (let
       ((fused-block-result.75
         (loop-block (frame-shape 1000000)
          (map () (iota iota.64)
           (let ()
            (let ()
             (let ()
              (let ()
               (let
                ((fused-block-result.80
                  (loop-block (frame-shape 10)
                   (map () (iota (iota.66 : iota.64))
                    (let
                     ((fusion-target-map-result.78
                       (let ((iota.57 iota.66)) iota.57)))
                     (values fusion-target-map-result.78
                      (let ((reduce-arg.72 fusion-target-map-result.78))
                       (values reduce-arg.72)))))
                   (body-matcher (map-result.65 (reduce-arg.70)))
                   (map-result (map-result.65))
                   (consumer
                    (reduce (shape) (reduce-arg1.61 reduce-arg2.62 reduce-arg.70)
                     (+ reduce-arg1.61 reduce-arg2.62))))))
                (let
                 ((fusion-target-map-result.81
                   (values (#0 (#0 fused-block-result.80))))
                  (fusion-archer-map-result.77 (values))
                  (fusion-archer-consumer-result.79 (#1 fused-block-result.80)))
                 (let
                  ((fusion-target-map-result.74
                    (#0 (#0 (values fusion-target-map-result.81 (values))))))
                  (values fusion-target-map-result.74
                   (let ((row.69 fusion-target-map-result.74))
                    (let ((row.59 row.69))
                     (let ((reduce-arg.71 row.59))
                      (#1
                       (values fusion-archer-map-result.77
                        fusion-archer-consumer-result.79))))))))))))))
          (body-matcher (map-result.63 map-result.68))
          (map-result (map-result.63 map-result.68)) (consumer (values)))))
       (let
        ((fusion-target-map-result.76 (values (#0 (#0 fused-block-result.75))))
         (fusion-archer-map-result.73 (values (#1 (#0 fused-block-result.75)))))
        (let ((row.67 (#0 (#0 (values fusion-target-map-result.76 (values))))))
         (#0 (#0 (values fusion-archer-map-result.73 (values)))))))))
    Result of stage Simplify Nested:
    (#0
     (#0
      (loop-block (frame-shape 1000000)
       (map () (iota iota.64)
        (#1
         (loop-block (frame-shape 10) (map () (iota (iota.66 : iota.64)) iota.66)
          (body-matcher reduce-arg.70) (map-result ())
          (consumer
           (reduce (shape) (reduce-arg1.61 reduce-arg2.62 reduce-arg.70)
            (+ reduce-arg1.61 reduce-arg2.62))))))
       (body-matcher map-result.68) (map-result (map-result.68))
       (consumer (values)))))
    Result of stage Kernelize:
    (#0
     (map-kernel (frame-shape 1000000) () (iota iota.64)
      (body-matcher map-result.68) (map-result (map-result.68))
      (#1
       (loop-block (frame-shape 10) (map () (iota (iota.66 : iota.64)) iota.66)
        (body-matcher reduce-arg.70) (map-result ())
        (consumer
         (reduce (shape) (reduce-arg1.61 reduce-arg2.62 reduce-arg.70)
          (+ reduce-arg1.61 reduce-arg2.62))))))) |}]
;;
