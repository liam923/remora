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
      @> (module PrintResult (Nested) (Nest.Stage (Source.UnitBuilder)))
      @> (module PrintResult (Nested) (FuseAndSimplify.Stage (Source.UnitBuilder)))
      @> (module PrintResult (Corn) (Kernelize.Stage (Source.UnitBuilder)))
      @> (module PrintResult (Acorn.SansCaptures) (Alloc.Stage (Source.UnitBuilder)))
      @> (module PrintResult (Acorn.WithCaptures) (Capture.Stage (Source.UnitBuilder)))
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
       (((binding ((name f) (id 64))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 62)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 63)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 64))))))
         (args (((id ((name +arg1) (id 62)))) ((id ((name +arg2) (id 63))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 65)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 66)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 67)))
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
                  ((id ((name +arg1) (id 66)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id ((name +arg2) (id 67)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))))
            (type' (Literal IntLiteral)))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((f.68 (values)) (+arg1.69 1) (+arg2.70 2))
     (let ((f.65 f.68) (+arg1.66 +arg1.69) (+arg2.67 +arg2.70))
      (+ +arg1.66 +arg2.67)))
    Result of stage Fuse and Simplify:
    3
    Result of stage Kernelize:
    3
    Result of stage Alloc:
    3
    Result of stage Capture:
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
     ((binding ((name add) (id 62)))
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding ((name x) (id 63)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding ((name y) (id 64)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args
                ((Ref ((id ((name x) (id 63))))) (Ref ((id ((name y) (id 64))))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name add) (id 62))))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name add) (id 62)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name x) (id 63)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name y) (id 64)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 67)))
                     (value (Primitive ((name (Func Add))))))
                    ((binding ((name +arg1) (id 65)))
                     (value (Ref ((id ((name x) (id 63)))))))
                    ((binding ((name +arg2) (id 66)))
                     (value (Ref ((id ((name y) (id 64)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 67))))))
                     (args
                      (((id ((name +arg1) (id 65))))
                       ((id ((name +arg2) (id 66))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 70)))
            (value (Ref ((id ((name add) (id 62)))))))
           ((binding ((name x) (id 68)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding ((name y) (id 69)))
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 70))))))
            (args (((id ((name x) (id 68)))) ((id ((name y) (id 69))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name add) (id 72)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 73)))
            (value
             (Ref
              ((id ((name add) (id 72)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 75)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name y) (id 77)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 74)))
               (value
                (AtomAsArray
                 ((element (Values ((elements ()) (type' ()))))
                  (type' ((element (Tuple ())) (shape ())))))))
              ((binding ((name +arg1) (id 76)))
               (value
                (Ref
                 ((id ((name x) (id 75)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name +arg2) (id 78)))
               (value
                (Ref
                 ((id ((name y) (id 77)))
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
                        ((id ((name +arg1) (id 76)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 78)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))))
                  (type' (Literal IntLiteral)))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((add.79 (values)))
     (let ((add.72 add.79))
      (let ((f.80 add.72) (x.81 5) (y.82 10))
       (let ((f.73 f.80) (x.75 x.81) (y.77 y.82))
        (let ((f.83 (values)) (+arg1.84 x.75) (+arg2.85 y.77))
         (let ((f.74 f.83) (+arg1.76 +arg1.84) (+arg2.78 +arg2.85))
          (+ +arg1.76 +arg2.78)))))))
    Result of stage Fuse and Simplify:
    15
    Result of stage Kernelize:
    15
    Result of stage Alloc:
    15
    Result of stage Capture:
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
     ((binding ((name id) (id 62)))
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @t) (id 63))) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name e) (id 64)))
                     (bound (ArrayRef ((name @t) (id 63)))))))
                  (body (Ref ((id ((name e) (id 64))))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id ((name id) (id 62))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name id) (id 62)))
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding ((name @t) (id 63))) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name e) (id 64)))
                        (bound (ArrayRef ((name @t) (id 63)))))))
                     (body (Ref ((id ((name e) (id 64)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 66)))
            (value
             (TypeApplication
              ((tFunc (Ref ((id ((name id) (id 62))))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding ((name e) (id 65)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 66))))))
            (args (((id ((name e) (id 65))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 68)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 69)))
            (value
             (Ref
              ((id ((name id) (id 68)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name e) (id 70)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name e) (id 70)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((id.71 (values)))
     (let ((id.68 id.71))
      (let ((f.72 id.68) (e.73 5)) (let ((f.69 f.72) (e.70 e.73)) e.70))))
    Result of stage Fuse and Simplify:
    5
    Result of stage Kernelize:
    5
    Result of stage Alloc:
    5
    Result of stage Capture:
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
       (((binding ((name f) (id 64))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 62)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding ((name +arg2) (id 63)))
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
          (((binding ((name +arg1) (id 65)))
            (value (Ref ((id ((name +arg1) (id 62)))))))
           ((binding ((name +arg2) (id 66)))
            (value (Ref ((id ((name +arg2) (id 63)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg2) (id 67)))
               (value (Ref ((id ((name +arg2) (id 66)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 64))))))
               (args
                (((id ((name +arg1) (id 65)))) ((id ((name +arg2) (id 67))))))
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
       (((binding ((name f) (id 68)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 69)))
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
        ((binding ((name +arg2) (id 71)))
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
          (((binding ((name +arg1) (id 70)))
            (value
             (Ref
              ((id ((name +arg1) (id 69)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding ((name +arg2) (id 72)))
            (value
             (Ref
              ((id ((name +arg2) (id 71)))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 73)))
               (value
                (Ref
                 ((id ((name +arg2) (id 72)))
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
                        ((id ((name +arg1) (id 70)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 73)))
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
    Result of stage Nest:
    (let
     ((f.74 (values)) (+arg1.75 (frame 1 2))
      (+arg2.76 (frame (frame 3 4 5) (frame 6 7 8))))
     (let ((f.68 f.74) (+arg1.69 +arg1.75) (+arg2.71 +arg2.76))
      (let ((+arg1.77 +arg1.69) (+arg2.78 +arg2.71))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.80 +arg1.77) (+arg2.81 +arg2.78))
           (let ((+arg1.70 +arg1.80) (+arg2.72 +arg2.81))
            (let ((+arg2.82 +arg2.72))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg2.84 +arg2.82))
                 (let ((+arg2.73 +arg2.84)) (+ +arg1.70 +arg2.73)))
                (body-matcher map-result.83) (map-result (map-result.83))
                (consumer (values))))))))
          (body-matcher map-result.79) (map-result (map-result.79))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let ((+arg1.77 (frame 1 2)) (+arg2.78 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.80 +arg1.77) (+arg2.81 +arg2.78))
         (let ((+arg2.82 +arg2.81))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.84 +arg2.82)) (+ +arg1.80 +arg2.84))
             (body-matcher map-result.83) (map-result (map-result.83))
             (consumer (values)))))))
        (body-matcher map-result.79) (map-result (map-result.79))
        (consumer (values))))))
    Result of stage Kernelize:
    (let ((+arg1.77 (frame 1 2)) (+arg2.78 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.80 +arg1.77) (+arg2.81 +arg2.78))
         (let ((+arg2.82 +arg2.81))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.84 +arg2.82)) (+ +arg1.80 +arg2.84))
             (body-matcher map-result.83) (map-result (map-result.83))
             (consumer (values)))))))
        (body-matcher map-result.79) (map-result (map-result.79))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.106 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.99
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.96 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.77
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.96) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.96) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.96)))
       (+arg2.78
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.99) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.99) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.99) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.99) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.99) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.99) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.99))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((+arg1.80 +arg1.77) (+arg2.81 +arg2.78) (map-mem.107 map-mem.106))
          (let ((+arg2.82 +arg2.81))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.84 +arg2.82) (map-mem.108 map-mem.107))
               (let ((expr-result.109 (+ +arg1.80 +arg2.84)))
                (begin (putmem expr-result.109 map-mem.108) expr-result.109)))
              (body-matcher map-result.83) (map-result (map-result.83))
              (map-result-mem-interim (values map-mem.107))
              (map-result-mem-final (values map-mem.107)) (consumer (values)))))))
         (body-matcher map-result.79) (map-result (map-result.79))
         (map-result-mem-interim (values map-mem.106))
         (map-result-mem-final (values map-mem.106)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.106 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.99
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.96 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.77
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.96) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.96) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.96)))
       (+arg2.78
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.99) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.99) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.99) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.99) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.99) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.99) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.99))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((+arg1.80 +arg1.77) (+arg2.81 +arg2.78) (map-mem.107 map-mem.106))
          (let ((+arg2.82 +arg2.81))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.84 +arg2.82) (map-mem.108 map-mem.107))
               (let ((expr-result.109 (+ +arg1.80 +arg2.84)))
                (begin (putmem expr-result.109 map-mem.108) expr-result.109)))
              (body-matcher map-result.83) (map-result (map-result.83))
              (map-result-mem-interim (values map-mem.107))
              (map-result-mem-final (values map-mem.107)) (consumer (values)))))))
         (body-matcher map-result.79) (map-result (map-result.79))
         (map-result-mem-interim (values map-mem.106))
         (map-result-mem-final (values map-mem.106)) (consumer (values))))))) |}]
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
     ((binding ((name words) (id 62)))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 63)) 1)))))))))))))))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 63)) 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings ((((name len) (id 64)) Dim)))
         (valueBinding ((name word) (id 65)))
         (box (Ref ((id ((name words) (id 62))))))
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
                             (((binding ((name d) (id 1))) (bound Dim))
                              ((binding ((name @cell-shape) (id 2)))
                               (bound Shape))))
                            (body
                             (Scalar
                              ((element
                                (TypeLambda
                                 ((params
                                   (((binding ((name t) (id 3))) (bound Atom))))
                                  (body
                                   (Scalar
                                    ((element
                                      (TermLambda
                                       ((params
                                         (((binding ((name arr) (id 4)))
                                           (bound
                                            (Arr
                                             ((element
                                               (AtomRef ((name t) (id 3))))
                                              (shape
                                               ((Add
                                                 ((const 0)
                                                  (refs ((((name d) (id 1)) 1)))))
                                                (ShapeRef
                                                 ((name @cell-shape) (id 2)))))))))))
                                        (body
                                         (ReifyIndex
                                          ((index
                                            (Dimension
                                             ((const 0)
                                              (refs ((((name d) (id 1)) 1)))))))))))))))))))))))))))
                      (args
                       ((Dimension ((const 0) (refs ((((name len) (id 64)) 1)))))
                        (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id ((name word) (id 65)))))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name words) (id 62)))
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
                       ((Add ((const 0) (refs ((((name len) (id 63)) 1)))))))))))))))
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
                       ((Add ((const 0) (refs ((((name len) (id 63)) 1)))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name box) (id 71)))
            (value (Ref ((id ((name words) (id 62)))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 64)))
               (indexValue
                (FromBox (box (Ref ((id ((name box) (id 71)))))) (i 0)))
               (sort Dim))))
            (body
             (Map
              ((args
                (((binding ((name word) (id 65)))
                  (value (BoxValue ((box (Ref ((id ((name box) (id 71))))))))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 70)))
                     (value (Primitive ((name (Func Equal))))))
                    ((binding ((name =arg1) (id 66)))
                     (value (Scalar ((element (Literal (IntLiteral 3)))))))
                    ((binding ((name =arg2) (id 69)))
                     (value
                      (Map
                       ((args
                         (((binding ((name f) (id 68)))
                           (value
                            (TypeApplication
                             ((tFunc
                               (IndexApplication
                                ((iFunc
                                  (Scalar
                                   ((element
                                     (IndexLambda
                                      ((params
                                        (((binding ((name d) (id 1)))
                                          (bound Dim))
                                         ((binding ((name @cell-shape) (id 2)))
                                          (bound Shape))))
                                       (body
                                        (Scalar
                                         ((element
                                           (TypeLambda
                                            ((params
                                              (((binding ((name t) (id 3)))
                                                (bound Atom))))
                                             (body
                                              (Scalar
                                               ((element
                                                 (TermLambda
                                                  ((params
                                                    (((binding
                                                       ((name arr) (id 4)))
                                                      (bound
                                                       (Arr
                                                        ((element
                                                          (AtomRef
                                                           ((name t) (id 3))))
                                                         (shape
                                                          ((Add
                                                            ((const 0)
                                                             (refs
                                                              ((((name d) (id 1))
                                                                1)))))
                                                           (ShapeRef
                                                            ((name @cell-shape)
                                                             (id 2)))))))))))
                                                   (body
                                                    (ReifyIndex
                                                     ((index
                                                       (Dimension
                                                        ((const 0)
                                                         (refs
                                                          ((((name d) (id 1)) 1)))))))))))))))))))))))))))
                                 (args
                                  ((Dimension
                                    ((const 0) (refs ((((name len) (id 64)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal CharacterLiteral))))))))
                          ((binding ((name arr) (id 67)))
                           (value (Ref ((id ((name word) (id 65)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 68))))))
                           (args (((id ((name arr) (id 67))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 70))))))
                     (args
                      (((id ((name =arg1) (id 66))))
                       ((id ((name =arg2) (id 69))))))
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
       (((binding ((name words) (id 77)))
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
                      ((Add ((const 0) (refs ((((name len) (id 63)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 63))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 63)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 63))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 63)) 1))))))))))))
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
                      ((Add ((const 0) (refs ((((name len) (id 63)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 63))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 63)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 63))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 63)) 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 63))) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 63)) 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name box) (id 78)))
            (value
             (Ref
              ((id ((name words) (id 77)))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 63))) (bound Dim))))
                    (body
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 63)) 1))))))))))))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 64)))
               (indexValue
                (FromBox
                 (box
                  (Ref
                   ((id ((name box) (id 78)))
                    (type'
                     ((element
                       (Sigma
                        ((parameters
                          (((binding ((name len) (id 63))) (bound Dim))))
                         (body
                          ((element (Literal CharacterLiteral))
                           (shape
                            ((Add ((const 0) (refs ((((name len) (id 63)) 1))))))))))))
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
                   (((binding ((name f) (id 72)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name =arg1) (id 73)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 3)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name =arg2) (id 76)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 75)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ReifyIndex
                          ((index
                            (Dimension
                             ((const 0) (refs ((((name len) (id 64)) 1))))))
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
                              ((id ((name =arg1) (id 73)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name =arg2) (id 76)))
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
    Result of stage Nest:
    (let
     ((words.79 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (let ((words.77 words.79))
      (let ((box.80 words.77))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((box.82 box.80))
           (let ((box.78 box.82))
            (index-let ((len.64 box-index-0 box.78))
             (let ()
              (let ()
               (let
                ((f.83 (values)) (=arg1.84 3)
                 (=arg2.86
                  (let ((f.85 (values)))
                   (let ((f.75 f.85)) (reify-index len.64)))))
                (let ((f.72 f.83) (=arg1.73 =arg1.84) (=arg2.76 =arg2.86))
                 (= =arg1.73 =arg2.76))))))))
          (body-matcher map-result.81) (map-result (map-result.81))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((box.80 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.82 box.80))
         (index-let ((len.64 box-index-0 box.82)) (= 3 (reify-index len.64))))
        (body-matcher map-result.81) (map-result (map-result.81))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((box.80 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.82 box.80))
         (index-let ((len.64 box-index-0 box.82)) (= 3 (reify-index len.64))))
        (body-matcher map-result.81) (map-result (map-result.81))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.98 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.94
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.89
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.88
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 63))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.63))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.80
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.89) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.89) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.89) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.89)))
           (index (mem frame-array.88) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 63))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.63)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.94) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.94) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.94)))
           (index (mem frame-array.88) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 63))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.63))))))))))))
         (getmem frame-array.88))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.82 box.80) (map-mem.99 map-mem.98))
          (index-let ((len.64 box-index-0 box.82))
           (let ((expr-result.100 (= 3 (reify-dimension-index len.64))))
            (begin (putmem expr-result.100 map-mem.99) expr-result.100))))
         (body-matcher map-result.81) (map-result (map-result.81))
         (map-result-mem-interim (values map-mem.98))
         (map-result-mem-final (values map-mem.98)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.98 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.94
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.89
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.88
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 63))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.63))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.80
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.89) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.89) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.89) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.89)))
           (index (mem frame-array.88) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 63))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.63)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.94) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.94) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.94)))
           (index (mem frame-array.88) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 63))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.63))))))))))))
         (getmem frame-array.88))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.82 box.80) (map-mem.99 map-mem.98))
          (index-let ((len.64 box-index-0 box.82))
           (let ((expr-result.100 (= 3 (reify-dimension-index len.64))))
            (begin (putmem expr-result.100 map-mem.99) expr-result.100))))
         (body-matcher map-result.81) (map-result (map-result.81))
         (map-result-mem-interim (values map-mem.98))
         (map-result-mem-final (values map-mem.98)) (consumer (values))))))) |}]
;;

let%expect_test "sum rows" =
  printStages
    {|
    (define (sum-row{ | d-1} [row [int (+ d-1 1)]])
      (reduce{int | d-1 []} + row))
    (sum-row{ | 9} iota{ | [1000000 10]})
    |};
  [%expect
    {|
    Result of stage Type Check:
    (Let
     ((binding ((name sum-row) (id 62)))
      (value
       (Scalar
        ((element
          (IndexLambda
           ((params (((binding ((name d-1) (id 63))) (bound Dim))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name row) (id 64)))
                     (bound
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape
                         ((Add ((const 1) (refs ((((name d-1) (id 63)) 1)))))))))))))
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
                              ((const 0) (refs ((((name d-1) (id 63)) 1)))))
                             (Shape ()))))))
                        (args ((Atom (Literal IntLiteral)))))))
                     (args
                      ((Primitive ((name (Func Add))))
                       (Ref ((id ((name row) (id 64))))))))))))))))))))))
      (body
       (TermApplication
        ((func
          (IndexApplication
           ((iFunc (Ref ((id ((name sum-row) (id 62))))))
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
       (((binding ((name sum-row) (id 62)))
         (value
          (Scalar
           ((element
             (IndexLambda
              ((params (((binding ((name d-1) (id 63))) (bound Dim))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name row) (id 64)))
                        (bound
                         (Arr
                          ((element (Literal IntLiteral))
                           (shape
                            ((Add ((const 1) (refs ((((name d-1) (id 63)) 1)))))))))))))
                     (body
                      (Map
                       ((args
                         (((binding ((name f) (id 67)))
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
                                    ((const 0) (refs ((((name d-1) (id 63)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal IntLiteral))))))))
                          ((binding ((name reduce-f-arg) (id 65)))
                           (value (Primitive ((name (Func Add))))))
                          ((binding ((name reduce-array-arg) (id 66)))
                           (value (Ref ((id ((name row) (id 64)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 67))))))
                           (args
                            (((id ((name reduce-f-arg) (id 65))))
                             ((id ((name reduce-array-arg) (id 66))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 69)))
            (value
             (IndexApplication
              ((iFunc (Ref ((id ((name sum-row) (id 62))))))
               (args ((Dimension ((const 9) (refs ())))))))))
           ((binding ((name row) (id 68)))
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
             (((binding ((name row) (id 70)))
               (value (Ref ((id ((name row) (id 68)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 69))))))
               (args (((id ((name row) (id 70))))))
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
       (((binding ((name sum-row) (id 72)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 73)))
            (value
             (Ref
              ((id ((name sum-row) (id 72)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name row) (id 79)))
            (value
             (ArrayPrimitive
              (Map
               (frameShape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
               (args ()) (iotaVar (((name iota) (id 78))))
               (body
                (Ref
                 ((id ((name iota) (id 78)))
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
             (((binding ((name row) (id 80)))
               (value
                (Ref
                 ((id ((name row) (id 79)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape
                     ((Add ((const 1000000) (refs ())))
                      (Add ((const 10) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 74)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name reduce-f-arg) (id 75)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name reduce-array-arg) (id 81)))
                  (value
                   (Ref
                    ((id ((name row) (id 80)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 10) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Reduce
                  (arg
                   ((firstBinding ((name reduce-arg1) (id 82)))
                    (secondBinding ((name reduce-arg2) (id 83)))
                    (value
                     (Ref
                      ((id ((name reduce-array-arg) (id 81)))
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
                              ((id ((name reduce-arg1) (id 82)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name reduce-arg2) (id 83)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))))
                        (type' (Literal IntLiteral)))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (d ((const 10) (refs ()))) (cellShape ()) (associative true)
                  (character Reduce)
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
    Result of stage Nest:
    (let ((sum-row.84 (values)))
     (let ((sum-row.72 sum-row.84))
      (let
       ((f.85 sum-row.72)
        (row.90
         (let ()
          (#0
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota iota.87)
              (#0
               (#0
                (loop-block (frame-shape 10)
                 (map () (iota (iota.89 : iota.87))
                  (let ((iota.78 iota.89)) iota.78))
                 (body-matcher map-result.88) (map-result (map-result.88))
                 (consumer (values))))))
             (body-matcher map-result.86) (map-result (map-result.86))
             (consumer (values))))))))
       (let ((f.73 f.85) (row.79 row.90))
        (let ((row.91 row.79))
         (#0
          (#0
           (loop-block (frame-shape 1000000)
            (map ((row.93 row.91))
             (let ((row.80 row.93))
              (let
               ((f.94 (values)) (reduce-f-arg.95 (values))
                (reduce-array-arg.96 row.80))
               (let
                ((f.74 f.94) (reduce-f-arg.75 reduce-f-arg.95)
                 (reduce-array-arg.81 reduce-array-arg.96))
                (let ((reduce-arg.98 reduce-array-arg.81))
                 (#1
                  (loop-block (frame-shape 10)
                   (map ((reduce-arg.99 reduce-arg.98)) (values reduce-arg.99))
                   (body-matcher (reduce-arg.97)) (map-result ())
                   (consumer
                    (reduce (reduce-arg1.82 reduce-arg2.83 reduce-arg.97)
                     (+ reduce-arg1.82 reduce-arg2.83))))))))))
            (body-matcher map-result.92) (map-result (map-result.92))
            (consumer (values))))))))))
    Result of stage Fuse and Simplify:
    (#0
     (#0
      (loop-block (frame-shape 1000000)
       (map () (iota iota.87)
        (#1
         (loop-block (frame-shape 10) (map () (iota (iota.89 : iota.87)) iota.89)
          (body-matcher reduce-arg.97) (map-result ())
          (consumer
           (reduce (reduce-arg1.82 reduce-arg2.83 reduce-arg.97)
            (+ reduce-arg1.82 reduce-arg2.83))))))
       (body-matcher map-result.92) (map-result (map-result.92))
       (consumer (values)))))
    Result of stage Kernelize:
    (#0
     (kernel (blocks 320) (threads 32)
      (map-kernel (frame-shape 1000000) () (iota iota.87)
       (body-matcher map-result.92) (map-result (map-result.92))
       (#1
        (loop-block (frame-shape 10) (map () (iota (iota.89 : iota.87)) iota.89)
         (body-matcher reduce-arg.97) (map-result ())
         (consumer
          (reduce (reduce-arg1.82 reduce-arg2.83 reduce-arg.97)
           (+ reduce-arg1.82 reduce-arg2.83))))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.136 (Tuple ()) device)
      (map-mem.134
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.133
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host))
     (#0
      (begin
       (kernel (blocks 320) (threads 32)
        ((map
          (map-kernel (frame-shape 1000000) ((map-mem.135 map-mem.134))
           (iota iota.87)
           (putmem
            (loop (frame-shape 10)
             (map ((map-mem.137 map-mem.136)) (iota (iota.89 : iota.87)) iota.89)
             (body-matcher reduce-arg.97) (map-result ())
             (map-result-mem-interim map-mem.136)
             (map-result-mem-final map-mem.136)
             (consumer
              (reduce (reduce-arg1.82 reduce-arg2.83 reduce-arg.97)
               (+ reduce-arg1.82 reduce-arg2.83))))
            (#0 map-mem.135))))
         (map-result-mem-interim map-mem.134)
         (map-result-mem-final (values map-mem-result.0.133))))
       (getmem (values map-mem-result.0.133)))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.136 (Tuple ()) device)
      (map-mem.134
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.133
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host))
     (#0
      (begin
       (kernel captures
        ((expr-captures ())
         (mem-captures
          ((((name map-mem) (id 134))
            (Tuple
             ((Array ((element (Literal IntLiteral)) (shape (shape 1000000)))))))
           (((name map-mem) (id 136)) (Tuple ()))))
         (index-captures ()))
        (blocks 320) (threads 32)
        ((map
          (map-kernel (frame-shape 1000000) ((map-mem.135 map-mem.134))
           (iota iota.87)
           (putmem
            (loop (frame-shape 10)
             (map ((map-mem.137 map-mem.136)) (iota (iota.89 : iota.87)) iota.89)
             (body-matcher reduce-arg.97) (map-result ())
             (map-result-mem-interim map-mem.136)
             (map-result-mem-final map-mem.136)
             (consumer
              (reduce (reduce-arg1.82 reduce-arg2.83 reduce-arg.97)
               (+ reduce-arg1.82 reduce-arg2.83))))
            (#0 map-mem.135))))
         (map-result-mem-interim map-mem.134)
         (map-result-mem-final (values map-mem-result.0.133))))
       (getmem (values map-mem-result.0.133))))) |}]
;;
