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
       (((binding ((name f) (id 41))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 39)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 40)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 41))))))
         (args (((id ((name +arg1) (id 39)))) ((id ((name +arg2) (id 40))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 42)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 43)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 44)))
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
                  ((id ((name +arg1) (id 43)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id ((name +arg2) (id 44)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))))
            (type' (Literal IntLiteral)))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((f.45 (values)) (+arg1.46 1) (+arg2.47 2))
     (let ((f.42 f.45) (+arg1.43 +arg1.46) (+arg2.44 +arg2.47))
      (+ +arg1.43 +arg2.44)))
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
     ((binding ((name add) (id 39)))
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding ((name x) (id 40)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding ((name y) (id 41)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args
                ((Ref ((id ((name x) (id 40))))) (Ref ((id ((name y) (id 41))))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name add) (id 39))))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name add) (id 39)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name x) (id 40)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name y) (id 41)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 44)))
                     (value (Primitive ((name (Func Add))))))
                    ((binding ((name +arg1) (id 42)))
                     (value (Ref ((id ((name x) (id 40)))))))
                    ((binding ((name +arg2) (id 43)))
                     (value (Ref ((id ((name y) (id 41)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 44))))))
                     (args
                      (((id ((name +arg1) (id 42))))
                       ((id ((name +arg2) (id 43))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 47)))
            (value (Ref ((id ((name add) (id 39)))))))
           ((binding ((name x) (id 45)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding ((name y) (id 46)))
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 47))))))
            (args (((id ((name x) (id 45)))) ((id ((name y) (id 46))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name add) (id 49)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 50)))
            (value
             (Ref
              ((id ((name add) (id 49)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 52)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name y) (id 54)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 51)))
               (value
                (AtomAsArray
                 ((element (Values ((elements ()) (type' ()))))
                  (type' ((element (Tuple ())) (shape ())))))))
              ((binding ((name +arg1) (id 53)))
               (value
                (Ref
                 ((id ((name x) (id 52)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name +arg2) (id 55)))
               (value
                (Ref
                 ((id ((name y) (id 54)))
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
                        ((id ((name +arg1) (id 53)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 55)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))))
                  (type' (Literal IntLiteral)))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((add.56 (values)))
     (let ((add.49 add.56))
      (let ((f.57 add.49) (x.58 5) (y.59 10))
       (let ((f.50 f.57) (x.52 x.58) (y.54 y.59))
        (let ((f.60 (values)) (+arg1.61 x.52) (+arg2.62 y.54))
         (let ((f.51 f.60) (+arg1.53 +arg1.61) (+arg2.55 +arg2.62))
          (+ +arg1.53 +arg2.55)))))))
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
     ((binding ((name id) (id 39)))
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @t) (id 40))) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name e) (id 41)))
                     (bound (ArrayRef ((name @t) (id 40)))))))
                  (body (Ref ((id ((name e) (id 41))))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id ((name id) (id 39))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name id) (id 39)))
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding ((name @t) (id 40))) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name e) (id 41)))
                        (bound (ArrayRef ((name @t) (id 40)))))))
                     (body (Ref ((id ((name e) (id 41)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 43)))
            (value
             (TypeApplication
              ((tFunc (Ref ((id ((name id) (id 39))))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding ((name e) (id 42)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 43))))))
            (args (((id ((name e) (id 42))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 45)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 46)))
            (value
             (Ref
              ((id ((name id) (id 45)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name e) (id 47)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name e) (id 47)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((id.48 (values)))
     (let ((id.45 id.48))
      (let ((f.49 id.45) (e.50 5)) (let ((f.46 f.49) (e.47 e.50)) e.47))))
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
       (((binding ((name f) (id 41))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 39)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding ((name +arg2) (id 40)))
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
          (((binding ((name +arg1) (id 42)))
            (value (Ref ((id ((name +arg1) (id 39)))))))
           ((binding ((name +arg2) (id 43)))
            (value (Ref ((id ((name +arg2) (id 40)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg2) (id 44)))
               (value (Ref ((id ((name +arg2) (id 43)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 41))))))
               (args
                (((id ((name +arg1) (id 42)))) ((id ((name +arg2) (id 44))))))
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
       (((binding ((name f) (id 45)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 46)))
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
        ((binding ((name +arg2) (id 48)))
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
          (((binding ((name +arg1) (id 47)))
            (value
             (Ref
              ((id ((name +arg1) (id 46)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding ((name +arg2) (id 49)))
            (value
             (Ref
              ((id ((name +arg2) (id 48)))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 50)))
               (value
                (Ref
                 ((id ((name +arg2) (id 49)))
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
                        ((id ((name +arg1) (id 47)))
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
     ((f.51 (values)) (+arg1.52 (frame 1 2))
      (+arg2.53 (frame (frame 3 4 5) (frame 6 7 8))))
     (let ((f.45 f.51) (+arg1.46 +arg1.52) (+arg2.48 +arg2.53))
      (let ((+arg1.54 +arg1.46) (+arg2.55 +arg2.48))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.57 +arg1.54) (+arg2.58 +arg2.55))
           (let ((+arg1.47 +arg1.57) (+arg2.49 +arg2.58))
            (let ((+arg2.59 +arg2.49))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg2.61 +arg2.59))
                 (let ((+arg2.50 +arg2.61)) (+ +arg1.47 +arg2.50)))
                (body-matcher map-result.60) (map-result (map-result.60))
                (consumer (values))))))))
          (body-matcher map-result.56) (map-result (map-result.56))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let ((+arg1.54 (frame 1 2)) (+arg2.55 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.57 +arg1.54) (+arg2.58 +arg2.55))
         (let ((+arg2.59 +arg2.58))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.61 +arg2.59)) (+ +arg1.57 +arg2.61))
             (body-matcher map-result.60) (map-result (map-result.60))
             (consumer (values)))))))
        (body-matcher map-result.56) (map-result (map-result.56))
        (consumer (values))))))
    Result of stage Kernelize:
    (let ((+arg1.54 (frame 1 2)) (+arg2.55 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.57 +arg1.54) (+arg2.58 +arg2.55))
         (let ((+arg2.59 +arg2.58))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.61 +arg2.59)) (+ +arg1.57 +arg2.61))
             (body-matcher map-result.60) (map-result (map-result.60))
             (consumer (values)))))))
        (body-matcher map-result.56) (map-result (map-result.56))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.83 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.76
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.73 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.54
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.73) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.73) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.73)))
       (+arg2.55
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.76) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.76) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.76) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.76) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.76) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.76) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.76))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((+arg1.57 +arg1.54) (+arg2.58 +arg2.55) (map-mem.84 map-mem.83))
          (let ((+arg2.59 +arg2.58))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.61 +arg2.59) (map-mem.85 map-mem.84))
               (let ((expr-result.86 (+ +arg1.57 +arg2.61)))
                (begin (putmem expr-result.86 map-mem.85) expr-result.86)))
              (body-matcher map-result.60) (map-result (map-result.60))
              (map-result-mem-interim (values map-mem.84))
              (map-result-mem-final (values map-mem.84)) (consumer (values)))))))
         (body-matcher map-result.56) (map-result (map-result.56))
         (map-result-mem-interim (values map-mem.83))
         (map-result-mem-final (values map-mem.83)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.83 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.76
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.73 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.54
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.73) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.73) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.73)))
       (+arg2.55
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.76) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.76) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.76) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.76) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.76) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.76) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.76))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((+arg1.57 +arg1.54) (+arg2.58 +arg2.55) (map-mem.84 map-mem.83))
          (let ((+arg2.59 +arg2.58))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.61 +arg2.59) (map-mem.85 map-mem.84))
               (let ((expr-result.86 (+ +arg1.57 +arg2.61)))
                (begin (putmem expr-result.86 map-mem.85) expr-result.86)))
              (body-matcher map-result.60) (map-result (map-result.60))
              (map-result-mem-interim (values map-mem.84))
              (map-result-mem-final (values map-mem.84)) (consumer (values)))))))
         (body-matcher map-result.56) (map-result (map-result.56))
         (map-result-mem-interim (values map-mem.83))
         (map-result-mem-final (values map-mem.83)) (consumer (values))))))) |}]
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
     ((binding ((name words) (id 39)))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 40)) 1)))))))))))))))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 40)) 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings ((((name len) (id 41)) Dim)))
         (valueBinding ((name word) (id 42)))
         (box (Ref ((id ((name words) (id 39))))))
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
                       ((Dimension ((const 0) (refs ((((name len) (id 41)) 1)))))
                        (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id ((name word) (id 42)))))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name words) (id 39)))
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
                       ((Add ((const 0) (refs ((((name len) (id 40)) 1)))))))))))))))
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
                       ((Add ((const 0) (refs ((((name len) (id 40)) 1)))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name box) (id 48)))
            (value (Ref ((id ((name words) (id 39)))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 41)))
               (indexValue
                (FromBox (box (Ref ((id ((name box) (id 48)))))) (i 0)))
               (sort Dim))))
            (body
             (Map
              ((args
                (((binding ((name word) (id 42)))
                  (value (BoxValue ((box (Ref ((id ((name box) (id 48))))))))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 47)))
                     (value (Primitive ((name (Func Equal))))))
                    ((binding ((name =arg1) (id 43)))
                     (value (Scalar ((element (Literal (IntLiteral 3)))))))
                    ((binding ((name =arg2) (id 46)))
                     (value
                      (Map
                       ((args
                         (((binding ((name f) (id 45)))
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
                                    ((const 0) (refs ((((name len) (id 41)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal CharacterLiteral))))))))
                          ((binding ((name arr) (id 44)))
                           (value (Ref ((id ((name word) (id 42)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 45))))))
                           (args (((id ((name arr) (id 44))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 47))))))
                     (args
                      (((id ((name =arg1) (id 43))))
                       ((id ((name =arg2) (id 46))))))
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
       (((binding ((name words) (id 54)))
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
                      ((Add ((const 0) (refs ((((name len) (id 40)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 40))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 40)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 40))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 40)) 1))))))))))))
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
                      ((Add ((const 0) (refs ((((name len) (id 40)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 40))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 40)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 40))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 40)) 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 40))) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 40)) 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name box) (id 55)))
            (value
             (Ref
              ((id ((name words) (id 54)))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 40))) (bound Dim))))
                    (body
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 40)) 1))))))))))))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 41)))
               (indexValue
                (FromBox
                 (box
                  (Ref
                   ((id ((name box) (id 55)))
                    (type'
                     ((element
                       (Sigma
                        ((parameters
                          (((binding ((name len) (id 40))) (bound Dim))))
                         (body
                          ((element (Literal CharacterLiteral))
                           (shape
                            ((Add ((const 0) (refs ((((name len) (id 40)) 1))))))))))))
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
                   (((binding ((name f) (id 49)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name =arg1) (id 50)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 3)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name =arg2) (id 53)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 52)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ReifyIndex
                          ((index
                            (Dimension
                             ((const 0) (refs ((((name len) (id 41)) 1))))))
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
                              ((id ((name =arg1) (id 50)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name =arg2) (id 53)))
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
     ((words.56 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (let ((words.54 words.56))
      (let ((box.57 words.54))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((box.59 box.57))
           (let ((box.55 box.59))
            (index-let ((len.41 box-index-0 box.55))
             (let ()
              (let ()
               (let
                ((f.60 (values)) (=arg1.61 3)
                 (=arg2.63
                  (let ((f.62 (values)))
                   (let ((f.52 f.62)) (reify-index len.41)))))
                (let ((f.49 f.60) (=arg1.50 =arg1.61) (=arg2.53 =arg2.63))
                 (= =arg1.50 =arg2.53))))))))
          (body-matcher map-result.58) (map-result (map-result.58))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((box.57 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.59 box.57))
         (index-let ((len.41 box-index-0 box.59)) (= 3 (reify-index len.41))))
        (body-matcher map-result.58) (map-result (map-result.58))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((box.57 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.59 box.57))
         (index-let ((len.41 box-index-0 box.59)) (= 3 (reify-index len.41))))
        (body-matcher map-result.58) (map-result (map-result.58))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.75 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.71
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.66
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.65
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 40))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.40))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.57
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.66) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.66) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.66) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.66)))
           (index (mem frame-array.65) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 40))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.40)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.71) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.71) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.71)))
           (index (mem frame-array.65) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 40))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.40))))))))))))
         (getmem frame-array.65))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.59 box.57) (map-mem.76 map-mem.75))
          (index-let ((len.41 box-index-0 box.59))
           (let ((expr-result.77 (= 3 (reify-dimension-index len.41))))
            (begin (putmem expr-result.77 map-mem.76) expr-result.77))))
         (body-matcher map-result.58) (map-result (map-result.58))
         (map-result-mem-interim (values map-mem.75))
         (map-result-mem-final (values map-mem.75)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.75 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.71
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.66
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.65
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 40))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.40))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.57
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.66) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.66) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.66) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.66)))
           (index (mem frame-array.65) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 40))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.40)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.71) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.71) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.71)))
           (index (mem frame-array.65) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 40))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.40))))))))))))
         (getmem frame-array.65))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.59 box.57) (map-mem.76 map-mem.75))
          (index-let ((len.41 box-index-0 box.59))
           (let ((expr-result.77 (= 3 (reify-dimension-index len.41))))
            (begin (putmem expr-result.77 map-mem.76) expr-result.77))))
         (body-matcher map-result.58) (map-result (map-result.58))
         (map-result-mem-interim (values map-mem.75))
         (map-result-mem-final (values map-mem.75)) (consumer (values))))))) |}]
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
     ((binding ((name sum-row) (id 39)))
      (value
       (Scalar
        ((element
          (IndexLambda
           ((params (((binding ((name d-1) (id 40))) (bound Dim))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name row) (id 41)))
                     (bound
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape
                         ((Add ((const 1) (refs ((((name d-1) (id 40)) 1)))))))))))))
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
                              ((const 0) (refs ((((name d-1) (id 40)) 1)))))
                             (Shape ()))))))
                        (args ((Atom (Literal IntLiteral)))))))
                     (args
                      ((Primitive ((name (Func Add))))
                       (Ref ((id ((name row) (id 41))))))))))))))))))))))
      (body
       (TermApplication
        ((func
          (IndexApplication
           ((iFunc (Ref ((id ((name sum-row) (id 39))))))
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
       (((binding ((name sum-row) (id 39)))
         (value
          (Scalar
           ((element
             (IndexLambda
              ((params (((binding ((name d-1) (id 40))) (bound Dim))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name row) (id 41)))
                        (bound
                         (Arr
                          ((element (Literal IntLiteral))
                           (shape
                            ((Add ((const 1) (refs ((((name d-1) (id 40)) 1)))))))))))))
                     (body
                      (Map
                       ((args
                         (((binding ((name f) (id 44)))
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
                                    ((const 0) (refs ((((name d-1) (id 40)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal IntLiteral))))))))
                          ((binding ((name reduce-f-arg) (id 42)))
                           (value (Primitive ((name (Func Add))))))
                          ((binding ((name reduce-array-arg) (id 43)))
                           (value (Ref ((id ((name row) (id 41)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 44))))))
                           (args
                            (((id ((name reduce-f-arg) (id 42))))
                             ((id ((name reduce-array-arg) (id 43))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 46)))
            (value
             (IndexApplication
              ((iFunc (Ref ((id ((name sum-row) (id 39))))))
               (args ((Dimension ((const 9) (refs ())))))))))
           ((binding ((name row) (id 45)))
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
             (((binding ((name row) (id 47)))
               (value (Ref ((id ((name row) (id 45)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 46))))))
               (args (((id ((name row) (id 47))))))
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
       (((binding ((name sum-row) (id 49)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 50)))
            (value
             (Ref
              ((id ((name sum-row) (id 49)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name row) (id 56)))
            (value
             (ArrayPrimitive
              (Map
               (frameShape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
               (args ()) (iotaVar (((name iota) (id 55))))
               (body
                (Ref
                 ((id ((name iota) (id 55)))
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
             (((binding ((name row) (id 57)))
               (value
                (Ref
                 ((id ((name row) (id 56)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape
                     ((Add ((const 1000000) (refs ())))
                      (Add ((const 10) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 51)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name reduce-f-arg) (id 52)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name reduce-array-arg) (id 58)))
                  (value
                   (Ref
                    ((id ((name row) (id 57)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 10) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Reduce
                  (arg
                   ((firstBinding ((name reduce-arg1) (id 59)))
                    (secondBinding ((name reduce-arg2) (id 60)))
                    (value
                     (Ref
                      ((id ((name reduce-array-arg) (id 58)))
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
                              ((id ((name reduce-arg1) (id 59)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name reduce-arg2) (id 60)))
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
    (let ((sum-row.61 (values)))
     (let ((sum-row.49 sum-row.61))
      (let
       ((f.62 sum-row.49)
        (row.67
         (let ()
          (#0
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota iota.64)
              (#0
               (#0
                (loop-block (frame-shape 10)
                 (map () (iota (iota.66 : iota.64))
                  (let ((iota.55 iota.66)) iota.55))
                 (body-matcher map-result.65) (map-result (map-result.65))
                 (consumer (values))))))
             (body-matcher map-result.63) (map-result (map-result.63))
             (consumer (values))))))))
       (let ((f.50 f.62) (row.56 row.67))
        (let ((row.68 row.56))
         (#0
          (#0
           (loop-block (frame-shape 1000000)
            (map ((row.70 row.68))
             (let ((row.57 row.70))
              (let
               ((f.71 (values)) (reduce-f-arg.72 (values))
                (reduce-array-arg.73 row.57))
               (let
                ((f.51 f.71) (reduce-f-arg.52 reduce-f-arg.72)
                 (reduce-array-arg.58 reduce-array-arg.73))
                (let ((reduce-arg.75 reduce-array-arg.58))
                 (#1
                  (loop-block (frame-shape 10)
                   (map ((reduce-arg.76 reduce-arg.75)) (values reduce-arg.76))
                   (body-matcher (reduce-arg.74)) (map-result ())
                   (consumer
                    (reduce (reduce-arg1.59 reduce-arg2.60 reduce-arg.74)
                     (+ reduce-arg1.59 reduce-arg2.60))))))))))
            (body-matcher map-result.69) (map-result (map-result.69))
            (consumer (values))))))))))
    Result of stage Fuse and Simplify:
    (#0
     (#0
      (loop-block (frame-shape 1000000)
       (map () (iota iota.64)
        (#1
         (loop-block (frame-shape 10) (map () (iota (iota.66 : iota.64)) iota.66)
          (body-matcher reduce-arg.74) (map-result ())
          (consumer
           (reduce (reduce-arg1.59 reduce-arg2.60 reduce-arg.74)
            (+ reduce-arg1.59 reduce-arg2.60))))))
       (body-matcher map-result.69) (map-result (map-result.69))
       (consumer (values)))))
    Result of stage Kernelize:
    (#0
     (kernel (blocks 320) (threads 32)
      (map-kernel (frame-shape 1000000) () (iota iota.64)
       (body-matcher map-result.69) (map-result (map-result.69))
       (#1
        (loop-block (frame-shape 10) (map () (iota (iota.66 : iota.64)) iota.66)
         (body-matcher reduce-arg.74) (map-result ())
         (consumer
          (reduce (reduce-arg1.59 reduce-arg2.60 reduce-arg.74)
           (+ reduce-arg1.59 reduce-arg2.60))))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.116 (Tuple ()) device)
      (map-mem.111
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.110
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host))
     (#0
      (begin
       (kernel (blocks 320) (threads 32)
        ((map
          (map-kernel (frame-shape 1000000)
           ((map-mem.112 map-mem.111) (map-mem.113 map-mem.116)) (iota iota.64)
           (putmem
            (loop (frame-shape 10)
             (map ((map-mem.114 map-mem.113)) (iota (iota.66 : iota.64)) iota.66)
             (body-matcher reduce-arg.74) (map-result ())
             (map-result-mem-interim map-mem.113)
             (map-result-mem-final map-mem.113)
             (consumer
              (reduce (reduce-arg1.59 reduce-arg2.60 reduce-arg.74)
               (+ reduce-arg1.59 reduce-arg2.60))))
            (#0 map-mem.112))))
         (map-result-mem-interim map-mem.111)
         (map-result-mem-final (values map-mem-result.0.110))))
       (getmem (values map-mem-result.0.110)))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.116 (Tuple ()) device)
      (map-mem.111
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.110
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host))
     (#0
      (begin
       (kernel captures
        ((expr-captures ())
         (mem-captures
          ((((name map-mem) (id 111))
            (Tuple
             ((Array ((element (Literal IntLiteral)) (shape (shape 1000000)))))))
           (((name map-mem) (id 116)) (Tuple ()))))
         (index-captures ()))
        (blocks 320) (threads 32)
        ((map
          (map-kernel (frame-shape 1000000)
           ((map-mem.112 map-mem.111) (map-mem.113 map-mem.116)) (iota iota.64)
           (putmem
            (loop (frame-shape 10)
             (map ((map-mem.114 map-mem.113)) (iota (iota.66 : iota.64)) iota.66)
             (body-matcher reduce-arg.74) (map-result ())
             (map-result-mem-interim map-mem.113)
             (map-result-mem-final map-mem.113)
             (consumer
              (reduce (reduce-arg1.59 reduce-arg2.60 reduce-arg.74)
               (+ reduce-arg1.59 reduce-arg2.60))))
            (#0 map-mem.112))))
         (map-result-mem-interim map-mem.111)
         (map-result-mem-final (values map-mem-result.0.110))))
       (getmem (values map-mem-result.0.110))))) |}]
;;
