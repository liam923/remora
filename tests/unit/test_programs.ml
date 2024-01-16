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
       (((binding ((name f) (id 46))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 44)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 45)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 46))))))
         (args (((id ((name +arg1) (id 44)))) ((id ((name +arg2) (id 45))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
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
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 49)))
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
                  ((id ((name +arg1) (id 48)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id ((name +arg2) (id 49)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))))
            (type' (Literal IntLiteral)))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((f.50 (values)) (+arg1.51 1) (+arg2.52 2))
     (let ((f.47 f.50) (+arg1.48 +arg1.51) (+arg2.49 +arg2.52))
      (+ +arg1.48 +arg2.49)))
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
     ((binding ((name add) (id 44)))
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding ((name x) (id 45)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding ((name y) (id 46)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args
                ((Ref ((id ((name x) (id 45))))) (Ref ((id ((name y) (id 46))))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name add) (id 44))))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name add) (id 44)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name x) (id 45)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name y) (id 46)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 49)))
                     (value (Primitive ((name (Func Add))))))
                    ((binding ((name +arg1) (id 47)))
                     (value (Ref ((id ((name x) (id 45)))))))
                    ((binding ((name +arg2) (id 48)))
                     (value (Ref ((id ((name y) (id 46)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 49))))))
                     (args
                      (((id ((name +arg1) (id 47))))
                       ((id ((name +arg2) (id 48))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 52)))
            (value (Ref ((id ((name add) (id 44)))))))
           ((binding ((name x) (id 50)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding ((name y) (id 51)))
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 52))))))
            (args (((id ((name x) (id 50)))) ((id ((name y) (id 51))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name add) (id 54)))
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
             (Ref
              ((id ((name add) (id 54)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 57)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name y) (id 59)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 56)))
               (value
                (AtomAsArray
                 ((element (Values ((elements ()) (type' ()))))
                  (type' ((element (Tuple ())) (shape ())))))))
              ((binding ((name +arg1) (id 58)))
               (value
                (Ref
                 ((id ((name x) (id 57)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name +arg2) (id 60)))
               (value
                (Ref
                 ((id ((name y) (id 59)))
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
                        ((id ((name +arg2) (id 60)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))))
                  (type' (Literal IntLiteral)))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((add.61 (values)))
     (let ((add.54 add.61))
      (let ((f.62 add.54) (x.63 5) (y.64 10))
       (let ((f.55 f.62) (x.57 x.63) (y.59 y.64))
        (let ((f.65 (values)) (+arg1.66 x.57) (+arg2.67 y.59))
         (let ((f.56 f.65) (+arg1.58 +arg1.66) (+arg2.60 +arg2.67))
          (+ +arg1.58 +arg2.60)))))))
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
     ((binding ((name id) (id 44)))
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @t) (id 45))) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name e) (id 46)))
                     (bound (ArrayRef ((name @t) (id 45)))))))
                  (body (Ref ((id ((name e) (id 46))))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id ((name id) (id 44))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name id) (id 44)))
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding ((name @t) (id 45))) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name e) (id 46)))
                        (bound (ArrayRef ((name @t) (id 45)))))))
                     (body (Ref ((id ((name e) (id 46)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 48)))
            (value
             (TypeApplication
              ((tFunc (Ref ((id ((name id) (id 44))))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding ((name e) (id 47)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 48))))))
            (args (((id ((name e) (id 47))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 50)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 51)))
            (value
             (Ref
              ((id ((name id) (id 50)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name e) (id 52)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name e) (id 52)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((id.53 (values)))
     (let ((id.50 id.53))
      (let ((f.54 id.50) (e.55 5)) (let ((f.51 f.54) (e.52 e.55)) e.52))))
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
       (((binding ((name f) (id 46))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 44)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding ((name +arg2) (id 45)))
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
          (((binding ((name +arg1) (id 47)))
            (value (Ref ((id ((name +arg1) (id 44)))))))
           ((binding ((name +arg2) (id 48)))
            (value (Ref ((id ((name +arg2) (id 45)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg2) (id 49)))
               (value (Ref ((id ((name +arg2) (id 48)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 46))))))
               (args
                (((id ((name +arg1) (id 47)))) ((id ((name +arg2) (id 49))))))
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
       (((binding ((name f) (id 50)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 51)))
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
        ((binding ((name +arg2) (id 53)))
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
          (((binding ((name +arg1) (id 52)))
            (value
             (Ref
              ((id ((name +arg1) (id 51)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding ((name +arg2) (id 54)))
            (value
             (Ref
              ((id ((name +arg2) (id 53)))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 55)))
               (value
                (Ref
                 ((id ((name +arg2) (id 54)))
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
                        ((id ((name +arg1) (id 52)))
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
     ((f.56 (values)) (+arg1.57 (frame 1 2))
      (+arg2.58 (frame (frame 3 4 5) (frame 6 7 8))))
     (let ((f.50 f.56) (+arg1.51 +arg1.57) (+arg2.53 +arg2.58))
      (let ((+arg1.59 +arg1.51) (+arg2.60 +arg2.53))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.62 +arg1.59) (+arg2.63 +arg2.60))
           (let ((+arg1.52 +arg1.62) (+arg2.54 +arg2.63))
            (let ((+arg2.64 +arg2.54))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg2.66 +arg2.64))
                 (let ((+arg2.55 +arg2.66)) (+ +arg1.52 +arg2.55)))
                (body-matcher map-result.65) (map-result (map-result.65))
                (consumer (values))))))))
          (body-matcher map-result.61) (map-result (map-result.61))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let ((+arg1.59 (frame 1 2)) (+arg2.60 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.62 +arg1.59) (+arg2.63 +arg2.60))
         (let ((+arg2.64 +arg2.63))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.66 +arg2.64)) (+ +arg1.62 +arg2.66))
             (body-matcher map-result.65) (map-result (map-result.65))
             (consumer (values)))))))
        (body-matcher map-result.61) (map-result (map-result.61))
        (consumer (values))))))
    Result of stage Kernelize:
    (let ((+arg1.59 (frame 1 2)) (+arg2.60 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.62 +arg1.59) (+arg2.63 +arg2.60))
         (let ((+arg2.64 +arg2.63))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.66 +arg2.64)) (+ +arg1.62 +arg2.66))
             (body-matcher map-result.65) (map-result (map-result.65))
             (consumer (values)))))))
        (body-matcher map-result.61) (map-result (map-result.61))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.88 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.81
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.78 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.59
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.78) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.78) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.78)))
       (+arg2.60
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.81) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.81) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.81) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.81) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.81) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.81) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.81))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((+arg1.62 +arg1.59) (+arg2.63 +arg2.60) (map-mem.89 map-mem.88))
          (let ((+arg2.64 +arg2.63))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.66 +arg2.64) (map-mem.90 map-mem.89))
               (let ((expr-result.91 (+ +arg1.62 +arg2.66)))
                (begin (putmem expr-result.91 map-mem.90) expr-result.91)))
              (body-matcher map-result.65) (map-result (map-result.65))
              (map-result-mem-interim (values map-mem.89))
              (map-result-mem-final (values map-mem.89)) (consumer (values)))))))
         (body-matcher map-result.61) (map-result (map-result.61))
         (map-result-mem-interim (values map-mem.88))
         (map-result-mem-final (values map-mem.88)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.88 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.81
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.78 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.59
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.78) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.78) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.78)))
       (+arg2.60
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.81) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.81) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.81) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.81) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.81) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.81) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.81))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((+arg1.62 +arg1.59) (+arg2.63 +arg2.60) (map-mem.89 map-mem.88))
          (let ((+arg2.64 +arg2.63))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.66 +arg2.64) (map-mem.90 map-mem.89))
               (let ((expr-result.91 (+ +arg1.62 +arg2.66)))
                (begin (putmem expr-result.91 map-mem.90) expr-result.91)))
              (body-matcher map-result.65) (map-result (map-result.65))
              (map-result-mem-interim (values map-mem.89))
              (map-result-mem-final (values map-mem.89)) (consumer (values)))))))
         (body-matcher map-result.61) (map-result (map-result.61))
         (map-result-mem-interim (values map-mem.88))
         (map-result-mem-final (values map-mem.88)) (consumer (values))))))) |}]
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
     ((binding ((name words) (id 44)))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 45)) 1)))))))))))))))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 45)) 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings ((((name len) (id 46)) Dim)))
         (valueBinding ((name word) (id 47)))
         (box (Ref ((id ((name words) (id 44))))))
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
                       ((Dimension ((const 0) (refs ((((name len) (id 46)) 1)))))
                        (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id ((name word) (id 47)))))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name words) (id 44)))
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
                       ((Add ((const 0) (refs ((((name len) (id 45)) 1)))))))))))))))
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
                       ((Add ((const 0) (refs ((((name len) (id 45)) 1)))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name box) (id 53)))
            (value (Ref ((id ((name words) (id 44)))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 46)))
               (indexValue
                (FromBox (box (Ref ((id ((name box) (id 53)))))) (i 0)))
               (sort Dim))))
            (body
             (Map
              ((args
                (((binding ((name word) (id 47)))
                  (value (BoxValue ((box (Ref ((id ((name box) (id 53))))))))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 52)))
                     (value (Primitive ((name (Func Equal))))))
                    ((binding ((name =arg1) (id 48)))
                     (value (Scalar ((element (Literal (IntLiteral 3)))))))
                    ((binding ((name =arg2) (id 51)))
                     (value
                      (Map
                       ((args
                         (((binding ((name f) (id 50)))
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
                                    ((const 0) (refs ((((name len) (id 46)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal CharacterLiteral))))))))
                          ((binding ((name arr) (id 49)))
                           (value (Ref ((id ((name word) (id 47)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 50))))))
                           (args (((id ((name arr) (id 49))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 52))))))
                     (args
                      (((id ((name =arg1) (id 48))))
                       ((id ((name =arg2) (id 51))))))
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
       (((binding ((name words) (id 59)))
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
                      ((Add ((const 0) (refs ((((name len) (id 45)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 45))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 45)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 45))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 45)) 1))))))))))))
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
                      ((Add ((const 0) (refs ((((name len) (id 45)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 45))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 45)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 45))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 45)) 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 45))) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 45)) 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name box) (id 60)))
            (value
             (Ref
              ((id ((name words) (id 59)))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 45))) (bound Dim))))
                    (body
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 45)) 1))))))))))))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 46)))
               (indexValue
                (FromBox
                 (box
                  (Ref
                   ((id ((name box) (id 60)))
                    (type'
                     ((element
                       (Sigma
                        ((parameters
                          (((binding ((name len) (id 45))) (bound Dim))))
                         (body
                          ((element (Literal CharacterLiteral))
                           (shape
                            ((Add ((const 0) (refs ((((name len) (id 45)) 1))))))))))))
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
                   (((binding ((name f) (id 54)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name =arg1) (id 55)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 3)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name =arg2) (id 58)))
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
                          ((index
                            (Dimension
                             ((const 0) (refs ((((name len) (id 46)) 1))))))
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
                              ((id ((name =arg1) (id 55)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name =arg2) (id 58)))
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
     ((words.61 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (let ((words.59 words.61))
      (let ((box.62 words.59))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((box.64 box.62))
           (let ((box.60 box.64))
            (index-let ((len.46 box-index-0 box.60))
             (let ()
              (let ()
               (let
                ((f.65 (values)) (=arg1.66 3)
                 (=arg2.68
                  (let ((f.67 (values)))
                   (let ((f.57 f.67)) (reify-index len.46)))))
                (let ((f.54 f.65) (=arg1.55 =arg1.66) (=arg2.58 =arg2.68))
                 (= =arg1.55 =arg2.58))))))))
          (body-matcher map-result.63) (map-result (map-result.63))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((box.62 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.64 box.62))
         (index-let ((len.46 box-index-0 box.64)) (= 3 (reify-index len.46))))
        (body-matcher map-result.63) (map-result (map-result.63))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((box.62 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.64 box.62))
         (index-let ((len.46 box-index-0 box.64)) (= 3 (reify-index len.46))))
        (body-matcher map-result.63) (map-result (map-result.63))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.80 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.76
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.71
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.70
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 45))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.45))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.62
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.71) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.71) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.71) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.71)))
           (index (mem frame-array.70) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 45))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.45)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.76) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.76) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.76)))
           (index (mem frame-array.70) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 45))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.45))))))))))))
         (getmem frame-array.70))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.64 box.62) (map-mem.81 map-mem.80))
          (index-let ((len.46 box-index-0 box.64))
           (let ((expr-result.82 (= 3 (reify-dimension-index len.46))))
            (begin (putmem expr-result.82 map-mem.81) expr-result.82))))
         (body-matcher map-result.63) (map-result (map-result.63))
         (map-result-mem-interim (values map-mem.80))
         (map-result-mem-final (values map-mem.80)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.80 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.76
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.71
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.70
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 45))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.45))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.62
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.71) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.71) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.71) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.71)))
           (index (mem frame-array.70) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 45))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.45)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.76) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.76) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.76)))
           (index (mem frame-array.70) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 45))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.45))))))))))))
         (getmem frame-array.70))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.64 box.62) (map-mem.81 map-mem.80))
          (index-let ((len.46 box-index-0 box.64))
           (let ((expr-result.82 (= 3 (reify-dimension-index len.46))))
            (begin (putmem expr-result.82 map-mem.81) expr-result.82))))
         (body-matcher map-result.63) (map-result (map-result.63))
         (map-result-mem-interim (values map-mem.80))
         (map-result-mem-final (values map-mem.80)) (consumer (values))))))) |}]
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
     ((binding ((name sum-row) (id 44)))
      (value
       (Scalar
        ((element
          (IndexLambda
           ((params (((binding ((name d-1) (id 45))) (bound Dim))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name row) (id 46)))
                     (bound
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape
                         ((Add ((const 1) (refs ((((name d-1) (id 45)) 1)))))))))))))
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
                              ((const 0) (refs ((((name d-1) (id 45)) 1)))))
                             (Shape ()))))))
                        (args ((Atom (Literal IntLiteral)))))))
                     (args
                      ((Primitive ((name (Func Add))))
                       (Ref ((id ((name row) (id 46))))))))))))))))))))))
      (body
       (TermApplication
        ((func
          (IndexApplication
           ((iFunc (Ref ((id ((name sum-row) (id 44))))))
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
       (((binding ((name sum-row) (id 44)))
         (value
          (Scalar
           ((element
             (IndexLambda
              ((params (((binding ((name d-1) (id 45))) (bound Dim))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name row) (id 46)))
                        (bound
                         (Arr
                          ((element (Literal IntLiteral))
                           (shape
                            ((Add ((const 1) (refs ((((name d-1) (id 45)) 1)))))))))))))
                     (body
                      (Map
                       ((args
                         (((binding ((name f) (id 49)))
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
                                    ((const 0) (refs ((((name d-1) (id 45)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal IntLiteral))))))))
                          ((binding ((name reduce-f-arg) (id 47)))
                           (value (Primitive ((name (Func Add))))))
                          ((binding ((name reduce-array-arg) (id 48)))
                           (value (Ref ((id ((name row) (id 46)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 49))))))
                           (args
                            (((id ((name reduce-f-arg) (id 47))))
                             ((id ((name reduce-array-arg) (id 48))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 51)))
            (value
             (IndexApplication
              ((iFunc (Ref ((id ((name sum-row) (id 44))))))
               (args ((Dimension ((const 9) (refs ())))))))))
           ((binding ((name row) (id 50)))
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
             (((binding ((name row) (id 52)))
               (value (Ref ((id ((name row) (id 50)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 51))))))
               (args (((id ((name row) (id 52))))))
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
       (((binding ((name sum-row) (id 54)))
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
             (Ref
              ((id ((name sum-row) (id 54)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name row) (id 61)))
            (value
             (ArrayPrimitive
              (Map
               (frameShape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
               (args ()) (iotaVar (((name iota) (id 60))))
               (body
                (Ref
                 ((id ((name iota) (id 60)))
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
             (((binding ((name row) (id 62)))
               (value
                (Ref
                 ((id ((name row) (id 61)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape
                     ((Add ((const 1000000) (refs ())))
                      (Add ((const 10) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 56)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name reduce-f-arg) (id 57)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name reduce-array-arg) (id 63)))
                  (value
                   (Ref
                    ((id ((name row) (id 62)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 10) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Reduce
                  (arg
                   ((firstBinding ((name reduce-arg1) (id 64)))
                    (secondBinding ((name reduce-arg2) (id 65)))
                    (value
                     (Ref
                      ((id ((name reduce-array-arg) (id 63)))
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
                              ((id ((name reduce-arg1) (id 64)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name reduce-arg2) (id 65)))
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
    (let ((sum-row.66 (values)))
     (let ((sum-row.54 sum-row.66))
      (let
       ((f.67 sum-row.54)
        (row.72
         (let ()
          (#0
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota iota.69)
              (#0
               (#0
                (loop-block (frame-shape 10)
                 (map () (iota (iota.71 : iota.69))
                  (let ((iota.60 iota.71)) iota.60))
                 (body-matcher map-result.70) (map-result (map-result.70))
                 (consumer (values))))))
             (body-matcher map-result.68) (map-result (map-result.68))
             (consumer (values))))))))
       (let ((f.55 f.67) (row.61 row.72))
        (let ((row.73 row.61))
         (#0
          (#0
           (loop-block (frame-shape 1000000)
            (map ((row.75 row.73))
             (let ((row.62 row.75))
              (let
               ((f.76 (values)) (reduce-f-arg.77 (values))
                (reduce-array-arg.78 row.62))
               (let
                ((f.56 f.76) (reduce-f-arg.57 reduce-f-arg.77)
                 (reduce-array-arg.63 reduce-array-arg.78))
                (let ((reduce-arg.80 reduce-array-arg.63))
                 (#1
                  (loop-block (frame-shape 10)
                   (map ((reduce-arg.81 reduce-arg.80)) (values reduce-arg.81))
                   (body-matcher (reduce-arg.79)) (map-result ())
                   (consumer
                    (reduce (reduce-arg1.64 reduce-arg2.65 reduce-arg.79)
                     (+ reduce-arg1.64 reduce-arg2.65))))))))))
            (body-matcher map-result.74) (map-result (map-result.74))
            (consumer (values))))))))))
    Result of stage Fuse and Simplify:
    (#0
     (#0
      (loop-block (frame-shape 1000000)
       (map () (iota iota.69)
        (#1
         (loop-block (frame-shape 10) (map () (iota (iota.71 : iota.69)) iota.71)
          (body-matcher reduce-arg.79) (map-result ())
          (consumer
           (reduce (reduce-arg1.64 reduce-arg2.65 reduce-arg.79)
            (+ reduce-arg1.64 reduce-arg2.65))))))
       (body-matcher map-result.74) (map-result (map-result.74))
       (consumer (values)))))
    Result of stage Kernelize:
    (#0
     (kernel (blocks 320) (threads 32)
      (map-kernel (frame-shape 1000000) () (iota iota.69)
       (body-matcher map-result.74) (map-result (map-result.74))
       (#1
        (loop-block (frame-shape 10) (map () (iota (iota.71 : iota.69)) iota.71)
         (body-matcher reduce-arg.79) (map-result ())
         (consumer
          (reduce (reduce-arg1.64 reduce-arg2.65 reduce-arg.79)
           (+ reduce-arg1.64 reduce-arg2.65))))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.121 (Tuple ()) device)
      (map-mem.116
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.115
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host))
     (#0
      (begin
       (kernel (blocks 320) (threads 32)
        ((map
          (map-kernel (frame-shape 1000000)
           ((map-mem.117 map-mem.116) (map-mem.118 map-mem.121)) (iota iota.69)
           (putmem
            (loop (frame-shape 10)
             (map ((map-mem.119 map-mem.118)) (iota (iota.71 : iota.69)) iota.71)
             (body-matcher reduce-arg.79) (map-result ())
             (map-result-mem-interim map-mem.118)
             (map-result-mem-final map-mem.118)
             (consumer
              (reduce (reduce-arg1.64 reduce-arg2.65 reduce-arg.79)
               (+ reduce-arg1.64 reduce-arg2.65))))
            (#0 map-mem.117))))
         (map-result-mem-interim map-mem.116)
         (map-result-mem-final (values map-mem-result.0.115))))
       (getmem (values map-mem-result.0.115)))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.121 (Tuple ()) device)
      (map-mem.116
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.115
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host))
     (#0
      (begin
       (kernel captures
        ((expr-captures ())
         (mem-captures
          ((((name map-mem) (id 116))
            (Tuple
             ((Array ((element (Literal IntLiteral)) (shape (shape 1000000)))))))
           (((name map-mem) (id 121)) (Tuple ()))))
         (index-captures ()))
        (blocks 320) (threads 32)
        ((map
          (map-kernel (frame-shape 1000000)
           ((map-mem.117 map-mem.116) (map-mem.118 map-mem.121)) (iota iota.69)
           (putmem
            (loop (frame-shape 10)
             (map ((map-mem.119 map-mem.118)) (iota (iota.71 : iota.69)) iota.71)
             (body-matcher reduce-arg.79) (map-result ())
             (map-result-mem-interim map-mem.118)
             (map-result-mem-final map-mem.118)
             (consumer
              (reduce (reduce-arg1.64 reduce-arg2.65 reduce-arg.79)
               (+ reduce-arg1.64 reduce-arg2.65))))
            (#0 map-mem.117))))
         (map-result-mem-interim map-mem.116)
         (map-result-mem-final (values map-mem-result.0.115))))
       (getmem (values map-mem-result.0.115))))) |}]
;;
