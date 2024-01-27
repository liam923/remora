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
       (((binding ((name f) (id 134))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 132)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 133)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 134))))))
         (args (((id ((name +arg1) (id 132)))) ((id ((name +arg2) (id 133))))))
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
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
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((f.138 (values)) (+arg1.139 1) (+arg2.140 2))
     (let ((f.135 f.138) (+arg1.136 +arg1.139) (+arg2.137 +arg2.140))
      (+ +arg1.136 +arg2.137)))
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
     ((binding ((name add) (id 132)))
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding ((name x) (id 133)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding ((name y) (id 134)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args
                ((Ref ((id ((name x) (id 133)))))
                 (Ref ((id ((name y) (id 134))))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name add) (id 132))))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name add) (id 132)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name x) (id 133)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name y) (id 134)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 137)))
                     (value (Primitive ((name (Func Add))))))
                    ((binding ((name +arg1) (id 135)))
                     (value (Ref ((id ((name x) (id 133)))))))
                    ((binding ((name +arg2) (id 136)))
                     (value (Ref ((id ((name y) (id 134)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 137))))))
                     (args
                      (((id ((name +arg1) (id 135))))
                       ((id ((name +arg2) (id 136))))))
                     (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 140)))
            (value (Ref ((id ((name add) (id 132)))))))
           ((binding ((name x) (id 138)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding ((name y) (id 139)))
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 140))))))
            (args (((id ((name x) (id 138)))) ((id ((name y) (id 139))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name add) (id 142)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 143)))
            (value
             (Ref
              ((id ((name add) (id 142)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 150)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name y) (id 152)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 149)))
               (value
                (AtomAsArray
                 ((element (Values ((elements ()) (type' ()))))
                  (type' ((element (Tuple ())) (shape ())))))))
              ((binding ((name +arg1) (id 151)))
               (value
                (Ref
                 ((id ((name x) (id 150)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name +arg2) (id 153)))
               (value
                (Ref
                 ((id ((name y) (id 152)))
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
                        ((id ((name +arg1) (id 151)))
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
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((add.154 (values)))
     (let ((add.142 add.154))
      (let ((f.155 add.142) (x.156 5) (y.157 10))
       (let ((f.143 f.155) (x.150 x.156) (y.152 y.157))
        (let ((f.158 (values)) (+arg1.159 x.150) (+arg2.160 y.152))
         (let ((f.149 f.158) (+arg1.151 +arg1.159) (+arg2.153 +arg2.160))
          (+ +arg1.151 +arg2.153)))))))
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
     ((binding ((name id) (id 132)))
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @t) (id 133))) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name e) (id 134)))
                     (bound (ArrayRef ((name @t) (id 133)))))))
                  (body (Ref ((id ((name e) (id 134))))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id ((name id) (id 132))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name id) (id 132)))
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding ((name @t) (id 133))) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name e) (id 134)))
                        (bound (ArrayRef ((name @t) (id 133)))))))
                     (body (Ref ((id ((name e) (id 134)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 136)))
            (value
             (TypeApplication
              ((tFunc (Ref ((id ((name id) (id 132))))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding ((name e) (id 135)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 136))))))
            (args (((id ((name e) (id 135))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
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
           ((binding ((name e) (id 141)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name e) (id 141)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((id.142 (values)))
     (let ((id.138 id.142))
      (let ((f.143 id.138) (e.144 5)) (let ((f.139 f.143) (e.141 e.144)) e.141))))
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
       (((binding ((name f) (id 134))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 132)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding ((name +arg2) (id 133)))
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
          (((binding ((name +arg1) (id 135)))
            (value (Ref ((id ((name +arg1) (id 132)))))))
           ((binding ((name +arg2) (id 136)))
            (value (Ref ((id ((name +arg2) (id 133)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg2) (id 137)))
               (value (Ref ((id ((name +arg2) (id 136)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 134))))))
               (args
                (((id ((name +arg1) (id 135)))) ((id ((name +arg2) (id 137))))))
               (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
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
       (((binding ((name f) (id 138)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 139)))
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
        ((binding ((name +arg2) (id 141)))
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
          (((binding ((name +arg1) (id 140)))
            (value
             (Ref
              ((id ((name +arg1) (id 139)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding ((name +arg2) (id 142)))
            (value
             (Ref
              ((id ((name +arg2) (id 141)))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 143)))
               (value
                (Ref
                 ((id ((name +arg2) (id 142)))
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
                        ((id ((name +arg1) (id 140)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 143)))
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
     ((f.144 (values)) (+arg1.145 (frame 1 2))
      (+arg2.146 (frame (frame 3 4 5) (frame 6 7 8))))
     (let ((f.138 f.144) (+arg1.139 +arg1.145) (+arg2.141 +arg2.146))
      (let ((+arg1.147 +arg1.139) (+arg2.148 +arg2.141))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.150 +arg1.147) (+arg2.151 +arg2.148))
           (let ((+arg1.140 +arg1.150) (+arg2.142 +arg2.151))
            (let ((+arg2.152 +arg2.142))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg2.154 +arg2.152))
                 (let ((+arg2.143 +arg2.154)) (+ +arg1.140 +arg2.143)))
                (body-matcher map-result.153) (map-result (map-result.153))
                (consumer (values))))))))
          (body-matcher map-result.149) (map-result (map-result.149))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((+arg1.147 (frame 1 2)) (+arg2.148 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.150 +arg1.147) (+arg2.151 +arg2.148))
         (let ((+arg2.152 +arg2.151))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.154 +arg2.152)) (+ +arg1.150 +arg2.154))
             (body-matcher map-result.153) (map-result (map-result.153))
             (consumer (values)))))))
        (body-matcher map-result.149) (map-result (map-result.149))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((+arg1.147 (frame 1 2)) (+arg2.148 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.150 +arg1.147) (+arg2.151 +arg2.148))
         (let ((+arg2.152 +arg2.151))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.154 +arg2.152)) (+ +arg1.150 +arg2.154))
             (body-matcher map-result.153) (map-result (map-result.153))
             (consumer (values)))))))
        (body-matcher map-result.149) (map-result (map-result.149))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.176 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.169
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.166 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.147
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.166) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.166) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.166)))
       (+arg2.148
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.169) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.169) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.169) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.169) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.169) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.169) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.169))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.150 +arg1.147) (+arg2.151 +arg2.148) (map-mem.177 map-mem.176))
          (let ((+arg2.152 +arg2.151))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.154 +arg2.152) (map-mem.178 map-mem.177))
               (let ((expr-result.179 (+ +arg1.150 +arg2.154)))
                (begin (putmem expr-result.179 map-mem.178) expr-result.179)))
              (body-matcher map-result.153) (map-result (map-result.153))
              (map-result-mem-final (values map-mem.177)) (consumer (values)))))))
         (body-matcher map-result.149) (map-result (map-result.149))
         (map-result-mem-final (values map-mem.176)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.176 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.169
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.166 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.147
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.166) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.166) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.166)))
       (+arg2.148
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.169) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.169) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.169) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.169) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.169) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.169) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.169))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.150 +arg1.147) (+arg2.151 +arg2.148) (map-mem.177 map-mem.176))
          (let ((+arg2.152 +arg2.151))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.154 +arg2.152) (map-mem.178 map-mem.177))
               (let ((expr-result.179 (+ +arg1.150 +arg2.154)))
                (begin (putmem expr-result.179 map-mem.178) expr-result.179)))
              (body-matcher map-result.153) (map-result (map-result.153))
              (map-result-mem-final (values map-mem.177)) (consumer (values)))))))
         (body-matcher map-result.149) (map-result (map-result.149))
         (map-result-mem-final (values map-mem.176)) (consumer (values))))))) |}]
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
     ((binding ((name words) (id 132)))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 133)) 1)))))))))))))))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 133)) 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings ((((name len) (id 134)) Dim)))
         (valueBinding ((name word) (id 135)))
         (box (Ref ((id ((name words) (id 132))))))
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
                       ((Dimension
                         ((const 0) (refs ((((name len) (id 134)) 1)))))
                        (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id ((name word) (id 135)))))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name words) (id 132)))
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
                       ((Add ((const 0) (refs ((((name len) (id 133)) 1)))))))))))))))
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
                       ((Add ((const 0) (refs ((((name len) (id 133)) 1)))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name box) (id 141)))
            (value (Ref ((id ((name words) (id 132)))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 134)))
               (indexValue
                (FromBox (box (Ref ((id ((name box) (id 141)))))) (i 0)))
               (sort Dim))))
            (body
             (Map
              ((args
                (((binding ((name word) (id 135)))
                  (value (BoxValue ((box (Ref ((id ((name box) (id 141))))))))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 140)))
                     (value (Primitive ((name (Func Equal))))))
                    ((binding ((name =arg1) (id 136)))
                     (value (Scalar ((element (Literal (IntLiteral 3)))))))
                    ((binding ((name =arg2) (id 139)))
                     (value
                      (Map
                       ((args
                         (((binding ((name f) (id 138)))
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
                                    ((const 0)
                                     (refs ((((name len) (id 134)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal CharacterLiteral))))))))
                          ((binding ((name arr) (id 137)))
                           (value (Ref ((id ((name word) (id 135)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 138))))))
                           (args (((id ((name arr) (id 137))))))
                           (type'
                            (Arr ((element (Literal IntLiteral)) (shape ())))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 140))))))
                     (args
                      (((id ((name =arg1) (id 136))))
                       ((id ((name =arg2) (id 139))))))
                     (type'
                      (Arr ((element (Literal BooleanLiteral)) (shape ())))))))
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
       (((binding ((name words) (id 148)))
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
                      ((Add ((const 0) (refs ((((name len) (id 133)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 133))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 133)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 133))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 133)) 1))))))))))))
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
                      ((Add ((const 0) (refs ((((name len) (id 133)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 133))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 133)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 133))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 133)) 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 133))) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 133)) 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name box) (id 149)))
            (value
             (Ref
              ((id ((name words) (id 148)))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 133))) (bound Dim))))
                    (body
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 133)) 1))))))))))))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 134)))
               (indexValue
                (FromBox
                 (box
                  (Ref
                   ((id ((name box) (id 149)))
                    (type'
                     ((element
                       (Sigma
                        ((parameters
                          (((binding ((name len) (id 133))) (bound Dim))))
                         (body
                          ((element (Literal CharacterLiteral))
                           (shape
                            ((Add ((const 0) (refs ((((name len) (id 133)) 1))))))))))))
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
                   (((binding ((name f) (id 142)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name =arg1) (id 143)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 3)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name =arg2) (id 147)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 145)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ReifyIndex
                          ((index
                            (Dimension
                             ((const 0) (refs ((((name len) (id 134)) 1))))))
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
                              ((id ((name =arg1) (id 143)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name =arg2) (id 147)))
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
     ((words.150 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (let ((words.148 words.150))
      (let ((box.151 words.148))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((box.153 box.151))
           (let ((box.149 box.153))
            (index-let ((len.134 box-index-0 box.149))
             (let ()
              (let ()
               (let
                ((f.154 (values)) (=arg1.155 3)
                 (=arg2.157
                  (let ((f.156 (values)))
                   (let ((f.145 f.156)) (reify-index len.134)))))
                (let ((f.142 f.154) (=arg1.143 =arg1.155) (=arg2.147 =arg2.157))
                 (= =arg1.143 =arg2.147))))))))
          (body-matcher map-result.152) (map-result (map-result.152))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((box.151 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.153 box.151))
         (index-let ((len.134 box-index-0 box.153)) (= 3 (reify-index len.134))))
        (body-matcher map-result.152) (map-result (map-result.152))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((box.151 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.153 box.151))
         (index-let ((len.134 box-index-0 box.153)) (= 3 (reify-index len.134))))
        (body-matcher map-result.152) (map-result (map-result.152))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.169 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.165
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.160
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.159
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 133))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.133))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.151
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.160) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.160) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.160) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.160)))
           (index (mem frame-array.159) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 133))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.133)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.165) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.165) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.165)))
           (index (mem frame-array.159) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 133))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.133))))))))))))
         (getmem frame-array.159))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.153 box.151) (map-mem.170 map-mem.169))
          (index-let ((len.134 box-index-0 box.153))
           (let ((expr-result.171 (= 3 (reify-dimension-index len.134))))
            (begin (putmem expr-result.171 map-mem.170) expr-result.171))))
         (body-matcher map-result.152) (map-result (map-result.152))
         (map-result-mem-final (values map-mem.169)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.169 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.165
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.160
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.159
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 133))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.133))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.151
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.160) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.160) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.160) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.160)))
           (index (mem frame-array.159) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 133))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.133)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.165) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.165) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.165)))
           (index (mem frame-array.159) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 133))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.133))))))))))))
         (getmem frame-array.159))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.153 box.151) (map-mem.170 map-mem.169))
          (index-let ((len.134 box-index-0 box.153))
           (let ((expr-result.171 (= 3 (reify-dimension-index len.134))))
            (begin (putmem expr-result.171 map-mem.170) expr-result.171))))
         (body-matcher map-result.152) (map-result (map-result.152))
         (map-result-mem-final (values map-mem.169)) (consumer (values))))))) |}]
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
     ((binding ((name sum-row) (id 132)))
      (value
       (Scalar
        ((element
          (IndexLambda
           ((params (((binding ((name d-1) (id 133))) (bound Dim))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name row) (id 134)))
                     (bound
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape
                         ((Add ((const 1) (refs ((((name d-1) (id 133)) 1)))))))))))))
                  (body
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
                                  (((binding ((name d-1) (id 64))) (bound Dim))
                                   ((binding ((name @item-pad) (id 65)))
                                    (bound Shape))
                                   ((binding ((name @cell-shape) (id 66)))
                                    (bound Shape))))
                                 (body
                                  (Scalar
                                   ((element
                                     (TypeLambda
                                      ((params
                                        (((binding ((name t) (id 67)))
                                          (bound Atom))))
                                       (body
                                        (Scalar
                                         ((element
                                           (TermLambda
                                            ((params
                                              (((binding ((name op) (id 68)))
                                                (bound
                                                 (Arr
                                                  ((element
                                                    (Func
                                                     ((parameters
                                                       ((Arr
                                                         ((element
                                                           (AtomRef
                                                            ((name t) (id 67))))
                                                          (shape
                                                           ((ShapeRef
                                                             ((name @cell-shape)
                                                              (id 66)))))))
                                                        (Arr
                                                         ((element
                                                           (AtomRef
                                                            ((name t) (id 67))))
                                                          (shape
                                                           ((ShapeRef
                                                             ((name @cell-shape)
                                                              (id 66)))))))))
                                                      (return
                                                       (Arr
                                                        ((element
                                                          (AtomRef
                                                           ((name t) (id 67))))
                                                         (shape
                                                          ((ShapeRef
                                                            ((name @cell-shape)
                                                             (id 66)))))))))))
                                                   (shape ())))))
                                               ((binding ((name arr) (id 69)))
                                                (bound
                                                 (Arr
                                                  ((element
                                                    (AtomRef ((name t) (id 67))))
                                                   (shape
                                                    ((Add
                                                      ((const 1)
                                                       (refs
                                                        ((((name d-1) (id 64)) 1)))))
                                                     (ShapeRef
                                                      ((name @item-pad) (id 65)))
                                                     (ShapeRef
                                                      ((name @cell-shape)
                                                       (id 66)))))))))))
                                             (body
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
                                                             (((binding
                                                                ((name d)
                                                                 (id 54)))
                                                               (bound Dim))
                                                              ((binding
                                                                ((name @item-pad)
                                                                 (id 55)))
                                                               (bound Shape))
                                                              ((binding
                                                                ((name
                                                                  @cell-shape)
                                                                 (id 56)))
                                                               (bound Shape))))
                                                            (body
                                                             (Scalar
                                                              ((element
                                                                (TypeLambda
                                                                 ((params
                                                                   (((binding
                                                                      ((name t)
                                                                       (id 57)))
                                                                     (bound Atom))))
                                                                  (body
                                                                   (Scalar
                                                                    ((element
                                                                      (TermLambda
                                                                       ((params
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 58)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (Func
                                                                        ((parameters
                                                                        ((Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))
                                                                        (return
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))))
                                                                        (shape
                                                                        ())))))
                                                                        ((binding
                                                                        ((name
                                                                        init)
                                                                        (id 59)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))
                                                                        ((binding
                                                                        ((name
                                                                        arr)
                                                                        (id 60)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d)
                                                                        (id 54))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))))
                                                                        (body
                                                                        (Let
                                                                        ((binding
                                                                        ((name
                                                                        up-ranked-f)
                                                                        (id 61)))
                                                                        (value
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name a)
                                                                        (id 62)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))
                                                                        ((binding
                                                                        ((name b)
                                                                        (id 63)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 58))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        ((name a)
                                                                        (id 62)))))
                                                                        (Ref
                                                                        ((id
                                                                        ((name b)
                                                                        (id 63))))))))))))))))
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
                                                                        (Reduce
                                                                        (character
                                                                        Reduce)))))))
                                                                        (args
                                                                        ((Dimension
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d)
                                                                        (id 54))
                                                                        1)))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56))))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57)))))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        ((name
                                                                        up-ranked-f)
                                                                        (id 61)))))
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
                                                                        (((binding
                                                                        ((name
                                                                        @s)
                                                                        (id 6)))
                                                                        (bound
                                                                        Shape))
                                                                        ((binding
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 7)))
                                                                        (bound
                                                                        Shape))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name t)
                                                                        (id 8)))
                                                                        (bound
                                                                        Atom))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name v)
                                                                        (id 9)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 8))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 7)))))))))))
                                                                        (body
                                                                        (Let
                                                                        ((binding
                                                                        ((name
                                                                        make)
                                                                        (id 10)))
                                                                        (value
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name
                                                                        foo)
                                                                        (id 11)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (Literal
                                                                        IntLiteral))
                                                                        (shape
                                                                        ())))))
                                                                        ((binding
                                                                        ((name v)
                                                                        (id 12)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 8))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 7)))))))))))
                                                                        (body
                                                                        (Ref
                                                                        ((id
                                                                        ((name v)
                                                                        (id 12))))))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        make)
                                                                        (id 10))))))
                                                                        (args
                                                                        ((IndexApplication
                                                                        ((iFunc
                                                                        (Primitive
                                                                        ((name
                                                                        (Val
                                                                        Iota)))))
                                                                        (args
                                                                        ((Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @s)
                                                                        (id 6)))))))))
                                                                        (Ref
                                                                        ((id
                                                                        ((name v)
                                                                        (id 9)))))))))))))))))))))))))))))))
                                                                        (args
                                                                        ((Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56))))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57)))))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        ((name
                                                                        init)
                                                                        (id 59)))))))))
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 60)))))))))))))))))))))))))))))))
                                                      (args
                                                       ((Dimension
                                                         ((const 0)
                                                          (refs
                                                           ((((name d-1) (id 64))
                                                             1)))))
                                                        (Shape
                                                         ((ShapeRef
                                                           ((name @item-pad)
                                                            (id 65)))))
                                                        (Shape
                                                         ((ShapeRef
                                                           ((name @cell-shape)
                                                            (id 66))))))))))
                                                   (args
                                                    ((Atom
                                                      (AtomRef
                                                       ((name t) (id 67)))))))))
                                                (args
                                                 ((Ref
                                                   ((id ((name op) (id 68)))))
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
                                                                 (((binding
                                                                    ((name d-1)
                                                                     (id 24)))
                                                                   (bound Dim))
                                                                  ((binding
                                                                    ((name
                                                                      @cell-shape)
                                                                     (id 25)))
                                                                   (bound Shape))))
                                                                (body
                                                                 (Scalar
                                                                  ((element
                                                                    (TypeLambda
                                                                     ((params
                                                                       (((binding
                                                                        ((name t)
                                                                        (id 26)))
                                                                        (bound
                                                                        Atom))))
                                                                      (body
                                                                       (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name
                                                                        arr)
                                                                        (id 27)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 26))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 24))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 25)))))))))))
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
                                                                        ContiguousSubArray)))))
                                                                        (args
                                                                        ((Shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 24))
                                                                        1)))))))
                                                                        (Shape
                                                                        ())
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 25)))))
                                                                        (Dimension
                                                                        ((const
                                                                        1)
                                                                        (refs ()))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 26)))))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 27)))))
                                                                        (Frame
                                                                        ((dimensions
                                                                        (1))
                                                                        (elements
                                                                        ((Scalar
                                                                        ((element
                                                                        (Literal
                                                                        (IntLiteral
                                                                        0))))))))))))))))))))))))))))))))
                                                          (args
                                                           ((Dimension
                                                             ((const 0)
                                                              (refs
                                                               ((((name d-1)
                                                                  (id 64))
                                                                 1)))))
                                                            (Shape
                                                             ((ShapeRef
                                                               ((name @item-pad)
                                                                (id 65)))
                                                              (ShapeRef
                                                               ((name
                                                                 @cell-shape)
                                                                (id 66))))))))))
                                                       (args
                                                        ((Atom
                                                          (AtomRef
                                                           ((name t) (id 67)))))))))
                                                    (args
                                                     ((Ref
                                                       ((id ((name arr) (id 69)))))))))
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
                                                                 (((binding
                                                                    ((name d-1)
                                                                     (id 28)))
                                                                   (bound Dim))
                                                                  ((binding
                                                                    ((name
                                                                      @cell-shape)
                                                                     (id 29)))
                                                                   (bound Shape))))
                                                                (body
                                                                 (Scalar
                                                                  ((element
                                                                    (TypeLambda
                                                                     ((params
                                                                       (((binding
                                                                        ((name t)
                                                                        (id 30)))
                                                                        (bound
                                                                        Atom))))
                                                                      (body
                                                                       (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name
                                                                        arr)
                                                                        (id 31)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 30))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 28))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 29)))))))))))
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
                                                                        ContiguousSubArray)))))
                                                                        (args
                                                                        ((Shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 28))
                                                                        1)))))))
                                                                        (Shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 28))
                                                                        1)))))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 29)))))
                                                                        (Dimension
                                                                        ((const
                                                                        1)
                                                                        (refs ()))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 30)))))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 31)))))
                                                                        (Frame
                                                                        ((dimensions
                                                                        (1))
                                                                        (elements
                                                                        ((Scalar
                                                                        ((element
                                                                        (Literal
                                                                        (IntLiteral
                                                                        1))))))))))))))))))))))))))))))))
                                                          (args
                                                           ((Dimension
                                                             ((const 0)
                                                              (refs
                                                               ((((name d-1)
                                                                  (id 64))
                                                                 1)))))
                                                            (Shape
                                                             ((ShapeRef
                                                               ((name @item-pad)
                                                                (id 65)))
                                                              (ShapeRef
                                                               ((name
                                                                 @cell-shape)
                                                                (id 66))))))))))
                                                       (args
                                                        ((Atom
                                                          (AtomRef
                                                           ((name t) (id 67)))))))))
                                                    (args
                                                     ((Ref
                                                       ((id ((name arr) (id 69))))))))))))))))))))))))))))))))
                           (args
                            ((Dimension
                              ((const 0) (refs ((((name d-1) (id 133)) 1)))))
                             (Shape ()) (Shape ()))))))
                        (args ((Atom (Literal IntLiteral)))))))
                     (args
                      ((Primitive ((name (Func Add))))
                       (Ref ((id ((name row) (id 134))))))))))))))))))))))
      (body
       (TermApplication
        ((func
          (IndexApplication
           ((iFunc (Ref ((id ((name sum-row) (id 132))))))
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
       (((binding ((name sum-row) (id 132)))
         (value
          (Scalar
           ((element
             (IndexLambda
              ((params (((binding ((name d-1) (id 133))) (bound Dim))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name row) (id 134)))
                        (bound
                         (Arr
                          ((element (Literal IntLiteral))
                           (shape
                            ((Add ((const 1) (refs ((((name d-1) (id 133)) 1)))))))))))))
                     (body
                      (Map
                       ((args
                         (((binding ((name f) (id 167)))
                           (value
                            (TypeApplication
                             ((tFunc
                               (IndexApplication
                                ((iFunc
                                  (Scalar
                                   ((element
                                     (IndexLambda
                                      ((params
                                        (((binding ((name d-1) (id 64)))
                                          (bound Dim))
                                         ((binding ((name @item-pad) (id 65)))
                                          (bound Shape))
                                         ((binding ((name @cell-shape) (id 66)))
                                          (bound Shape))))
                                       (body
                                        (Scalar
                                         ((element
                                           (TypeLambda
                                            ((params
                                              (((binding ((name t) (id 67)))
                                                (bound Atom))))
                                             (body
                                              (Scalar
                                               ((element
                                                 (TermLambda
                                                  ((params
                                                    (((binding
                                                       ((name op) (id 68)))
                                                      (bound
                                                       (Arr
                                                        ((element
                                                          (Func
                                                           ((parameters
                                                             ((Arr
                                                               ((element
                                                                 (AtomRef
                                                                  ((name t)
                                                                   (id 67))))
                                                                (shape
                                                                 ((ShapeRef
                                                                   ((name
                                                                     @cell-shape)
                                                                    (id 66)))))))
                                                              (Arr
                                                               ((element
                                                                 (AtomRef
                                                                  ((name t)
                                                                   (id 67))))
                                                                (shape
                                                                 ((ShapeRef
                                                                   ((name
                                                                     @cell-shape)
                                                                    (id 66)))))))))
                                                            (return
                                                             (Arr
                                                              ((element
                                                                (AtomRef
                                                                 ((name t)
                                                                  (id 67))))
                                                               (shape
                                                                ((ShapeRef
                                                                  ((name
                                                                    @cell-shape)
                                                                   (id 66)))))))))))
                                                         (shape ())))))
                                                     ((binding
                                                       ((name arr) (id 69)))
                                                      (bound
                                                       (Arr
                                                        ((element
                                                          (AtomRef
                                                           ((name t) (id 67))))
                                                         (shape
                                                          ((Add
                                                            ((const 1)
                                                             (refs
                                                              ((((name d-1)
                                                                 (id 64))
                                                                1)))))
                                                           (ShapeRef
                                                            ((name @item-pad)
                                                             (id 65)))
                                                           (ShapeRef
                                                            ((name @cell-shape)
                                                             (id 66)))))))))))
                                                   (body
                                                    (Map
                                                     ((args
                                                       (((binding
                                                          ((name f) (id 165)))
                                                         (value
                                                          (TypeApplication
                                                           ((tFunc
                                                             (IndexApplication
                                                              ((iFunc
                                                                (Scalar
                                                                 ((element
                                                                   (IndexLambda
                                                                    ((params
                                                                      (((binding
                                                                        ((name d)
                                                                        (id 54)))
                                                                        (bound
                                                                        Dim))
                                                                       ((binding
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (bound
                                                                        Shape))
                                                                       ((binding
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))
                                                                        (bound
                                                                        Shape))))
                                                                     (body
                                                                      (Scalar
                                                                       ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name t)
                                                                        (id 57)))
                                                                        (bound
                                                                        Atom))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 58)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (Func
                                                                        ((parameters
                                                                        ((Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))
                                                                        (return
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))))
                                                                        (shape
                                                                        ())))))
                                                                        ((binding
                                                                        ((name
                                                                        init)
                                                                        (id 59)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))
                                                                        ((binding
                                                                        ((name
                                                                        arr)
                                                                        (id 60)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d)
                                                                        (id 54))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name
                                                                        up-ranked-f)
                                                                        (id 61)))
                                                                        (value
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name a)
                                                                        (id 62)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))
                                                                        ((binding
                                                                        ((name b)
                                                                        (id 63)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 152)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 58)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg0)
                                                                        (id 150)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name a)
                                                                        (id 62)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg1)
                                                                        (id 151)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name b)
                                                                        (id 63)))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name
                                                                        arg0)
                                                                        (id 153)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arg0)
                                                                        (id 150)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg1)
                                                                        (id 154)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arg1)
                                                                        (id 151)))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 152))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        arg0)
                                                                        (id 153))))
                                                                        ((id
                                                                        ((name
                                                                        arg1)
                                                                        (id 154))))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))))
                                                                        (frameShape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56))))))))))))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 164)))
                                                                        (value
                                                                        (TypeApplication
                                                                        ((tFunc
                                                                        (IndexApplication
                                                                        ((iFunc
                                                                        (Primitive
                                                                        ((name
                                                                        (Func
                                                                        (Reduce
                                                                        (character
                                                                        Reduce)))))))
                                                                        (args
                                                                        ((Dimension
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d)
                                                                        (id 54))
                                                                        1)))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56))))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))))))))
                                                                        ((binding
                                                                        ((name
                                                                        reduce-f-arg)
                                                                        (id 155)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        up-ranked-f)
                                                                        (id 61)))))))
                                                                        ((binding
                                                                        ((name
                                                                        reduce-zero-arg)
                                                                        (id 162)))
                                                                        (value
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 161)))
                                                                        (value
                                                                        (TypeApplication
                                                                        ((tFunc
                                                                        (IndexApplication
                                                                        ((iFunc
                                                                        (Scalar
                                                                        ((element
                                                                        (IndexLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name
                                                                        @s)
                                                                        (id 6)))
                                                                        (bound
                                                                        Shape))
                                                                        ((binding
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 7)))
                                                                        (bound
                                                                        Shape))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name t)
                                                                        (id 8)))
                                                                        (bound
                                                                        Atom))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name v)
                                                                        (id 9)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 8))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 7)))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name
                                                                        make)
                                                                        (id 10)))
                                                                        (value
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name
                                                                        foo)
                                                                        (id 11)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (Literal
                                                                        IntLiteral))
                                                                        (shape
                                                                        ())))))
                                                                        ((binding
                                                                        ((name v)
                                                                        (id 12)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 8))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 7)))))))))))
                                                                        (body
                                                                        (Ref
                                                                        ((id
                                                                        ((name v)
                                                                        (id 12)))))))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 159)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        make)
                                                                        (id 10)))))))
                                                                        ((binding
                                                                        ((name
                                                                        foo)
                                                                        (id 157)))
                                                                        (value
                                                                        (IndexApplication
                                                                        ((iFunc
                                                                        (Primitive
                                                                        ((name
                                                                        (Val
                                                                        Iota)))))
                                                                        (args
                                                                        ((Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @s)
                                                                        (id 6)))))))))))
                                                                        ((binding
                                                                        ((name v)
                                                                        (id 158)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name v)
                                                                        (id 9)))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name
                                                                        foo)
                                                                        (id 160)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        foo)
                                                                        (id 157)))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 159))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        foo)
                                                                        (id 160))))
                                                                        ((id
                                                                        ((name v)
                                                                        (id 158))))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 8))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 7)))))))))))
                                                                        (frameShape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @s)
                                                                        (id 6)))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 8))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @s)
                                                                        (id 6)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 7)))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 8))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @s)
                                                                        (id 6)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 7)))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 8))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @s)
                                                                        (id 6)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 7)))))))))))))))))))))))))))))
                                                                        (args
                                                                        ((Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56))))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))))))))
                                                                        ((binding
                                                                        ((name v)
                                                                        (id 156)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        init)
                                                                        (id 59)))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 161))))))
                                                                        (args
                                                                        (((id
                                                                        ((name v)
                                                                        (id 156))))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56))))))))))))
                                                                        ((binding
                                                                        ((name
                                                                        reduce-array-arg)
                                                                        (id 163)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 60)))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 164))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        reduce-f-arg)
                                                                        (id 155))))
                                                                        ((id
                                                                        ((name
                                                                        reduce-zero-arg)
                                                                        (id 162))))
                                                                        ((id
                                                                        ((name
                                                                        reduce-array-arg)
                                                                        (id 163))))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 57))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 55)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 56)))))))))))))))))))))))))))))
                                                               (args
                                                                ((Dimension
                                                                  ((const 0)
                                                                   (refs
                                                                    ((((name d-1)
                                                                       (id 64))
                                                                      1)))))
                                                                 (Shape
                                                                  ((ShapeRef
                                                                    ((name
                                                                      @item-pad)
                                                                     (id 65)))))
                                                                 (Shape
                                                                  ((ShapeRef
                                                                    ((name
                                                                      @cell-shape)
                                                                     (id 66))))))))))
                                                            (args
                                                             ((Atom
                                                               (AtomRef
                                                                ((name t)
                                                                 (id 67))))))))))
                                                        ((binding
                                                          ((name f) (id 137)))
                                                         (value
                                                          (Ref
                                                           ((id
                                                             ((name op) (id 68)))))))
                                                        ((binding
                                                          ((name init) (id 143)))
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding
                                                                ((name f)
                                                                 (id 142)))
                                                               (value
                                                                (TypeApplication
                                                                 ((tFunc
                                                                   (IndexApplication
                                                                    ((iFunc
                                                                      (Scalar
                                                                       ((element
                                                                        (IndexLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name
                                                                        d-1)
                                                                        (id 24)))
                                                                        (bound
                                                                        Dim))
                                                                        ((binding
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 25)))
                                                                        (bound
                                                                        Shape))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name t)
                                                                        (id 26)))
                                                                        (bound
                                                                        Atom))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name
                                                                        arr)
                                                                        (id 27)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 26))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 24))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 25)))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 141)))
                                                                        (value
                                                                        (TypeApplication
                                                                        ((tFunc
                                                                        (IndexApplication
                                                                        ((iFunc
                                                                        (Primitive
                                                                        ((name
                                                                        (Func
                                                                        ContiguousSubArray)))))
                                                                        (args
                                                                        ((Shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 24))
                                                                        1)))))))
                                                                        (Shape
                                                                        ())
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 25)))))
                                                                        (Dimension
                                                                        ((const
                                                                        1)
                                                                        (refs ()))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 26))))))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 139)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 27)))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 140)))
                                                                        (value
                                                                        (Frame
                                                                        ((dimensions
                                                                        (1))
                                                                        (elements
                                                                        ((Scalar
                                                                        ((element
                                                                        (Literal
                                                                        (IntLiteral
                                                                        0)))))))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 141))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 139))))
                                                                        ((id
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 140))))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 26))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 25)))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 26))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 25)))))))))))))))))))))))))))))
                                                                     (args
                                                                      ((Dimension
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 64))
                                                                        1)))))
                                                                       (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 65)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 66))))))))))
                                                                  (args
                                                                   ((Atom
                                                                     (AtomRef
                                                                      ((name t)
                                                                       (id 67))))))))))
                                                              ((binding
                                                                ((name arr)
                                                                 (id 138)))
                                                               (value
                                                                (Ref
                                                                 ((id
                                                                   ((name arr)
                                                                    (id 69)))))))))
                                                            (body
                                                             (TermApplication
                                                              ((func
                                                                (Ref
                                                                 ((id
                                                                   ((name f)
                                                                    (id 142))))))
                                                               (args
                                                                (((id
                                                                   ((name arr)
                                                                    (id 138))))))
                                                               (type'
                                                                (Arr
                                                                 ((element
                                                                   (AtomRef
                                                                    ((name t)
                                                                     (id 67))))
                                                                  (shape
                                                                   ((ShapeRef
                                                                     ((name
                                                                       @item-pad)
                                                                      (id 65)))
                                                                    (ShapeRef
                                                                     ((name
                                                                       @cell-shape)
                                                                      (id 66)))))))))))
                                                            (frameShape ())
                                                            (type'
                                                             (Arr
                                                              ((element
                                                                (AtomRef
                                                                 ((name t)
                                                                  (id 67))))
                                                               (shape
                                                                ((ShapeRef
                                                                  ((name
                                                                    @item-pad)
                                                                   (id 65)))
                                                                 (ShapeRef
                                                                  ((name
                                                                    @cell-shape)
                                                                   (id 66))))))))))))
                                                        ((binding
                                                          ((name arr) (id 149)))
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding
                                                                ((name f)
                                                                 (id 148)))
                                                               (value
                                                                (TypeApplication
                                                                 ((tFunc
                                                                   (IndexApplication
                                                                    ((iFunc
                                                                      (Scalar
                                                                       ((element
                                                                        (IndexLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name
                                                                        d-1)
                                                                        (id 28)))
                                                                        (bound
                                                                        Dim))
                                                                        ((binding
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 29)))
                                                                        (bound
                                                                        Shape))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name t)
                                                                        (id 30)))
                                                                        (bound
                                                                        Atom))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name
                                                                        arr)
                                                                        (id 31)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 30))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 28))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 29)))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 147)))
                                                                        (value
                                                                        (TypeApplication
                                                                        ((tFunc
                                                                        (IndexApplication
                                                                        ((iFunc
                                                                        (Primitive
                                                                        ((name
                                                                        (Func
                                                                        ContiguousSubArray)))))
                                                                        (args
                                                                        ((Shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 28))
                                                                        1)))))))
                                                                        (Shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 28))
                                                                        1)))))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 29)))))
                                                                        (Dimension
                                                                        ((const
                                                                        1)
                                                                        (refs ()))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 30))))))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 145)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 31)))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 146)))
                                                                        (value
                                                                        (Frame
                                                                        ((dimensions
                                                                        (1))
                                                                        (elements
                                                                        ((Scalar
                                                                        ((element
                                                                        (Literal
                                                                        (IntLiteral
                                                                        1)))))))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 147))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 145))))
                                                                        ((id
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 146))))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 30))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 28))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 29)))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 30))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 28))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 29)))))))))))))))))))))))))))))
                                                                     (args
                                                                      ((Dimension
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 64))
                                                                        1)))))
                                                                       (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 65)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 66))))))))))
                                                                  (args
                                                                   ((Atom
                                                                     (AtomRef
                                                                      ((name t)
                                                                       (id 67))))))))))
                                                              ((binding
                                                                ((name arr)
                                                                 (id 144)))
                                                               (value
                                                                (Ref
                                                                 ((id
                                                                   ((name arr)
                                                                    (id 69)))))))))
                                                            (body
                                                             (TermApplication
                                                              ((func
                                                                (Ref
                                                                 ((id
                                                                   ((name f)
                                                                    (id 148))))))
                                                               (args
                                                                (((id
                                                                   ((name arr)
                                                                    (id 144))))))
                                                               (type'
                                                                (Arr
                                                                 ((element
                                                                   (AtomRef
                                                                    ((name t)
                                                                     (id 67))))
                                                                  (shape
                                                                   ((Add
                                                                     ((const 0)
                                                                      (refs
                                                                       ((((name
                                                                        d-1)
                                                                        (id 64))
                                                                        1)))))
                                                                    (ShapeRef
                                                                     ((name
                                                                       @item-pad)
                                                                      (id 65)))
                                                                    (ShapeRef
                                                                     ((name
                                                                       @cell-shape)
                                                                      (id 66)))))))))))
                                                            (frameShape ())
                                                            (type'
                                                             (Arr
                                                              ((element
                                                                (AtomRef
                                                                 ((name t)
                                                                  (id 67))))
                                                               (shape
                                                                ((Add
                                                                  ((const 0)
                                                                   (refs
                                                                    ((((name d-1)
                                                                       (id 64))
                                                                      1)))))
                                                                 (ShapeRef
                                                                  ((name
                                                                    @item-pad)
                                                                   (id 65)))
                                                                 (ShapeRef
                                                                  ((name
                                                                    @cell-shape)
                                                                   (id 66))))))))))))))
                                                      (body
                                                       (Map
                                                        ((args
                                                          (((binding
                                                             ((name init)
                                                              (id 166)))
                                                            (value
                                                             (Ref
                                                              ((id
                                                                ((name init)
                                                                 (id 143)))))))))
                                                         (body
                                                          (TermApplication
                                                           ((func
                                                             (Ref
                                                              ((id
                                                                ((name f)
                                                                 (id 165))))))
                                                            (args
                                                             (((id
                                                                ((name f)
                                                                 (id 137))))
                                                              ((id
                                                                ((name init)
                                                                 (id 166))))
                                                              ((id
                                                                ((name arr)
                                                                 (id 149))))))
                                                            (type'
                                                             (Arr
                                                              ((element
                                                                (AtomRef
                                                                 ((name t)
                                                                  (id 67))))
                                                               (shape
                                                                ((ShapeRef
                                                                  ((name
                                                                    @item-pad)
                                                                   (id 65)))
                                                                 (ShapeRef
                                                                  ((name
                                                                    @cell-shape)
                                                                   (id 66)))))))))))
                                                         (frameShape
                                                          ((ShapeRef
                                                            ((name @item-pad)
                                                             (id 65)))))
                                                         (type'
                                                          (Arr
                                                           ((element
                                                             (AtomRef
                                                              ((name t) (id 67))))
                                                            (shape
                                                             ((ShapeRef
                                                               ((name @item-pad)
                                                                (id 65)))
                                                              (ShapeRef
                                                               ((name @item-pad)
                                                                (id 65)))
                                                              (ShapeRef
                                                               ((name
                                                                 @cell-shape)
                                                                (id 66)))))))))))
                                                      (frameShape ())
                                                      (type'
                                                       (Arr
                                                        ((element
                                                          (AtomRef
                                                           ((name t) (id 67))))
                                                         (shape
                                                          ((ShapeRef
                                                            ((name @item-pad)
                                                             (id 65)))
                                                           (ShapeRef
                                                            ((name @item-pad)
                                                             (id 65)))
                                                           (ShapeRef
                                                            ((name @cell-shape)
                                                             (id 66)))))))))))))))))))))))))))))
                                 (args
                                  ((Dimension
                                    ((const 0)
                                     (refs ((((name d-1) (id 133)) 1)))))
                                   (Shape ()) (Shape ()))))))
                              (args ((Atom (Literal IntLiteral))))))))
                          ((binding ((name op) (id 135)))
                           (value (Primitive ((name (Func Add))))))
                          ((binding ((name arr) (id 136)))
                           (value (Ref ((id ((name row) (id 134)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 167))))))
                           (args
                            (((id ((name op) (id 135))))
                             ((id ((name arr) (id 136))))))
                           (type'
                            (Arr ((element (Literal IntLiteral)) (shape ())))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 169)))
            (value
             (IndexApplication
              ((iFunc (Ref ((id ((name sum-row) (id 132))))))
               (args ((Dimension ((const 9) (refs ())))))))))
           ((binding ((name row) (id 168)))
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
             (((binding ((name row) (id 170)))
               (value (Ref ((id ((name row) (id 168)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 169))))))
               (args (((id ((name row) (id 170))))))
               (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
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
       (((binding ((name sum-row) (id 172)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 173)))
            (value
             (Ref
              ((id ((name sum-row) (id 172)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name row) (id 320)))
            (value
             (ArrayPrimitive
              (Map
               (frameShape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
               (args ()) (iotaVar (((name iota) (id 319))))
               (body
                (Ref
                 ((id ((name iota) (id 319)))
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
             (((binding ((name row) (id 321)))
               (value
                (Ref
                 ((id ((name row) (id 320)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape
                     ((Add ((const 1000000) (refs ())))
                      (Add ((const 10) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 223)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name op) (id 309)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 322)))
                  (value
                   (Ref
                    ((id ((name row) (id 321)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 10) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 269)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name f) (id 310)))
                     (value
                      (Ref
                       ((id ((name op) (id 309)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name init) (id 359)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 350)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 356)))
                           (value
                            (Ref
                             ((id ((name arr) (id 322)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 10) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 355)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding
                               ((name contiguous-subarray-array) (id 357)))
                              (value
                               (Ref
                                ((id ((name arr) (id 356)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding
                               ((name contiguous-subarray-index) (id 358)))
                              (value
                               (Frame
                                ((dimensions (1))
                                 (elements
                                  ((AtomAsArray
                                    ((element (Literal (IntLiteral 0)))
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
                                ((id ((name contiguous-subarray-array) (id 357)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id ((name contiguous-subarray-index) (id 358)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 1) (refs ()))))))))))
                              (originalShape ((Add ((const 10) (refs ())))))
                              (resultShape ()) (cellShape ())
                              (l ((const 1) (refs ())))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name arr) (id 326)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 313)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 323)))
                           (value
                            (Ref
                             ((id ((name arr) (id 322)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 10) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 318)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding
                               ((name contiguous-subarray-array) (id 324)))
                              (value
                               (Ref
                                ((id ((name arr) (id 323)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding
                               ((name contiguous-subarray-index) (id 325)))
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
                                ((id ((name contiguous-subarray-array) (id 324)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id ((name contiguous-subarray-index) (id 325)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 1) (refs ()))))))))))
                              (originalShape ((Add ((const 10) (refs ())))))
                              (resultShape ((Add ((const 9) (refs ())))))
                              (cellShape ()) (l ((const 1) (refs ())))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 9) (refs ()))))))))))
                           (type'
                            ((element (Literal IntLiteral))
                             (shape ((Add ((const 9) (refs ()))))))))))
                        (type'
                         ((element (Literal IntLiteral))
                          (shape ((Add ((const 9) (refs ())))))))))))))
                  (body
                   (ArrayPrimitive
                    (Map (frameShape ())
                     (args
                      (((binding ((name init) (id 360)))
                        (value
                         (Ref
                          ((id ((name init) (id 359)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name up-ranked-f) (id 297)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 295)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding ((name reduce-f-arg) (id 298)))
                              (value
                               (Ref
                                ((id ((name up-ranked-f) (id 297)))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding ((name reduce-zero-arg) (id 363)))
                              (value
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding ((name f) (id 335)))
                                    (value
                                     (AtomAsArray
                                      ((element
                                        (Values ((elements ()) (type' ()))))
                                       (type' ((element (Tuple ())) (shape ())))))))
                                   ((binding ((name v) (id 361)))
                                    (value
                                     (Ref
                                      ((id ((name init) (id 360)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name make) (id 345)))
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
                                        (((binding ((name f) (id 346)))
                                          (value
                                           (Ref
                                            ((id ((name make) (id 345)))
                                             (type'
                                              ((element (Tuple ())) (shape ())))))))
                                         ((binding ((name v) (id 362)))
                                          (value
                                           (Ref
                                            ((id ((name v) (id 361)))
                                             (type'
                                              ((element (Literal IntLiteral))
                                               (shape ())))))))))
                                       (body
                                        (ArrayPrimitive
                                         (Map (frameShape ()) (args ())
                                          (body
                                           (Ref
                                            ((id ((name v) (id 362)))
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
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ())))))))
                             ((binding ((name reduce-array-arg) (id 327)))
                              (value
                               (Ref
                                ((id ((name arr) (id 326)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 9) (refs ())))))))))))))
                           (body
                            (ArrayPrimitive
                             (Reduce
                              (arg
                               ((firstBinding ((name reduce-arg1) (id 328)))
                                (secondBinding ((name reduce-arg2) (id 331)))
                                (value
                                 (Ref
                                  ((id ((name reduce-array-arg) (id 327)))
                                   (type'
                                    ((element (Literal IntLiteral))
                                     (shape ((Add ((const 9) (refs ()))))))))))))
                              (zero
                               (Ref
                                ((id ((name reduce-zero-arg) (id 363)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding ((name f) (id 311)))
                                    (value
                                     (Ref
                                      ((id ((name f) (id 310)))
                                       (type' ((element (Tuple ())) (shape ())))))))
                                   ((binding ((name arg0) (id 329)))
                                    (value
                                     (Ref
                                      ((id ((name reduce-arg1) (id 328)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))
                                   ((binding ((name arg1) (id 332)))
                                    (value
                                     (Ref
                                      ((id ((name reduce-arg2) (id 331)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name arg0) (id 330)))
                                       (value
                                        (Ref
                                         ((id ((name arg0) (id 329)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))
                                      ((binding ((name arg1) (id 333)))
                                       (value
                                        (Ref
                                         ((id ((name arg1) (id 332)))
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
                                                ((id ((name arg0) (id 330)))
                                                 (type'
                                                  ((element (Literal IntLiteral))
                                                   (shape ()))))))
                                              (type' (Literal IntLiteral))))
                                            (ArrayAsAtom
                                             ((array
                                               (Ref
                                                ((id ((name arg1) (id 333)))
                                                 (type'
                                                  ((element (Literal IntLiteral))
                                                   (shape ()))))))
                                              (type' (Literal IntLiteral))))))
                                          (type' (Literal IntLiteral)))))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ()))))))
                                    (type'
                                     ((element (Literal IntLiteral)) (shape ()))))))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (d ((const 9) (refs ()))) (cellShape ())
                              (character Reduce)
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
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
    (let ((sum-row.364 (values)))
     (let ((sum-row.172 sum-row.364))
      (let
       ((f.365 sum-row.172)
        (row.370
         (let ()
          (#0
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota iota.367)
              (#0
               (#0
                (loop-block (frame-shape 10)
                 (map () (iota (iota.369 : iota.367))
                  (let ((iota.319 iota.369)) iota.319))
                 (body-matcher map-result.368) (map-result (map-result.368))
                 (consumer (values))))))
             (body-matcher map-result.366) (map-result (map-result.366))
             (consumer (values))))))))
       (let ((f.173 f.365) (row.320 row.370))
        (let ((row.371 row.320))
         (#0
          (#0
           (loop-block (frame-shape 1000000)
            (map ((row.373 row.371))
             (let ((row.321 row.373))
              (let ((f.374 (values)) (op.375 (values)) (arr.376 row.321))
               (let ((f.223 f.374) (op.309 op.375) (arr.322 arr.376))
                (let
                 ((f.377 (values)) (f.378 op.309)
                  (init.384
                   (let ((f.379 (values)) (arr.380 arr.322))
                    (let ((f.350 f.379) (arr.356 arr.380))
                     (let
                      ((f.381 (values)) (contiguous-subarray-array.382 arr.356)
                       (contiguous-subarray-index.383 (frame 0)))
                      (let
                       ((f.355 f.381)
                        (contiguous-subarray-array.357
                         contiguous-subarray-array.382)
                        (contiguous-subarray-index.358
                         contiguous-subarray-index.383))
                       (contiguous-subarray contiguous-subarray-array.357
                        contiguous-subarray-index.358 (shape 10) (shape)))))))
                  (arr.390
                   (let ((f.385 (values)) (arr.386 arr.322))
                    (let ((f.313 f.385) (arr.323 arr.386))
                     (let
                      ((f.387 (values)) (contiguous-subarray-array.388 arr.323)
                       (contiguous-subarray-index.389 (frame 1)))
                      (let
                       ((f.318 f.387)
                        (contiguous-subarray-array.324
                         contiguous-subarray-array.388)
                        (contiguous-subarray-index.325
                         contiguous-subarray-index.389))
                       (contiguous-subarray contiguous-subarray-array.324
                        contiguous-subarray-index.325 (shape 10) (shape 9))))))))
                 (let
                  ((f.269 f.377) (f.310 f.378) (init.359 init.384)
                   (arr.326 arr.390))
                  (let ((init.391 init.359))
                   (let ((init.360 init.391))
                    (let ((up-ranked-f.392 (values)))
                     (let ((up-ranked-f.297 up-ranked-f.392))
                      (let
                       ((f.393 (values)) (reduce-f-arg.394 up-ranked-f.297)
                        (reduce-zero-arg.400
                         (let ((f.395 (values)) (v.396 init.360))
                          (let ((f.335 f.395) (v.361 v.396))
                           (let ((make.397 (values)))
                            (let ((make.345 make.397))
                             (let ((f.398 make.345) (v.399 v.361))
                              (let ((f.346 f.398) (v.362 v.399))
                               (let () (let () v.362)))))))))
                        (reduce-array-arg.401 arr.326))
                       (let
                        ((f.295 f.393) (reduce-f-arg.298 reduce-f-arg.394)
                         (reduce-zero-arg.363 reduce-zero-arg.400)
                         (reduce-array-arg.327 reduce-array-arg.401))
                        (let ((reduce-arg.408 reduce-array-arg.327))
                         (#1
                          (loop-block (frame-shape 9)
                           (map ((reduce-arg.409 reduce-arg.408))
                            (values reduce-arg.409))
                           (body-matcher (reduce-arg.402)) (map-result ())
                           (consumer
                            (reduce-zero reduce-zero-arg.363
                             (reduce-arg1.328 reduce-arg2.331 reduce-arg.402)
                             (let
                              ((f.403 f.310) (arg0.404 reduce-arg1.328)
                               (arg1.405 reduce-arg2.331))
                              (let
                               ((f.311 f.403) (arg0.329 arg0.404)
                                (arg1.332 arg1.405))
                               (let ((arg0.406 arg0.329) (arg1.407 arg1.332))
                                (let ((arg0.330 arg0.406) (arg1.333 arg1.407))
                                 (+ arg0.330 arg1.333))))))))))))))))))))))
            (body-matcher map-result.372) (map-result (map-result.372))
            (consumer (values))))))))))
    Result of stage Fuse and Simplify:
    (let
     ((contiguous-subarray-index.383 (frame 0))
      (contiguous-subarray-index.389 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 1000000)
        (map () (iota iota.367)
         (let
          ((map-result.441
            (#0
             (loop-block (frame-shape 10)
              (map () (iota (iota.369 : iota.367)) iota.369)
              (body-matcher map-result.368) (map-result (map-result.368))
              (consumer (values))))))
          (let
           ((reduce-arg.436
             (contiguous-subarray (#0 map-result.441)
              contiguous-subarray-index.389 (shape 10) (shape 9))))
           (#1
            (loop-block (frame-shape 9)
             (map ((reduce-arg.409 reduce-arg.436)) reduce-arg.409)
             (body-matcher reduce-arg.402) (map-result ())
             (consumer
              (reduce-zero
               (contiguous-subarray (#0 map-result.441)
                contiguous-subarray-index.383 (shape 10) (shape))
               (reduce-arg1.328 reduce-arg2.331 reduce-arg.402)
               (+ reduce-arg1.328 reduce-arg2.331))))))))
        (body-matcher map-result.372) (map-result (map-result.372))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((contiguous-subarray-index.383 (frame 0))
      (contiguous-subarray-index.389 (frame 1)))
     (#0
      (kernel (blocks 20) (threads 512)
       (map-kernel (frame-shape 1000000) () (iota iota.367)
        (body-matcher map-result.372) (map-result (map-result.372))
        (let
         ((map-result.441
           (#0
            (loop-block (frame-shape 10)
             (map () (iota (iota.369 : iota.367)) iota.369)
             (body-matcher map-result.368) (map-result (map-result.368))
             (consumer (values))))))
         (let
          ((reduce-arg.436
            (contiguous-subarray (#0 map-result.441)
             contiguous-subarray-index.389 (shape 10) (shape 9))))
          (#1
           (loop-block (frame-shape 9)
            (map ((reduce-arg.409 reduce-arg.436)) reduce-arg.409)
            (body-matcher reduce-arg.402) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.441)
               contiguous-subarray-index.383 (shape 10) (shape))
              (reduce-arg1.328 reduce-arg2.331 reduce-arg.402)
              (+ reduce-arg1.328 reduce-arg2.331)))))))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.467 (Tuple ()) device)
      (map-mem.463
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.461
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.460
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.458 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.456 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.383
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.456) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.456)))
       (contiguous-subarray-index.389
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.458) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.458))))
      (#0
       (begin
        (kernel (blocks 20) (threads 512)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.462 map-mem.461))
            (iota iota.367)
            (do-expr
             (let
              ((map-result.441
                (#0
                 (loop (frame-shape 10)
                  (map ((map-mem.464 map-mem.463)) (iota (iota.369 : iota.367))
                   (let ((expr-result.465 iota.369))
                    (begin (putmem expr-result.465 (#0 map-mem.464))
                     expr-result.465)))
                  (body-matcher map-result.368) (map-result (map-result.368))
                  (map-result-mem-final map-mem.463) (consumer (values))))))
              (let
               ((reduce-arg.436
                 (index
                  (#0
                   (let ((expr-result.466 map-result.441))
                    (values (#0 expr-result.466))))
                  contiguous-subarray-index.389 (shape 10) (shape 9))))
               (#1
                (let
                 ((expr-result.470
                   (loop (frame-shape 9)
                    (map
                     ((reduce-arg.409 reduce-arg.436) (map-mem.468 map-mem.467))
                     reduce-arg.409)
                    (body-matcher reduce-arg.402) (map-result ())
                    (map-result-mem-final map-mem.467)
                    (consumer
                     (reduce-zero
                      (index
                       (#0
                        (let ((expr-result.469 map-result.441))
                         (values (#0 expr-result.469))))
                       contiguous-subarray-index.383 (shape 10) (shape))
                      (reduce-arg1.328 reduce-arg2.331 reduce-arg.402)
                      (+ reduce-arg1.328 reduce-arg2.331))))))
                 (begin (putmem (#1 expr-result.470) (#0 map-mem.462))
                  expr-result.470))))))))
          (map-result-mem-interim map-mem.461)
          (map-result-mem-final (values map-mem-result.0.460))))
        (getmem (values map-mem-result.0.460))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.467 (Tuple ()) device)
      (map-mem.463
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.461
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.460
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.458 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.456 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.383
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.456) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.456)))
       (contiguous-subarray-index.389
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.458) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.458))))
      (#0
       (begin
        (kernel captures
         ((expr-captures
           ((((name contiguous-subarray-index) (id 383))
             (Array ((element (Literal IntLiteral)) (shape (shape 1)))))
            (((name contiguous-subarray-index) (id 389))
             (Array ((element (Literal IntLiteral)) (shape (shape 1)))))))
          (mem-captures
           ((((name map-mem) (id 461))
             (Tuple
              ((Array ((element (Literal IntLiteral)) (shape (shape 1000000)))))))
            (((name map-mem) (id 463))
             (Tuple
              ((Array ((element (Literal IntLiteral)) (shape (shape 10)))))))
            (((name map-mem) (id 467)) (Tuple ()))))
          (index-captures ()))
         (blocks 20) (threads 512)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.462 map-mem.461))
            (iota iota.367)
            (do-expr
             (let
              ((map-result.441
                (#0
                 (loop (frame-shape 10)
                  (map ((map-mem.464 map-mem.463)) (iota (iota.369 : iota.367))
                   (let ((expr-result.465 iota.369))
                    (begin (putmem expr-result.465 (#0 map-mem.464))
                     expr-result.465)))
                  (body-matcher map-result.368) (map-result (map-result.368))
                  (map-result-mem-final map-mem.463) (consumer (values))))))
              (let
               ((reduce-arg.436
                 (index
                  (#0
                   (let ((expr-result.466 map-result.441))
                    (values (#0 expr-result.466))))
                  contiguous-subarray-index.389 (shape 10) (shape 9))))
               (#1
                (let
                 ((expr-result.470
                   (loop (frame-shape 9)
                    (map
                     ((reduce-arg.409 reduce-arg.436) (map-mem.468 map-mem.467))
                     reduce-arg.409)
                    (body-matcher reduce-arg.402) (map-result ())
                    (map-result-mem-final map-mem.467)
                    (consumer
                     (reduce-zero
                      (index
                       (#0
                        (let ((expr-result.469 map-result.441))
                         (values (#0 expr-result.469))))
                       contiguous-subarray-index.383 (shape 10) (shape))
                      (reduce-arg1.328 reduce-arg2.331 reduce-arg.402)
                      (+ reduce-arg1.328 reduce-arg2.331))))))
                 (begin (putmem (#1 expr-result.470) (#0 map-mem.462))
                  expr-result.470))))))))
          (map-result-mem-interim map-mem.461)
          (map-result-mem-final (values map-mem-result.0.460))))
        (getmem (values map-mem-result.0.460)))))) |}]
;;
