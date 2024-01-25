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
       (((binding ((name f) (id 125))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 123)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 124)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 125))))))
         (args (((id ((name +arg1) (id 123)))) ((id ((name +arg2) (id 124))))))
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 126)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 127)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 128)))
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
                  ((id ((name +arg1) (id 127)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id ((name +arg2) (id 128)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))))
            (type' (Literal IntLiteral)))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((f.129 (values)) (+arg1.130 1) (+arg2.131 2))
     (let ((f.126 f.129) (+arg1.127 +arg1.130) (+arg2.128 +arg2.131))
      (+ +arg1.127 +arg2.128)))
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
     ((binding ((name add) (id 123)))
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding ((name x) (id 124)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding ((name y) (id 125)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args
                ((Ref ((id ((name x) (id 124)))))
                 (Ref ((id ((name y) (id 125))))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name add) (id 123))))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name add) (id 123)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name x) (id 124)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name y) (id 125)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 128)))
                     (value (Primitive ((name (Func Add))))))
                    ((binding ((name +arg1) (id 126)))
                     (value (Ref ((id ((name x) (id 124)))))))
                    ((binding ((name +arg2) (id 127)))
                     (value (Ref ((id ((name y) (id 125)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 128))))))
                     (args
                      (((id ((name +arg1) (id 126))))
                       ((id ((name +arg2) (id 127))))))
                     (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 131)))
            (value (Ref ((id ((name add) (id 123)))))))
           ((binding ((name x) (id 129)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding ((name y) (id 130)))
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 131))))))
            (args (((id ((name x) (id 129)))) ((id ((name y) (id 130))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name add) (id 133)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 134)))
            (value
             (Ref
              ((id ((name add) (id 133)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 141)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name y) (id 143)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 140)))
               (value
                (AtomAsArray
                 ((element (Values ((elements ()) (type' ()))))
                  (type' ((element (Tuple ())) (shape ())))))))
              ((binding ((name +arg1) (id 142)))
               (value
                (Ref
                 ((id ((name x) (id 141)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name +arg2) (id 144)))
               (value
                (Ref
                 ((id ((name y) (id 143)))
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
                        ((id ((name +arg1) (id 142)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 144)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))))
                  (type' (Literal IntLiteral)))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((add.145 (values)))
     (let ((add.133 add.145))
      (let ((f.146 add.133) (x.147 5) (y.148 10))
       (let ((f.134 f.146) (x.141 x.147) (y.143 y.148))
        (let ((f.149 (values)) (+arg1.150 x.141) (+arg2.151 y.143))
         (let ((f.140 f.149) (+arg1.142 +arg1.150) (+arg2.144 +arg2.151))
          (+ +arg1.142 +arg2.144)))))))
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
     ((binding ((name id) (id 123)))
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @t) (id 124))) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name e) (id 125)))
                     (bound (ArrayRef ((name @t) (id 124)))))))
                  (body (Ref ((id ((name e) (id 125))))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id ((name id) (id 123))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name id) (id 123)))
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding ((name @t) (id 124))) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name e) (id 125)))
                        (bound (ArrayRef ((name @t) (id 124)))))))
                     (body (Ref ((id ((name e) (id 125)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 127)))
            (value
             (TypeApplication
              ((tFunc (Ref ((id ((name id) (id 123))))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding ((name e) (id 126)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 127))))))
            (args (((id ((name e) (id 126))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 129)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 130)))
            (value
             (Ref
              ((id ((name id) (id 129)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name e) (id 132)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name e) (id 132)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((id.133 (values)))
     (let ((id.129 id.133))
      (let ((f.134 id.129) (e.135 5)) (let ((f.130 f.134) (e.132 e.135)) e.132))))
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
       (((binding ((name f) (id 125))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 123)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding ((name +arg2) (id 124)))
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
          (((binding ((name +arg1) (id 126)))
            (value (Ref ((id ((name +arg1) (id 123)))))))
           ((binding ((name +arg2) (id 127)))
            (value (Ref ((id ((name +arg2) (id 124)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg2) (id 128)))
               (value (Ref ((id ((name +arg2) (id 127)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 125))))))
               (args
                (((id ((name +arg1) (id 126)))) ((id ((name +arg2) (id 128))))))
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
       (((binding ((name f) (id 129)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 130)))
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
        ((binding ((name +arg2) (id 132)))
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
          (((binding ((name +arg1) (id 131)))
            (value
             (Ref
              ((id ((name +arg1) (id 130)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding ((name +arg2) (id 133)))
            (value
             (Ref
              ((id ((name +arg2) (id 132)))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 134)))
               (value
                (Ref
                 ((id ((name +arg2) (id 133)))
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
                        ((id ((name +arg1) (id 131)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 134)))
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
     ((f.135 (values)) (+arg1.136 (frame 1 2))
      (+arg2.137 (frame (frame 3 4 5) (frame 6 7 8))))
     (let ((f.129 f.135) (+arg1.130 +arg1.136) (+arg2.132 +arg2.137))
      (let ((+arg1.138 +arg1.130) (+arg2.139 +arg2.132))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.141 +arg1.138) (+arg2.142 +arg2.139))
           (let ((+arg1.131 +arg1.141) (+arg2.133 +arg2.142))
            (let ((+arg2.143 +arg2.133))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg2.145 +arg2.143))
                 (let ((+arg2.134 +arg2.145)) (+ +arg1.131 +arg2.134)))
                (body-matcher map-result.144) (map-result (map-result.144))
                (consumer (values))))))))
          (body-matcher map-result.140) (map-result (map-result.140))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((+arg1.138 (frame 1 2)) (+arg2.139 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.141 +arg1.138) (+arg2.142 +arg2.139))
         (let ((+arg2.143 +arg2.142))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.145 +arg2.143)) (+ +arg1.141 +arg2.145))
             (body-matcher map-result.144) (map-result (map-result.144))
             (consumer (values)))))))
        (body-matcher map-result.140) (map-result (map-result.140))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((+arg1.138 (frame 1 2)) (+arg2.139 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.141 +arg1.138) (+arg2.142 +arg2.139))
         (let ((+arg2.143 +arg2.142))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.145 +arg2.143)) (+ +arg1.141 +arg2.145))
             (body-matcher map-result.144) (map-result (map-result.144))
             (consumer (values)))))))
        (body-matcher map-result.140) (map-result (map-result.140))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.167 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.160
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.157 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.138
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.157) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.157) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.157)))
       (+arg2.139
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.160) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.160) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.160) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.160) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.160) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.160) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.160))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.141 +arg1.138) (+arg2.142 +arg2.139) (map-mem.168 map-mem.167))
          (let ((+arg2.143 +arg2.142))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.145 +arg2.143) (map-mem.169 map-mem.168))
               (let ((expr-result.170 (+ +arg1.141 +arg2.145)))
                (begin (putmem expr-result.170 map-mem.169) expr-result.170)))
              (body-matcher map-result.144) (map-result (map-result.144))
              (map-result-mem-interim (values map-mem.168))
              (map-result-mem-final (values map-mem.168)) (consumer (values)))))))
         (body-matcher map-result.140) (map-result (map-result.140))
         (map-result-mem-interim (values map-mem.167))
         (map-result-mem-final (values map-mem.167)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.167 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.160
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.157 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.138
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.157) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.157) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.157)))
       (+arg2.139
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.160) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.160) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.160) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.160) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.160) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.160) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.160))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.141 +arg1.138) (+arg2.142 +arg2.139) (map-mem.168 map-mem.167))
          (let ((+arg2.143 +arg2.142))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.145 +arg2.143) (map-mem.169 map-mem.168))
               (let ((expr-result.170 (+ +arg1.141 +arg2.145)))
                (begin (putmem expr-result.170 map-mem.169) expr-result.170)))
              (body-matcher map-result.144) (map-result (map-result.144))
              (map-result-mem-interim (values map-mem.168))
              (map-result-mem-final (values map-mem.168)) (consumer (values)))))))
         (body-matcher map-result.140) (map-result (map-result.140))
         (map-result-mem-interim (values map-mem.167))
         (map-result-mem-final (values map-mem.167)) (consumer (values))))))) |}]
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
     ((binding ((name words) (id 123)))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 124)) 1)))))))))))))))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 124)) 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings ((((name len) (id 125)) Dim)))
         (valueBinding ((name word) (id 126)))
         (box (Ref ((id ((name words) (id 123))))))
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
                         ((const 0) (refs ((((name len) (id 125)) 1)))))
                        (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id ((name word) (id 126)))))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name words) (id 123)))
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
                       ((Add ((const 0) (refs ((((name len) (id 124)) 1)))))))))))))))
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
                       ((Add ((const 0) (refs ((((name len) (id 124)) 1)))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name box) (id 132)))
            (value (Ref ((id ((name words) (id 123)))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 125)))
               (indexValue
                (FromBox (box (Ref ((id ((name box) (id 132)))))) (i 0)))
               (sort Dim))))
            (body
             (Map
              ((args
                (((binding ((name word) (id 126)))
                  (value (BoxValue ((box (Ref ((id ((name box) (id 132))))))))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 131)))
                     (value (Primitive ((name (Func Equal))))))
                    ((binding ((name =arg1) (id 127)))
                     (value (Scalar ((element (Literal (IntLiteral 3)))))))
                    ((binding ((name =arg2) (id 130)))
                     (value
                      (Map
                       ((args
                         (((binding ((name f) (id 129)))
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
                                     (refs ((((name len) (id 125)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal CharacterLiteral))))))))
                          ((binding ((name arr) (id 128)))
                           (value (Ref ((id ((name word) (id 126)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 129))))))
                           (args (((id ((name arr) (id 128))))))
                           (type'
                            (Arr ((element (Literal IntLiteral)) (shape ())))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 131))))))
                     (args
                      (((id ((name =arg1) (id 127))))
                       ((id ((name =arg2) (id 130))))))
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
       (((binding ((name words) (id 139)))
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
                      ((Add ((const 0) (refs ((((name len) (id 124)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 124))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 124)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 124))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 124)) 1))))))))))))
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
                      ((Add ((const 0) (refs ((((name len) (id 124)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 124))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 124)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 124))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 124)) 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 124))) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 124)) 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name box) (id 140)))
            (value
             (Ref
              ((id ((name words) (id 139)))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 124))) (bound Dim))))
                    (body
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 124)) 1))))))))))))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 125)))
               (indexValue
                (FromBox
                 (box
                  (Ref
                   ((id ((name box) (id 140)))
                    (type'
                     ((element
                       (Sigma
                        ((parameters
                          (((binding ((name len) (id 124))) (bound Dim))))
                         (body
                          ((element (Literal CharacterLiteral))
                           (shape
                            ((Add ((const 0) (refs ((((name len) (id 124)) 1))))))))))))
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
                   (((binding ((name f) (id 133)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name =arg1) (id 134)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 3)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name =arg2) (id 138)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 136)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ReifyIndex
                          ((index
                            (Dimension
                             ((const 0) (refs ((((name len) (id 125)) 1))))))
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
                              ((id ((name =arg1) (id 134)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name =arg2) (id 138)))
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
     ((words.141 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (let ((words.139 words.141))
      (let ((box.142 words.139))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((box.144 box.142))
           (let ((box.140 box.144))
            (index-let ((len.125 box-index-0 box.140))
             (let ()
              (let ()
               (let
                ((f.145 (values)) (=arg1.146 3)
                 (=arg2.148
                  (let ((f.147 (values)))
                   (let ((f.136 f.147)) (reify-index len.125)))))
                (let ((f.133 f.145) (=arg1.134 =arg1.146) (=arg2.138 =arg2.148))
                 (= =arg1.134 =arg2.138))))))))
          (body-matcher map-result.143) (map-result (map-result.143))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((box.142 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.144 box.142))
         (index-let ((len.125 box-index-0 box.144)) (= 3 (reify-index len.125))))
        (body-matcher map-result.143) (map-result (map-result.143))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((box.142 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.144 box.142))
         (index-let ((len.125 box-index-0 box.144)) (= 3 (reify-index len.125))))
        (body-matcher map-result.143) (map-result (map-result.143))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.160 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.156
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.151
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.150
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 124))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.124))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.142
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.151) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.151) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.151) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.151)))
           (index (mem frame-array.150) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 124))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.124)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.156) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.156) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.156)))
           (index (mem frame-array.150) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 124))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.124))))))))))))
         (getmem frame-array.150))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.144 box.142) (map-mem.161 map-mem.160))
          (index-let ((len.125 box-index-0 box.144))
           (let ((expr-result.162 (= 3 (reify-dimension-index len.125))))
            (begin (putmem expr-result.162 map-mem.161) expr-result.162))))
         (body-matcher map-result.143) (map-result (map-result.143))
         (map-result-mem-interim (values map-mem.160))
         (map-result-mem-final (values map-mem.160)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.160 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.156
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.151
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.150
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 124))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.124))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.142
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.151) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.151) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.151) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.151)))
           (index (mem frame-array.150) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 124))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.124)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.156) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.156) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.156)))
           (index (mem frame-array.150) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 124))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.124))))))))))))
         (getmem frame-array.150))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.144 box.142) (map-mem.161 map-mem.160))
          (index-let ((len.125 box-index-0 box.144))
           (let ((expr-result.162 (= 3 (reify-dimension-index len.125))))
            (begin (putmem expr-result.162 map-mem.161) expr-result.162))))
         (body-matcher map-result.143) (map-result (map-result.143))
         (map-result-mem-interim (values map-mem.160))
         (map-result-mem-final (values map-mem.160)) (consumer (values))))))) |}]
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
     ((binding ((name sum-row) (id 123)))
      (value
       (Scalar
        ((element
          (IndexLambda
           ((params (((binding ((name d-1) (id 124))) (bound Dim))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name row) (id 125)))
                     (bound
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape
                         ((Add ((const 1) (refs ((((name d-1) (id 124)) 1)))))))))))))
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
                              ((const 0) (refs ((((name d-1) (id 124)) 1)))))
                             (Shape ()) (Shape ()))))))
                        (args ((Atom (Literal IntLiteral)))))))
                     (args
                      ((Primitive ((name (Func Add))))
                       (Ref ((id ((name row) (id 125))))))))))))))))))))))
      (body
       (TermApplication
        ((func
          (IndexApplication
           ((iFunc (Ref ((id ((name sum-row) (id 123))))))
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
       (((binding ((name sum-row) (id 123)))
         (value
          (Scalar
           ((element
             (IndexLambda
              ((params (((binding ((name d-1) (id 124))) (bound Dim))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name row) (id 125)))
                        (bound
                         (Arr
                          ((element (Literal IntLiteral))
                           (shape
                            ((Add ((const 1) (refs ((((name d-1) (id 124)) 1)))))))))))))
                     (body
                      (Map
                       ((args
                         (((binding ((name f) (id 158)))
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
                                                          ((name f) (id 156)))
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
                                                                        (id 143)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 58)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg0)
                                                                        (id 141)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name a)
                                                                        (id 62)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg1)
                                                                        (id 142)))
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
                                                                        (id 144)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arg0)
                                                                        (id 141)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg1)
                                                                        (id 145)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arg1)
                                                                        (id 142)))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 143))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        arg0)
                                                                        (id 144))))
                                                                        ((id
                                                                        ((name
                                                                        arg1)
                                                                        (id 145))))))
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
                                                                        (id 155)))
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
                                                                        (id 146)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        up-ranked-f)
                                                                        (id 61)))))))
                                                                        ((binding
                                                                        ((name
                                                                        reduce-zero-arg)
                                                                        (id 153)))
                                                                        (value
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 152)))
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
                                                                        (id 150)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        make)
                                                                        (id 10)))))))
                                                                        ((binding
                                                                        ((name
                                                                        foo)
                                                                        (id 148)))
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
                                                                        (id 149)))
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
                                                                        (id 151)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        foo)
                                                                        (id 148)))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 150))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        foo)
                                                                        (id 151))))
                                                                        ((id
                                                                        ((name v)
                                                                        (id 149))))))
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
                                                                        (id 147)))
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
                                                                        (id 152))))))
                                                                        (args
                                                                        (((id
                                                                        ((name v)
                                                                        (id 147))))))
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
                                                                        (id 154)))
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
                                                                        (id 155))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        reduce-f-arg)
                                                                        (id 146))))
                                                                        ((id
                                                                        ((name
                                                                        reduce-zero-arg)
                                                                        (id 153))))
                                                                        ((id
                                                                        ((name
                                                                        reduce-array-arg)
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
                                                          ((name f) (id 128)))
                                                         (value
                                                          (Ref
                                                           ((id
                                                             ((name op) (id 68)))))))
                                                        ((binding
                                                          ((name init) (id 134)))
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding
                                                                ((name f)
                                                                 (id 133)))
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
                                                                        (id 132)))
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
                                                                        (id 130)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 27)))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 131)))
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
                                                                        (id 132))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 130))))
                                                                        ((id
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 131))))))
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
                                                                 (id 129)))
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
                                                                    (id 133))))))
                                                               (args
                                                                (((id
                                                                   ((name arr)
                                                                    (id 129))))))
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
                                                          ((name arr) (id 140)))
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding
                                                                ((name f)
                                                                 (id 139)))
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
                                                                        (id 138)))
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
                                                                        (id 136)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 31)))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 137)))
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
                                                                        (id 138))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 136))))
                                                                        ((id
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 137))))))
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
                                                                 (id 135)))
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
                                                                    (id 139))))))
                                                               (args
                                                                (((id
                                                                   ((name arr)
                                                                    (id 135))))))
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
                                                              (id 157)))
                                                            (value
                                                             (Ref
                                                              ((id
                                                                ((name init)
                                                                 (id 134)))))))))
                                                         (body
                                                          (TermApplication
                                                           ((func
                                                             (Ref
                                                              ((id
                                                                ((name f)
                                                                 (id 156))))))
                                                            (args
                                                             (((id
                                                                ((name f)
                                                                 (id 128))))
                                                              ((id
                                                                ((name init)
                                                                 (id 157))))
                                                              ((id
                                                                ((name arr)
                                                                 (id 140))))))
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
                                     (refs ((((name d-1) (id 124)) 1)))))
                                   (Shape ()) (Shape ()))))))
                              (args ((Atom (Literal IntLiteral))))))))
                          ((binding ((name op) (id 126)))
                           (value (Primitive ((name (Func Add))))))
                          ((binding ((name arr) (id 127)))
                           (value (Ref ((id ((name row) (id 125)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 158))))))
                           (args
                            (((id ((name op) (id 126))))
                             ((id ((name arr) (id 127))))))
                           (type'
                            (Arr ((element (Literal IntLiteral)) (shape ())))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 160)))
            (value
             (IndexApplication
              ((iFunc (Ref ((id ((name sum-row) (id 123))))))
               (args ((Dimension ((const 9) (refs ())))))))))
           ((binding ((name row) (id 159)))
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
             (((binding ((name row) (id 161)))
               (value (Ref ((id ((name row) (id 159)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 160))))))
               (args (((id ((name row) (id 161))))))
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
       (((binding ((name sum-row) (id 163)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 164)))
            (value
             (Ref
              ((id ((name sum-row) (id 163)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name row) (id 311)))
            (value
             (ArrayPrimitive
              (Map
               (frameShape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
               (args ()) (iotaVar (((name iota) (id 310))))
               (body
                (Ref
                 ((id ((name iota) (id 310)))
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
             (((binding ((name row) (id 312)))
               (value
                (Ref
                 ((id ((name row) (id 311)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape
                     ((Add ((const 1000000) (refs ())))
                      (Add ((const 10) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 214)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name op) (id 300)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 313)))
                  (value
                   (Ref
                    ((id ((name row) (id 312)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 10) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 260)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name f) (id 301)))
                     (value
                      (Ref
                       ((id ((name op) (id 300)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name init) (id 350)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 341)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 347)))
                           (value
                            (Ref
                             ((id ((name arr) (id 313)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 10) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 346)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding
                               ((name contiguous-subarray-array) (id 348)))
                              (value
                               (Ref
                                ((id ((name arr) (id 347)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding
                               ((name contiguous-subarray-index) (id 349)))
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
                                ((id ((name contiguous-subarray-array) (id 348)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id ((name contiguous-subarray-index) (id 349)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 1) (refs ()))))))))))
                              (originalShape ((Add ((const 10) (refs ())))))
                              (resultShape ()) (cellShape ())
                              (l ((const 1) (refs ())))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name arr) (id 317)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 304)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 314)))
                           (value
                            (Ref
                             ((id ((name arr) (id 313)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 10) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 309)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding
                               ((name contiguous-subarray-array) (id 315)))
                              (value
                               (Ref
                                ((id ((name arr) (id 314)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding
                               ((name contiguous-subarray-index) (id 316)))
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
                                ((id ((name contiguous-subarray-array) (id 315)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id ((name contiguous-subarray-index) (id 316)))
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
                      (((binding ((name init) (id 351)))
                        (value
                         (Ref
                          ((id ((name init) (id 350)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name up-ranked-f) (id 288)))
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
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding ((name reduce-f-arg) (id 289)))
                              (value
                               (Ref
                                ((id ((name up-ranked-f) (id 288)))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding ((name reduce-zero-arg) (id 354)))
                              (value
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding ((name f) (id 326)))
                                    (value
                                     (AtomAsArray
                                      ((element
                                        (Values ((elements ()) (type' ()))))
                                       (type' ((element (Tuple ())) (shape ())))))))
                                   ((binding ((name v) (id 352)))
                                    (value
                                     (Ref
                                      ((id ((name init) (id 351)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name make) (id 336)))
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
                                        (((binding ((name f) (id 337)))
                                          (value
                                           (Ref
                                            ((id ((name make) (id 336)))
                                             (type'
                                              ((element (Tuple ())) (shape ())))))))
                                         ((binding ((name v) (id 353)))
                                          (value
                                           (Ref
                                            ((id ((name v) (id 352)))
                                             (type'
                                              ((element (Literal IntLiteral))
                                               (shape ())))))))))
                                       (body
                                        (ArrayPrimitive
                                         (Map (frameShape ()) (args ())
                                          (body
                                           (Ref
                                            ((id ((name v) (id 353)))
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
                             ((binding ((name reduce-array-arg) (id 318)))
                              (value
                               (Ref
                                ((id ((name arr) (id 317)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 9) (refs ())))))))))))))
                           (body
                            (ArrayPrimitive
                             (Reduce
                              (arg
                               ((firstBinding ((name reduce-arg1) (id 319)))
                                (secondBinding ((name reduce-arg2) (id 322)))
                                (value
                                 (Ref
                                  ((id ((name reduce-array-arg) (id 318)))
                                   (type'
                                    ((element (Literal IntLiteral))
                                     (shape ((Add ((const 9) (refs ()))))))))))))
                              (zero
                               (Ref
                                ((id ((name reduce-zero-arg) (id 354)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding ((name f) (id 302)))
                                    (value
                                     (Ref
                                      ((id ((name f) (id 301)))
                                       (type' ((element (Tuple ())) (shape ())))))))
                                   ((binding ((name arg0) (id 320)))
                                    (value
                                     (Ref
                                      ((id ((name reduce-arg1) (id 319)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))
                                   ((binding ((name arg1) (id 323)))
                                    (value
                                     (Ref
                                      ((id ((name reduce-arg2) (id 322)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name arg0) (id 321)))
                                       (value
                                        (Ref
                                         ((id ((name arg0) (id 320)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))
                                      ((binding ((name arg1) (id 324)))
                                       (value
                                        (Ref
                                         ((id ((name arg1) (id 323)))
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
                                                ((id ((name arg0) (id 321)))
                                                 (type'
                                                  ((element (Literal IntLiteral))
                                                   (shape ()))))))
                                              (type' (Literal IntLiteral))))
                                            (ArrayAsAtom
                                             ((array
                                               (Ref
                                                ((id ((name arg1) (id 324)))
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
    (let ((sum-row.355 (values)))
     (let ((sum-row.163 sum-row.355))
      (let
       ((f.356 sum-row.163)
        (row.361
         (let ()
          (#0
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota iota.358)
              (#0
               (#0
                (loop-block (frame-shape 10)
                 (map () (iota (iota.360 : iota.358))
                  (let ((iota.310 iota.360)) iota.310))
                 (body-matcher map-result.359) (map-result (map-result.359))
                 (consumer (values))))))
             (body-matcher map-result.357) (map-result (map-result.357))
             (consumer (values))))))))
       (let ((f.164 f.356) (row.311 row.361))
        (let ((row.362 row.311))
         (#0
          (#0
           (loop-block (frame-shape 1000000)
            (map ((row.364 row.362))
             (let ((row.312 row.364))
              (let ((f.365 (values)) (op.366 (values)) (arr.367 row.312))
               (let ((f.214 f.365) (op.300 op.366) (arr.313 arr.367))
                (let
                 ((f.368 (values)) (f.369 op.300)
                  (init.375
                   (let ((f.370 (values)) (arr.371 arr.313))
                    (let ((f.341 f.370) (arr.347 arr.371))
                     (let
                      ((f.372 (values)) (contiguous-subarray-array.373 arr.347)
                       (contiguous-subarray-index.374 (frame 0)))
                      (let
                       ((f.346 f.372)
                        (contiguous-subarray-array.348
                         contiguous-subarray-array.373)
                        (contiguous-subarray-index.349
                         contiguous-subarray-index.374))
                       (contiguous-subarray contiguous-subarray-array.348
                        contiguous-subarray-index.349 (shape 10) (shape)))))))
                  (arr.381
                   (let ((f.376 (values)) (arr.377 arr.313))
                    (let ((f.304 f.376) (arr.314 arr.377))
                     (let
                      ((f.378 (values)) (contiguous-subarray-array.379 arr.314)
                       (contiguous-subarray-index.380 (frame 1)))
                      (let
                       ((f.309 f.378)
                        (contiguous-subarray-array.315
                         contiguous-subarray-array.379)
                        (contiguous-subarray-index.316
                         contiguous-subarray-index.380))
                       (contiguous-subarray contiguous-subarray-array.315
                        contiguous-subarray-index.316 (shape 10) (shape 9))))))))
                 (let
                  ((f.260 f.368) (f.301 f.369) (init.350 init.375)
                   (arr.317 arr.381))
                  (let ((init.382 init.350))
                   (let ((init.351 init.382))
                    (let ((up-ranked-f.383 (values)))
                     (let ((up-ranked-f.288 up-ranked-f.383))
                      (let
                       ((f.384 (values)) (reduce-f-arg.385 up-ranked-f.288)
                        (reduce-zero-arg.391
                         (let ((f.386 (values)) (v.387 init.351))
                          (let ((f.326 f.386) (v.352 v.387))
                           (let ((make.388 (values)))
                            (let ((make.336 make.388))
                             (let ((f.389 make.336) (v.390 v.352))
                              (let ((f.337 f.389) (v.353 v.390))
                               (let () (let () v.353)))))))))
                        (reduce-array-arg.392 arr.317))
                       (let
                        ((f.286 f.384) (reduce-f-arg.289 reduce-f-arg.385)
                         (reduce-zero-arg.354 reduce-zero-arg.391)
                         (reduce-array-arg.318 reduce-array-arg.392))
                        (let ((reduce-arg.399 reduce-array-arg.318))
                         (#1
                          (loop-block (frame-shape 9)
                           (map ((reduce-arg.400 reduce-arg.399))
                            (values reduce-arg.400))
                           (body-matcher (reduce-arg.393)) (map-result ())
                           (consumer
                            (reduce-zero reduce-zero-arg.354
                             (reduce-arg1.319 reduce-arg2.322 reduce-arg.393)
                             (let
                              ((f.394 f.301) (arg0.395 reduce-arg1.319)
                               (arg1.396 reduce-arg2.322))
                              (let
                               ((f.302 f.394) (arg0.320 arg0.395)
                                (arg1.323 arg1.396))
                               (let ((arg0.397 arg0.320) (arg1.398 arg1.323))
                                (let ((arg0.321 arg0.397) (arg1.324 arg1.398))
                                 (+ arg0.321 arg1.324))))))))))))))))))))))
            (body-matcher map-result.363) (map-result (map-result.363))
            (consumer (values))))))))))
    Result of stage Fuse and Simplify:
    (let
     ((contiguous-subarray-index.374 (frame 0))
      (contiguous-subarray-index.380 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 1000000)
        (map () (iota iota.358)
         (let
          ((map-result.432
            (#0
             (loop-block (frame-shape 10)
              (map () (iota (iota.360 : iota.358)) iota.360)
              (body-matcher map-result.359) (map-result (map-result.359))
              (consumer (values))))))
          (let
           ((reduce-arg.427
             (contiguous-subarray (#0 map-result.432)
              contiguous-subarray-index.380 (shape 10) (shape 9))))
           (#1
            (loop-block (frame-shape 9)
             (map ((reduce-arg.400 reduce-arg.427)) reduce-arg.400)
             (body-matcher reduce-arg.393) (map-result ())
             (consumer
              (reduce-zero
               (contiguous-subarray (#0 map-result.432)
                contiguous-subarray-index.374 (shape 10) (shape))
               (reduce-arg1.319 reduce-arg2.322 reduce-arg.393)
               (+ reduce-arg1.319 reduce-arg2.322))))))))
        (body-matcher map-result.363) (map-result (map-result.363))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((contiguous-subarray-index.374 (frame 0))
      (contiguous-subarray-index.380 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.358)
        (body-matcher map-result.363) (map-result (map-result.363))
        (let
         ((map-result.432
           (#0
            (loop-block (frame-shape 10)
             (map () (iota (iota.360 : iota.358)) iota.360)
             (body-matcher map-result.359) (map-result (map-result.359))
             (consumer (values))))))
         (let
          ((reduce-arg.427
            (contiguous-subarray (#0 map-result.432)
             contiguous-subarray-index.380 (shape 10) (shape 9))))
          (#1
           (loop-block (frame-shape 9)
            (map ((reduce-arg.400 reduce-arg.427)) reduce-arg.400)
            (body-matcher reduce-arg.393) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.432)
               contiguous-subarray-index.374 (shape 10) (shape))
              (reduce-arg1.319 reduce-arg2.322 reduce-arg.393)
              (+ reduce-arg1.319 reduce-arg2.322)))))))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.458 (Tuple ()) device)
      (map-mem.454
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.452
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.451
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.449 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.447 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.374
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.447) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.447)))
       (contiguous-subarray-index.380
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.449) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.449))))
      (#0
       (begin
        (kernel (blocks 320) (threads 32)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.453 map-mem.452))
            (iota iota.358)
            (do-expr
             (let
              ((map-result.432
                (#0
                 (loop (frame-shape 10)
                  (map ((map-mem.455 map-mem.454)) (iota (iota.360 : iota.358))
                   (let ((expr-result.456 iota.360))
                    (begin (putmem expr-result.456 (#0 map-mem.455))
                     expr-result.456)))
                  (body-matcher map-result.359) (map-result (map-result.359))
                  (map-result-mem-interim map-mem.454)
                  (map-result-mem-final map-mem.454) (consumer (values))))))
              (let
               ((reduce-arg.427
                 (index
                  (#0
                   (let ((expr-result.457 map-result.432))
                    (values (#0 expr-result.457))))
                  contiguous-subarray-index.380 (shape 10) (shape 9))))
               (#1
                (let
                 ((expr-result.461
                   (loop (frame-shape 9)
                    (map
                     ((reduce-arg.400 reduce-arg.427) (map-mem.459 map-mem.458))
                     reduce-arg.400)
                    (body-matcher reduce-arg.393) (map-result ())
                    (map-result-mem-interim map-mem.458)
                    (map-result-mem-final map-mem.458)
                    (consumer
                     (reduce-zero
                      (index
                       (#0
                        (let ((expr-result.460 map-result.432))
                         (values (#0 expr-result.460))))
                       contiguous-subarray-index.374 (shape 10) (shape))
                      (reduce-arg1.319 reduce-arg2.322 reduce-arg.393)
                      (+ reduce-arg1.319 reduce-arg2.322))))))
                 (begin (putmem (#1 expr-result.461) (#0 map-mem.453))
                  expr-result.461))))))))
          (map-result-mem-interim map-mem.452)
          (map-result-mem-final (values map-mem-result.0.451))))
        (getmem (values map-mem-result.0.451))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.458 (Tuple ()) device)
      (map-mem.454
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.452
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.451
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.449 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.447 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.374
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.447) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.447)))
       (contiguous-subarray-index.380
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.449) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.449))))
      (#0
       (begin
        (kernel captures
         ((expr-captures
           ((((name contiguous-subarray-index) (id 374))
             (Array ((element (Literal IntLiteral)) (shape (shape 1)))))
            (((name contiguous-subarray-index) (id 380))
             (Array ((element (Literal IntLiteral)) (shape (shape 1)))))))
          (mem-captures
           ((((name map-mem) (id 452))
             (Tuple
              ((Array ((element (Literal IntLiteral)) (shape (shape 1000000)))))))
            (((name map-mem) (id 454))
             (Tuple
              ((Array ((element (Literal IntLiteral)) (shape (shape 10)))))))
            (((name map-mem) (id 458)) (Tuple ()))))
          (index-captures ()))
         (blocks 320) (threads 32)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.453 map-mem.452))
            (iota iota.358)
            (do-expr
             (let
              ((map-result.432
                (#0
                 (loop (frame-shape 10)
                  (map ((map-mem.455 map-mem.454)) (iota (iota.360 : iota.358))
                   (let ((expr-result.456 iota.360))
                    (begin (putmem expr-result.456 (#0 map-mem.455))
                     expr-result.456)))
                  (body-matcher map-result.359) (map-result (map-result.359))
                  (map-result-mem-interim map-mem.454)
                  (map-result-mem-final map-mem.454) (consumer (values))))))
              (let
               ((reduce-arg.427
                 (index
                  (#0
                   (let ((expr-result.457 map-result.432))
                    (values (#0 expr-result.457))))
                  contiguous-subarray-index.380 (shape 10) (shape 9))))
               (#1
                (let
                 ((expr-result.461
                   (loop (frame-shape 9)
                    (map
                     ((reduce-arg.400 reduce-arg.427) (map-mem.459 map-mem.458))
                     reduce-arg.400)
                    (body-matcher reduce-arg.393) (map-result ())
                    (map-result-mem-interim map-mem.458)
                    (map-result-mem-final map-mem.458)
                    (consumer
                     (reduce-zero
                      (index
                       (#0
                        (let ((expr-result.460 map-result.432))
                         (values (#0 expr-result.460))))
                       contiguous-subarray-index.374 (shape 10) (shape))
                      (reduce-arg1.319 reduce-arg2.322 reduce-arg.393)
                      (+ reduce-arg1.319 reduce-arg2.322))))))
                 (begin (putmem (#1 expr-result.461) (#0 map-mem.453))
                  expr-result.461))))))))
          (map-result-mem-interim map-mem.452)
          (map-result-mem-final (values map-mem-result.0.451))))
        (getmem (values map-mem-result.0.451)))))) |}]
;;
