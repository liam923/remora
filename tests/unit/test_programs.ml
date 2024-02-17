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
       (((binding f.150) (value (Primitive ((name (Func Add))))))
        ((binding +arg1.148)
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding +arg2.149)
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id f.150)))) (args (((id +arg1.148)) ((id +arg2.149))))
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding f.151)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding +arg1.152)
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding +arg2.153)
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
                  ((id +arg1.152)
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id +arg2.153)
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))))
            (type' (Literal IntLiteral)))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((f.154 (values)) (+arg1.155 1) (+arg2.156 2))
     (let ((f.151 f.154) (+arg1.152 +arg1.155) (+arg2.153 +arg2.156))
      (+ +arg1.152 +arg2.153)))
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
     ((binding add.148)
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding x.149)
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding y.150)
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args ((Ref ((id x.149))) (Ref ((id y.150))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id add.148))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding add.148)
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding x.149)
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding y.150)
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding f.153) (value (Primitive ((name (Func Add))))))
                    ((binding +arg1.151) (value (Ref ((id x.149)))))
                    ((binding +arg2.152) (value (Ref ((id y.150)))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id f.153))))
                     (args (((id +arg1.151)) ((id +arg2.152))))
                     (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding f.156) (value (Ref ((id add.148)))))
           ((binding x.154)
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding y.155)
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id f.156)))) (args (((id x.154)) ((id y.155))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding add.158)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.159)
            (value
             (Ref ((id add.158) (type' ((element (Tuple ())) (shape ())))))))
           ((binding x.166)
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding y.168)
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding f.165)
               (value
                (AtomAsArray
                 ((element (Values ((elements ()) (type' ()))))
                  (type' ((element (Tuple ())) (shape ())))))))
              ((binding +arg1.167)
               (value
                (Ref
                 ((id x.166) (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding +arg2.169)
               (value
                (Ref
                 ((id y.168) (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (AtomAsArray
              ((element
                (AtomicPrimitive
                 ((op Add)
                  (args
                   ((ArrayAsAtom
                     ((array
                       (Ref
                        ((id +arg1.167)
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id +arg2.169)
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))))
                  (type' (Literal IntLiteral)))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((add.170 (values)))
     (let ((add.158 add.170))
      (let ((f.171 add.158) (x.172 5) (y.173 10))
       (let ((f.159 f.171) (x.166 x.172) (y.168 y.173))
        (let ((f.174 (values)) (+arg1.175 x.166) (+arg2.176 y.168))
         (let ((f.165 f.174) (+arg1.167 +arg1.175) (+arg2.169 +arg2.176))
          (+ +arg1.167 +arg2.169)))))))
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
     ((binding id.148)
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding @t.149) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params (((binding e.150) (bound (ArrayRef @t.149)))))
                  (body (Ref ((id e.150))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id id.148))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding id.148)
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding @t.149) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params (((binding e.150) (bound (ArrayRef @t.149)))))
                     (body (Ref ((id e.150)))))))))))))))))))
      (body
       (Map
        ((args
          (((binding f.152)
            (value
             (TypeApplication
              ((tFunc (Ref ((id id.148))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding e.151)
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id f.152)))) (args (((id e.151))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding id.154)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.155)
            (value (Ref ((id id.154) (type' ((element (Tuple ())) (shape ())))))))
           ((binding e.157)
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref ((id e.157) (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((id.158 (values)))
     (let ((id.154 id.158))
      (let ((f.159 id.154) (e.160 5)) (let ((f.155 f.159) (e.157 e.160)) e.157))))
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
       (((binding f.150) (value (Primitive ((name (Func Add))))))
        ((binding +arg1.148)
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding +arg2.149)
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
          (((binding +arg1.151) (value (Ref ((id +arg1.148)))))
           ((binding +arg2.152) (value (Ref ((id +arg2.149)))))))
         (body
          (Map
           ((args (((binding +arg2.153) (value (Ref ((id +arg2.152)))))))
            (body
             (TermApplication
              ((func (Ref ((id f.150))))
               (args (((id +arg1.151)) ((id +arg2.153))))
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
       (((binding f.154)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding +arg1.155)
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
        ((binding +arg2.157)
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
          (((binding +arg1.156)
            (value
             (Ref
              ((id +arg1.155)
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding +arg2.158)
            (value
             (Ref
              ((id +arg2.157)
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding +arg2.159)
               (value
                (Ref
                 ((id +arg2.158)
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
                        ((id +arg1.156)
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id +arg2.159)
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
     ((f.160 (values)) (+arg1.161 (frame 1 2))
      (+arg2.162 (frame (frame 3 4 5) (frame 6 7 8))))
     (let ((f.154 f.160) (+arg1.155 +arg1.161) (+arg2.157 +arg2.162))
      (let ((+arg1.163 +arg1.155) (+arg2.164 +arg2.157))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.166 +arg1.163) (+arg2.167 +arg2.164))
           (let ((+arg1.156 +arg1.166) (+arg2.158 +arg2.167))
            (let ((+arg2.168 +arg2.158))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg2.170 +arg2.168))
                 (let ((+arg2.159 +arg2.170)) (+ +arg1.156 +arg2.159)))
                (body-matcher map-result.169) (map-result (map-result.169))
                (consumer (values))))))))
          (body-matcher map-result.165) (map-result (map-result.165))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((+arg1.163 (frame 1 2)) (+arg2.164 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.166 +arg1.163) (+arg2.167 +arg2.164))
         (let ((+arg2.168 +arg2.167))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.170 +arg2.168)) (+ +arg1.166 +arg2.170))
             (body-matcher map-result.169) (map-result (map-result.169))
             (consumer (values)))))))
        (body-matcher map-result.165) (map-result (map-result.165))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((+arg1.163 (frame 1 2)) (+arg2.164 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.166 +arg1.163) (+arg2.167 +arg2.164))
         (let ((+arg2.168 +arg2.167))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.170 +arg2.168)) (+ +arg1.166 +arg2.170))
             (body-matcher map-result.169) (map-result (map-result.169))
             (consumer (values)))))))
        (body-matcher map-result.165) (map-result (map-result.165))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.192 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.185
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.182 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.163
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.182) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.182) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.182)))
       (+arg2.164
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.185) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.185) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.185) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.185) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.185) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.185) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.185))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.166 +arg1.163) (+arg2.167 +arg2.164) (map-mem.193 map-mem.192))
          (let ((+arg2.168 +arg2.167))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.170 +arg2.168) (map-mem.194 map-mem.193))
               (let ((expr-result.195 (+ +arg1.166 +arg2.170)))
                (begin (putmem expr-result.195 map-mem.194) expr-result.195)))
              (body-matcher map-result.169) (map-result (map-result.169))
              (map-result-mem-final (values map-mem.193)) (consumer (values)))))))
         (body-matcher map-result.165) (map-result (map-result.165))
         (map-result-mem-final (values map-mem.192)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.192 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.185
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.182 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.163
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.182) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.182) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.182)))
       (+arg2.164
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.185) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.185) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.185) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.185) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.185) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.185) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.185))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.166 +arg1.163) (+arg2.167 +arg2.164) (map-mem.193 map-mem.192))
          (let ((+arg2.168 +arg2.167))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.170 +arg2.168) (map-mem.194 map-mem.193))
               (let ((expr-result.195 (+ +arg1.166 +arg2.170)))
                (begin (putmem expr-result.195 map-mem.194) expr-result.195)))
              (body-matcher map-result.169) (map-result (map-result.169))
              (map-result-mem-final (values map-mem.193)) (consumer (values)))))))
         (body-matcher map-result.165) (map-result (map-result.165))
         (map-result-mem-final (values map-mem.192)) (consumer (values))))))) |}]
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
     ((binding words.148)
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
                   (shape ((Add ((const 0) (refs ((len.149 1)))))))))))))))
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
                   (shape ((Add ((const 0) (refs ((len.149 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings ((len.150 Dim))) (valueBinding word.151)
         (box (Ref ((id words.148))))
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
                             (((binding d.1) (bound Dim))
                              ((binding @cell-shape.2) (bound Shape))))
                            (body
                             (Scalar
                              ((element
                                (TypeLambda
                                 ((params (((binding t.3) (bound Atom))))
                                  (body
                                   (Scalar
                                    ((element
                                      (TermLambda
                                       ((params
                                         (((binding arr.4)
                                           (bound
                                            (Arr
                                             ((element (AtomRef t.3))
                                              (shape
                                               ((Add
                                                 ((const 0) (refs ((d.1 1)))))
                                                (ShapeRef @cell-shape.2)))))))))
                                        (body
                                         (ReifyIndex
                                          ((index
                                            (Dimension
                                             ((const 0) (refs ((d.1 1)))))))))))))))))))))))))))
                      (args
                       ((Dimension ((const 0) (refs ((len.150 1))))) (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id word.151)))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding words.148)
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
                      (shape ((Add ((const 0) (refs ((len.149 1)))))))))))))))
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
                      (shape ((Add ((const 0) (refs ((len.149 1)))))))))))))))))))))))
      (body
       (Map
        ((args (((binding box.157) (value (Ref ((id words.148)))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding len.150)
               (indexValue (FromBox (box (Ref ((id box.157)))) (i 0)))
               (sort Dim))))
            (body
             (Map
              ((args
                (((binding word.151)
                  (value (BoxValue ((box (Ref ((id box.157))))))))))
               (body
                (Map
                 ((args
                   (((binding f.156) (value (Primitive ((name (Func Equal))))))
                    ((binding =arg1.152)
                     (value (Scalar ((element (Literal (IntLiteral 3)))))))
                    ((binding =arg2.155)
                     (value
                      (Map
                       ((args
                         (((binding f.154)
                           (value
                            (TypeApplication
                             ((tFunc
                               (IndexApplication
                                ((iFunc
                                  (Scalar
                                   ((element
                                     (IndexLambda
                                      ((params
                                        (((binding d.1) (bound Dim))
                                         ((binding @cell-shape.2) (bound Shape))))
                                       (body
                                        (Scalar
                                         ((element
                                           (TypeLambda
                                            ((params
                                              (((binding t.3) (bound Atom))))
                                             (body
                                              (Scalar
                                               ((element
                                                 (TermLambda
                                                  ((params
                                                    (((binding arr.4)
                                                      (bound
                                                       (Arr
                                                        ((element (AtomRef t.3))
                                                         (shape
                                                          ((Add
                                                            ((const 0)
                                                             (refs ((d.1 1)))))
                                                           (ShapeRef
                                                            @cell-shape.2)))))))))
                                                   (body
                                                    (ReifyIndex
                                                     ((index
                                                       (Dimension
                                                        ((const 0)
                                                         (refs ((d.1 1)))))))))))))))))))))))))))
                                 (args
                                  ((Dimension ((const 0) (refs ((len.150 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal CharacterLiteral))))))))
                          ((binding arr.153) (value (Ref ((id word.151)))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id f.154)))) (args (((id arr.153))))
                           (type'
                            (Arr ((element (Literal IntLiteral)) (shape ())))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id f.156))))
                     (args (((id =arg1.152)) ((id =arg2.155))))
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
       (((binding words.164)
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
                     (shape ((Add ((const 0) (refs ((len.149 1)))))))))
                   (type'
                    ((parameters (((binding len.149) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape ((Add ((const 0) (refs ((len.149 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding len.149) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape ((Add ((const 0) (refs ((len.149 1))))))))))))
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
                     (shape ((Add ((const 0) (refs ((len.149 1)))))))))
                   (type'
                    ((parameters (((binding len.149) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape ((Add ((const 0) (refs ((len.149 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding len.149) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape ((Add ((const 0) (refs ((len.149 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding len.149) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((len.149 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding box.165)
            (value
             (Ref
              ((id words.164)
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding len.149) (bound Dim))))
                    (body
                     ((element (Literal CharacterLiteral))
                      (shape ((Add ((const 0) (refs ((len.149 1))))))))))))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding len.150)
               (indexValue
                (FromBox
                 (box
                  (Ref
                   ((id box.165)
                    (type'
                     ((element
                       (Sigma
                        ((parameters (((binding len.149) (bound Dim))))
                         (body
                          ((element (Literal CharacterLiteral))
                           (shape ((Add ((const 0) (refs ((len.149 1))))))))))))
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
                   (((binding f.158)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding =arg1.159)
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 3)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding =arg2.163)
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.161)
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ReifyIndex
                          ((index (Dimension ((const 0) (refs ((len.150 1))))))
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
                              ((id =arg1.159)
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id =arg2.163)
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
     ((words.166 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (let ((words.164 words.166))
      (let ((box.167 words.164))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((box.169 box.167))
           (let ((box.165 box.169))
            (index-let ((len.150 box-index-0 box.165))
             (let ()
              (let ()
               (let
                ((f.170 (values)) (=arg1.171 3)
                 (=arg2.173
                  (let ((f.172 (values)))
                   (let ((f.161 f.172)) (reify-index len.150)))))
                (let ((f.158 f.170) (=arg1.159 =arg1.171) (=arg2.163 =arg2.173))
                 (= =arg1.159 =arg2.163))))))))
          (body-matcher map-result.168) (map-result (map-result.168))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((box.167 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.169 box.167))
         (index-let ((len.150 box-index-0 box.169)) (= 3 (reify-index len.150))))
        (body-matcher map-result.168) (map-result (map-result.168))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((box.167 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.169 box.167))
         (index-let ((len.150 box-index-0 box.169)) (= 3 (reify-index len.150))))
        (body-matcher map-result.168) (map-result (map-result.168))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.188 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.184
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.179
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.178
       (Array
        ((element
          (Sigma
           ((parameters (((binding len.149) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.149))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.167
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.179) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.179) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.179) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.179)))
           (index (mem frame-array.178) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding len.149) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.149)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.184) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.184) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.184)))
           (index (mem frame-array.178) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding len.149) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.149))))))))))))
         (getmem frame-array.178))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.169 box.167) (map-mem.189 map-mem.188))
          (index-let ((len.150 box-index-0 box.169))
           (let ((expr-result.190 (= 3 (reify-dimension-index len.150))))
            (begin (putmem expr-result.190 map-mem.189) expr-result.190))))
         (body-matcher map-result.168) (map-result (map-result.168))
         (map-result-mem-final (values map-mem.188)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.188 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.184
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.179
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.178
       (Array
        ((element
          (Sigma
           ((parameters (((binding len.149) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.149))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.167
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.179) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.179) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.179) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.179)))
           (index (mem frame-array.178) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding len.149) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.149)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.184) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.184) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.184)))
           (index (mem frame-array.178) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding len.149) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.149))))))))))))
         (getmem frame-array.178))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.169 box.167) (map-mem.189 map-mem.188))
          (index-let ((len.150 box-index-0 box.169))
           (let ((expr-result.190 (= 3 (reify-dimension-index len.150))))
            (begin (putmem expr-result.190 map-mem.189) expr-result.190))))
         (body-matcher map-result.168) (map-result (map-result.168))
         (map-result-mem-final (values map-mem.188)) (consumer (values))))))) |}]
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
     ((binding sum-row.148)
      (value
       (Scalar
        ((element
          (IndexLambda
           ((params (((binding d-1.149) (bound Dim))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding row.150)
                     (bound
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape ((Add ((const 1) (refs ((d-1.149 1)))))))))))))
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
                                  (((binding d-1.64) (bound Dim))
                                   ((binding @item-pad.65) (bound Shape))
                                   ((binding @cell-shape.66) (bound Shape))))
                                 (body
                                  (Scalar
                                   ((element
                                     (TypeLambda
                                      ((params (((binding t.67) (bound Atom))))
                                       (body
                                        (Scalar
                                         ((element
                                           (TermLambda
                                            ((params
                                              (((binding op.68)
                                                (bound
                                                 (Arr
                                                  ((element
                                                    (Func
                                                     ((parameters
                                                       ((Arr
                                                         ((element
                                                           (AtomRef t.67))
                                                          (shape
                                                           ((ShapeRef
                                                             @cell-shape.66)))))
                                                        (Arr
                                                         ((element
                                                           (AtomRef t.67))
                                                          (shape
                                                           ((ShapeRef
                                                             @cell-shape.66)))))))
                                                      (return
                                                       (Arr
                                                        ((element (AtomRef t.67))
                                                         (shape
                                                          ((ShapeRef
                                                            @cell-shape.66)))))))))
                                                   (shape ())))))
                                               ((binding arr.69)
                                                (bound
                                                 (Arr
                                                  ((element (AtomRef t.67))
                                                   (shape
                                                    ((Add
                                                      ((const 1)
                                                       (refs ((d-1.64 1)))))
                                                     (ShapeRef @item-pad.65)
                                                     (ShapeRef @cell-shape.66)))))))))
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
                                                             (((binding d.54)
                                                               (bound Dim))
                                                              ((binding
                                                                @item-pad.55)
                                                               (bound Shape))
                                                              ((binding
                                                                @cell-shape.56)
                                                               (bound Shape))))
                                                            (body
                                                             (Scalar
                                                              ((element
                                                                (TypeLambda
                                                                 ((params
                                                                   (((binding
                                                                      t.57)
                                                                     (bound Atom))))
                                                                  (body
                                                                   (Scalar
                                                                    ((element
                                                                      (TermLambda
                                                                       ((params
                                                                        (((binding
                                                                        f.58)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (Func
                                                                        ((parameters
                                                                        ((Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.56)))))
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.56)))))))
                                                                        (return
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.56)))))))))
                                                                        (shape
                                                                        ())))))
                                                                        ((binding
                                                                        init.59)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.56)))))))
                                                                        ((binding
                                                                        arr.60)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((d.54 1)))))
                                                                        (ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56)))))))))
                                                                        (body
                                                                        (Let
                                                                        ((binding
                                                                        up-ranked-f.61)
                                                                        (value
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        a.62)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56)))))))
                                                                        ((binding
                                                                        b.63)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56)))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        f.58))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        a.62)))
                                                                        (Ref
                                                                        ((id
                                                                        b.63))))))))))))))
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
                                                                        ((d.54 1)))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        t.57)))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        up-ranked-f.61)))
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
                                                                        @s.6)
                                                                        (bound
                                                                        Shape))
                                                                        ((binding
                                                                        @cell-shape.7)
                                                                        (bound
                                                                        Shape))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        t.8)
                                                                        (bound
                                                                        Atom))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        v.9)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.8))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.7)))))))))
                                                                        (body
                                                                        (Let
                                                                        ((binding
                                                                        make.10)
                                                                        (value
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        foo.11)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (Literal
                                                                        IntLiteral))
                                                                        (shape
                                                                        ())))))
                                                                        ((binding
                                                                        v.12)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.8))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.7)))))))))
                                                                        (body
                                                                        (Ref
                                                                        ((id
                                                                        v.12))))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        make.10))))
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
                                                                        @s.6)))))))
                                                                        (Ref
                                                                        ((id v.9)))))))))))))))))))))))))))))
                                                                        (args
                                                                        ((Shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        @cell-shape.56))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        t.57)))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        init.59)))))))
                                                                        (Ref
                                                                        ((id
                                                                        arr.60)))))))))))))))))))))))))))))
                                                      (args
                                                       ((Dimension
                                                         ((const 0)
                                                          (refs ((d-1.64 1)))))
                                                        (Shape
                                                         ((ShapeRef @item-pad.65)))
                                                        (Shape
                                                         ((ShapeRef
                                                           @cell-shape.66))))))))
                                                   (args ((Atom (AtomRef t.67)))))))
                                                (args
                                                 ((Ref ((id op.68)))
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
                                                                    d-1.24)
                                                                   (bound Dim))
                                                                  ((binding
                                                                    @cell-shape.25)
                                                                   (bound Shape))))
                                                                (body
                                                                 (Scalar
                                                                  ((element
                                                                    (TypeLambda
                                                                     ((params
                                                                       (((binding
                                                                        t.26)
                                                                        (bound
                                                                        Atom))))
                                                                      (body
                                                                       (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        arr.27)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.26))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((d-1.24
                                                                        1)))))
                                                                        (ShapeRef
                                                                        @cell-shape.25)))))))))
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
                                                                        ((d-1.24
                                                                        1)))))))
                                                                        (Shape
                                                                        ())
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        @cell-shape.25)))
                                                                        (Dimension
                                                                        ((const
                                                                        1)
                                                                        (refs ()))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        t.26)))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        arr.27)))
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
                                                              (refs ((d-1.64 1)))))
                                                            (Shape
                                                             ((ShapeRef
                                                               @item-pad.65)
                                                              (ShapeRef
                                                               @cell-shape.66))))))))
                                                       (args
                                                        ((Atom (AtomRef t.67)))))))
                                                    (args ((Ref ((id arr.69)))))))
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
                                                                    d-1.28)
                                                                   (bound Dim))
                                                                  ((binding
                                                                    @cell-shape.29)
                                                                   (bound Shape))))
                                                                (body
                                                                 (Scalar
                                                                  ((element
                                                                    (TypeLambda
                                                                     ((params
                                                                       (((binding
                                                                        t.30)
                                                                        (bound
                                                                        Atom))))
                                                                      (body
                                                                       (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        arr.31)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.30))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((d-1.28
                                                                        1)))))
                                                                        (ShapeRef
                                                                        @cell-shape.29)))))))))
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
                                                                        ((d-1.28
                                                                        1)))))))
                                                                        (Shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((d-1.28
                                                                        1)))))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        @cell-shape.29)))
                                                                        (Dimension
                                                                        ((const
                                                                        1)
                                                                        (refs ()))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        t.30)))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        arr.31)))
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
                                                              (refs ((d-1.64 1)))))
                                                            (Shape
                                                             ((ShapeRef
                                                               @item-pad.65)
                                                              (ShapeRef
                                                               @cell-shape.66))))))))
                                                       (args
                                                        ((Atom (AtomRef t.67)))))))
                                                    (args ((Ref ((id arr.69))))))))))))))))))))))))))))))
                           (args
                            ((Dimension ((const 0) (refs ((d-1.149 1)))))
                             (Shape ()) (Shape ()))))))
                        (args ((Atom (Literal IntLiteral)))))))
                     (args
                      ((Primitive ((name (Func Add)))) (Ref ((id row.150))))))))))))))))))))
      (body
       (TermApplication
        ((func
          (IndexApplication
           ((iFunc (Ref ((id sum-row.148))))
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
       (((binding sum-row.148)
         (value
          (Scalar
           ((element
             (IndexLambda
              ((params (((binding d-1.149) (bound Dim))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding row.150)
                        (bound
                         (Arr
                          ((element (Literal IntLiteral))
                           (shape ((Add ((const 1) (refs ((d-1.149 1)))))))))))))
                     (body
                      (Map
                       ((args
                         (((binding f.183)
                           (value
                            (TypeApplication
                             ((tFunc
                               (IndexApplication
                                ((iFunc
                                  (Scalar
                                   ((element
                                     (IndexLambda
                                      ((params
                                        (((binding d-1.64) (bound Dim))
                                         ((binding @item-pad.65) (bound Shape))
                                         ((binding @cell-shape.66) (bound Shape))))
                                       (body
                                        (Scalar
                                         ((element
                                           (TypeLambda
                                            ((params
                                              (((binding t.67) (bound Atom))))
                                             (body
                                              (Scalar
                                               ((element
                                                 (TermLambda
                                                  ((params
                                                    (((binding op.68)
                                                      (bound
                                                       (Arr
                                                        ((element
                                                          (Func
                                                           ((parameters
                                                             ((Arr
                                                               ((element
                                                                 (AtomRef t.67))
                                                                (shape
                                                                 ((ShapeRef
                                                                   @cell-shape.66)))))
                                                              (Arr
                                                               ((element
                                                                 (AtomRef t.67))
                                                                (shape
                                                                 ((ShapeRef
                                                                   @cell-shape.66)))))))
                                                            (return
                                                             (Arr
                                                              ((element
                                                                (AtomRef t.67))
                                                               (shape
                                                                ((ShapeRef
                                                                  @cell-shape.66)))))))))
                                                         (shape ())))))
                                                     ((binding arr.69)
                                                      (bound
                                                       (Arr
                                                        ((element (AtomRef t.67))
                                                         (shape
                                                          ((Add
                                                            ((const 1)
                                                             (refs ((d-1.64 1)))))
                                                           (ShapeRef
                                                            @item-pad.65)
                                                           (ShapeRef
                                                            @cell-shape.66)))))))))
                                                   (body
                                                    (Map
                                                     ((args
                                                       (((binding f.181)
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
                                                                        d.54)
                                                                        (bound
                                                                        Dim))
                                                                       ((binding
                                                                        @item-pad.55)
                                                                        (bound
                                                                        Shape))
                                                                       ((binding
                                                                        @cell-shape.56)
                                                                        (bound
                                                                        Shape))))
                                                                     (body
                                                                      (Scalar
                                                                       ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        t.57)
                                                                        (bound
                                                                        Atom))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        f.58)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (Func
                                                                        ((parameters
                                                                        ((Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.56)))))
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.56)))))))
                                                                        (return
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.56)))))))))
                                                                        (shape
                                                                        ())))))
                                                                        ((binding
                                                                        init.59)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.56)))))))
                                                                        ((binding
                                                                        arr.60)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((d.54 1)))))
                                                                        (ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56)))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        up-ranked-f.61)
                                                                        (value
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        a.62)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56)))))))
                                                                        ((binding
                                                                        b.63)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56)))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        f.168)
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        f.58)))))
                                                                        ((binding
                                                                        arg0.166)
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        a.62)))))
                                                                        ((binding
                                                                        arg1.167)
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        b.63)))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        arg0.169)
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        arg0.166)))))
                                                                        ((binding
                                                                        arg1.170)
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        arg1.167)))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        f.168))))
                                                                        (args
                                                                        (((id
                                                                        arg0.169))
                                                                        ((id
                                                                        arg1.170))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.56)))))))))
                                                                        (frameShape
                                                                        ((ShapeRef
                                                                        @item-pad.55)))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56)))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56))))))))))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        f.180)
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
                                                                        ((d.54 1)))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        t.57))))))))
                                                                        ((binding
                                                                        reduce-f-arg.171)
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        up-ranked-f.61)))))
                                                                        ((binding
                                                                        reduce-zero-arg.178)
                                                                        (value
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        f.177)
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
                                                                        @s.6)
                                                                        (bound
                                                                        Shape))
                                                                        ((binding
                                                                        @cell-shape.7)
                                                                        (bound
                                                                        Shape))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        t.8)
                                                                        (bound
                                                                        Atom))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        v.9)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.8))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.7)))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        make.10)
                                                                        (value
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        foo.11)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (Literal
                                                                        IntLiteral))
                                                                        (shape
                                                                        ())))))
                                                                        ((binding
                                                                        v.12)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.8))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.7)))))))))
                                                                        (body
                                                                        (Ref
                                                                        ((id
                                                                        v.12)))))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        f.175)
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        make.10)))))
                                                                        ((binding
                                                                        foo.173)
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
                                                                        @s.6)))))))))
                                                                        ((binding
                                                                        v.174)
                                                                        (value
                                                                        (Ref
                                                                        ((id v.9)))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        foo.176)
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        foo.173)))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        f.175))))
                                                                        (args
                                                                        (((id
                                                                        foo.176))
                                                                        ((id
                                                                        v.174))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.8))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.7)))))))))
                                                                        (frameShape
                                                                        ((ShapeRef
                                                                        @s.6)))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.8))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @s.6)
                                                                        (ShapeRef
                                                                        @cell-shape.7)))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.8))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @s.6)
                                                                        (ShapeRef
                                                                        @cell-shape.7)))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.8))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @s.6)
                                                                        (ShapeRef
                                                                        @cell-shape.7)))))))))))))))))))))))))))
                                                                        (args
                                                                        ((Shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        @cell-shape.56))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        t.57))))))))
                                                                        ((binding
                                                                        v.172)
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        init.59)))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        f.177))))
                                                                        (args
                                                                        (((id
                                                                        v.172))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56)))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56))))))))))
                                                                        ((binding
                                                                        reduce-array-arg.179)
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        arr.60)))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        f.180))))
                                                                        (args
                                                                        (((id
                                                                        reduce-f-arg.171))
                                                                        ((id
                                                                        reduce-zero-arg.178))
                                                                        ((id
                                                                        reduce-array-arg.179))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56)))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56)))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.57))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @item-pad.55)
                                                                        (ShapeRef
                                                                        @cell-shape.56)))))))))))))))))))))))))))
                                                               (args
                                                                ((Dimension
                                                                  ((const 0)
                                                                   (refs
                                                                    ((d-1.64 1)))))
                                                                 (Shape
                                                                  ((ShapeRef
                                                                    @item-pad.65)))
                                                                 (Shape
                                                                  ((ShapeRef
                                                                    @cell-shape.66))))))))
                                                            (args
                                                             ((Atom
                                                               (AtomRef t.67))))))))
                                                        ((binding f.153)
                                                         (value
                                                          (Ref ((id op.68)))))
                                                        ((binding init.159)
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding f.158)
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
                                                                        d-1.24)
                                                                        (bound
                                                                        Dim))
                                                                        ((binding
                                                                        @cell-shape.25)
                                                                        (bound
                                                                        Shape))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        t.26)
                                                                        (bound
                                                                        Atom))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        arr.27)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.26))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((d-1.24
                                                                        1)))))
                                                                        (ShapeRef
                                                                        @cell-shape.25)))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        f.157)
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
                                                                        ((d-1.24
                                                                        1)))))))
                                                                        (Shape
                                                                        ())
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        @cell-shape.25)))
                                                                        (Dimension
                                                                        ((const
                                                                        1)
                                                                        (refs ()))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        t.26))))))))
                                                                        ((binding
                                                                        contiguous-subarray-array.155)
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        arr.27)))))
                                                                        ((binding
                                                                        contiguous-subarray-index.156)
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
                                                                        f.157))))
                                                                        (args
                                                                        (((id
                                                                        contiguous-subarray-array.155))
                                                                        ((id
                                                                        contiguous-subarray-index.156))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.26))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.25)))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.26))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        @cell-shape.25)))))))))))))))))))))))))))
                                                                     (args
                                                                      ((Dimension
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((d-1.64
                                                                        1)))))
                                                                       (Shape
                                                                        ((ShapeRef
                                                                        @item-pad.65)
                                                                        (ShapeRef
                                                                        @cell-shape.66))))))))
                                                                  (args
                                                                   ((Atom
                                                                     (AtomRef
                                                                      t.67))))))))
                                                              ((binding arr.154)
                                                               (value
                                                                (Ref
                                                                 ((id arr.69)))))))
                                                            (body
                                                             (TermApplication
                                                              ((func
                                                                (Ref
                                                                 ((id f.158))))
                                                               (args
                                                                (((id arr.154))))
                                                               (type'
                                                                (Arr
                                                                 ((element
                                                                   (AtomRef t.67))
                                                                  (shape
                                                                   ((ShapeRef
                                                                     @item-pad.65)
                                                                    (ShapeRef
                                                                     @cell-shape.66)))))))))
                                                            (frameShape ())
                                                            (type'
                                                             (Arr
                                                              ((element
                                                                (AtomRef t.67))
                                                               (shape
                                                                ((ShapeRef
                                                                  @item-pad.65)
                                                                 (ShapeRef
                                                                  @cell-shape.66))))))))))
                                                        ((binding arr.165)
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding f.164)
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
                                                                        d-1.28)
                                                                        (bound
                                                                        Dim))
                                                                        ((binding
                                                                        @cell-shape.29)
                                                                        (bound
                                                                        Shape))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        t.30)
                                                                        (bound
                                                                        Atom))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        arr.31)
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.30))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((d-1.28
                                                                        1)))))
                                                                        (ShapeRef
                                                                        @cell-shape.29)))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        f.163)
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
                                                                        ((d-1.28
                                                                        1)))))))
                                                                        (Shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((d-1.28
                                                                        1)))))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        @cell-shape.29)))
                                                                        (Dimension
                                                                        ((const
                                                                        1)
                                                                        (refs ()))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        t.30))))))))
                                                                        ((binding
                                                                        contiguous-subarray-array.161)
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        arr.31)))))
                                                                        ((binding
                                                                        contiguous-subarray-index.162)
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
                                                                        f.163))))
                                                                        (args
                                                                        (((id
                                                                        contiguous-subarray-array.161))
                                                                        ((id
                                                                        contiguous-subarray-index.162))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.30))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((d-1.28
                                                                        1)))))
                                                                        (ShapeRef
                                                                        @cell-shape.29)))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        t.30))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((d-1.28
                                                                        1)))))
                                                                        (ShapeRef
                                                                        @cell-shape.29)))))))))))))))))))))))))))
                                                                     (args
                                                                      ((Dimension
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((d-1.64
                                                                        1)))))
                                                                       (Shape
                                                                        ((ShapeRef
                                                                        @item-pad.65)
                                                                        (ShapeRef
                                                                        @cell-shape.66))))))))
                                                                  (args
                                                                   ((Atom
                                                                     (AtomRef
                                                                      t.67))))))))
                                                              ((binding arr.160)
                                                               (value
                                                                (Ref
                                                                 ((id arr.69)))))))
                                                            (body
                                                             (TermApplication
                                                              ((func
                                                                (Ref
                                                                 ((id f.164))))
                                                               (args
                                                                (((id arr.160))))
                                                               (type'
                                                                (Arr
                                                                 ((element
                                                                   (AtomRef t.67))
                                                                  (shape
                                                                   ((Add
                                                                     ((const 0)
                                                                      (refs
                                                                       ((d-1.64
                                                                        1)))))
                                                                    (ShapeRef
                                                                     @item-pad.65)
                                                                    (ShapeRef
                                                                     @cell-shape.66)))))))))
                                                            (frameShape ())
                                                            (type'
                                                             (Arr
                                                              ((element
                                                                (AtomRef t.67))
                                                               (shape
                                                                ((Add
                                                                  ((const 0)
                                                                   (refs
                                                                    ((d-1.64 1)))))
                                                                 (ShapeRef
                                                                  @item-pad.65)
                                                                 (ShapeRef
                                                                  @cell-shape.66))))))))))))
                                                      (body
                                                       (Map
                                                        ((args
                                                          (((binding init.182)
                                                            (value
                                                             (Ref
                                                              ((id init.159)))))))
                                                         (body
                                                          (TermApplication
                                                           ((func
                                                             (Ref ((id f.181))))
                                                            (args
                                                             (((id f.153))
                                                              ((id init.182))
                                                              ((id arr.165))))
                                                            (type'
                                                             (Arr
                                                              ((element
                                                                (AtomRef t.67))
                                                               (shape
                                                                ((ShapeRef
                                                                  @item-pad.65)
                                                                 (ShapeRef
                                                                  @cell-shape.66)))))))))
                                                         (frameShape
                                                          ((ShapeRef
                                                            @item-pad.65)))
                                                         (type'
                                                          (Arr
                                                           ((element
                                                             (AtomRef t.67))
                                                            (shape
                                                             ((ShapeRef
                                                               @item-pad.65)
                                                              (ShapeRef
                                                               @item-pad.65)
                                                              (ShapeRef
                                                               @cell-shape.66)))))))))
                                                      (frameShape ())
                                                      (type'
                                                       (Arr
                                                        ((element (AtomRef t.67))
                                                         (shape
                                                          ((ShapeRef
                                                            @item-pad.65)
                                                           (ShapeRef
                                                            @item-pad.65)
                                                           (ShapeRef
                                                            @cell-shape.66)))))))))))))))))))))))))))
                                 (args
                                  ((Dimension ((const 0) (refs ((d-1.149 1)))))
                                   (Shape ()) (Shape ()))))))
                              (args ((Atom (Literal IntLiteral))))))))
                          ((binding op.151)
                           (value (Primitive ((name (Func Add))))))
                          ((binding arr.152) (value (Ref ((id row.150)))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id f.183))))
                           (args (((id op.151)) ((id arr.152))))
                           (type'
                            (Arr ((element (Literal IntLiteral)) (shape ())))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding f.185)
            (value
             (IndexApplication
              ((iFunc (Ref ((id sum-row.148))))
               (args ((Dimension ((const 9) (refs ())))))))))
           ((binding row.184)
            (value
             (IndexApplication
              ((iFunc (Primitive ((name (Val Iota)))))
               (args
                ((Shape
                  ((Add ((const 1000000) (refs ())))
                   (Add ((const 10) (refs ())))))))))))))
         (body
          (Map
           ((args (((binding row.186) (value (Ref ((id row.184)))))))
            (body
             (TermApplication
              ((func (Ref ((id f.185)))) (args (((id row.186))))
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
       (((binding sum-row.188)
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding f.189)
            (value
             (Ref ((id sum-row.188) (type' ((element (Tuple ())) (shape ())))))))
           ((binding row.336)
            (value
             (ArrayPrimitive
              (Map
               (frameShape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
               (args ()) (iotaVar (iota.335))
               (body
                (Ref
                 ((id iota.335)
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
             (((binding row.337)
               (value
                (Ref
                 ((id row.336)
                  (type'
                   ((element (Literal IntLiteral))
                    (shape
                     ((Add ((const 1000000) (refs ())))
                      (Add ((const 10) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding f.239)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding op.325)
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding arr.338)
                  (value
                   (Ref
                    ((id row.337)
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 10) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding f.285)
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding f.326)
                     (value
                      (Ref
                       ((id op.325) (type' ((element (Tuple ())) (shape ())))))))
                    ((binding init.375)
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.366)
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding arr.372)
                           (value
                            (Ref
                             ((id arr.338)
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 10) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding f.371)
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding contiguous-subarray-array.373)
                              (value
                               (Ref
                                ((id arr.372)
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding contiguous-subarray-index.374)
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
                                ((id contiguous-subarray-array.373)
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id contiguous-subarray-index.374)
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 1) (refs ()))))))))))
                              (originalShape ((Add ((const 10) (refs ())))))
                              (resultShape ()) (cellShape ())
                              (l ((const 1) (refs ())))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding arr.342)
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding f.329)
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding arr.339)
                           (value
                            (Ref
                             ((id arr.338)
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 10) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding f.334)
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding contiguous-subarray-array.340)
                              (value
                               (Ref
                                ((id arr.339)
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding contiguous-subarray-index.341)
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
                                ((id contiguous-subarray-array.340)
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id contiguous-subarray-index.341)
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
                      (((binding init.376)
                        (value
                         (Ref
                          ((id init.375)
                           (type' ((element (Literal IntLiteral)) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding up-ranked-f.313)
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding f.311)
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding reduce-f-arg.314)
                              (value
                               (Ref
                                ((id up-ranked-f.313)
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding reduce-zero-arg.379)
                              (value
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding f.351)
                                    (value
                                     (AtomAsArray
                                      ((element
                                        (Values ((elements ()) (type' ()))))
                                       (type' ((element (Tuple ())) (shape ())))))))
                                   ((binding v.377)
                                    (value
                                     (Ref
                                      ((id init.376)
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding make.361)
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
                                        (((binding f.362)
                                          (value
                                           (Ref
                                            ((id make.361)
                                             (type'
                                              ((element (Tuple ())) (shape ())))))))
                                         ((binding v.378)
                                          (value
                                           (Ref
                                            ((id v.377)
                                             (type'
                                              ((element (Literal IntLiteral))
                                               (shape ())))))))))
                                       (body
                                        (ArrayPrimitive
                                         (Map (frameShape ()) (args ())
                                          (body
                                           (Ref
                                            ((id v.378)
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
                             ((binding reduce-array-arg.343)
                              (value
                               (Ref
                                ((id arr.342)
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 9) (refs ())))))))))))))
                           (body
                            (ArrayPrimitive
                             (Reduce
                              (arg
                               ((firstBinding reduce-arg1.344)
                                (secondBinding reduce-arg2.347)
                                (value
                                 (Ref
                                  ((id reduce-array-arg.343)
                                   (type'
                                    ((element (Literal IntLiteral))
                                     (shape ((Add ((const 9) (refs ()))))))))))))
                              (zero
                               (Ref
                                ((id reduce-zero-arg.379)
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding f.327)
                                    (value
                                     (Ref
                                      ((id f.326)
                                       (type' ((element (Tuple ())) (shape ())))))))
                                   ((binding arg0.345)
                                    (value
                                     (Ref
                                      ((id reduce-arg1.344)
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))
                                   ((binding arg1.348)
                                    (value
                                     (Ref
                                      ((id reduce-arg2.347)
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding arg0.346)
                                       (value
                                        (Ref
                                         ((id arg0.345)
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))
                                      ((binding arg1.349)
                                       (value
                                        (Ref
                                         ((id arg1.348)
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
                                                ((id arg0.346)
                                                 (type'
                                                  ((element (Literal IntLiteral))
                                                   (shape ()))))))
                                              (type' (Literal IntLiteral))))
                                            (ArrayAsAtom
                                             ((array
                                               (Ref
                                                ((id arg1.349)
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
    (let ((sum-row.380 (values)))
     (let ((sum-row.188 sum-row.380))
      (let
       ((f.381 sum-row.188)
        (row.388
         (let ()
          (#0
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota iota.383)
              (let ((iota-offset.387 (* iota.383 (shape-prod (shape 10)))))
               (#0
                (#0
                 (loop-block (frame-shape 10)
                  (map () (iota iota.385)
                   (let ((iota.386 (+ iota-offset.387 iota.385)))
                    (let ((iota.335 iota.386)) iota.335)))
                  (body-matcher map-result.384) (map-result (map-result.384))
                  (consumer (values)))))))
             (body-matcher map-result.382) (map-result (map-result.382))
             (consumer (values))))))))
       (let ((f.189 f.381) (row.336 row.388))
        (let ((row.389 row.336))
         (#0
          (#0
           (loop-block (frame-shape 1000000)
            (map ((row.391 row.389))
             (let ((row.337 row.391))
              (let ((f.392 (values)) (op.393 (values)) (arr.394 row.337))
               (let ((f.239 f.392) (op.325 op.393) (arr.338 arr.394))
                (let
                 ((f.395 (values)) (f.396 op.325)
                  (init.402
                   (let ((f.397 (values)) (arr.398 arr.338))
                    (let ((f.366 f.397) (arr.372 arr.398))
                     (let
                      ((f.399 (values)) (contiguous-subarray-array.400 arr.372)
                       (contiguous-subarray-index.401 (frame 0)))
                      (let
                       ((f.371 f.399)
                        (contiguous-subarray-array.373
                         contiguous-subarray-array.400)
                        (contiguous-subarray-index.374
                         contiguous-subarray-index.401))
                       (contiguous-subarray contiguous-subarray-array.373
                        contiguous-subarray-index.374 (shape 10) (shape)))))))
                  (arr.408
                   (let ((f.403 (values)) (arr.404 arr.338))
                    (let ((f.329 f.403) (arr.339 arr.404))
                     (let
                      ((f.405 (values)) (contiguous-subarray-array.406 arr.339)
                       (contiguous-subarray-index.407 (frame 1)))
                      (let
                       ((f.334 f.405)
                        (contiguous-subarray-array.340
                         contiguous-subarray-array.406)
                        (contiguous-subarray-index.341
                         contiguous-subarray-index.407))
                       (contiguous-subarray contiguous-subarray-array.340
                        contiguous-subarray-index.341 (shape 10) (shape 9))))))))
                 (let
                  ((f.285 f.395) (f.326 f.396) (init.375 init.402)
                   (arr.342 arr.408))
                  (let ((init.409 init.375))
                   (let ((init.376 init.409))
                    (let ((up-ranked-f.410 (values)))
                     (let ((up-ranked-f.313 up-ranked-f.410))
                      (let
                       ((f.411 (values)) (reduce-f-arg.412 up-ranked-f.313)
                        (reduce-zero-arg.418
                         (let ((f.413 (values)) (v.414 init.376))
                          (let ((f.351 f.413) (v.377 v.414))
                           (let ((make.415 (values)))
                            (let ((make.361 make.415))
                             (let ((f.416 make.361) (v.417 v.377))
                              (let ((f.362 f.416) (v.378 v.417))
                               (let () (let () v.378)))))))))
                        (reduce-array-arg.419 arr.342))
                       (let
                        ((f.311 f.411) (reduce-f-arg.314 reduce-f-arg.412)
                         (reduce-zero-arg.379 reduce-zero-arg.418)
                         (reduce-array-arg.343 reduce-array-arg.419))
                        (let ((reduce-arg.426 reduce-array-arg.343))
                         (#1
                          (loop-block (frame-shape 9)
                           (map ((reduce-arg.427 reduce-arg.426))
                            (values reduce-arg.427))
                           (body-matcher (reduce-arg.420)) (map-result ())
                           (consumer
                            (reduce-zero reduce-zero-arg.379
                             (reduce-arg1.344 reduce-arg2.347 reduce-arg.420)
                             (let
                              ((f.421 f.326) (arg0.422 reduce-arg1.344)
                               (arg1.423 reduce-arg2.347))
                              (let
                               ((f.327 f.421) (arg0.345 arg0.422)
                                (arg1.348 arg1.423))
                               (let ((arg0.424 arg0.345) (arg1.425 arg1.348))
                                (let ((arg0.346 arg0.424) (arg1.349 arg1.425))
                                 (+ arg0.346 arg1.349))))))))))))))))))))))
            (body-matcher map-result.390) (map-result (map-result.390))
            (consumer (values))))))))))
    Result of stage Fuse and Simplify:
    (let
     ((contiguous-subarray-index.401 (frame 0))
      (contiguous-subarray-index.407 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 1000000)
        (map () (iota iota.383)
         (let ((iota-offset.387 (* iota.383 10)))
          (let
           ((map-result.462
             (#0
              (loop-block (frame-shape 10)
               (map () (iota iota.385) (+ iota-offset.387 iota.385))
               (body-matcher map-result.384) (map-result (map-result.384))
               (consumer (values))))))
           (let
            ((reduce-arg.456
              (contiguous-subarray (#0 map-result.462)
               contiguous-subarray-index.407 (shape 10) (shape 9))))
            (#1
             (loop-block (frame-shape 9)
              (map ((reduce-arg.427 reduce-arg.456)) reduce-arg.427)
              (body-matcher reduce-arg.420) (map-result ())
              (consumer
               (reduce-zero
                (contiguous-subarray (#0 map-result.462)
                 contiguous-subarray-index.401 (shape 10) (shape))
                (reduce-arg1.344 reduce-arg2.347 reduce-arg.420)
                (+ reduce-arg1.344 reduce-arg2.347)))))))))
        (body-matcher map-result.390) (map-result (map-result.390))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((contiguous-subarray-index.401 (frame 0))
      (contiguous-subarray-index.407 (frame 1)))
     (#0
      (kernel (blocks 20) (threads 512)
       (map-kernel (frame-shape 1000000) () (iota iota.383)
        (body-matcher map-result.390) (map-result (map-result.390))
        (let ((iota-offset.387 (* iota.383 10)))
         (let
          ((map-result.462
            (#0
             (loop-block (frame-shape 10)
              (map () (iota iota.385) (+ iota-offset.387 iota.385))
              (body-matcher map-result.384) (map-result (map-result.384))
              (consumer (values))))))
          (let
           ((reduce-arg.456
             (contiguous-subarray (#0 map-result.462)
              contiguous-subarray-index.407 (shape 10) (shape 9))))
           (#1
            (loop-block (frame-shape 9)
             (map ((reduce-arg.427 reduce-arg.456)) reduce-arg.427)
             (body-matcher reduce-arg.420) (map-result ())
             (consumer
              (reduce-zero
               (contiguous-subarray (#0 map-result.462)
                contiguous-subarray-index.401 (shape 10) (shape))
               (reduce-arg1.344 reduce-arg2.347 reduce-arg.420)
               (+ reduce-arg1.344 reduce-arg2.347))))))))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.491 (Tuple ()) device)
      (map-mem.487
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.485
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.484
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.482 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.480 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.401
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.480) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.480)))
       (contiguous-subarray-index.407
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.482) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.482))))
      (#0
       (begin
        (kernel (blocks 20) (threads 512)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.486 map-mem.485))
            (iota iota.383)
            (do-expr
             (let ((iota-offset.387 (* iota.383 10)))
              (let
               ((map-result.462
                 (#0
                  (loop (frame-shape 10)
                   (map ((map-mem.488 map-mem.487)) (iota iota.385)
                    (let ((expr-result.489 (+ iota-offset.387 iota.385)))
                     (begin (putmem expr-result.489 (#0 map-mem.488))
                      expr-result.489)))
                   (body-matcher map-result.384) (map-result (map-result.384))
                   (map-result-mem-final map-mem.487) (consumer (values))))))
               (let
                ((reduce-arg.456
                  (index
                   (#0
                    (let ((expr-result.490 map-result.462))
                     (values (#0 expr-result.490))))
                   contiguous-subarray-index.407 (shape 10) (shape 9))))
                (#1
                 (let
                  ((expr-result.494
                    (loop (frame-shape 9)
                     (map
                      ((reduce-arg.427 reduce-arg.456) (map-mem.492 map-mem.491))
                      reduce-arg.427)
                     (body-matcher reduce-arg.420) (map-result ())
                     (map-result-mem-final map-mem.491)
                     (consumer
                      (reduce-zero
                       (index
                        (#0
                         (let ((expr-result.493 map-result.462))
                          (values (#0 expr-result.493))))
                        contiguous-subarray-index.401 (shape 10) (shape))
                       (reduce-arg1.344 reduce-arg2.347 reduce-arg.420)
                       (+ reduce-arg1.344 reduce-arg2.347))))))
                  (begin (putmem (#1 expr-result.494) (#0 map-mem.486))
                   expr-result.494)))))))))
          (map-result-mem-interim map-mem.485)
          (map-result-mem-final (values map-mem-result.0.484))))
        (getmem (values map-mem-result.0.484))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.491 (Tuple ()) device)
      (map-mem.487
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.485
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.484
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.482 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.480 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.401
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.480) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.480)))
       (contiguous-subarray-index.407
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.482) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.482))))
      (#0
       (begin
        (kernel
         (captures
          ((expr-captures
            ((contiguous-subarray-index.401
              (Array ((element (Literal IntLiteral)) (shape (shape 1)))))
             (contiguous-subarray-index.407
              (Array ((element (Literal IntLiteral)) (shape (shape 1)))))))
           (mem-captures
            ((map-mem.485
              (Tuple
               ((Array ((element (Literal IntLiteral)) (shape (shape 1000000)))))))
             (map-mem.487
              (Tuple
               ((Array ((element (Literal IntLiteral)) (shape (shape 10)))))))
             (map-mem.491 (Tuple ()))))
           (index-captures ())))
         (blocks 20) (threads 512)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.486 map-mem.485))
            (iota iota.383)
            (do-expr
             (let ((iota-offset.387 (* iota.383 10)))
              (let
               ((map-result.462
                 (#0
                  (loop (frame-shape 10)
                   (map ((map-mem.488 map-mem.487)) (iota iota.385)
                    (let ((expr-result.489 (+ iota-offset.387 iota.385)))
                     (begin (putmem expr-result.489 (#0 map-mem.488))
                      expr-result.489)))
                   (body-matcher map-result.384) (map-result (map-result.384))
                   (map-result-mem-final map-mem.487) (consumer (values))))))
               (let
                ((reduce-arg.456
                  (index
                   (#0
                    (let ((expr-result.490 map-result.462))
                     (values (#0 expr-result.490))))
                   contiguous-subarray-index.407 (shape 10) (shape 9))))
                (#1
                 (let
                  ((expr-result.494
                    (loop (frame-shape 9)
                     (map
                      ((reduce-arg.427 reduce-arg.456) (map-mem.492 map-mem.491))
                      reduce-arg.427)
                     (body-matcher reduce-arg.420) (map-result ())
                     (map-result-mem-final map-mem.491)
                     (consumer
                      (reduce-zero
                       (index
                        (#0
                         (let ((expr-result.493 map-result.462))
                          (values (#0 expr-result.493))))
                        contiguous-subarray-index.401 (shape 10) (shape))
                       (reduce-arg1.344 reduce-arg2.347 reduce-arg.420)
                       (+ reduce-arg1.344 reduce-arg2.347))))))
                  (begin (putmem (#1 expr-result.494) (#0 map-mem.486))
                   expr-result.494)))))))))
          (map-result-mem-interim map-mem.485)
          (map-result-mem-final (values map-mem-result.0.484))))
        (getmem (values map-mem-result.0.484)))))) |}]
;;
