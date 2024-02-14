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
       (((binding ((name f) (id 150))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 148)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 149)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 150))))))
         (args (((id ((name +arg1) (id 148)))) ((id ((name +arg2) (id 149))))))
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
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
     ((binding ((name add) (id 148)))
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding ((name x) (id 149)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding ((name y) (id 150)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args
                ((Ref ((id ((name x) (id 149)))))
                 (Ref ((id ((name y) (id 150))))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name add) (id 148))))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name add) (id 148)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name x) (id 149)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name y) (id 150)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 153)))
                     (value (Primitive ((name (Func Add))))))
                    ((binding ((name +arg1) (id 151)))
                     (value (Ref ((id ((name x) (id 149)))))))
                    ((binding ((name +arg2) (id 152)))
                     (value (Ref ((id ((name y) (id 150)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 153))))))
                     (args
                      (((id ((name +arg1) (id 151))))
                       ((id ((name +arg2) (id 152))))))
                     (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 156)))
            (value (Ref ((id ((name add) (id 148)))))))
           ((binding ((name x) (id 154)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding ((name y) (id 155)))
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 156))))))
            (args (((id ((name x) (id 154)))) ((id ((name y) (id 155))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name add) (id 158)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 159)))
            (value
             (Ref
              ((id ((name add) (id 158)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 166)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name y) (id 168)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 165)))
               (value
                (AtomAsArray
                 ((element (Values ((elements ()) (type' ()))))
                  (type' ((element (Tuple ())) (shape ())))))))
              ((binding ((name +arg1) (id 167)))
               (value
                (Ref
                 ((id ((name x) (id 166)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name +arg2) (id 169)))
               (value
                (Ref
                 ((id ((name y) (id 168)))
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
                        ((id ((name +arg1) (id 167)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 169)))
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
     ((binding ((name id) (id 148)))
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @t) (id 149))) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name e) (id 150)))
                     (bound (ArrayRef ((name @t) (id 149)))))))
                  (body (Ref ((id ((name e) (id 150))))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id ((name id) (id 148))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name id) (id 148)))
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding ((name @t) (id 149))) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name e) (id 150)))
                        (bound (ArrayRef ((name @t) (id 149)))))))
                     (body (Ref ((id ((name e) (id 150)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 152)))
            (value
             (TypeApplication
              ((tFunc (Ref ((id ((name id) (id 148))))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding ((name e) (id 151)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 152))))))
            (args (((id ((name e) (id 151))))))
            (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
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
           ((binding ((name e) (id 157)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name e) (id 157)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
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
       (((binding ((name f) (id 150))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 148)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding ((name +arg2) (id 149)))
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
          (((binding ((name +arg1) (id 151)))
            (value (Ref ((id ((name +arg1) (id 148)))))))
           ((binding ((name +arg2) (id 152)))
            (value (Ref ((id ((name +arg2) (id 149)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg2) (id 153)))
               (value (Ref ((id ((name +arg2) (id 152)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 150))))))
               (args
                (((id ((name +arg1) (id 151)))) ((id ((name +arg2) (id 153))))))
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
       (((binding ((name f) (id 154)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 155)))
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
        ((binding ((name +arg2) (id 157)))
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
          (((binding ((name +arg1) (id 156)))
            (value
             (Ref
              ((id ((name +arg1) (id 155)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding ((name +arg2) (id 158)))
            (value
             (Ref
              ((id ((name +arg2) (id 157)))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 159)))
               (value
                (Ref
                 ((id ((name +arg2) (id 158)))
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
                        ((id ((name +arg1) (id 156)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 159)))
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
     ((binding ((name words) (id 148)))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 149)) 1)))))))))))))))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 149)) 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings ((((name len) (id 150)) Dim)))
         (valueBinding ((name word) (id 151)))
         (box (Ref ((id ((name words) (id 148))))))
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
                         ((const 0) (refs ((((name len) (id 150)) 1)))))
                        (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id ((name word) (id 151)))))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name words) (id 148)))
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
                       ((Add ((const 0) (refs ((((name len) (id 149)) 1)))))))))))))))
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
                       ((Add ((const 0) (refs ((((name len) (id 149)) 1)))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name box) (id 157)))
            (value (Ref ((id ((name words) (id 148)))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 150)))
               (indexValue
                (FromBox (box (Ref ((id ((name box) (id 157)))))) (i 0)))
               (sort Dim))))
            (body
             (Map
              ((args
                (((binding ((name word) (id 151)))
                  (value (BoxValue ((box (Ref ((id ((name box) (id 157))))))))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 156)))
                     (value (Primitive ((name (Func Equal))))))
                    ((binding ((name =arg1) (id 152)))
                     (value (Scalar ((element (Literal (IntLiteral 3)))))))
                    ((binding ((name =arg2) (id 155)))
                     (value
                      (Map
                       ((args
                         (((binding ((name f) (id 154)))
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
                                     (refs ((((name len) (id 150)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal CharacterLiteral))))))))
                          ((binding ((name arr) (id 153)))
                           (value (Ref ((id ((name word) (id 151)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 154))))))
                           (args (((id ((name arr) (id 153))))))
                           (type'
                            (Arr ((element (Literal IntLiteral)) (shape ())))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 156))))))
                     (args
                      (((id ((name =arg1) (id 152))))
                       ((id ((name =arg2) (id 155))))))
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
       (((binding ((name words) (id 164)))
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
                      ((Add ((const 0) (refs ((((name len) (id 149)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 149))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 149)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 149))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 149)) 1))))))))))))
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
                      ((Add ((const 0) (refs ((((name len) (id 149)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 149))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 149)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 149))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 149)) 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 149))) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 149)) 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name box) (id 165)))
            (value
             (Ref
              ((id ((name words) (id 164)))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 149))) (bound Dim))))
                    (body
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 149)) 1))))))))))))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 150)))
               (indexValue
                (FromBox
                 (box
                  (Ref
                   ((id ((name box) (id 165)))
                    (type'
                     ((element
                       (Sigma
                        ((parameters
                          (((binding ((name len) (id 149))) (bound Dim))))
                         (body
                          ((element (Literal CharacterLiteral))
                           (shape
                            ((Add ((const 0) (refs ((((name len) (id 149)) 1))))))))))))
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
                   (((binding ((name f) (id 158)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name =arg1) (id 159)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 3)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name =arg2) (id 163)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 161)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ReifyIndex
                          ((index
                            (Dimension
                             ((const 0) (refs ((((name len) (id 150)) 1))))))
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
                              ((id ((name =arg1) (id 159)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name =arg2) (id 163)))
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
           ((parameters (((binding ((name len) (id 149))) (bound Dim))))
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
               ((parameters (((binding ((name len) (id 149))) (bound Dim))))
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
               ((parameters (((binding ((name len) (id 149))) (bound Dim))))
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
           ((parameters (((binding ((name len) (id 149))) (bound Dim))))
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
               ((parameters (((binding ((name len) (id 149))) (bound Dim))))
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
               ((parameters (((binding ((name len) (id 149))) (bound Dim))))
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
     ((binding ((name sum-row) (id 148)))
      (value
       (Scalar
        ((element
          (IndexLambda
           ((params (((binding ((name d-1) (id 149))) (bound Dim))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name row) (id 150)))
                     (bound
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape
                         ((Add ((const 1) (refs ((((name d-1) (id 149)) 1)))))))))))))
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
                              ((const 0) (refs ((((name d-1) (id 149)) 1)))))
                             (Shape ()) (Shape ()))))))
                        (args ((Atom (Literal IntLiteral)))))))
                     (args
                      ((Primitive ((name (Func Add))))
                       (Ref ((id ((name row) (id 150))))))))))))))))))))))
      (body
       (TermApplication
        ((func
          (IndexApplication
           ((iFunc (Ref ((id ((name sum-row) (id 148))))))
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
       (((binding ((name sum-row) (id 148)))
         (value
          (Scalar
           ((element
             (IndexLambda
              ((params (((binding ((name d-1) (id 149))) (bound Dim))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name row) (id 150)))
                        (bound
                         (Arr
                          ((element (Literal IntLiteral))
                           (shape
                            ((Add ((const 1) (refs ((((name d-1) (id 149)) 1)))))))))))))
                     (body
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
                                                          ((name f) (id 181)))
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
                                                                        (id 168)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 58)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg0)
                                                                        (id 166)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name a)
                                                                        (id 62)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg1)
                                                                        (id 167)))
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
                                                                        (id 169)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arg0)
                                                                        (id 166)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg1)
                                                                        (id 170)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arg1)
                                                                        (id 167)))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 168))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        arg0)
                                                                        (id 169))))
                                                                        ((id
                                                                        ((name
                                                                        arg1)
                                                                        (id 170))))))
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
                                                                        (id 180)))
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
                                                                        (id 171)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        up-ranked-f)
                                                                        (id 61)))))))
                                                                        ((binding
                                                                        ((name
                                                                        reduce-zero-arg)
                                                                        (id 178)))
                                                                        (value
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 177)))
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
                                                                        (id 175)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        make)
                                                                        (id 10)))))))
                                                                        ((binding
                                                                        ((name
                                                                        foo)
                                                                        (id 173)))
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
                                                                        (id 174)))
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
                                                                        (id 176)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        foo)
                                                                        (id 173)))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 175))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        foo)
                                                                        (id 176))))
                                                                        ((id
                                                                        ((name v)
                                                                        (id 174))))))
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
                                                                        (id 172)))
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
                                                                        (id 177))))))
                                                                        (args
                                                                        (((id
                                                                        ((name v)
                                                                        (id 172))))))
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
                                                                        (id 179)))
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
                                                                        (id 180))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        reduce-f-arg)
                                                                        (id 171))))
                                                                        ((id
                                                                        ((name
                                                                        reduce-zero-arg)
                                                                        (id 178))))
                                                                        ((id
                                                                        ((name
                                                                        reduce-array-arg)
                                                                        (id 179))))))
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
                                                          ((name f) (id 153)))
                                                         (value
                                                          (Ref
                                                           ((id
                                                             ((name op) (id 68)))))))
                                                        ((binding
                                                          ((name init) (id 159)))
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding
                                                                ((name f)
                                                                 (id 158)))
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
                                                                        (id 157)))
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
                                                                        (id 155)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 27)))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 156)))
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
                                                                        (id 157))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 155))))
                                                                        ((id
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 156))))))
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
                                                                 (id 154)))
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
                                                                    (id 158))))))
                                                               (args
                                                                (((id
                                                                   ((name arr)
                                                                    (id 154))))))
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
                                                          ((name arr) (id 165)))
                                                         (value
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
                                                                        (id 163)))
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
                                                                        (id 161)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 31)))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 162)))
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
                                                                        (id 163))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 161))))
                                                                        ((id
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 162))))))
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
                                                                 (id 160)))
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
                                                                    (id 164))))))
                                                               (args
                                                                (((id
                                                                   ((name arr)
                                                                    (id 160))))))
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
                                                              (id 182)))
                                                            (value
                                                             (Ref
                                                              ((id
                                                                ((name init)
                                                                 (id 159)))))))))
                                                         (body
                                                          (TermApplication
                                                           ((func
                                                             (Ref
                                                              ((id
                                                                ((name f)
                                                                 (id 181))))))
                                                            (args
                                                             (((id
                                                                ((name f)
                                                                 (id 153))))
                                                              ((id
                                                                ((name init)
                                                                 (id 182))))
                                                              ((id
                                                                ((name arr)
                                                                 (id 165))))))
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
                                     (refs ((((name d-1) (id 149)) 1)))))
                                   (Shape ()) (Shape ()))))))
                              (args ((Atom (Literal IntLiteral))))))))
                          ((binding ((name op) (id 151)))
                           (value (Primitive ((name (Func Add))))))
                          ((binding ((name arr) (id 152)))
                           (value (Ref ((id ((name row) (id 150)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 183))))))
                           (args
                            (((id ((name op) (id 151))))
                             ((id ((name arr) (id 152))))))
                           (type'
                            (Arr ((element (Literal IntLiteral)) (shape ())))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 185)))
            (value
             (IndexApplication
              ((iFunc (Ref ((id ((name sum-row) (id 148))))))
               (args ((Dimension ((const 9) (refs ())))))))))
           ((binding ((name row) (id 184)))
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
             (((binding ((name row) (id 186)))
               (value (Ref ((id ((name row) (id 184)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 185))))))
               (args (((id ((name row) (id 186))))))
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
       (((binding ((name sum-row) (id 188)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 189)))
            (value
             (Ref
              ((id ((name sum-row) (id 188)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name row) (id 336)))
            (value
             (ArrayPrimitive
              (Map
               (frameShape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
               (args ()) (iotaVar (((name iota) (id 335))))
               (body
                (Ref
                 ((id ((name iota) (id 335)))
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
             (((binding ((name row) (id 337)))
               (value
                (Ref
                 ((id ((name row) (id 336)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape
                     ((Add ((const 1000000) (refs ())))
                      (Add ((const 10) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 239)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name op) (id 325)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 338)))
                  (value
                   (Ref
                    ((id ((name row) (id 337)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 10) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 285)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name f) (id 326)))
                     (value
                      (Ref
                       ((id ((name op) (id 325)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name init) (id 375)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 366)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 372)))
                           (value
                            (Ref
                             ((id ((name arr) (id 338)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 10) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 371)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding
                               ((name contiguous-subarray-array) (id 373)))
                              (value
                               (Ref
                                ((id ((name arr) (id 372)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding
                               ((name contiguous-subarray-index) (id 374)))
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
                                ((id ((name contiguous-subarray-array) (id 373)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id ((name contiguous-subarray-index) (id 374)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 1) (refs ()))))))))))
                              (originalShape ((Add ((const 10) (refs ())))))
                              (resultShape ()) (cellShape ())
                              (l ((const 1) (refs ())))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name arr) (id 342)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 329)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 339)))
                           (value
                            (Ref
                             ((id ((name arr) (id 338)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 10) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 334)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding
                               ((name contiguous-subarray-array) (id 340)))
                              (value
                               (Ref
                                ((id ((name arr) (id 339)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding
                               ((name contiguous-subarray-index) (id 341)))
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
                                ((id ((name contiguous-subarray-array) (id 340)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id ((name contiguous-subarray-index) (id 341)))
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
                      (((binding ((name init) (id 376)))
                        (value
                         (Ref
                          ((id ((name init) (id 375)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name up-ranked-f) (id 313)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 311)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding ((name reduce-f-arg) (id 314)))
                              (value
                               (Ref
                                ((id ((name up-ranked-f) (id 313)))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding ((name reduce-zero-arg) (id 379)))
                              (value
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding ((name f) (id 351)))
                                    (value
                                     (AtomAsArray
                                      ((element
                                        (Values ((elements ()) (type' ()))))
                                       (type' ((element (Tuple ())) (shape ())))))))
                                   ((binding ((name v) (id 377)))
                                    (value
                                     (Ref
                                      ((id ((name init) (id 376)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name make) (id 361)))
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
                                        (((binding ((name f) (id 362)))
                                          (value
                                           (Ref
                                            ((id ((name make) (id 361)))
                                             (type'
                                              ((element (Tuple ())) (shape ())))))))
                                         ((binding ((name v) (id 378)))
                                          (value
                                           (Ref
                                            ((id ((name v) (id 377)))
                                             (type'
                                              ((element (Literal IntLiteral))
                                               (shape ())))))))))
                                       (body
                                        (ArrayPrimitive
                                         (Map (frameShape ()) (args ())
                                          (body
                                           (Ref
                                            ((id ((name v) (id 378)))
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
                             ((binding ((name reduce-array-arg) (id 343)))
                              (value
                               (Ref
                                ((id ((name arr) (id 342)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 9) (refs ())))))))))))))
                           (body
                            (ArrayPrimitive
                             (Reduce
                              (arg
                               ((firstBinding ((name reduce-arg1) (id 344)))
                                (secondBinding ((name reduce-arg2) (id 347)))
                                (value
                                 (Ref
                                  ((id ((name reduce-array-arg) (id 343)))
                                   (type'
                                    ((element (Literal IntLiteral))
                                     (shape ((Add ((const 9) (refs ()))))))))))))
                              (zero
                               (Ref
                                ((id ((name reduce-zero-arg) (id 379)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding ((name f) (id 327)))
                                    (value
                                     (Ref
                                      ((id ((name f) (id 326)))
                                       (type' ((element (Tuple ())) (shape ())))))))
                                   ((binding ((name arg0) (id 345)))
                                    (value
                                     (Ref
                                      ((id ((name reduce-arg1) (id 344)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))
                                   ((binding ((name arg1) (id 348)))
                                    (value
                                     (Ref
                                      ((id ((name reduce-arg2) (id 347)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name arg0) (id 346)))
                                       (value
                                        (Ref
                                         ((id ((name arg0) (id 345)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))
                                      ((binding ((name arg1) (id 349)))
                                       (value
                                        (Ref
                                         ((id ((name arg1) (id 348)))
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
                                                ((id ((name arg0) (id 346)))
                                                 (type'
                                                  ((element (Literal IntLiteral))
                                                   (shape ()))))))
                                              (type' (Literal IntLiteral))))
                                            (ArrayAsAtom
                                             ((array
                                               (Ref
                                                ((id ((name arg1) (id 349)))
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
        (row.386
         (let ()
          (#0
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota iota.383)
              (#0
               (#0
                (loop-block (frame-shape 10)
                 (map () (iota (iota.385 : iota.383))
                  (let ((iota.335 iota.385)) iota.335))
                 (body-matcher map-result.384) (map-result (map-result.384))
                 (consumer (values))))))
             (body-matcher map-result.382) (map-result (map-result.382))
             (consumer (values))))))))
       (let ((f.189 f.381) (row.336 row.386))
        (let ((row.387 row.336))
         (#0
          (#0
           (loop-block (frame-shape 1000000)
            (map ((row.389 row.387))
             (let ((row.337 row.389))
              (let ((f.390 (values)) (op.391 (values)) (arr.392 row.337))
               (let ((f.239 f.390) (op.325 op.391) (arr.338 arr.392))
                (let
                 ((f.393 (values)) (f.394 op.325)
                  (init.400
                   (let ((f.395 (values)) (arr.396 arr.338))
                    (let ((f.366 f.395) (arr.372 arr.396))
                     (let
                      ((f.397 (values)) (contiguous-subarray-array.398 arr.372)
                       (contiguous-subarray-index.399 (frame 0)))
                      (let
                       ((f.371 f.397)
                        (contiguous-subarray-array.373
                         contiguous-subarray-array.398)
                        (contiguous-subarray-index.374
                         contiguous-subarray-index.399))
                       (contiguous-subarray contiguous-subarray-array.373
                        contiguous-subarray-index.374 (shape 10) (shape)))))))
                  (arr.406
                   (let ((f.401 (values)) (arr.402 arr.338))
                    (let ((f.329 f.401) (arr.339 arr.402))
                     (let
                      ((f.403 (values)) (contiguous-subarray-array.404 arr.339)
                       (contiguous-subarray-index.405 (frame 1)))
                      (let
                       ((f.334 f.403)
                        (contiguous-subarray-array.340
                         contiguous-subarray-array.404)
                        (contiguous-subarray-index.341
                         contiguous-subarray-index.405))
                       (contiguous-subarray contiguous-subarray-array.340
                        contiguous-subarray-index.341 (shape 10) (shape 9))))))))
                 (let
                  ((f.285 f.393) (f.326 f.394) (init.375 init.400)
                   (arr.342 arr.406))
                  (let ((init.407 init.375))
                   (let ((init.376 init.407))
                    (let ((up-ranked-f.408 (values)))
                     (let ((up-ranked-f.313 up-ranked-f.408))
                      (let
                       ((f.409 (values)) (reduce-f-arg.410 up-ranked-f.313)
                        (reduce-zero-arg.416
                         (let ((f.411 (values)) (v.412 init.376))
                          (let ((f.351 f.411) (v.377 v.412))
                           (let ((make.413 (values)))
                            (let ((make.361 make.413))
                             (let ((f.414 make.361) (v.415 v.377))
                              (let ((f.362 f.414) (v.378 v.415))
                               (let () (let () v.378)))))))))
                        (reduce-array-arg.417 arr.342))
                       (let
                        ((f.311 f.409) (reduce-f-arg.314 reduce-f-arg.410)
                         (reduce-zero-arg.379 reduce-zero-arg.416)
                         (reduce-array-arg.343 reduce-array-arg.417))
                        (let ((reduce-arg.424 reduce-array-arg.343))
                         (#1
                          (loop-block (frame-shape 9)
                           (map ((reduce-arg.425 reduce-arg.424))
                            (values reduce-arg.425))
                           (body-matcher (reduce-arg.418)) (map-result ())
                           (consumer
                            (reduce-zero reduce-zero-arg.379
                             (reduce-arg1.344 reduce-arg2.347 reduce-arg.418)
                             (let
                              ((f.419 f.326) (arg0.420 reduce-arg1.344)
                               (arg1.421 reduce-arg2.347))
                              (let
                               ((f.327 f.419) (arg0.345 arg0.420)
                                (arg1.348 arg1.421))
                               (let ((arg0.422 arg0.345) (arg1.423 arg1.348))
                                (let ((arg0.346 arg0.422) (arg1.349 arg1.423))
                                 (+ arg0.346 arg1.349))))))))))))))))))))))
            (body-matcher map-result.388) (map-result (map-result.388))
            (consumer (values))))))))))
    Result of stage Fuse and Simplify:
    (let
     ((contiguous-subarray-index.399 (frame 0))
      (contiguous-subarray-index.405 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 1000000)
        (map () (iota iota.383)
         (let
          ((map-result.457
            (#0
             (loop-block (frame-shape 10)
              (map () (iota (iota.385 : iota.383)) iota.385)
              (body-matcher map-result.384) (map-result (map-result.384))
              (consumer (values))))))
          (let
           ((reduce-arg.452
             (contiguous-subarray (#0 map-result.457)
              contiguous-subarray-index.405 (shape 10) (shape 9))))
           (#1
            (loop-block (frame-shape 9)
             (map ((reduce-arg.425 reduce-arg.452)) reduce-arg.425)
             (body-matcher reduce-arg.418) (map-result ())
             (consumer
              (reduce-zero
               (contiguous-subarray (#0 map-result.457)
                contiguous-subarray-index.399 (shape 10) (shape))
               (reduce-arg1.344 reduce-arg2.347 reduce-arg.418)
               (+ reduce-arg1.344 reduce-arg2.347))))))))
        (body-matcher map-result.388) (map-result (map-result.388))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((contiguous-subarray-index.399 (frame 0))
      (contiguous-subarray-index.405 (frame 1)))
     (#0
      (kernel (blocks 20) (threads 512)
       (map-kernel (frame-shape 1000000) () (iota iota.383)
        (body-matcher map-result.388) (map-result (map-result.388))
        (let
         ((map-result.457
           (#0
            (loop-block (frame-shape 10)
             (map () (iota (iota.385 : iota.383)) iota.385)
             (body-matcher map-result.384) (map-result (map-result.384))
             (consumer (values))))))
         (let
          ((reduce-arg.452
            (contiguous-subarray (#0 map-result.457)
             contiguous-subarray-index.405 (shape 10) (shape 9))))
          (#1
           (loop-block (frame-shape 9)
            (map ((reduce-arg.425 reduce-arg.452)) reduce-arg.425)
            (body-matcher reduce-arg.418) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.457)
               contiguous-subarray-index.399 (shape 10) (shape))
              (reduce-arg1.344 reduce-arg2.347 reduce-arg.418)
              (+ reduce-arg1.344 reduce-arg2.347)))))))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.483 (Tuple ()) device)
      (map-mem.479
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.477
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.476
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.474 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.472 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.399
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.472) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.472)))
       (contiguous-subarray-index.405
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.474) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.474))))
      (#0
       (begin
        (kernel (blocks 20) (threads 512)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.478 map-mem.477))
            (iota iota.383)
            (do-expr
             (let
              ((map-result.457
                (#0
                 (loop (frame-shape 10)
                  (map ((map-mem.480 map-mem.479)) (iota (iota.385 : iota.383))
                   (let ((expr-result.481 iota.385))
                    (begin (putmem expr-result.481 (#0 map-mem.480))
                     expr-result.481)))
                  (body-matcher map-result.384) (map-result (map-result.384))
                  (map-result-mem-final map-mem.479) (consumer (values))))))
              (let
               ((reduce-arg.452
                 (index
                  (#0
                   (let ((expr-result.482 map-result.457))
                    (values (#0 expr-result.482))))
                  contiguous-subarray-index.405 (shape 10) (shape 9))))
               (#1
                (let
                 ((expr-result.486
                   (loop (frame-shape 9)
                    (map
                     ((reduce-arg.425 reduce-arg.452) (map-mem.484 map-mem.483))
                     reduce-arg.425)
                    (body-matcher reduce-arg.418) (map-result ())
                    (map-result-mem-final map-mem.483)
                    (consumer
                     (reduce-zero
                      (index
                       (#0
                        (let ((expr-result.485 map-result.457))
                         (values (#0 expr-result.485))))
                       contiguous-subarray-index.399 (shape 10) (shape))
                      (reduce-arg1.344 reduce-arg2.347 reduce-arg.418)
                      (+ reduce-arg1.344 reduce-arg2.347))))))
                 (begin (putmem (#1 expr-result.486) (#0 map-mem.478))
                  expr-result.486))))))))
          (map-result-mem-interim map-mem.477)
          (map-result-mem-final (values map-mem-result.0.476))))
        (getmem (values map-mem-result.0.476))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.483 (Tuple ()) device)
      (map-mem.479
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.477
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.476
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.474 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.472 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.399
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.472) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.472)))
       (contiguous-subarray-index.405
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.474) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.474))))
      (#0
       (begin
        (kernel
         (captures
          ((expr-captures
            ((((name contiguous-subarray-index) (id 399))
              (Array ((element (Literal IntLiteral)) (shape (shape 1)))))
             (((name contiguous-subarray-index) (id 405))
              (Array ((element (Literal IntLiteral)) (shape (shape 1)))))))
           (mem-captures
            ((((name map-mem) (id 477))
              (Tuple
               ((Array ((element (Literal IntLiteral)) (shape (shape 1000000)))))))
             (((name map-mem) (id 479))
              (Tuple
               ((Array ((element (Literal IntLiteral)) (shape (shape 10)))))))
             (((name map-mem) (id 483)) (Tuple ()))))
           (index-captures ())))
         (blocks 20) (threads 512)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.478 map-mem.477))
            (iota iota.383)
            (do-expr
             (let
              ((map-result.457
                (#0
                 (loop (frame-shape 10)
                  (map ((map-mem.480 map-mem.479)) (iota (iota.385 : iota.383))
                   (let ((expr-result.481 iota.385))
                    (begin (putmem expr-result.481 (#0 map-mem.480))
                     expr-result.481)))
                  (body-matcher map-result.384) (map-result (map-result.384))
                  (map-result-mem-final map-mem.479) (consumer (values))))))
              (let
               ((reduce-arg.452
                 (index
                  (#0
                   (let ((expr-result.482 map-result.457))
                    (values (#0 expr-result.482))))
                  contiguous-subarray-index.405 (shape 10) (shape 9))))
               (#1
                (let
                 ((expr-result.486
                   (loop (frame-shape 9)
                    (map
                     ((reduce-arg.425 reduce-arg.452) (map-mem.484 map-mem.483))
                     reduce-arg.425)
                    (body-matcher reduce-arg.418) (map-result ())
                    (map-result-mem-final map-mem.483)
                    (consumer
                     (reduce-zero
                      (index
                       (#0
                        (let ((expr-result.485 map-result.457))
                         (values (#0 expr-result.485))))
                       contiguous-subarray-index.399 (shape 10) (shape))
                      (reduce-arg1.344 reduce-arg2.347 reduce-arg.418)
                      (+ reduce-arg1.344 reduce-arg2.347))))))
                 (begin (putmem (#1 expr-result.486) (#0 map-mem.478))
                  expr-result.486))))))))
          (map-result-mem-interim map-mem.477)
          (map-result-mem-final (values map-mem-result.0.476))))
        (getmem (values map-mem-result.0.476)))))) |}]
;;
