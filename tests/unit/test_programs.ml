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
       (((binding ((name f) (id 113))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 111)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 112)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 113))))))
         (args (((id ((name +arg1) (id 111)))) ((id ((name +arg2) (id 112))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
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
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((f.117 (values)) (+arg1.118 1) (+arg2.119 2))
     (let ((f.114 f.117) (+arg1.115 +arg1.118) (+arg2.116 +arg2.119))
      (+ +arg1.115 +arg2.116)))
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
     ((binding ((name add) (id 111)))
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding ((name x) (id 112)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding ((name y) (id 113)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args
                ((Ref ((id ((name x) (id 112)))))
                 (Ref ((id ((name y) (id 113))))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name add) (id 111))))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name add) (id 111)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name x) (id 112)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name y) (id 113)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 116)))
                     (value (Primitive ((name (Func Add))))))
                    ((binding ((name +arg1) (id 114)))
                     (value (Ref ((id ((name x) (id 112)))))))
                    ((binding ((name +arg2) (id 115)))
                     (value (Ref ((id ((name y) (id 113)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 116))))))
                     (args
                      (((id ((name +arg1) (id 114))))
                       ((id ((name +arg2) (id 115))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 119)))
            (value (Ref ((id ((name add) (id 111)))))))
           ((binding ((name x) (id 117)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding ((name y) (id 118)))
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 119))))))
            (args (((id ((name x) (id 117)))) ((id ((name y) (id 118))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name add) (id 121)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 122)))
            (value
             (Ref
              ((id ((name add) (id 121)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 129)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name y) (id 131)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 128)))
               (value
                (AtomAsArray
                 ((element (Values ((elements ()) (type' ()))))
                  (type' ((element (Tuple ())) (shape ())))))))
              ((binding ((name +arg1) (id 130)))
               (value
                (Ref
                 ((id ((name x) (id 129)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name +arg2) (id 132)))
               (value
                (Ref
                 ((id ((name y) (id 131)))
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
                        ((id ((name +arg1) (id 130)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 132)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))))
                  (type' (Literal IntLiteral)))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((add.133 (values)))
     (let ((add.121 add.133))
      (let ((f.134 add.121) (x.135 5) (y.136 10))
       (let ((f.122 f.134) (x.129 x.135) (y.131 y.136))
        (let ((f.137 (values)) (+arg1.138 x.129) (+arg2.139 y.131))
         (let ((f.128 f.137) (+arg1.130 +arg1.138) (+arg2.132 +arg2.139))
          (+ +arg1.130 +arg2.132)))))))
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
     ((binding ((name id) (id 111)))
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @t) (id 112))) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name e) (id 113)))
                     (bound (ArrayRef ((name @t) (id 112)))))))
                  (body (Ref ((id ((name e) (id 113))))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id ((name id) (id 111))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name id) (id 111)))
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding ((name @t) (id 112))) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name e) (id 113)))
                        (bound (ArrayRef ((name @t) (id 112)))))))
                     (body (Ref ((id ((name e) (id 113)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 115)))
            (value
             (TypeApplication
              ((tFunc (Ref ((id ((name id) (id 111))))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding ((name e) (id 114)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 115))))))
            (args (((id ((name e) (id 114))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
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
           ((binding ((name e) (id 120)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name e) (id 120)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((id.121 (values)))
     (let ((id.117 id.121))
      (let ((f.122 id.117) (e.123 5)) (let ((f.118 f.122) (e.120 e.123)) e.120))))
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
       (((binding ((name f) (id 113))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 111)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding ((name +arg2) (id 112)))
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
          (((binding ((name +arg1) (id 114)))
            (value (Ref ((id ((name +arg1) (id 111)))))))
           ((binding ((name +arg2) (id 115)))
            (value (Ref ((id ((name +arg2) (id 112)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg2) (id 116)))
               (value (Ref ((id ((name +arg2) (id 115)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 113))))))
               (args
                (((id ((name +arg1) (id 114)))) ((id ((name +arg2) (id 116))))))
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
       (((binding ((name f) (id 117)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 118)))
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
        ((binding ((name +arg2) (id 120)))
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
          (((binding ((name +arg1) (id 119)))
            (value
             (Ref
              ((id ((name +arg1) (id 118)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding ((name +arg2) (id 121)))
            (value
             (Ref
              ((id ((name +arg2) (id 120)))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 122)))
               (value
                (Ref
                 ((id ((name +arg2) (id 121)))
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
                        ((id ((name +arg1) (id 119)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 122)))
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
     ((f.123 (values)) (+arg1.124 (frame 1 2))
      (+arg2.125 (frame (frame 3 4 5) (frame 6 7 8))))
     (let ((f.117 f.123) (+arg1.118 +arg1.124) (+arg2.120 +arg2.125))
      (let ((+arg1.126 +arg1.118) (+arg2.127 +arg2.120))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.129 +arg1.126) (+arg2.130 +arg2.127))
           (let ((+arg1.119 +arg1.129) (+arg2.121 +arg2.130))
            (let ((+arg2.131 +arg2.121))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg2.133 +arg2.131))
                 (let ((+arg2.122 +arg2.133)) (+ +arg1.119 +arg2.122)))
                (body-matcher map-result.132) (map-result (map-result.132))
                (consumer (values))))))))
          (body-matcher map-result.128) (map-result (map-result.128))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((+arg1.126 (frame 1 2)) (+arg2.127 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.129 +arg1.126) (+arg2.130 +arg2.127))
         (let ((+arg2.131 +arg2.130))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.133 +arg2.131)) (+ +arg1.129 +arg2.133))
             (body-matcher map-result.132) (map-result (map-result.132))
             (consumer (values)))))))
        (body-matcher map-result.128) (map-result (map-result.128))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((+arg1.126 (frame 1 2)) (+arg2.127 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.129 +arg1.126) (+arg2.130 +arg2.127))
         (let ((+arg2.131 +arg2.130))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.133 +arg2.131)) (+ +arg1.129 +arg2.133))
             (body-matcher map-result.132) (map-result (map-result.132))
             (consumer (values)))))))
        (body-matcher map-result.128) (map-result (map-result.128))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.155 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.148
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.145 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.126
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.145) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.145) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.145)))
       (+arg2.127
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.148) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.148) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.148) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.148) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.148) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.148) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.148))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.129 +arg1.126) (+arg2.130 +arg2.127) (map-mem.156 map-mem.155))
          (let ((+arg2.131 +arg2.130))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.133 +arg2.131) (map-mem.157 map-mem.156))
               (let ((expr-result.158 (+ +arg1.129 +arg2.133)))
                (begin (putmem expr-result.158 map-mem.157) expr-result.158)))
              (body-matcher map-result.132) (map-result (map-result.132))
              (map-result-mem-interim (values map-mem.156))
              (map-result-mem-final (values map-mem.156)) (consumer (values)))))))
         (body-matcher map-result.128) (map-result (map-result.128))
         (map-result-mem-interim (values map-mem.155))
         (map-result-mem-final (values map-mem.155)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.155 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.148
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.145 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.126
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.145) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.145) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.145)))
       (+arg2.127
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.148) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.148) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.148) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.148) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.148) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.148) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.148))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.129 +arg1.126) (+arg2.130 +arg2.127) (map-mem.156 map-mem.155))
          (let ((+arg2.131 +arg2.130))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.133 +arg2.131) (map-mem.157 map-mem.156))
               (let ((expr-result.158 (+ +arg1.129 +arg2.133)))
                (begin (putmem expr-result.158 map-mem.157) expr-result.158)))
              (body-matcher map-result.132) (map-result (map-result.132))
              (map-result-mem-interim (values map-mem.156))
              (map-result-mem-final (values map-mem.156)) (consumer (values)))))))
         (body-matcher map-result.128) (map-result (map-result.128))
         (map-result-mem-interim (values map-mem.155))
         (map-result-mem-final (values map-mem.155)) (consumer (values))))))) |}]
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
     ((binding ((name words) (id 111)))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 112)) 1)))))))))))))))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 112)) 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings ((((name len) (id 113)) Dim)))
         (valueBinding ((name word) (id 114)))
         (box (Ref ((id ((name words) (id 111))))))
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
                         ((const 0) (refs ((((name len) (id 113)) 1)))))
                        (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id ((name word) (id 114)))))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name words) (id 111)))
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
                       ((Add ((const 0) (refs ((((name len) (id 112)) 1)))))))))))))))
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
                       ((Add ((const 0) (refs ((((name len) (id 112)) 1)))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name box) (id 120)))
            (value (Ref ((id ((name words) (id 111)))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 113)))
               (indexValue
                (FromBox (box (Ref ((id ((name box) (id 120)))))) (i 0)))
               (sort Dim))))
            (body
             (Map
              ((args
                (((binding ((name word) (id 114)))
                  (value (BoxValue ((box (Ref ((id ((name box) (id 120))))))))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 119)))
                     (value (Primitive ((name (Func Equal))))))
                    ((binding ((name =arg1) (id 115)))
                     (value (Scalar ((element (Literal (IntLiteral 3)))))))
                    ((binding ((name =arg2) (id 118)))
                     (value
                      (Map
                       ((args
                         (((binding ((name f) (id 117)))
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
                                     (refs ((((name len) (id 113)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal CharacterLiteral))))))))
                          ((binding ((name arr) (id 116)))
                           (value (Ref ((id ((name word) (id 114)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 117))))))
                           (args (((id ((name arr) (id 116))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 119))))))
                     (args
                      (((id ((name =arg1) (id 115))))
                       ((id ((name =arg2) (id 118))))))
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
       (((binding ((name words) (id 127)))
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
                      ((Add ((const 0) (refs ((((name len) (id 112)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 112))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 112)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 112))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 112)) 1))))))))))))
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
                      ((Add ((const 0) (refs ((((name len) (id 112)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 112))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 112)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 112))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 112)) 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 112))) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 112)) 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name box) (id 128)))
            (value
             (Ref
              ((id ((name words) (id 127)))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 112))) (bound Dim))))
                    (body
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 112)) 1))))))))))))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 113)))
               (indexValue
                (FromBox
                 (box
                  (Ref
                   ((id ((name box) (id 128)))
                    (type'
                     ((element
                       (Sigma
                        ((parameters
                          (((binding ((name len) (id 112))) (bound Dim))))
                         (body
                          ((element (Literal CharacterLiteral))
                           (shape
                            ((Add ((const 0) (refs ((((name len) (id 112)) 1))))))))))))
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
                   (((binding ((name f) (id 121)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name =arg1) (id 122)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 3)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name =arg2) (id 126)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 124)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ReifyIndex
                          ((index
                            (Dimension
                             ((const 0) (refs ((((name len) (id 113)) 1))))))
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
                              ((id ((name =arg1) (id 122)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name =arg2) (id 126)))
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
     ((words.129 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (let ((words.127 words.129))
      (let ((box.130 words.127))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((box.132 box.130))
           (let ((box.128 box.132))
            (index-let ((len.113 box-index-0 box.128))
             (let ()
              (let ()
               (let
                ((f.133 (values)) (=arg1.134 3)
                 (=arg2.136
                  (let ((f.135 (values)))
                   (let ((f.124 f.135)) (reify-index len.113)))))
                (let ((f.121 f.133) (=arg1.122 =arg1.134) (=arg2.126 =arg2.136))
                 (= =arg1.122 =arg2.126))))))))
          (body-matcher map-result.131) (map-result (map-result.131))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((box.130 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.132 box.130))
         (index-let ((len.113 box-index-0 box.132)) (= 3 (reify-index len.113))))
        (body-matcher map-result.131) (map-result (map-result.131))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((box.130 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.132 box.130))
         (index-let ((len.113 box-index-0 box.132)) (= 3 (reify-index len.113))))
        (body-matcher map-result.131) (map-result (map-result.131))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.148 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.144
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.139
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.138
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 112))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.112))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.130
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.139) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.139) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.139) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.139)))
           (index (mem frame-array.138) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 112))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.112)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.144) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.144) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.144)))
           (index (mem frame-array.138) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 112))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.112))))))))))))
         (getmem frame-array.138))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.132 box.130) (map-mem.149 map-mem.148))
          (index-let ((len.113 box-index-0 box.132))
           (let ((expr-result.150 (= 3 (reify-dimension-index len.113))))
            (begin (putmem expr-result.150 map-mem.149) expr-result.150))))
         (body-matcher map-result.131) (map-result (map-result.131))
         (map-result-mem-interim (values map-mem.148))
         (map-result-mem-final (values map-mem.148)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.148 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.144
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.139
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.138
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 112))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.112))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.130
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.139) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.139) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.139) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.139)))
           (index (mem frame-array.138) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 112))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.112)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.144) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.144) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.144)))
           (index (mem frame-array.138) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 112))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.112))))))))))))
         (getmem frame-array.138))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.132 box.130) (map-mem.149 map-mem.148))
          (index-let ((len.113 box-index-0 box.132))
           (let ((expr-result.150 (= 3 (reify-dimension-index len.113))))
            (begin (putmem expr-result.150 map-mem.149) expr-result.150))))
         (body-matcher map-result.131) (map-result (map-result.131))
         (map-result-mem-interim (values map-mem.148))
         (map-result-mem-final (values map-mem.148)) (consumer (values))))))) |}]
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
     ((binding ((name sum-row) (id 111)))
      (value
       (Scalar
        ((element
          (IndexLambda
           ((params (((binding ((name d-1) (id 112))) (bound Dim))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name row) (id 113)))
                     (bound
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape
                         ((Add ((const 1) (refs ((((name d-1) (id 112)) 1)))))))))))))
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
                                  (((binding ((name d-1) (id 51))) (bound Dim))
                                   ((binding ((name @item-pad) (id 52)))
                                    (bound Shape))
                                   ((binding ((name @cell-shape) (id 53)))
                                    (bound Shape))))
                                 (body
                                  (Scalar
                                   ((element
                                     (TypeLambda
                                      ((params
                                        (((binding ((name t) (id 54)))
                                          (bound Atom))))
                                       (body
                                        (Scalar
                                         ((element
                                           (TermLambda
                                            ((params
                                              (((binding ((name op) (id 55)))
                                                (bound
                                                 (Arr
                                                  ((element
                                                    (Func
                                                     ((parameters
                                                       ((Arr
                                                         ((element
                                                           (AtomRef
                                                            ((name t) (id 54))))
                                                          (shape
                                                           ((ShapeRef
                                                             ((name @cell-shape)
                                                              (id 53)))))))
                                                        (Arr
                                                         ((element
                                                           (AtomRef
                                                            ((name t) (id 54))))
                                                          (shape
                                                           ((ShapeRef
                                                             ((name @cell-shape)
                                                              (id 53)))))))))
                                                      (return
                                                       (Arr
                                                        ((element
                                                          (AtomRef
                                                           ((name t) (id 54))))
                                                         (shape
                                                          ((ShapeRef
                                                            ((name @cell-shape)
                                                             (id 53)))))))))))
                                                   (shape ())))))
                                               ((binding ((name arr) (id 56)))
                                                (bound
                                                 (Arr
                                                  ((element
                                                    (AtomRef ((name t) (id 54))))
                                                   (shape
                                                    ((Add
                                                      ((const 1)
                                                       (refs
                                                        ((((name d-1) (id 51)) 1)))))
                                                     (ShapeRef
                                                      ((name @item-pad) (id 52)))
                                                     (ShapeRef
                                                      ((name @cell-shape)
                                                       (id 53)))))))))))
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
                                                                 (id 41)))
                                                               (bound Dim))
                                                              ((binding
                                                                ((name @item-pad)
                                                                 (id 42)))
                                                               (bound Shape))
                                                              ((binding
                                                                ((name
                                                                  @cell-shape)
                                                                 (id 43)))
                                                               (bound Shape))))
                                                            (body
                                                             (Scalar
                                                              ((element
                                                                (TypeLambda
                                                                 ((params
                                                                   (((binding
                                                                      ((name t)
                                                                       (id 44)))
                                                                     (bound Atom))))
                                                                  (body
                                                                   (Scalar
                                                                    ((element
                                                                      (TermLambda
                                                                       ((params
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 45)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (Func
                                                                        ((parameters
                                                                        ((Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))
                                                                        (return
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))))
                                                                        (shape
                                                                        ())))))
                                                                        ((binding
                                                                        ((name
                                                                        init)
                                                                        (id 46)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))
                                                                        ((binding
                                                                        ((name
                                                                        arr)
                                                                        (id 47)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d)
                                                                        (id 41))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))))
                                                                        (body
                                                                        (Let
                                                                        ((binding
                                                                        ((name
                                                                        up-ranked-f)
                                                                        (id 48)))
                                                                        (value
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name a)
                                                                        (id 49)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))
                                                                        ((binding
                                                                        ((name b)
                                                                        (id 50)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 45))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        ((name a)
                                                                        (id 49)))))
                                                                        (Ref
                                                                        ((id
                                                                        ((name b)
                                                                        (id 50))))))))))))))))
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
                                                                        (id 41))
                                                                        1)))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43))))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44)))))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        ((name
                                                                        up-ranked-f)
                                                                        (id 48)))))
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
                                                                        (id 42)))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43))))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44)))))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        ((name
                                                                        init)
                                                                        (id 46)))))))))
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 47)))))))))))))))))))))))))))))))
                                                      (args
                                                       ((Dimension
                                                         ((const 0)
                                                          (refs
                                                           ((((name d-1) (id 51))
                                                             1)))))
                                                        (Shape
                                                         ((ShapeRef
                                                           ((name @item-pad)
                                                            (id 52)))))
                                                        (Shape
                                                         ((ShapeRef
                                                           ((name @cell-shape)
                                                            (id 53))))))))))
                                                   (args
                                                    ((Atom
                                                      (AtomRef
                                                       ((name t) (id 54)))))))))
                                                (args
                                                 ((Ref
                                                   ((id ((name op) (id 55)))))
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
                                                                  (id 51))
                                                                 1)))))
                                                            (Shape
                                                             ((ShapeRef
                                                               ((name @item-pad)
                                                                (id 52)))
                                                              (ShapeRef
                                                               ((name
                                                                 @cell-shape)
                                                                (id 53))))))))))
                                                       (args
                                                        ((Atom
                                                          (AtomRef
                                                           ((name t) (id 54)))))))))
                                                    (args
                                                     ((Ref
                                                       ((id ((name arr) (id 56)))))))))
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
                                                                  (id 51))
                                                                 1)))))
                                                            (Shape
                                                             ((ShapeRef
                                                               ((name @item-pad)
                                                                (id 52)))
                                                              (ShapeRef
                                                               ((name
                                                                 @cell-shape)
                                                                (id 53))))))))))
                                                       (args
                                                        ((Atom
                                                          (AtomRef
                                                           ((name t) (id 54)))))))))
                                                    (args
                                                     ((Ref
                                                       ((id ((name arr) (id 56))))))))))))))))))))))))))))))))
                           (args
                            ((Dimension
                              ((const 0) (refs ((((name d-1) (id 112)) 1)))))
                             (Shape ()) (Shape ()))))))
                        (args ((Atom (Literal IntLiteral)))))))
                     (args
                      ((Primitive ((name (Func Add))))
                       (Ref ((id ((name row) (id 113))))))))))))))))))))))
      (body
       (TermApplication
        ((func
          (IndexApplication
           ((iFunc (Ref ((id ((name sum-row) (id 111))))))
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
       (((binding ((name sum-row) (id 111)))
         (value
          (Scalar
           ((element
             (IndexLambda
              ((params (((binding ((name d-1) (id 112))) (bound Dim))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name row) (id 113)))
                        (bound
                         (Arr
                          ((element (Literal IntLiteral))
                           (shape
                            ((Add ((const 1) (refs ((((name d-1) (id 112)) 1)))))))))))))
                     (body
                      (Map
                       ((args
                         (((binding ((name f) (id 146)))
                           (value
                            (TypeApplication
                             ((tFunc
                               (IndexApplication
                                ((iFunc
                                  (Scalar
                                   ((element
                                     (IndexLambda
                                      ((params
                                        (((binding ((name d-1) (id 51)))
                                          (bound Dim))
                                         ((binding ((name @item-pad) (id 52)))
                                          (bound Shape))
                                         ((binding ((name @cell-shape) (id 53)))
                                          (bound Shape))))
                                       (body
                                        (Scalar
                                         ((element
                                           (TypeLambda
                                            ((params
                                              (((binding ((name t) (id 54)))
                                                (bound Atom))))
                                             (body
                                              (Scalar
                                               ((element
                                                 (TermLambda
                                                  ((params
                                                    (((binding
                                                       ((name op) (id 55)))
                                                      (bound
                                                       (Arr
                                                        ((element
                                                          (Func
                                                           ((parameters
                                                             ((Arr
                                                               ((element
                                                                 (AtomRef
                                                                  ((name t)
                                                                   (id 54))))
                                                                (shape
                                                                 ((ShapeRef
                                                                   ((name
                                                                     @cell-shape)
                                                                    (id 53)))))))
                                                              (Arr
                                                               ((element
                                                                 (AtomRef
                                                                  ((name t)
                                                                   (id 54))))
                                                                (shape
                                                                 ((ShapeRef
                                                                   ((name
                                                                     @cell-shape)
                                                                    (id 53)))))))))
                                                            (return
                                                             (Arr
                                                              ((element
                                                                (AtomRef
                                                                 ((name t)
                                                                  (id 54))))
                                                               (shape
                                                                ((ShapeRef
                                                                  ((name
                                                                    @cell-shape)
                                                                   (id 53)))))))))))
                                                         (shape ())))))
                                                     ((binding
                                                       ((name arr) (id 56)))
                                                      (bound
                                                       (Arr
                                                        ((element
                                                          (AtomRef
                                                           ((name t) (id 54))))
                                                         (shape
                                                          ((Add
                                                            ((const 1)
                                                             (refs
                                                              ((((name d-1)
                                                                 (id 51))
                                                                1)))))
                                                           (ShapeRef
                                                            ((name @item-pad)
                                                             (id 52)))
                                                           (ShapeRef
                                                            ((name @cell-shape)
                                                             (id 53)))))))))))
                                                   (body
                                                    (Map
                                                     ((args
                                                       (((binding
                                                          ((name f) (id 144)))
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
                                                                        (id 41)))
                                                                        (bound
                                                                        Dim))
                                                                       ((binding
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (bound
                                                                        Shape))
                                                                       ((binding
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))
                                                                        (bound
                                                                        Shape))))
                                                                     (body
                                                                      (Scalar
                                                                       ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name t)
                                                                        (id 44)))
                                                                        (bound
                                                                        Atom))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 45)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (Func
                                                                        ((parameters
                                                                        ((Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))
                                                                        (return
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))))
                                                                        (shape
                                                                        ())))))
                                                                        ((binding
                                                                        ((name
                                                                        init)
                                                                        (id 46)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))
                                                                        ((binding
                                                                        ((name
                                                                        arr)
                                                                        (id 47)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d)
                                                                        (id 41))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name
                                                                        up-ranked-f)
                                                                        (id 48)))
                                                                        (value
                                                                        (Scalar
                                                                        ((element
                                                                        (TermLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name a)
                                                                        (id 49)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))
                                                                        ((binding
                                                                        ((name b)
                                                                        (id 50)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 131)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 45)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg0)
                                                                        (id 129)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name a)
                                                                        (id 49)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg1)
                                                                        (id 130)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name b)
                                                                        (id 50)))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name
                                                                        arg0)
                                                                        (id 132)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arg0)
                                                                        (id 129)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg1)
                                                                        (id 133)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arg1)
                                                                        (id 130)))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 131))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        arg0)
                                                                        (id 132))))
                                                                        ((id
                                                                        ((name
                                                                        arg1)
                                                                        (id 133))))))
                                                                        (type'
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43))))))))))
                                                                        (frameShape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))))
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43))))))))))))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 143)))
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
                                                                        (id 41))
                                                                        1)))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43))))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))))))))
                                                                        ((binding
                                                                        ((name
                                                                        reduce-f-arg)
                                                                        (id 134)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        up-ranked-f)
                                                                        (id 48)))))))
                                                                        ((binding
                                                                        ((name
                                                                        reduce-zero-arg)
                                                                        (id 141)))
                                                                        (value
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 140)))
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
                                                                        (id 138)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        make)
                                                                        (id 10)))))))
                                                                        ((binding
                                                                        ((name
                                                                        foo)
                                                                        (id 136)))
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
                                                                        (id 137)))
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
                                                                        (id 139)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        foo)
                                                                        (id 136)))))))))
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
                                                                        foo)
                                                                        (id 139))))
                                                                        ((id
                                                                        ((name v)
                                                                        (id 137))))))
                                                                        (type'
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 8))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 7))))))))))
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
                                                                        (id 42)))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43))))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))))))))
                                                                        ((binding
                                                                        ((name v)
                                                                        (id 135)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        init)
                                                                        (id 46)))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 140))))))
                                                                        (args
                                                                        (((id
                                                                        ((name v)
                                                                        (id 135))))))
                                                                        (type'
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43))))))))))))
                                                                        ((binding
                                                                        ((name
                                                                        reduce-array-arg)
                                                                        (id 142)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 47)))))))))
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
                                                                        reduce-f-arg)
                                                                        (id 134))))
                                                                        ((id
                                                                        ((name
                                                                        reduce-zero-arg)
                                                                        (id 141))))
                                                                        ((id
                                                                        ((name
                                                                        reduce-array-arg)
                                                                        (id 142))))))
                                                                        (type'
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 44))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 42)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 43)))))))))))))))))))))))))))))
                                                               (args
                                                                ((Dimension
                                                                  ((const 0)
                                                                   (refs
                                                                    ((((name d-1)
                                                                       (id 51))
                                                                      1)))))
                                                                 (Shape
                                                                  ((ShapeRef
                                                                    ((name
                                                                      @item-pad)
                                                                     (id 52)))))
                                                                 (Shape
                                                                  ((ShapeRef
                                                                    ((name
                                                                      @cell-shape)
                                                                     (id 53))))))))))
                                                            (args
                                                             ((Atom
                                                               (AtomRef
                                                                ((name t)
                                                                 (id 54))))))))))
                                                        ((binding
                                                          ((name f) (id 116)))
                                                         (value
                                                          (Ref
                                                           ((id
                                                             ((name op) (id 55)))))))
                                                        ((binding
                                                          ((name init) (id 122)))
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding
                                                                ((name f)
                                                                 (id 121)))
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
                                                                        (id 120)))
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
                                                                        (id 118)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 27)))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 119)))
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
                                                                        (id 120))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 118))))
                                                                        ((id
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 119))))))
                                                                        (type'
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 26))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 25))))))))))
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
                                                                        (id 51))
                                                                        1)))))
                                                                       (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 52)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 53))))))))))
                                                                  (args
                                                                   ((Atom
                                                                     (AtomRef
                                                                      ((name t)
                                                                       (id 54))))))))))
                                                              ((binding
                                                                ((name arr)
                                                                 (id 117)))
                                                               (value
                                                                (Ref
                                                                 ((id
                                                                   ((name arr)
                                                                    (id 56)))))))))
                                                            (body
                                                             (TermApplication
                                                              ((func
                                                                (Ref
                                                                 ((id
                                                                   ((name f)
                                                                    (id 121))))))
                                                               (args
                                                                (((id
                                                                   ((name arr)
                                                                    (id 117))))))
                                                               (type'
                                                                ((element
                                                                  (AtomRef
                                                                   ((name t)
                                                                    (id 54))))
                                                                 (shape
                                                                  ((ShapeRef
                                                                    ((name
                                                                      @item-pad)
                                                                     (id 52)))
                                                                   (ShapeRef
                                                                    ((name
                                                                      @cell-shape)
                                                                     (id 53))))))))))
                                                            (frameShape ())
                                                            (type'
                                                             (Arr
                                                              ((element
                                                                (AtomRef
                                                                 ((name t)
                                                                  (id 54))))
                                                               (shape
                                                                ((ShapeRef
                                                                  ((name
                                                                    @item-pad)
                                                                   (id 52)))
                                                                 (ShapeRef
                                                                  ((name
                                                                    @cell-shape)
                                                                   (id 53))))))))))))
                                                        ((binding
                                                          ((name arr) (id 128)))
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding
                                                                ((name f)
                                                                 (id 127)))
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
                                                                        (id 126)))
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
                                                                        (id 124)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 31)))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 125)))
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
                                                                        (id 126))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 124))))
                                                                        ((id
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 125))))))
                                                                        (type'
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
                                                                        (id 29))))))))))
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
                                                                        (id 51))
                                                                        1)))))
                                                                       (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @item-pad)
                                                                        (id 52)))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 53))))))))))
                                                                  (args
                                                                   ((Atom
                                                                     (AtomRef
                                                                      ((name t)
                                                                       (id 54))))))))))
                                                              ((binding
                                                                ((name arr)
                                                                 (id 123)))
                                                               (value
                                                                (Ref
                                                                 ((id
                                                                   ((name arr)
                                                                    (id 56)))))))))
                                                            (body
                                                             (TermApplication
                                                              ((func
                                                                (Ref
                                                                 ((id
                                                                   ((name f)
                                                                    (id 127))))))
                                                               (args
                                                                (((id
                                                                   ((name arr)
                                                                    (id 123))))))
                                                               (type'
                                                                ((element
                                                                  (AtomRef
                                                                   ((name t)
                                                                    (id 54))))
                                                                 (shape
                                                                  ((Add
                                                                    ((const 0)
                                                                     (refs
                                                                      ((((name
                                                                        d-1)
                                                                        (id 51))
                                                                        1)))))
                                                                   (ShapeRef
                                                                    ((name
                                                                      @item-pad)
                                                                     (id 52)))
                                                                   (ShapeRef
                                                                    ((name
                                                                      @cell-shape)
                                                                     (id 53))))))))))
                                                            (frameShape ())
                                                            (type'
                                                             (Arr
                                                              ((element
                                                                (AtomRef
                                                                 ((name t)
                                                                  (id 54))))
                                                               (shape
                                                                ((Add
                                                                  ((const 0)
                                                                   (refs
                                                                    ((((name d-1)
                                                                       (id 51))
                                                                      1)))))
                                                                 (ShapeRef
                                                                  ((name
                                                                    @item-pad)
                                                                   (id 52)))
                                                                 (ShapeRef
                                                                  ((name
                                                                    @cell-shape)
                                                                   (id 53))))))))))))))
                                                      (body
                                                       (Map
                                                        ((args
                                                          (((binding
                                                             ((name init)
                                                              (id 145)))
                                                            (value
                                                             (Ref
                                                              ((id
                                                                ((name init)
                                                                 (id 122)))))))))
                                                         (body
                                                          (TermApplication
                                                           ((func
                                                             (Ref
                                                              ((id
                                                                ((name f)
                                                                 (id 144))))))
                                                            (args
                                                             (((id
                                                                ((name f)
                                                                 (id 116))))
                                                              ((id
                                                                ((name init)
                                                                 (id 145))))
                                                              ((id
                                                                ((name arr)
                                                                 (id 128))))))
                                                            (type'
                                                             ((element
                                                               (AtomRef
                                                                ((name t)
                                                                 (id 54))))
                                                              (shape
                                                               ((ShapeRef
                                                                 ((name
                                                                   @item-pad)
                                                                  (id 52)))
                                                                (ShapeRef
                                                                 ((name
                                                                   @cell-shape)
                                                                  (id 53))))))))))
                                                         (frameShape
                                                          ((ShapeRef
                                                            ((name @item-pad)
                                                             (id 52)))))
                                                         (type'
                                                          (Arr
                                                           ((element
                                                             (AtomRef
                                                              ((name t) (id 54))))
                                                            (shape
                                                             ((ShapeRef
                                                               ((name @item-pad)
                                                                (id 52)))
                                                              (ShapeRef
                                                               ((name @item-pad)
                                                                (id 52)))
                                                              (ShapeRef
                                                               ((name
                                                                 @cell-shape)
                                                                (id 53)))))))))))
                                                      (frameShape ())
                                                      (type'
                                                       (Arr
                                                        ((element
                                                          (AtomRef
                                                           ((name t) (id 54))))
                                                         (shape
                                                          ((ShapeRef
                                                            ((name @item-pad)
                                                             (id 52)))
                                                           (ShapeRef
                                                            ((name @item-pad)
                                                             (id 52)))
                                                           (ShapeRef
                                                            ((name @cell-shape)
                                                             (id 53)))))))))))))))))))))))))))))
                                 (args
                                  ((Dimension
                                    ((const 0)
                                     (refs ((((name d-1) (id 112)) 1)))))
                                   (Shape ()) (Shape ()))))))
                              (args ((Atom (Literal IntLiteral))))))))
                          ((binding ((name op) (id 114)))
                           (value (Primitive ((name (Func Add))))))
                          ((binding ((name arr) (id 115)))
                           (value (Ref ((id ((name row) (id 113)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 146))))))
                           (args
                            (((id ((name op) (id 114))))
                             ((id ((name arr) (id 115))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 148)))
            (value
             (IndexApplication
              ((iFunc (Ref ((id ((name sum-row) (id 111))))))
               (args ((Dimension ((const 9) (refs ())))))))))
           ((binding ((name row) (id 147)))
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
             (((binding ((name row) (id 149)))
               (value (Ref ((id ((name row) (id 147)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 148))))))
               (args (((id ((name row) (id 149))))))
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
       (((binding ((name sum-row) (id 151)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 152)))
            (value
             (Ref
              ((id ((name sum-row) (id 151)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name row) (id 299)))
            (value
             (ArrayPrimitive
              (Map
               (frameShape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
               (args ()) (iotaVar (((name iota) (id 298))))
               (body
                (Ref
                 ((id ((name iota) (id 298)))
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
             (((binding ((name row) (id 300)))
               (value
                (Ref
                 ((id ((name row) (id 299)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape
                     ((Add ((const 1000000) (refs ())))
                      (Add ((const 10) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 202)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name op) (id 288)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 301)))
                  (value
                   (Ref
                    ((id ((name row) (id 300)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 10) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 248)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name f) (id 289)))
                     (value
                      (Ref
                       ((id ((name op) (id 288)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name init) (id 338)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 329)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 335)))
                           (value
                            (Ref
                             ((id ((name arr) (id 301)))
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
                               ((name contiguous-subarray-array) (id 336)))
                              (value
                               (Ref
                                ((id ((name arr) (id 335)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding
                               ((name contiguous-subarray-index) (id 337)))
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
                                ((id ((name contiguous-subarray-array) (id 336)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id ((name contiguous-subarray-index) (id 337)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 1) (refs ()))))))))))
                              (originalShape ((Add ((const 10) (refs ())))))
                              (resultShape ()) (cellShape ())
                              (l ((const 1) (refs ())))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name arr) (id 305)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 292)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 302)))
                           (value
                            (Ref
                             ((id ((name arr) (id 301)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 10) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 297)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding
                               ((name contiguous-subarray-array) (id 303)))
                              (value
                               (Ref
                                ((id ((name arr) (id 302)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding
                               ((name contiguous-subarray-index) (id 304)))
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
                                ((id ((name contiguous-subarray-array) (id 303)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id ((name contiguous-subarray-index) (id 304)))
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
                      (((binding ((name init) (id 339)))
                        (value
                         (Ref
                          ((id ((name init) (id 338)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name up-ranked-f) (id 276)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 274)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding ((name reduce-f-arg) (id 277)))
                              (value
                               (Ref
                                ((id ((name up-ranked-f) (id 276)))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding ((name reduce-zero-arg) (id 342)))
                              (value
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding ((name f) (id 314)))
                                    (value
                                     (AtomAsArray
                                      ((element
                                        (Values ((elements ()) (type' ()))))
                                       (type' ((element (Tuple ())) (shape ())))))))
                                   ((binding ((name v) (id 340)))
                                    (value
                                     (Ref
                                      ((id ((name init) (id 339)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name make) (id 324)))
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
                                        (((binding ((name f) (id 325)))
                                          (value
                                           (Ref
                                            ((id ((name make) (id 324)))
                                             (type'
                                              ((element (Tuple ())) (shape ())))))))
                                         ((binding ((name v) (id 341)))
                                          (value
                                           (Ref
                                            ((id ((name v) (id 340)))
                                             (type'
                                              ((element (Literal IntLiteral))
                                               (shape ())))))))))
                                       (body
                                        (ArrayPrimitive
                                         (Map (frameShape ()) (args ())
                                          (body
                                           (Ref
                                            ((id ((name v) (id 341)))
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
                             ((binding ((name reduce-array-arg) (id 306)))
                              (value
                               (Ref
                                ((id ((name arr) (id 305)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 9) (refs ())))))))))))))
                           (body
                            (ArrayPrimitive
                             (Reduce
                              (arg
                               ((firstBinding ((name reduce-arg1) (id 307)))
                                (secondBinding ((name reduce-arg2) (id 310)))
                                (value
                                 (Ref
                                  ((id ((name reduce-array-arg) (id 306)))
                                   (type'
                                    ((element (Literal IntLiteral))
                                     (shape ((Add ((const 9) (refs ()))))))))))))
                              (zero
                               (Ref
                                ((id ((name reduce-zero-arg) (id 342)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding ((name f) (id 290)))
                                    (value
                                     (Ref
                                      ((id ((name f) (id 289)))
                                       (type' ((element (Tuple ())) (shape ())))))))
                                   ((binding ((name arg0) (id 308)))
                                    (value
                                     (Ref
                                      ((id ((name reduce-arg1) (id 307)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))
                                   ((binding ((name arg1) (id 311)))
                                    (value
                                     (Ref
                                      ((id ((name reduce-arg2) (id 310)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name arg0) (id 309)))
                                       (value
                                        (Ref
                                         ((id ((name arg0) (id 308)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))
                                      ((binding ((name arg1) (id 312)))
                                       (value
                                        (Ref
                                         ((id ((name arg1) (id 311)))
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
                                                ((id ((name arg0) (id 309)))
                                                 (type'
                                                  ((element (Literal IntLiteral))
                                                   (shape ()))))))
                                              (type' (Literal IntLiteral))))
                                            (ArrayAsAtom
                                             ((array
                                               (Ref
                                                ((id ((name arg1) (id 312)))
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
    (let ((sum-row.343 (values)))
     (let ((sum-row.151 sum-row.343))
      (let
       ((f.344 sum-row.151)
        (row.349
         (let ()
          (#0
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota iota.346)
              (#0
               (#0
                (loop-block (frame-shape 10)
                 (map () (iota (iota.348 : iota.346))
                  (let ((iota.298 iota.348)) iota.298))
                 (body-matcher map-result.347) (map-result (map-result.347))
                 (consumer (values))))))
             (body-matcher map-result.345) (map-result (map-result.345))
             (consumer (values))))))))
       (let ((f.152 f.344) (row.299 row.349))
        (let ((row.350 row.299))
         (#0
          (#0
           (loop-block (frame-shape 1000000)
            (map ((row.352 row.350))
             (let ((row.300 row.352))
              (let ((f.353 (values)) (op.354 (values)) (arr.355 row.300))
               (let ((f.202 f.353) (op.288 op.354) (arr.301 arr.355))
                (let
                 ((f.356 (values)) (f.357 op.288)
                  (init.363
                   (let ((f.358 (values)) (arr.359 arr.301))
                    (let ((f.329 f.358) (arr.335 arr.359))
                     (let
                      ((f.360 (values)) (contiguous-subarray-array.361 arr.335)
                       (contiguous-subarray-index.362 (frame 0)))
                      (let
                       ((f.334 f.360)
                        (contiguous-subarray-array.336
                         contiguous-subarray-array.361)
                        (contiguous-subarray-index.337
                         contiguous-subarray-index.362))
                       (contiguous-subarray contiguous-subarray-array.336
                        contiguous-subarray-index.337 (shape 10) (shape)))))))
                  (arr.369
                   (let ((f.364 (values)) (arr.365 arr.301))
                    (let ((f.292 f.364) (arr.302 arr.365))
                     (let
                      ((f.366 (values)) (contiguous-subarray-array.367 arr.302)
                       (contiguous-subarray-index.368 (frame 1)))
                      (let
                       ((f.297 f.366)
                        (contiguous-subarray-array.303
                         contiguous-subarray-array.367)
                        (contiguous-subarray-index.304
                         contiguous-subarray-index.368))
                       (contiguous-subarray contiguous-subarray-array.303
                        contiguous-subarray-index.304 (shape 10) (shape 9))))))))
                 (let
                  ((f.248 f.356) (f.289 f.357) (init.338 init.363)
                   (arr.305 arr.369))
                  (let ((init.370 init.338))
                   (let ((init.339 init.370))
                    (let ((up-ranked-f.371 (values)))
                     (let ((up-ranked-f.276 up-ranked-f.371))
                      (let
                       ((f.372 (values)) (reduce-f-arg.373 up-ranked-f.276)
                        (reduce-zero-arg.379
                         (let ((f.374 (values)) (v.375 init.339))
                          (let ((f.314 f.374) (v.340 v.375))
                           (let ((make.376 (values)))
                            (let ((make.324 make.376))
                             (let ((f.377 make.324) (v.378 v.340))
                              (let ((f.325 f.377) (v.341 v.378))
                               (let () (let () v.341)))))))))
                        (reduce-array-arg.380 arr.305))
                       (let
                        ((f.274 f.372) (reduce-f-arg.277 reduce-f-arg.373)
                         (reduce-zero-arg.342 reduce-zero-arg.379)
                         (reduce-array-arg.306 reduce-array-arg.380))
                        (let ((reduce-arg.387 reduce-array-arg.306))
                         (#1
                          (loop-block (frame-shape 9)
                           (map ((reduce-arg.388 reduce-arg.387))
                            (values reduce-arg.388))
                           (body-matcher (reduce-arg.381)) (map-result ())
                           (consumer
                            (reduce-zero reduce-zero-arg.342
                             (reduce-arg1.307 reduce-arg2.310 reduce-arg.381)
                             (let
                              ((f.382 f.289) (arg0.383 reduce-arg1.307)
                               (arg1.384 reduce-arg2.310))
                              (let
                               ((f.290 f.382) (arg0.308 arg0.383)
                                (arg1.311 arg1.384))
                               (let ((arg0.385 arg0.308) (arg1.386 arg1.311))
                                (let ((arg0.309 arg0.385) (arg1.312 arg1.386))
                                 (+ arg0.309 arg1.312))))))))))))))))))))))
            (body-matcher map-result.351) (map-result (map-result.351))
            (consumer (values))))))))))
    Result of stage Fuse and Simplify:
    (let
     ((contiguous-subarray-index.362 (frame 0))
      (contiguous-subarray-index.368 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 1000000)
        (map () (iota iota.346)
         (let
          ((map-result.420
            (#0
             (loop-block (frame-shape 10)
              (map () (iota (iota.348 : iota.346)) iota.348)
              (body-matcher map-result.347) (map-result (map-result.347))
              (consumer (values))))))
          (let
           ((reduce-arg.415
             (contiguous-subarray (#0 map-result.420)
              contiguous-subarray-index.368 (shape 10) (shape 9))))
           (#1
            (loop-block (frame-shape 9)
             (map ((reduce-arg.388 reduce-arg.415)) reduce-arg.388)
             (body-matcher reduce-arg.381) (map-result ())
             (consumer
              (reduce-zero
               (contiguous-subarray (#0 map-result.420)
                contiguous-subarray-index.362 (shape 10) (shape))
               (reduce-arg1.307 reduce-arg2.310 reduce-arg.381)
               (+ reduce-arg1.307 reduce-arg2.310))))))))
        (body-matcher map-result.351) (map-result (map-result.351))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((contiguous-subarray-index.362 (frame 0))
      (contiguous-subarray-index.368 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.346)
        (body-matcher map-result.351) (map-result (map-result.351))
        (let
         ((map-result.420
           (#0
            (loop-block (frame-shape 10)
             (map () (iota (iota.348 : iota.346)) iota.348)
             (body-matcher map-result.347) (map-result (map-result.347))
             (consumer (values))))))
         (let
          ((reduce-arg.415
            (contiguous-subarray (#0 map-result.420)
             contiguous-subarray-index.368 (shape 10) (shape 9))))
          (#1
           (loop-block (frame-shape 9)
            (map ((reduce-arg.388 reduce-arg.415)) reduce-arg.388)
            (body-matcher reduce-arg.381) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.420)
               contiguous-subarray-index.362 (shape 10) (shape))
              (reduce-arg1.307 reduce-arg2.310 reduce-arg.381)
              (+ reduce-arg1.307 reduce-arg2.310)))))))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.446 (Tuple ()) device)
      (map-mem.442
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.440
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.439
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.437 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.435 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.362
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.435) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.435)))
       (contiguous-subarray-index.368
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.437) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.437))))
      (#0
       (begin
        (kernel (blocks 320) (threads 32)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.441 map-mem.440))
            (iota iota.346)
            (do-expr
             (let
              ((map-result.420
                (#0
                 (loop (frame-shape 10)
                  (map ((map-mem.443 map-mem.442)) (iota (iota.348 : iota.346))
                   (let ((expr-result.444 iota.348))
                    (begin (putmem expr-result.444 (#0 map-mem.443))
                     expr-result.444)))
                  (body-matcher map-result.347) (map-result (map-result.347))
                  (map-result-mem-interim map-mem.442)
                  (map-result-mem-final map-mem.442) (consumer (values))))))
              (let
               ((reduce-arg.415
                 (index
                  (#0
                   (let ((expr-result.445 map-result.420))
                    (values (#0 expr-result.445))))
                  contiguous-subarray-index.368 (shape 10) (shape 9))))
               (#1
                (let
                 ((expr-result.449
                   (loop (frame-shape 9)
                    (map
                     ((reduce-arg.388 reduce-arg.415) (map-mem.447 map-mem.446))
                     reduce-arg.388)
                    (body-matcher reduce-arg.381) (map-result ())
                    (map-result-mem-interim map-mem.446)
                    (map-result-mem-final map-mem.446)
                    (consumer
                     (reduce-zero
                      (index
                       (#0
                        (let ((expr-result.448 map-result.420))
                         (values (#0 expr-result.448))))
                       contiguous-subarray-index.362 (shape 10) (shape))
                      (reduce-arg1.307 reduce-arg2.310 reduce-arg.381)
                      (+ reduce-arg1.307 reduce-arg2.310))))))
                 (begin (putmem (#1 expr-result.449) (#0 map-mem.441))
                  expr-result.449))))))))
          (map-result-mem-interim map-mem.440)
          (map-result-mem-final (values map-mem-result.0.439))))
        (getmem (values map-mem-result.0.439))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.446 (Tuple ()) device)
      (map-mem.442
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.440
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.439
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.437 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.435 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.362
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.435) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.435)))
       (contiguous-subarray-index.368
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.437) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.437))))
      (#0
       (begin
        (kernel captures
         ((expr-captures
           ((((name contiguous-subarray-index) (id 362))
             (Array ((element (Literal IntLiteral)) (shape (shape 1)))))
            (((name contiguous-subarray-index) (id 368))
             (Array ((element (Literal IntLiteral)) (shape (shape 1)))))))
          (mem-captures
           ((((name map-mem) (id 440))
             (Tuple
              ((Array ((element (Literal IntLiteral)) (shape (shape 1000000)))))))
            (((name map-mem) (id 442))
             (Tuple
              ((Array ((element (Literal IntLiteral)) (shape (shape 10)))))))
            (((name map-mem) (id 446)) (Tuple ()))))
          (index-captures ()))
         (blocks 320) (threads 32)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.441 map-mem.440))
            (iota iota.346)
            (do-expr
             (let
              ((map-result.420
                (#0
                 (loop (frame-shape 10)
                  (map ((map-mem.443 map-mem.442)) (iota (iota.348 : iota.346))
                   (let ((expr-result.444 iota.348))
                    (begin (putmem expr-result.444 (#0 map-mem.443))
                     expr-result.444)))
                  (body-matcher map-result.347) (map-result (map-result.347))
                  (map-result-mem-interim map-mem.442)
                  (map-result-mem-final map-mem.442) (consumer (values))))))
              (let
               ((reduce-arg.415
                 (index
                  (#0
                   (let ((expr-result.445 map-result.420))
                    (values (#0 expr-result.445))))
                  contiguous-subarray-index.368 (shape 10) (shape 9))))
               (#1
                (let
                 ((expr-result.449
                   (loop (frame-shape 9)
                    (map
                     ((reduce-arg.388 reduce-arg.415) (map-mem.447 map-mem.446))
                     reduce-arg.388)
                    (body-matcher reduce-arg.381) (map-result ())
                    (map-result-mem-interim map-mem.446)
                    (map-result-mem-final map-mem.446)
                    (consumer
                     (reduce-zero
                      (index
                       (#0
                        (let ((expr-result.448 map-result.420))
                         (values (#0 expr-result.448))))
                       contiguous-subarray-index.362 (shape 10) (shape))
                      (reduce-arg1.307 reduce-arg2.310 reduce-arg.381)
                      (+ reduce-arg1.307 reduce-arg2.310))))))
                 (begin (putmem (#1 expr-result.449) (#0 map-mem.441))
                  expr-result.449))))))))
          (map-result-mem-interim map-mem.440)
          (map-result-mem-final (values map-mem-result.0.439))))
        (getmem (values map-mem-result.0.439)))))) |}]
;;
