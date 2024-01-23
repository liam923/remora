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
       (((binding ((name f) (id 94))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 92)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 93)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 94))))))
         (args (((id ((name +arg1) (id 92)))) ((id ((name +arg2) (id 93))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 95)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 96)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 97)))
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
                  ((id ((name +arg1) (id 96)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id ((name +arg2) (id 97)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))))
            (type' (Literal IntLiteral)))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((f.98 (values)) (+arg1.99 1) (+arg2.100 2))
     (let ((f.95 f.98) (+arg1.96 +arg1.99) (+arg2.97 +arg2.100))
      (+ +arg1.96 +arg2.97)))
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
     ((binding ((name add) (id 92)))
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding ((name x) (id 93)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding ((name y) (id 94)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args
                ((Ref ((id ((name x) (id 93))))) (Ref ((id ((name y) (id 94))))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name add) (id 92))))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name add) (id 92)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name x) (id 93)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name y) (id 94)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 97)))
                     (value (Primitive ((name (Func Add))))))
                    ((binding ((name +arg1) (id 95)))
                     (value (Ref ((id ((name x) (id 93)))))))
                    ((binding ((name +arg2) (id 96)))
                     (value (Ref ((id ((name y) (id 94)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 97))))))
                     (args
                      (((id ((name +arg1) (id 95))))
                       ((id ((name +arg2) (id 96))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 100)))
            (value (Ref ((id ((name add) (id 92)))))))
           ((binding ((name x) (id 98)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding ((name y) (id 99)))
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 100))))))
            (args (((id ((name x) (id 98)))) ((id ((name y) (id 99))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name add) (id 102)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 103)))
            (value
             (Ref
              ((id ((name add) (id 102)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 110)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name y) (id 112)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 109)))
               (value
                (AtomAsArray
                 ((element (Values ((elements ()) (type' ()))))
                  (type' ((element (Tuple ())) (shape ())))))))
              ((binding ((name +arg1) (id 111)))
               (value
                (Ref
                 ((id ((name x) (id 110)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name +arg2) (id 113)))
               (value
                (Ref
                 ((id ((name y) (id 112)))
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
                        ((id ((name +arg1) (id 111)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 113)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))))
                  (type' (Literal IntLiteral)))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((add.114 (values)))
     (let ((add.102 add.114))
      (let ((f.115 add.102) (x.116 5) (y.117 10))
       (let ((f.103 f.115) (x.110 x.116) (y.112 y.117))
        (let ((f.118 (values)) (+arg1.119 x.110) (+arg2.120 y.112))
         (let ((f.109 f.118) (+arg1.111 +arg1.119) (+arg2.113 +arg2.120))
          (+ +arg1.111 +arg2.113)))))))
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
     ((binding ((name id) (id 92)))
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @t) (id 93))) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name e) (id 94)))
                     (bound (ArrayRef ((name @t) (id 93)))))))
                  (body (Ref ((id ((name e) (id 94))))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id ((name id) (id 92))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name id) (id 92)))
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding ((name @t) (id 93))) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name e) (id 94)))
                        (bound (ArrayRef ((name @t) (id 93)))))))
                     (body (Ref ((id ((name e) (id 94)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 96)))
            (value
             (TypeApplication
              ((tFunc (Ref ((id ((name id) (id 92))))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding ((name e) (id 95)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 96))))))
            (args (((id ((name e) (id 95))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 98)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 99)))
            (value
             (Ref
              ((id ((name id) (id 98)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name e) (id 101)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name e) (id 101)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((id.102 (values)))
     (let ((id.98 id.102))
      (let ((f.103 id.98) (e.104 5)) (let ((f.99 f.103) (e.101 e.104)) e.101))))
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
       (((binding ((name f) (id 94))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 92)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding ((name +arg2) (id 93)))
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
          (((binding ((name +arg1) (id 95)))
            (value (Ref ((id ((name +arg1) (id 92)))))))
           ((binding ((name +arg2) (id 96)))
            (value (Ref ((id ((name +arg2) (id 93)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg2) (id 97)))
               (value (Ref ((id ((name +arg2) (id 96)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 94))))))
               (args
                (((id ((name +arg1) (id 95)))) ((id ((name +arg2) (id 97))))))
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
       (((binding ((name f) (id 98)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 99)))
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
        ((binding ((name +arg2) (id 101)))
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
          (((binding ((name +arg1) (id 100)))
            (value
             (Ref
              ((id ((name +arg1) (id 99)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding ((name +arg2) (id 102)))
            (value
             (Ref
              ((id ((name +arg2) (id 101)))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 103)))
               (value
                (Ref
                 ((id ((name +arg2) (id 102)))
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
                        ((id ((name +arg1) (id 100)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 103)))
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
     ((f.104 (values)) (+arg1.105 (frame 1 2))
      (+arg2.106 (frame (frame 3 4 5) (frame 6 7 8))))
     (let ((f.98 f.104) (+arg1.99 +arg1.105) (+arg2.101 +arg2.106))
      (let ((+arg1.107 +arg1.99) (+arg2.108 +arg2.101))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.110 +arg1.107) (+arg2.111 +arg2.108))
           (let ((+arg1.100 +arg1.110) (+arg2.102 +arg2.111))
            (let ((+arg2.112 +arg2.102))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg2.114 +arg2.112))
                 (let ((+arg2.103 +arg2.114)) (+ +arg1.100 +arg2.103)))
                (body-matcher map-result.113) (map-result (map-result.113))
                (consumer (values))))))))
          (body-matcher map-result.109) (map-result (map-result.109))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((+arg1.107 (frame 1 2)) (+arg2.108 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.110 +arg1.107) (+arg2.111 +arg2.108))
         (let ((+arg2.112 +arg2.111))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.114 +arg2.112)) (+ +arg1.110 +arg2.114))
             (body-matcher map-result.113) (map-result (map-result.113))
             (consumer (values)))))))
        (body-matcher map-result.109) (map-result (map-result.109))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((+arg1.107 (frame 1 2)) (+arg2.108 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.110 +arg1.107) (+arg2.111 +arg2.108))
         (let ((+arg2.112 +arg2.111))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.114 +arg2.112)) (+ +arg1.110 +arg2.114))
             (body-matcher map-result.113) (map-result (map-result.113))
             (consumer (values)))))))
        (body-matcher map-result.109) (map-result (map-result.109))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.136 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.129
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.126 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.107
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.126) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.126) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.126)))
       (+arg2.108
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.129) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.129) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.129) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.129) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.129) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.129) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.129))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.110 +arg1.107) (+arg2.111 +arg2.108) (map-mem.137 map-mem.136))
          (let ((+arg2.112 +arg2.111))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.114 +arg2.112) (map-mem.138 map-mem.137))
               (let ((expr-result.139 (+ +arg1.110 +arg2.114)))
                (begin (putmem expr-result.139 map-mem.138) expr-result.139)))
              (body-matcher map-result.113) (map-result (map-result.113))
              (map-result-mem-interim (values map-mem.137))
              (map-result-mem-final (values map-mem.137)) (consumer (values)))))))
         (body-matcher map-result.109) (map-result (map-result.109))
         (map-result-mem-interim (values map-mem.136))
         (map-result-mem-final (values map-mem.136)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.136 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.129
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.126 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.107
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.126) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.126) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.126)))
       (+arg2.108
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.129) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.129) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.129) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.129) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.129) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.129) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.129))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.110 +arg1.107) (+arg2.111 +arg2.108) (map-mem.137 map-mem.136))
          (let ((+arg2.112 +arg2.111))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.114 +arg2.112) (map-mem.138 map-mem.137))
               (let ((expr-result.139 (+ +arg1.110 +arg2.114)))
                (begin (putmem expr-result.139 map-mem.138) expr-result.139)))
              (body-matcher map-result.113) (map-result (map-result.113))
              (map-result-mem-interim (values map-mem.137))
              (map-result-mem-final (values map-mem.137)) (consumer (values)))))))
         (body-matcher map-result.109) (map-result (map-result.109))
         (map-result-mem-interim (values map-mem.136))
         (map-result-mem-final (values map-mem.136)) (consumer (values))))))) |}]
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
     ((binding ((name words) (id 92)))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 93)) 1)))))))))))))))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 93)) 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings ((((name len) (id 94)) Dim)))
         (valueBinding ((name word) (id 95)))
         (box (Ref ((id ((name words) (id 92))))))
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
                       ((Dimension ((const 0) (refs ((((name len) (id 94)) 1)))))
                        (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id ((name word) (id 95)))))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name words) (id 92)))
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
                       ((Add ((const 0) (refs ((((name len) (id 93)) 1)))))))))))))))
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
                       ((Add ((const 0) (refs ((((name len) (id 93)) 1)))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name box) (id 101)))
            (value (Ref ((id ((name words) (id 92)))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 94)))
               (indexValue
                (FromBox (box (Ref ((id ((name box) (id 101)))))) (i 0)))
               (sort Dim))))
            (body
             (Map
              ((args
                (((binding ((name word) (id 95)))
                  (value (BoxValue ((box (Ref ((id ((name box) (id 101))))))))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 100)))
                     (value (Primitive ((name (Func Equal))))))
                    ((binding ((name =arg1) (id 96)))
                     (value (Scalar ((element (Literal (IntLiteral 3)))))))
                    ((binding ((name =arg2) (id 99)))
                     (value
                      (Map
                       ((args
                         (((binding ((name f) (id 98)))
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
                                    ((const 0) (refs ((((name len) (id 94)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal CharacterLiteral))))))))
                          ((binding ((name arr) (id 97)))
                           (value (Ref ((id ((name word) (id 95)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 98))))))
                           (args (((id ((name arr) (id 97))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 100))))))
                     (args
                      (((id ((name =arg1) (id 96))))
                       ((id ((name =arg2) (id 99))))))
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
       (((binding ((name words) (id 108)))
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
                      ((Add ((const 0) (refs ((((name len) (id 93)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 93))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 93)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 93))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 93)) 1))))))))))))
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
                      ((Add ((const 0) (refs ((((name len) (id 93)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 93))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 93)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 93))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 93)) 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 93))) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 93)) 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name box) (id 109)))
            (value
             (Ref
              ((id ((name words) (id 108)))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 93))) (bound Dim))))
                    (body
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 93)) 1))))))))))))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 94)))
               (indexValue
                (FromBox
                 (box
                  (Ref
                   ((id ((name box) (id 109)))
                    (type'
                     ((element
                       (Sigma
                        ((parameters
                          (((binding ((name len) (id 93))) (bound Dim))))
                         (body
                          ((element (Literal CharacterLiteral))
                           (shape
                            ((Add ((const 0) (refs ((((name len) (id 93)) 1))))))))))))
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
                   (((binding ((name f) (id 102)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name =arg1) (id 103)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 3)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name =arg2) (id 107)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 105)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ReifyIndex
                          ((index
                            (Dimension
                             ((const 0) (refs ((((name len) (id 94)) 1))))))
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
                              ((id ((name =arg1) (id 103)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name =arg2) (id 107)))
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
     ((words.110 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (let ((words.108 words.110))
      (let ((box.111 words.108))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((box.113 box.111))
           (let ((box.109 box.113))
            (index-let ((len.94 box-index-0 box.109))
             (let ()
              (let ()
               (let
                ((f.114 (values)) (=arg1.115 3)
                 (=arg2.117
                  (let ((f.116 (values)))
                   (let ((f.105 f.116)) (reify-index len.94)))))
                (let ((f.102 f.114) (=arg1.103 =arg1.115) (=arg2.107 =arg2.117))
                 (= =arg1.103 =arg2.107))))))))
          (body-matcher map-result.112) (map-result (map-result.112))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((box.111 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.113 box.111))
         (index-let ((len.94 box-index-0 box.113)) (= 3 (reify-index len.94))))
        (body-matcher map-result.112) (map-result (map-result.112))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((box.111 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.113 box.111))
         (index-let ((len.94 box-index-0 box.113)) (= 3 (reify-index len.94))))
        (body-matcher map-result.112) (map-result (map-result.112))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.129 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.125
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.120
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.119
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 93))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.93))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.111
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.120) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.120) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.120) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.120)))
           (index (mem frame-array.119) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 93))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.93)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.125) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.125) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.125)))
           (index (mem frame-array.119) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 93))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.93))))))))))))
         (getmem frame-array.119))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.113 box.111) (map-mem.130 map-mem.129))
          (index-let ((len.94 box-index-0 box.113))
           (let ((expr-result.131 (= 3 (reify-dimension-index len.94))))
            (begin (putmem expr-result.131 map-mem.130) expr-result.131))))
         (body-matcher map-result.112) (map-result (map-result.112))
         (map-result-mem-interim (values map-mem.129))
         (map-result-mem-final (values map-mem.129)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.129 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.125
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.120
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.119
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 93))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.93))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.111
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.120) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.120) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.120) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.120)))
           (index (mem frame-array.119) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 93))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.93)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.125) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.125) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.125)))
           (index (mem frame-array.119) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 93))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.93))))))))))))
         (getmem frame-array.119))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.113 box.111) (map-mem.130 map-mem.129))
          (index-let ((len.94 box-index-0 box.113))
           (let ((expr-result.131 (= 3 (reify-dimension-index len.94))))
            (begin (putmem expr-result.131 map-mem.130) expr-result.131))))
         (body-matcher map-result.112) (map-result (map-result.112))
         (map-result-mem-interim (values map-mem.129))
         (map-result-mem-final (values map-mem.129)) (consumer (values))))))) |}]
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
     ((binding ((name sum-row) (id 92)))
      (value
       (Scalar
        ((element
          (IndexLambda
           ((params (((binding ((name d-1) (id 93))) (bound Dim))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name row) (id 94)))
                     (bound
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape
                         ((Add ((const 1) (refs ((((name d-1) (id 93)) 1)))))))))))))
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
                                  (((binding ((name d-1) (id 33))) (bound Dim))
                                   ((binding ((name @cell-shape) (id 34)))
                                    (bound Shape))))
                                 (body
                                  (Scalar
                                   ((element
                                     (TypeLambda
                                      ((params
                                        (((binding ((name t) (id 35)))
                                          (bound Atom))))
                                       (body
                                        (Scalar
                                         ((element
                                           (TermLambda
                                            ((params
                                              (((binding ((name op) (id 36)))
                                                (bound
                                                 (Arr
                                                  ((element
                                                    (Func
                                                     ((parameters
                                                       ((Arr
                                                         ((element
                                                           (AtomRef
                                                            ((name t) (id 35))))
                                                          (shape
                                                           ((ShapeRef
                                                             ((name @cell-shape)
                                                              (id 34)))))))
                                                        (Arr
                                                         ((element
                                                           (AtomRef
                                                            ((name t) (id 35))))
                                                          (shape
                                                           ((ShapeRef
                                                             ((name @cell-shape)
                                                              (id 34)))))))))
                                                      (return
                                                       (Arr
                                                        ((element
                                                          (AtomRef
                                                           ((name t) (id 35))))
                                                         (shape
                                                          ((ShapeRef
                                                            ((name @cell-shape)
                                                             (id 34)))))))))))
                                                   (shape ())))))
                                               ((binding ((name arr) (id 37)))
                                                (bound
                                                 (Arr
                                                  ((element
                                                    (AtomRef ((name t) (id 35))))
                                                   (shape
                                                    ((Add
                                                      ((const 1)
                                                       (refs
                                                        ((((name d-1) (id 33)) 1)))))
                                                     (ShapeRef
                                                      ((name @cell-shape)
                                                       (id 34)))))))))))
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
                                                            (associative true)
                                                            (character Reduce)))))))
                                                      (args
                                                       ((Dimension
                                                         ((const 0)
                                                          (refs
                                                           ((((name d-1) (id 33))
                                                             1)))))
                                                        (Shape
                                                         ((ShapeRef
                                                           ((name @cell-shape)
                                                            (id 34))))))))))
                                                   (args
                                                    ((Atom
                                                      (AtomRef
                                                       ((name t) (id 35)))))))))
                                                (args
                                                 ((Ref
                                                   ((id ((name op) (id 36)))))
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
                                                                     (id 16)))
                                                                   (bound Dim))
                                                                  ((binding
                                                                    ((name
                                                                      @cell-shape)
                                                                     (id 17)))
                                                                   (bound Shape))))
                                                                (body
                                                                 (Scalar
                                                                  ((element
                                                                    (TypeLambda
                                                                     ((params
                                                                       (((binding
                                                                        ((name t)
                                                                        (id 18)))
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
                                                                        (id 19)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 18))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 16))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 17)))))))))))
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
                                                                        (id 16))
                                                                        1)))))))
                                                                        (Shape
                                                                        ())
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 17)))))
                                                                        (Dimension
                                                                        ((const
                                                                        1)
                                                                        (refs ()))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 18)))))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 19)))))
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
                                                                  (id 33))
                                                                 1)))))
                                                            (Shape
                                                             ((ShapeRef
                                                               ((name
                                                                 @cell-shape)
                                                                (id 34))))))))))
                                                       (args
                                                        ((Atom
                                                          (AtomRef
                                                           ((name t) (id 35)))))))))
                                                    (args
                                                     ((Ref
                                                       ((id ((name arr) (id 37)))))))))
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
                                                                     (id 20)))
                                                                   (bound Dim))
                                                                  ((binding
                                                                    ((name
                                                                      @cell-shape)
                                                                     (id 21)))
                                                                   (bound Shape))))
                                                                (body
                                                                 (Scalar
                                                                  ((element
                                                                    (TypeLambda
                                                                     ((params
                                                                       (((binding
                                                                        ((name t)
                                                                        (id 22)))
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
                                                                        (id 23)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 22))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 20))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 21)))))))))))
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
                                                                        (id 20))
                                                                        1)))))))
                                                                        (Shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 20))
                                                                        1)))))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 21)))))
                                                                        (Dimension
                                                                        ((const
                                                                        1)
                                                                        (refs ()))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 22)))))))))
                                                                        (args
                                                                        ((Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 23)))))
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
                                                                  (id 33))
                                                                 1)))))
                                                            (Shape
                                                             ((ShapeRef
                                                               ((name
                                                                 @cell-shape)
                                                                (id 34))))))))))
                                                       (args
                                                        ((Atom
                                                          (AtomRef
                                                           ((name t) (id 35)))))))))
                                                    (args
                                                     ((Ref
                                                       ((id ((name arr) (id 37))))))))))))))))))))))))))))))))
                           (args
                            ((Dimension
                              ((const 0) (refs ((((name d-1) (id 93)) 1)))))
                             (Shape ()))))))
                        (args ((Atom (Literal IntLiteral)))))))
                     (args
                      ((Primitive ((name (Func Add))))
                       (Ref ((id ((name row) (id 94))))))))))))))))))))))
      (body
       (TermApplication
        ((func
          (IndexApplication
           ((iFunc (Ref ((id ((name sum-row) (id 92))))))
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
       (((binding ((name sum-row) (id 92)))
         (value
          (Scalar
           ((element
             (IndexLambda
              ((params (((binding ((name d-1) (id 93))) (bound Dim))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name row) (id 94)))
                        (bound
                         (Arr
                          ((element (Literal IntLiteral))
                           (shape
                            ((Add ((const 1) (refs ((((name d-1) (id 93)) 1)))))))))))))
                     (body
                      (Map
                       ((args
                         (((binding ((name f) (id 111)))
                           (value
                            (TypeApplication
                             ((tFunc
                               (IndexApplication
                                ((iFunc
                                  (Scalar
                                   ((element
                                     (IndexLambda
                                      ((params
                                        (((binding ((name d-1) (id 33)))
                                          (bound Dim))
                                         ((binding ((name @cell-shape) (id 34)))
                                          (bound Shape))))
                                       (body
                                        (Scalar
                                         ((element
                                           (TypeLambda
                                            ((params
                                              (((binding ((name t) (id 35)))
                                                (bound Atom))))
                                             (body
                                              (Scalar
                                               ((element
                                                 (TermLambda
                                                  ((params
                                                    (((binding
                                                       ((name op) (id 36)))
                                                      (bound
                                                       (Arr
                                                        ((element
                                                          (Func
                                                           ((parameters
                                                             ((Arr
                                                               ((element
                                                                 (AtomRef
                                                                  ((name t)
                                                                   (id 35))))
                                                                (shape
                                                                 ((ShapeRef
                                                                   ((name
                                                                     @cell-shape)
                                                                    (id 34)))))))
                                                              (Arr
                                                               ((element
                                                                 (AtomRef
                                                                  ((name t)
                                                                   (id 35))))
                                                                (shape
                                                                 ((ShapeRef
                                                                   ((name
                                                                     @cell-shape)
                                                                    (id 34)))))))))
                                                            (return
                                                             (Arr
                                                              ((element
                                                                (AtomRef
                                                                 ((name t)
                                                                  (id 35))))
                                                               (shape
                                                                ((ShapeRef
                                                                  ((name
                                                                    @cell-shape)
                                                                   (id 34)))))))))))
                                                         (shape ())))))
                                                     ((binding
                                                       ((name arr) (id 37)))
                                                      (bound
                                                       (Arr
                                                        ((element
                                                          (AtomRef
                                                           ((name t) (id 35))))
                                                         (shape
                                                          ((Add
                                                            ((const 1)
                                                             (refs
                                                              ((((name d-1)
                                                                 (id 33))
                                                                1)))))
                                                           (ShapeRef
                                                            ((name @cell-shape)
                                                             (id 34)))))))))))
                                                   (body
                                                    (Map
                                                     ((args
                                                       (((binding
                                                          ((name f) (id 110)))
                                                         (value
                                                          (TypeApplication
                                                           ((tFunc
                                                             (IndexApplication
                                                              ((iFunc
                                                                (Primitive
                                                                 ((name
                                                                   (Func
                                                                    (Reduce
                                                                     (associative
                                                                      true)
                                                                     (character
                                                                      Reduce)))))))
                                                               (args
                                                                ((Dimension
                                                                  ((const 0)
                                                                   (refs
                                                                    ((((name d-1)
                                                                       (id 33))
                                                                      1)))))
                                                                 (Shape
                                                                  ((ShapeRef
                                                                    ((name
                                                                      @cell-shape)
                                                                     (id 34))))))))))
                                                            (args
                                                             ((Atom
                                                               (AtomRef
                                                                ((name t)
                                                                 (id 35))))))))))
                                                        ((binding
                                                          ((name reduce-f-arg)
                                                           (id 97)))
                                                         (value
                                                          (Ref
                                                           ((id
                                                             ((name op) (id 36)))))))
                                                        ((binding
                                                          ((name reduce-zero-arg)
                                                           (id 103)))
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding
                                                                ((name f)
                                                                 (id 102)))
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
                                                                        (id 16)))
                                                                        (bound
                                                                        Dim))
                                                                        ((binding
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 17)))
                                                                        (bound
                                                                        Shape))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name t)
                                                                        (id 18)))
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
                                                                        (id 19)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 18))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 16))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 17)))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 101)))
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
                                                                        (id 16))
                                                                        1)))))))
                                                                        (Shape
                                                                        ())
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 17)))))
                                                                        (Dimension
                                                                        ((const
                                                                        1)
                                                                        (refs ()))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 18))))))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 99)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 19)))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 100)))
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
                                                                        (id 101))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 99))))
                                                                        ((id
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 100))))))
                                                                        (type'
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 18))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 17))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 18))))
                                                                        (shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 17)))))))))))))))))))))))))))))
                                                                     (args
                                                                      ((Dimension
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 33))
                                                                        1)))))
                                                                       (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 34))))))))))
                                                                  (args
                                                                   ((Atom
                                                                     (AtomRef
                                                                      ((name t)
                                                                       (id 35))))))))))
                                                              ((binding
                                                                ((name arr)
                                                                 (id 98)))
                                                               (value
                                                                (Ref
                                                                 ((id
                                                                   ((name arr)
                                                                    (id 37)))))))))
                                                            (body
                                                             (TermApplication
                                                              ((func
                                                                (Ref
                                                                 ((id
                                                                   ((name f)
                                                                    (id 102))))))
                                                               (args
                                                                (((id
                                                                   ((name arr)
                                                                    (id 98))))))
                                                               (type'
                                                                ((element
                                                                  (AtomRef
                                                                   ((name t)
                                                                    (id 35))))
                                                                 (shape
                                                                  ((ShapeRef
                                                                    ((name
                                                                      @cell-shape)
                                                                     (id 34))))))))))
                                                            (frameShape ())
                                                            (type'
                                                             (Arr
                                                              ((element
                                                                (AtomRef
                                                                 ((name t)
                                                                  (id 35))))
                                                               (shape
                                                                ((ShapeRef
                                                                  ((name
                                                                    @cell-shape)
                                                                   (id 34))))))))))))
                                                        ((binding
                                                          ((name
                                                            reduce-array-arg)
                                                           (id 109)))
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding
                                                                ((name f)
                                                                 (id 108)))
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
                                                                        (id 20)))
                                                                        (bound
                                                                        Dim))
                                                                        ((binding
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 21)))
                                                                        (bound
                                                                        Shape))))
                                                                        (body
                                                                        (Scalar
                                                                        ((element
                                                                        (TypeLambda
                                                                        ((params
                                                                        (((binding
                                                                        ((name t)
                                                                        (id 22)))
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
                                                                        (id 23)))
                                                                        (bound
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 22))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        1)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 20))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 21)))))))))))
                                                                        (body
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 107)))
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
                                                                        (id 20))
                                                                        1)))))))
                                                                        (Shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 20))
                                                                        1)))))))
                                                                        (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 21)))))
                                                                        (Dimension
                                                                        ((const
                                                                        1)
                                                                        (refs ()))))))))
                                                                        (args
                                                                        ((Atom
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 22))))))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 105)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 23)))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 106)))
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
                                                                        (id 107))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 105))))
                                                                        ((id
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 106))))))
                                                                        (type'
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 22))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 20))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 21))))))))))
                                                                        (frameShape
                                                                        ())
                                                                        (type'
                                                                        (Arr
                                                                        ((element
                                                                        (AtomRef
                                                                        ((name t)
                                                                        (id 22))))
                                                                        (shape
                                                                        ((Add
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 20))
                                                                        1)))))
                                                                        (ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 21)))))))))))))))))))))))))))))
                                                                     (args
                                                                      ((Dimension
                                                                        ((const
                                                                        0)
                                                                        (refs
                                                                        ((((name
                                                                        d-1)
                                                                        (id 33))
                                                                        1)))))
                                                                       (Shape
                                                                        ((ShapeRef
                                                                        ((name
                                                                        @cell-shape)
                                                                        (id 34))))))))))
                                                                  (args
                                                                   ((Atom
                                                                     (AtomRef
                                                                      ((name t)
                                                                       (id 35))))))))))
                                                              ((binding
                                                                ((name arr)
                                                                 (id 104)))
                                                               (value
                                                                (Ref
                                                                 ((id
                                                                   ((name arr)
                                                                    (id 37)))))))))
                                                            (body
                                                             (TermApplication
                                                              ((func
                                                                (Ref
                                                                 ((id
                                                                   ((name f)
                                                                    (id 108))))))
                                                               (args
                                                                (((id
                                                                   ((name arr)
                                                                    (id 104))))))
                                                               (type'
                                                                ((element
                                                                  (AtomRef
                                                                   ((name t)
                                                                    (id 35))))
                                                                 (shape
                                                                  ((Add
                                                                    ((const 0)
                                                                     (refs
                                                                      ((((name
                                                                        d-1)
                                                                        (id 33))
                                                                        1)))))
                                                                   (ShapeRef
                                                                    ((name
                                                                      @cell-shape)
                                                                     (id 34))))))))))
                                                            (frameShape ())
                                                            (type'
                                                             (Arr
                                                              ((element
                                                                (AtomRef
                                                                 ((name t)
                                                                  (id 35))))
                                                               (shape
                                                                ((Add
                                                                  ((const 0)
                                                                   (refs
                                                                    ((((name d-1)
                                                                       (id 33))
                                                                      1)))))
                                                                 (ShapeRef
                                                                  ((name
                                                                    @cell-shape)
                                                                   (id 34))))))))))))))
                                                      (body
                                                       (TermApplication
                                                        ((func
                                                          (Ref
                                                           ((id
                                                             ((name f) (id 110))))))
                                                         (args
                                                          (((id
                                                             ((name reduce-f-arg)
                                                              (id 97))))
                                                           ((id
                                                             ((name
                                                               reduce-zero-arg)
                                                              (id 103))))
                                                           ((id
                                                             ((name
                                                               reduce-array-arg)
                                                              (id 109))))))
                                                         (type'
                                                          ((element
                                                            (AtomRef
                                                             ((name t) (id 35))))
                                                           (shape
                                                            ((ShapeRef
                                                              ((name @cell-shape)
                                                               (id 34))))))))))
                                                      (frameShape ())
                                                      (type'
                                                       (Arr
                                                        ((element
                                                          (AtomRef
                                                           ((name t) (id 35))))
                                                         (shape
                                                          ((ShapeRef
                                                            ((name @cell-shape)
                                                             (id 34)))))))))))))))))))))))))))))
                                 (args
                                  ((Dimension
                                    ((const 0) (refs ((((name d-1) (id 93)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal IntLiteral))))))))
                          ((binding ((name op) (id 95)))
                           (value (Primitive ((name (Func Add))))))
                          ((binding ((name arr) (id 96)))
                           (value (Ref ((id ((name row) (id 94)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 111))))))
                           (args
                            (((id ((name op) (id 95))))
                             ((id ((name arr) (id 96))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 113)))
            (value
             (IndexApplication
              ((iFunc (Ref ((id ((name sum-row) (id 92))))))
               (args ((Dimension ((const 9) (refs ())))))))))
           ((binding ((name row) (id 112)))
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
             (((binding ((name row) (id 114)))
               (value (Ref ((id ((name row) (id 112)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 113))))))
               (args (((id ((name row) (id 114))))))
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
       (((binding ((name sum-row) (id 116)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 117)))
            (value
             (Ref
              ((id ((name sum-row) (id 116)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name row) (id 173)))
            (value
             (ArrayPrimitive
              (Map
               (frameShape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
               (args ()) (iotaVar (((name iota) (id 172))))
               (body
                (Ref
                 ((id ((name iota) (id 172)))
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
             (((binding ((name row) (id 174)))
               (value
                (Ref
                 ((id ((name row) (id 173)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape
                     ((Add ((const 1000000) (refs ())))
                      (Add ((const 10) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 141)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name op) (id 161)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 175)))
                  (value
                   (Ref
                    ((id ((name row) (id 174)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 10) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 160)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-f-arg) (id 162)))
                     (value
                      (Ref
                       ((id ((name op) (id 161)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name reduce-zero-arg) (id 192)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 183)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 189)))
                           (value
                            (Ref
                             ((id ((name arr) (id 175)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 10) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 188)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding
                               ((name contiguous-subarray-array) (id 190)))
                              (value
                               (Ref
                                ((id ((name arr) (id 189)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding
                               ((name contiguous-subarray-index) (id 191)))
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
                                ((id ((name contiguous-subarray-array) (id 190)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id ((name contiguous-subarray-index) (id 191)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 1) (refs ()))))))))))
                              (originalShape ((Add ((const 10) (refs ())))))
                              (resultShape ()) (cellShape ())
                              (l ((const 1) (refs ())))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name reduce-array-arg) (id 179)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 166)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 176)))
                           (value
                            (Ref
                             ((id ((name arr) (id 175)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 10) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 171)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding
                               ((name contiguous-subarray-array) (id 177)))
                              (value
                               (Ref
                                ((id ((name arr) (id 176)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding
                               ((name contiguous-subarray-index) (id 178)))
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
                                ((id ((name contiguous-subarray-array) (id 177)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id ((name contiguous-subarray-index) (id 178)))
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
                    (Reduce
                     (arg
                      ((firstBinding ((name reduce-arg1) (id 180)))
                       (secondBinding ((name reduce-arg2) (id 181)))
                       (value
                        (Ref
                         ((id ((name reduce-array-arg) (id 179)))
                          (type'
                           ((element (Literal IntLiteral))
                            (shape ((Add ((const 9) (refs ()))))))))))))
                     (zero
                      ((Ref
                        ((id ((name reduce-zero-arg) (id 192)))
                         (type' ((element (Literal IntLiteral)) (shape ())))))))
                     (body
                      (AtomAsArray
                       ((element
                         (AtomicPrimitive
                          ((op Add)
                           (args
                            ((ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name reduce-arg1) (id 180)))
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))
                             (ArrayAsAtom
                              ((array
                                (Ref
                                 ((id ((name reduce-arg2) (id 181)))
                                  (type'
                                   ((element (Literal IntLiteral)) (shape ()))))))
                               (type' (Literal IntLiteral))))))
                           (type' (Literal IntLiteral)))))
                        (type' ((element (Literal IntLiteral)) (shape ()))))))
                     (d ((const 9) (refs ()))) (cellShape ()) (associative true)
                     (character Reduce)
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
    (let ((sum-row.193 (values)))
     (let ((sum-row.116 sum-row.193))
      (let
       ((f.194 sum-row.116)
        (row.199
         (let ()
          (#0
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota iota.196)
              (#0
               (#0
                (loop-block (frame-shape 10)
                 (map () (iota (iota.198 : iota.196))
                  (let ((iota.172 iota.198)) iota.172))
                 (body-matcher map-result.197) (map-result (map-result.197))
                 (consumer (values))))))
             (body-matcher map-result.195) (map-result (map-result.195))
             (consumer (values))))))))
       (let ((f.117 f.194) (row.173 row.199))
        (let ((row.200 row.173))
         (#0
          (#0
           (loop-block (frame-shape 1000000)
            (map ((row.202 row.200))
             (let ((row.174 row.202))
              (let ((f.203 (values)) (op.204 (values)) (arr.205 row.174))
               (let ((f.141 f.203) (op.161 op.204) (arr.175 arr.205))
                (let
                 ((f.206 (values)) (reduce-f-arg.207 op.161)
                  (reduce-zero-arg.213
                   (let ((f.208 (values)) (arr.209 arr.175))
                    (let ((f.183 f.208) (arr.189 arr.209))
                     (let
                      ((f.210 (values)) (contiguous-subarray-array.211 arr.189)
                       (contiguous-subarray-index.212 (frame 0)))
                      (let
                       ((f.188 f.210)
                        (contiguous-subarray-array.190
                         contiguous-subarray-array.211)
                        (contiguous-subarray-index.191
                         contiguous-subarray-index.212))
                       (contiguous-subarray contiguous-subarray-array.190
                        contiguous-subarray-index.191 (shape 10) (shape)))))))
                  (reduce-array-arg.219
                   (let ((f.214 (values)) (arr.215 arr.175))
                    (let ((f.166 f.214) (arr.176 arr.215))
                     (let
                      ((f.216 (values)) (contiguous-subarray-array.217 arr.176)
                       (contiguous-subarray-index.218 (frame 1)))
                      (let
                       ((f.171 f.216)
                        (contiguous-subarray-array.177
                         contiguous-subarray-array.217)
                        (contiguous-subarray-index.178
                         contiguous-subarray-index.218))
                       (contiguous-subarray contiguous-subarray-array.177
                        contiguous-subarray-index.178 (shape 10) (shape 9))))))))
                 (let
                  ((f.160 f.206) (reduce-f-arg.162 reduce-f-arg.207)
                   (reduce-zero-arg.192 reduce-zero-arg.213)
                   (reduce-array-arg.179 reduce-array-arg.219))
                  (let ((reduce-arg.221 reduce-array-arg.179))
                   (#1
                    (loop-block (frame-shape 9)
                     (map ((reduce-arg.222 reduce-arg.221))
                      (values reduce-arg.222))
                     (body-matcher (reduce-arg.220)) (map-result ())
                     (consumer
                      (reduce-zero reduce-zero-arg.192
                       (reduce-arg1.180 reduce-arg2.181 reduce-arg.220)
                       (+ reduce-arg1.180 reduce-arg2.181))))))))))))
            (body-matcher map-result.201) (map-result (map-result.201))
            (consumer (values))))))))))
    Result of stage Fuse and Simplify:
    (let
     ((contiguous-subarray-index.212 (frame 0))
      (contiguous-subarray-index.218 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 1000000)
        (map () (iota iota.196)
         (let
          ((map-result.254
            (#0
             (loop-block (frame-shape 10)
              (map () (iota (iota.198 : iota.196)) iota.198)
              (body-matcher map-result.197) (map-result (map-result.197))
              (consumer (values))))))
          (let
           ((reduce-arg.249
             (contiguous-subarray (#0 map-result.254)
              contiguous-subarray-index.218 (shape 10) (shape 9))))
           (#1
            (loop-block (frame-shape 9)
             (map ((reduce-arg.222 reduce-arg.249)) reduce-arg.222)
             (body-matcher reduce-arg.220) (map-result ())
             (consumer
              (reduce-zero
               (contiguous-subarray (#0 map-result.254)
                contiguous-subarray-index.212 (shape 10) (shape))
               (reduce-arg1.180 reduce-arg2.181 reduce-arg.220)
               (+ reduce-arg1.180 reduce-arg2.181))))))))
        (body-matcher map-result.201) (map-result (map-result.201))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((contiguous-subarray-index.212 (frame 0))
      (contiguous-subarray-index.218 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.196)
        (body-matcher map-result.201) (map-result (map-result.201))
        (let
         ((map-result.254
           (#0
            (loop-block (frame-shape 10)
             (map () (iota (iota.198 : iota.196)) iota.198)
             (body-matcher map-result.197) (map-result (map-result.197))
             (consumer (values))))))
         (let
          ((reduce-arg.249
            (contiguous-subarray (#0 map-result.254)
             contiguous-subarray-index.218 (shape 10) (shape 9))))
          (#1
           (loop-block (frame-shape 9)
            (map ((reduce-arg.222 reduce-arg.249)) reduce-arg.222)
            (body-matcher reduce-arg.220) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.254)
               contiguous-subarray-index.212 (shape 10) (shape))
              (reduce-arg1.180 reduce-arg2.181 reduce-arg.220)
              (+ reduce-arg1.180 reduce-arg2.181)))))))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.280 (Tuple ()) device)
      (map-mem.276
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.274
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.273
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.271 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.269 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.212
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.269) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.269)))
       (contiguous-subarray-index.218
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.271) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.271))))
      (#0
       (begin
        (kernel (blocks 320) (threads 32)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.275 map-mem.274))
            (iota iota.196)
            (do-expr
             (let
              ((map-result.254
                (#0
                 (loop (frame-shape 10)
                  (map ((map-mem.277 map-mem.276)) (iota (iota.198 : iota.196))
                   (let ((expr-result.278 iota.198))
                    (begin (putmem expr-result.278 (#0 map-mem.277))
                     expr-result.278)))
                  (body-matcher map-result.197) (map-result (map-result.197))
                  (map-result-mem-interim map-mem.276)
                  (map-result-mem-final map-mem.276) (consumer (values))))))
              (let
               ((reduce-arg.249
                 (index
                  (#0
                   (let ((expr-result.279 map-result.254))
                    (values (#0 expr-result.279))))
                  contiguous-subarray-index.218 (shape 10) (shape 9))))
               (#1
                (let
                 ((expr-result.283
                   (loop (frame-shape 9)
                    (map
                     ((reduce-arg.222 reduce-arg.249) (map-mem.281 map-mem.280))
                     reduce-arg.222)
                    (body-matcher reduce-arg.220) (map-result ())
                    (map-result-mem-interim map-mem.280)
                    (map-result-mem-final map-mem.280)
                    (consumer
                     (reduce-zero
                      (index
                       (#0
                        (let ((expr-result.282 map-result.254))
                         (values (#0 expr-result.282))))
                       contiguous-subarray-index.212 (shape 10) (shape))
                      (reduce-arg1.180 reduce-arg2.181 reduce-arg.220)
                      (+ reduce-arg1.180 reduce-arg2.181))))))
                 (begin (putmem (#1 expr-result.283) (#0 map-mem.275))
                  expr-result.283))))))))
          (map-result-mem-interim map-mem.274)
          (map-result-mem-final (values map-mem-result.0.273))))
        (getmem (values map-mem-result.0.273))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.280 (Tuple ()) device)
      (map-mem.276
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.274
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.273
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.271 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.269 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.212
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.269) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.269)))
       (contiguous-subarray-index.218
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.271) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.271))))
      (#0
       (begin
        (kernel captures
         ((expr-captures
           ((((name contiguous-subarray-index) (id 212))
             (Array ((element (Literal IntLiteral)) (shape (shape 1)))))
            (((name contiguous-subarray-index) (id 218))
             (Array ((element (Literal IntLiteral)) (shape (shape 1)))))))
          (mem-captures
           ((((name map-mem) (id 274))
             (Tuple
              ((Array ((element (Literal IntLiteral)) (shape (shape 1000000)))))))
            (((name map-mem) (id 276))
             (Tuple
              ((Array ((element (Literal IntLiteral)) (shape (shape 10)))))))
            (((name map-mem) (id 280)) (Tuple ()))))
          (index-captures ()))
         (blocks 320) (threads 32)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.275 map-mem.274))
            (iota iota.196)
            (do-expr
             (let
              ((map-result.254
                (#0
                 (loop (frame-shape 10)
                  (map ((map-mem.277 map-mem.276)) (iota (iota.198 : iota.196))
                   (let ((expr-result.278 iota.198))
                    (begin (putmem expr-result.278 (#0 map-mem.277))
                     expr-result.278)))
                  (body-matcher map-result.197) (map-result (map-result.197))
                  (map-result-mem-interim map-mem.276)
                  (map-result-mem-final map-mem.276) (consumer (values))))))
              (let
               ((reduce-arg.249
                 (index
                  (#0
                   (let ((expr-result.279 map-result.254))
                    (values (#0 expr-result.279))))
                  contiguous-subarray-index.218 (shape 10) (shape 9))))
               (#1
                (let
                 ((expr-result.283
                   (loop (frame-shape 9)
                    (map
                     ((reduce-arg.222 reduce-arg.249) (map-mem.281 map-mem.280))
                     reduce-arg.222)
                    (body-matcher reduce-arg.220) (map-result ())
                    (map-result-mem-interim map-mem.280)
                    (map-result-mem-final map-mem.280)
                    (consumer
                     (reduce-zero
                      (index
                       (#0
                        (let ((expr-result.282 map-result.254))
                         (values (#0 expr-result.282))))
                       contiguous-subarray-index.212 (shape 10) (shape))
                      (reduce-arg1.180 reduce-arg2.181 reduce-arg.220)
                      (+ reduce-arg1.180 reduce-arg2.181))))))
                 (begin (putmem (#1 expr-result.283) (#0 map-mem.275))
                  expr-result.283))))))))
          (map-result-mem-interim map-mem.274)
          (map-result-mem-final (values map-mem-result.0.273))))
        (getmem (values map-mem-result.0.273)))))) |}]
;;
