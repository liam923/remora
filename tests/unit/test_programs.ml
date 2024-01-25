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
       (((binding ((name f) (id 101))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 99)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 100)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 101))))))
         (args (((id ((name +arg1) (id 99)))) ((id ((name +arg2) (id 100))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 102)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 103)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 104)))
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
                  ((id ((name +arg1) (id 103)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))
              (ArrayAsAtom
               ((array
                 (Ref
                  ((id ((name +arg2) (id 104)))
                   (type' ((element (Literal IntLiteral)) (shape ()))))))
                (type' (Literal IntLiteral))))))
            (type' (Literal IntLiteral)))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((f.105 (values)) (+arg1.106 1) (+arg2.107 2))
     (let ((f.102 f.105) (+arg1.103 +arg1.106) (+arg2.104 +arg2.107))
      (+ +arg1.103 +arg2.104)))
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
     ((binding ((name add) (id 99)))
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding ((name x) (id 100)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding ((name y) (id 101)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args
                ((Ref ((id ((name x) (id 100)))))
                 (Ref ((id ((name y) (id 101))))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name add) (id 99))))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name add) (id 99)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name x) (id 100)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name y) (id 101)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 104)))
                     (value (Primitive ((name (Func Add))))))
                    ((binding ((name +arg1) (id 102)))
                     (value (Ref ((id ((name x) (id 100)))))))
                    ((binding ((name +arg2) (id 103)))
                     (value (Ref ((id ((name y) (id 101)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 104))))))
                     (args
                      (((id ((name +arg1) (id 102))))
                       ((id ((name +arg2) (id 103))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 107)))
            (value (Ref ((id ((name add) (id 99)))))))
           ((binding ((name x) (id 105)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding ((name y) (id 106)))
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 107))))))
            (args (((id ((name x) (id 105)))) ((id ((name y) (id 106))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name add) (id 109)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 110)))
            (value
             (Ref
              ((id ((name add) (id 109)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 117)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name y) (id 119)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 116)))
               (value
                (AtomAsArray
                 ((element (Values ((elements ()) (type' ()))))
                  (type' ((element (Tuple ())) (shape ())))))))
              ((binding ((name +arg1) (id 118)))
               (value
                (Ref
                 ((id ((name x) (id 117)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name +arg2) (id 120)))
               (value
                (Ref
                 ((id ((name y) (id 119)))
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
                        ((id ((name +arg1) (id 118)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 120)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))))
                  (type' (Literal IntLiteral)))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((add.121 (values)))
     (let ((add.109 add.121))
      (let ((f.122 add.109) (x.123 5) (y.124 10))
       (let ((f.110 f.122) (x.117 x.123) (y.119 y.124))
        (let ((f.125 (values)) (+arg1.126 x.117) (+arg2.127 y.119))
         (let ((f.116 f.125) (+arg1.118 +arg1.126) (+arg2.120 +arg2.127))
          (+ +arg1.118 +arg2.120)))))))
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
     ((binding ((name id) (id 99)))
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @t) (id 100))) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name e) (id 101)))
                     (bound (ArrayRef ((name @t) (id 100)))))))
                  (body (Ref ((id ((name e) (id 101))))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id ((name id) (id 99))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name id) (id 99)))
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding ((name @t) (id 100))) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name e) (id 101)))
                        (bound (ArrayRef ((name @t) (id 100)))))))
                     (body (Ref ((id ((name e) (id 101)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 103)))
            (value
             (TypeApplication
              ((tFunc (Ref ((id ((name id) (id 99))))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding ((name e) (id 102)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 103))))))
            (args (((id ((name e) (id 102))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 105)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 106)))
            (value
             (Ref
              ((id ((name id) (id 105)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name e) (id 108)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name e) (id 108)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((id.109 (values)))
     (let ((id.105 id.109))
      (let ((f.110 id.105) (e.111 5)) (let ((f.106 f.110) (e.108 e.111)) e.108))))
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
       (((binding ((name f) (id 101))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 99)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding ((name +arg2) (id 100)))
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
          (((binding ((name +arg1) (id 102)))
            (value (Ref ((id ((name +arg1) (id 99)))))))
           ((binding ((name +arg2) (id 103)))
            (value (Ref ((id ((name +arg2) (id 100)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg2) (id 104)))
               (value (Ref ((id ((name +arg2) (id 103)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 101))))))
               (args
                (((id ((name +arg1) (id 102)))) ((id ((name +arg2) (id 104))))))
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
       (((binding ((name f) (id 105)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 106)))
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
        ((binding ((name +arg2) (id 108)))
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
          (((binding ((name +arg1) (id 107)))
            (value
             (Ref
              ((id ((name +arg1) (id 106)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding ((name +arg2) (id 109)))
            (value
             (Ref
              ((id ((name +arg2) (id 108)))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 110)))
               (value
                (Ref
                 ((id ((name +arg2) (id 109)))
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
                        ((id ((name +arg1) (id 107)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 110)))
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
     ((f.111 (values)) (+arg1.112 (frame 1 2))
      (+arg2.113 (frame (frame 3 4 5) (frame 6 7 8))))
     (let ((f.105 f.111) (+arg1.106 +arg1.112) (+arg2.108 +arg2.113))
      (let ((+arg1.114 +arg1.106) (+arg2.115 +arg2.108))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.117 +arg1.114) (+arg2.118 +arg2.115))
           (let ((+arg1.107 +arg1.117) (+arg2.109 +arg2.118))
            (let ((+arg2.119 +arg2.109))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg2.121 +arg2.119))
                 (let ((+arg2.110 +arg2.121)) (+ +arg1.107 +arg2.110)))
                (body-matcher map-result.120) (map-result (map-result.120))
                (consumer (values))))))))
          (body-matcher map-result.116) (map-result (map-result.116))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((+arg1.114 (frame 1 2)) (+arg2.115 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.117 +arg1.114) (+arg2.118 +arg2.115))
         (let ((+arg2.119 +arg2.118))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.121 +arg2.119)) (+ +arg1.117 +arg2.121))
             (body-matcher map-result.120) (map-result (map-result.120))
             (consumer (values)))))))
        (body-matcher map-result.116) (map-result (map-result.116))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((+arg1.114 (frame 1 2)) (+arg2.115 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.117 +arg1.114) (+arg2.118 +arg2.115))
         (let ((+arg2.119 +arg2.118))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.121 +arg2.119)) (+ +arg1.117 +arg2.121))
             (body-matcher map-result.120) (map-result (map-result.120))
             (consumer (values)))))))
        (body-matcher map-result.116) (map-result (map-result.116))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.143 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.136
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.133 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.114
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.133) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.133) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.133)))
       (+arg2.115
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.136) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.136) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.136) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.136) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.136) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.136) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.136))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.117 +arg1.114) (+arg2.118 +arg2.115) (map-mem.144 map-mem.143))
          (let ((+arg2.119 +arg2.118))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.121 +arg2.119) (map-mem.145 map-mem.144))
               (let ((expr-result.146 (+ +arg1.117 +arg2.121)))
                (begin (putmem expr-result.146 map-mem.145) expr-result.146)))
              (body-matcher map-result.120) (map-result (map-result.120))
              (map-result-mem-interim (values map-mem.144))
              (map-result-mem-final (values map-mem.144)) (consumer (values)))))))
         (body-matcher map-result.116) (map-result (map-result.116))
         (map-result-mem-interim (values map-mem.143))
         (map-result-mem-final (values map-mem.143)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.143 (Array ((element (Literal IntLiteral)) (shape (shape 2 3))))
       host)
      (frame-array.136
       (Array ((element (Literal IntLiteral)) (shape (shape 2 3)))) host)
      (frame-array.133 (Array ((element (Literal IntLiteral)) (shape (shape 2))))
       host))
     (let
      ((+arg1.114
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.133) (offset 0)
            (type' (Atom (Literal IntLiteral)))))
          (putmem 2
           (index (mem frame-array.133) (offset 1)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.133)))
       (+arg2.115
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (index
             (mem
              (index (mem frame-array.136) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 4
            (index
             (mem
              (index (mem frame-array.136) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 5
            (index
             (mem
              (index (mem frame-array.136) (offset 0)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral))))))
          (begin-do
           (putmem 6
            (index
             (mem
              (index (mem frame-array.136) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 0) (type' (Atom (Literal IntLiteral)))))
           (putmem 7
            (index
             (mem
              (index (mem frame-array.136) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 1) (type' (Atom (Literal IntLiteral)))))
           (putmem 8
            (index
             (mem
              (index (mem frame-array.136) (offset 1)
               (type' (Array ((element (Literal IntLiteral)) (shape (shape 3)))))))
             (offset 2) (type' (Atom (Literal IntLiteral)))))))
         (getmem frame-array.136))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.117 +arg1.114) (+arg2.118 +arg2.115) (map-mem.144 map-mem.143))
          (let ((+arg2.119 +arg2.118))
           (#0
            (#0
             (loop (frame-shape 3)
              (map ((+arg2.121 +arg2.119) (map-mem.145 map-mem.144))
               (let ((expr-result.146 (+ +arg1.117 +arg2.121)))
                (begin (putmem expr-result.146 map-mem.145) expr-result.146)))
              (body-matcher map-result.120) (map-result (map-result.120))
              (map-result-mem-interim (values map-mem.144))
              (map-result-mem-final (values map-mem.144)) (consumer (values)))))))
         (body-matcher map-result.116) (map-result (map-result.116))
         (map-result-mem-interim (values map-mem.143))
         (map-result-mem-final (values map-mem.143)) (consumer (values))))))) |}]
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
     ((binding ((name words) (id 99)))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 100)) 1)))))))))))))))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 100)) 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings ((((name len) (id 101)) Dim)))
         (valueBinding ((name word) (id 102)))
         (box (Ref ((id ((name words) (id 99))))))
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
                         ((const 0) (refs ((((name len) (id 101)) 1)))))
                        (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id ((name word) (id 102)))))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name words) (id 99)))
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
                       ((Add ((const 0) (refs ((((name len) (id 100)) 1)))))))))))))))
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
                       ((Add ((const 0) (refs ((((name len) (id 100)) 1)))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name box) (id 108)))
            (value (Ref ((id ((name words) (id 99)))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 101)))
               (indexValue
                (FromBox (box (Ref ((id ((name box) (id 108)))))) (i 0)))
               (sort Dim))))
            (body
             (Map
              ((args
                (((binding ((name word) (id 102)))
                  (value (BoxValue ((box (Ref ((id ((name box) (id 108))))))))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 107)))
                     (value (Primitive ((name (Func Equal))))))
                    ((binding ((name =arg1) (id 103)))
                     (value (Scalar ((element (Literal (IntLiteral 3)))))))
                    ((binding ((name =arg2) (id 106)))
                     (value
                      (Map
                       ((args
                         (((binding ((name f) (id 105)))
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
                                     (refs ((((name len) (id 101)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal CharacterLiteral))))))))
                          ((binding ((name arr) (id 104)))
                           (value (Ref ((id ((name word) (id 102)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 105))))))
                           (args (((id ((name arr) (id 104))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 107))))))
                     (args
                      (((id ((name =arg1) (id 103))))
                       ((id ((name =arg2) (id 106))))))
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
       (((binding ((name words) (id 115)))
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
                      ((Add ((const 0) (refs ((((name len) (id 100)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 100))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 100)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 100))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 100)) 1))))))))))))
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
                      ((Add ((const 0) (refs ((((name len) (id 100)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 100))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 100)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 100))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 100)) 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 100))) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 100)) 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name box) (id 116)))
            (value
             (Ref
              ((id ((name words) (id 115)))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 100))) (bound Dim))))
                    (body
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 100)) 1))))))))))))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 101)))
               (indexValue
                (FromBox
                 (box
                  (Ref
                   ((id ((name box) (id 116)))
                    (type'
                     ((element
                       (Sigma
                        ((parameters
                          (((binding ((name len) (id 100))) (bound Dim))))
                         (body
                          ((element (Literal CharacterLiteral))
                           (shape
                            ((Add ((const 0) (refs ((((name len) (id 100)) 1))))))))))))
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
                   (((binding ((name f) (id 109)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name =arg1) (id 110)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 3)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name =arg2) (id 114)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 112)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ReifyIndex
                          ((index
                            (Dimension
                             ((const 0) (refs ((((name len) (id 101)) 1))))))
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
                              ((id ((name =arg1) (id 110)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name =arg2) (id 114)))
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
     ((words.117 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (let ((words.115 words.117))
      (let ((box.118 words.115))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((box.120 box.118))
           (let ((box.116 box.120))
            (index-let ((len.101 box-index-0 box.116))
             (let ()
              (let ()
               (let
                ((f.121 (values)) (=arg1.122 3)
                 (=arg2.124
                  (let ((f.123 (values)))
                   (let ((f.112 f.123)) (reify-index len.101)))))
                (let ((f.109 f.121) (=arg1.110 =arg1.122) (=arg2.114 =arg2.124))
                 (= =arg1.110 =arg2.114))))))))
          (body-matcher map-result.119) (map-result (map-result.119))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((box.118 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.120 box.118))
         (index-let ((len.101 box-index-0 box.120)) (= 3 (reify-index len.101))))
        (body-matcher map-result.119) (map-result (map-result.119))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((box.118 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.120 box.118))
         (index-let ((len.101 box-index-0 box.120)) (= 3 (reify-index len.101))))
        (body-matcher map-result.119) (map-result (map-result.119))
        (consumer (values))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.136 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.132
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.127
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.126
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 100))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.100))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.118
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.127) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.127) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.127) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.127)))
           (index (mem frame-array.126) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 100))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.100)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.132) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.132) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.132)))
           (index (mem frame-array.126) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 100))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.100))))))))))))
         (getmem frame-array.126))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.120 box.118) (map-mem.137 map-mem.136))
          (index-let ((len.101 box-index-0 box.120))
           (let ((expr-result.138 (= 3 (reify-dimension-index len.101))))
            (begin (putmem expr-result.138 map-mem.137) expr-result.138))))
         (body-matcher map-result.119) (map-result (map-result.119))
         (map-result-mem-interim (values map-mem.136))
         (map-result-mem-final (values map-mem.136)) (consumer (values)))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.136 (Array ((element (Literal BooleanLiteral)) (shape (shape 2))))
       host)
      (frame-array.132
       (Array ((element (Literal CharacterLiteral)) (shape (shape 2)))) host)
      (frame-array.127
       (Array ((element (Literal CharacterLiteral)) (shape (shape 3)))) host)
      (frame-array.126
       (Array
        ((element
          (Sigma
           ((parameters (((binding ((name len) (id 100))) (bound Dim))))
            (body
             (Array
              ((element (Literal CharacterLiteral)) (shape (shape len.100))))))))
         (shape (shape 2))))
       host))
     (let
      ((box.118
        (begin
         (begin-do
          (putmem
           (box (((expr (reify-dimension-index 3)) (index 3)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.127) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'e'
               (index (mem frame-array.127) (offset 1)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'y'
               (index (mem frame-array.127) (offset 2)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.127)))
           (index (mem frame-array.126) (offset 0)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 100))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.100)))))))))))
          (putmem
           (box (((expr (reify-dimension-index 2)) (index 2)))
            (begin
             (begin-do
              (putmem 'h'
               (index (mem frame-array.132) (offset 0)
                (type' (Atom (Literal CharacterLiteral)))))
              (putmem 'i'
               (index (mem frame-array.132) (offset 1)
                (type' (Atom (Literal CharacterLiteral))))))
             (getmem frame-array.132)))
           (index (mem frame-array.126) (offset 1)
            (type'
             (Atom
              (Sigma
               ((parameters (((binding ((name len) (id 100))) (bound Dim))))
                (body
                 (Array
                  ((element (Literal CharacterLiteral)) (shape (shape len.100))))))))))))
         (getmem frame-array.126))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map ((box.120 box.118) (map-mem.137 map-mem.136))
          (index-let ((len.101 box-index-0 box.120))
           (let ((expr-result.138 (= 3 (reify-dimension-index len.101))))
            (begin (putmem expr-result.138 map-mem.137) expr-result.138))))
         (body-matcher map-result.119) (map-result (map-result.119))
         (map-result-mem-interim (values map-mem.136))
         (map-result-mem-final (values map-mem.136)) (consumer (values))))))) |}]
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
     ((binding ((name sum-row) (id 99)))
      (value
       (Scalar
        ((element
          (IndexLambda
           ((params (((binding ((name d-1) (id 100))) (bound Dim))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name row) (id 101)))
                     (bound
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape
                         ((Add ((const 1) (refs ((((name d-1) (id 100)) 1)))))))))))))
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
                              ((const 0) (refs ((((name d-1) (id 100)) 1)))))
                             (Shape ()) (Shape ()))))))
                        (args ((Atom (Literal IntLiteral)))))))
                     (args
                      ((Primitive ((name (Func Add))))
                       (Ref ((id ((name row) (id 101))))))))))))))))))))))
      (body
       (TermApplication
        ((func
          (IndexApplication
           ((iFunc (Ref ((id ((name sum-row) (id 99))))))
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
       (((binding ((name sum-row) (id 99)))
         (value
          (Scalar
           ((element
             (IndexLambda
              ((params (((binding ((name d-1) (id 100))) (bound Dim))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name row) (id 101)))
                        (bound
                         (Arr
                          ((element (Literal IntLiteral))
                           (shape
                            ((Add ((const 1) (refs ((((name d-1) (id 100)) 1)))))))))))))
                     (body
                      (Map
                       ((args
                         (((binding ((name f) (id 134)))
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
                                                          ((name f) (id 132)))
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
                                                                        (id 119)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 45)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg0)
                                                                        (id 117)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name a)
                                                                        (id 49)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg1)
                                                                        (id 118)))
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
                                                                        (id 120)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arg0)
                                                                        (id 117)))))))
                                                                        ((binding
                                                                        ((name
                                                                        arg1)
                                                                        (id 121)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arg1)
                                                                        (id 118)))))))))
                                                                        (body
                                                                        (TermApplication
                                                                        ((func
                                                                        (Ref
                                                                        ((id
                                                                        ((name f)
                                                                        (id 119))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        arg0)
                                                                        (id 120))))
                                                                        ((id
                                                                        ((name
                                                                        arg1)
                                                                        (id 121))))))
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
                                                                        (id 131)))
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
                                                                        (id 122)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        up-ranked-f)
                                                                        (id 48)))))))
                                                                        ((binding
                                                                        ((name
                                                                        reduce-zero-arg)
                                                                        (id 129)))
                                                                        (value
                                                                        (Map
                                                                        ((args
                                                                        (((binding
                                                                        ((name f)
                                                                        (id 128)))
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
                                                                        (id 126)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        make)
                                                                        (id 10)))))))
                                                                        ((binding
                                                                        ((name
                                                                        foo)
                                                                        (id 124)))
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
                                                                        (id 125)))
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
                                                                        (id 127)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        foo)
                                                                        (id 124)))))))))
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
                                                                        foo)
                                                                        (id 127))))
                                                                        ((id
                                                                        ((name v)
                                                                        (id 125))))))
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
                                                                        (id 123)))
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
                                                                        (id 128))))))
                                                                        (args
                                                                        (((id
                                                                        ((name v)
                                                                        (id 123))))))
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
                                                                        (id 130)))
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
                                                                        (id 131))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        reduce-f-arg)
                                                                        (id 122))))
                                                                        ((id
                                                                        ((name
                                                                        reduce-zero-arg)
                                                                        (id 129))))
                                                                        ((id
                                                                        ((name
                                                                        reduce-array-arg)
                                                                        (id 130))))))
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
                                                          ((name f) (id 104)))
                                                         (value
                                                          (Ref
                                                           ((id
                                                             ((name op) (id 55)))))))
                                                        ((binding
                                                          ((name init) (id 110)))
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding
                                                                ((name f)
                                                                 (id 109)))
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
                                                                        (id 108)))
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
                                                                        (id 106)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 27)))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 107)))
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
                                                                        (id 108))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 106))))
                                                                        ((id
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 107))))))
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
                                                                 (id 105)))
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
                                                                    (id 109))))))
                                                               (args
                                                                (((id
                                                                   ((name arr)
                                                                    (id 105))))))
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
                                                          ((name arr) (id 116)))
                                                         (value
                                                          (Map
                                                           ((args
                                                             (((binding
                                                                ((name f)
                                                                 (id 115)))
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
                                                                        (id 114)))
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
                                                                        (id 112)))
                                                                        (value
                                                                        (Ref
                                                                        ((id
                                                                        ((name
                                                                        arr)
                                                                        (id 31)))))))
                                                                        ((binding
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 113)))
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
                                                                        (id 114))))))
                                                                        (args
                                                                        (((id
                                                                        ((name
                                                                        contiguous-subarray-array)
                                                                        (id 112))))
                                                                        ((id
                                                                        ((name
                                                                        contiguous-subarray-index)
                                                                        (id 113))))))
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
                                                                 (id 111)))
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
                                                                    (id 115))))))
                                                               (args
                                                                (((id
                                                                   ((name arr)
                                                                    (id 111))))))
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
                                                              (id 133)))
                                                            (value
                                                             (Ref
                                                              ((id
                                                                ((name init)
                                                                 (id 110)))))))))
                                                         (body
                                                          (TermApplication
                                                           ((func
                                                             (Ref
                                                              ((id
                                                                ((name f)
                                                                 (id 132))))))
                                                            (args
                                                             (((id
                                                                ((name f)
                                                                 (id 104))))
                                                              ((id
                                                                ((name init)
                                                                 (id 133))))
                                                              ((id
                                                                ((name arr)
                                                                 (id 116))))))
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
                                     (refs ((((name d-1) (id 100)) 1)))))
                                   (Shape ()) (Shape ()))))))
                              (args ((Atom (Literal IntLiteral))))))))
                          ((binding ((name op) (id 102)))
                           (value (Primitive ((name (Func Add))))))
                          ((binding ((name arr) (id 103)))
                           (value (Ref ((id ((name row) (id 101)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 134))))))
                           (args
                            (((id ((name op) (id 102))))
                             ((id ((name arr) (id 103))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 136)))
            (value
             (IndexApplication
              ((iFunc (Ref ((id ((name sum-row) (id 99))))))
               (args ((Dimension ((const 9) (refs ())))))))))
           ((binding ((name row) (id 135)))
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
             (((binding ((name row) (id 137)))
               (value (Ref ((id ((name row) (id 135)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 136))))))
               (args (((id ((name row) (id 137))))))
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
       (((binding ((name sum-row) (id 139)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 140)))
            (value
             (Ref
              ((id ((name sum-row) (id 139)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name row) (id 287)))
            (value
             (ArrayPrimitive
              (Map
               (frameShape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
               (args ()) (iotaVar (((name iota) (id 286))))
               (body
                (Ref
                 ((id ((name iota) (id 286)))
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
             (((binding ((name row) (id 288)))
               (value
                (Ref
                 ((id ((name row) (id 287)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape
                     ((Add ((const 1000000) (refs ())))
                      (Add ((const 10) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 190)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name op) (id 276)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name arr) (id 289)))
                  (value
                   (Ref
                    ((id ((name row) (id 288)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 10) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 236)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name f) (id 277)))
                     (value
                      (Ref
                       ((id ((name op) (id 276)))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name init) (id 326)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 317)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 323)))
                           (value
                            (Ref
                             ((id ((name arr) (id 289)))
                              (type'
                               ((element (Literal IntLiteral))
                                (shape ((Add ((const 10) (refs ())))))))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 322)))
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
                              (resultShape ()) (cellShape ())
                              (l ((const 1) (refs ())))
                              (type' ((element (Literal IntLiteral)) (shape ()))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name arr) (id 293)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 280)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))
                          ((binding ((name arr) (id 290)))
                           (value
                            (Ref
                             ((id ((name arr) (id 289)))
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
                             ((binding
                               ((name contiguous-subarray-array) (id 291)))
                              (value
                               (Ref
                                ((id ((name arr) (id 290)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ())))))))))))
                             ((binding
                               ((name contiguous-subarray-index) (id 292)))
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
                                ((id ((name contiguous-subarray-array) (id 291)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 10) (refs ()))))))))))
                              (indexArg
                               (Ref
                                ((id ((name contiguous-subarray-index) (id 292)))
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
                      (((binding ((name init) (id 327)))
                        (value
                         (Ref
                          ((id ((name init) (id 326)))
                           (type' ((element (Literal IntLiteral)) (shape ())))))))))
                     (body
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name up-ranked-f) (id 264)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ArrayPrimitive
                          (Map (frameShape ())
                           (args
                            (((binding ((name f) (id 262)))
                              (value
                               (AtomAsArray
                                ((element (Values ((elements ()) (type' ()))))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding ((name reduce-f-arg) (id 265)))
                              (value
                               (Ref
                                ((id ((name up-ranked-f) (id 264)))
                                 (type' ((element (Tuple ())) (shape ())))))))
                             ((binding ((name reduce-zero-arg) (id 330)))
                              (value
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding ((name f) (id 302)))
                                    (value
                                     (AtomAsArray
                                      ((element
                                        (Values ((elements ()) (type' ()))))
                                       (type' ((element (Tuple ())) (shape ())))))))
                                   ((binding ((name v) (id 328)))
                                    (value
                                     (Ref
                                      ((id ((name init) (id 327)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name make) (id 312)))
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
                                        (((binding ((name f) (id 313)))
                                          (value
                                           (Ref
                                            ((id ((name make) (id 312)))
                                             (type'
                                              ((element (Tuple ())) (shape ())))))))
                                         ((binding ((name v) (id 329)))
                                          (value
                                           (Ref
                                            ((id ((name v) (id 328)))
                                             (type'
                                              ((element (Literal IntLiteral))
                                               (shape ())))))))))
                                       (body
                                        (ArrayPrimitive
                                         (Map (frameShape ()) (args ())
                                          (body
                                           (Ref
                                            ((id ((name v) (id 329)))
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
                             ((binding ((name reduce-array-arg) (id 294)))
                              (value
                               (Ref
                                ((id ((name arr) (id 293)))
                                 (type'
                                  ((element (Literal IntLiteral))
                                   (shape ((Add ((const 9) (refs ())))))))))))))
                           (body
                            (ArrayPrimitive
                             (Reduce
                              (arg
                               ((firstBinding ((name reduce-arg1) (id 295)))
                                (secondBinding ((name reduce-arg2) (id 298)))
                                (value
                                 (Ref
                                  ((id ((name reduce-array-arg) (id 294)))
                                   (type'
                                    ((element (Literal IntLiteral))
                                     (shape ((Add ((const 9) (refs ()))))))))))))
                              (zero
                               (Ref
                                ((id ((name reduce-zero-arg) (id 330)))
                                 (type'
                                  ((element (Literal IntLiteral)) (shape ()))))))
                              (body
                               (ArrayPrimitive
                                (Map (frameShape ())
                                 (args
                                  (((binding ((name f) (id 278)))
                                    (value
                                     (Ref
                                      ((id ((name f) (id 277)))
                                       (type' ((element (Tuple ())) (shape ())))))))
                                   ((binding ((name arg0) (id 296)))
                                    (value
                                     (Ref
                                      ((id ((name reduce-arg1) (id 295)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))
                                   ((binding ((name arg1) (id 299)))
                                    (value
                                     (Ref
                                      ((id ((name reduce-arg2) (id 298)))
                                       (type'
                                        ((element (Literal IntLiteral))
                                         (shape ())))))))))
                                 (body
                                  (ArrayPrimitive
                                   (Map (frameShape ())
                                    (args
                                     (((binding ((name arg0) (id 297)))
                                       (value
                                        (Ref
                                         ((id ((name arg0) (id 296)))
                                          (type'
                                           ((element (Literal IntLiteral))
                                            (shape ())))))))
                                      ((binding ((name arg1) (id 300)))
                                       (value
                                        (Ref
                                         ((id ((name arg1) (id 299)))
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
                                                ((id ((name arg0) (id 297)))
                                                 (type'
                                                  ((element (Literal IntLiteral))
                                                   (shape ()))))))
                                              (type' (Literal IntLiteral))))
                                            (ArrayAsAtom
                                             ((array
                                               (Ref
                                                ((id ((name arg1) (id 300)))
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
    (let ((sum-row.331 (values)))
     (let ((sum-row.139 sum-row.331))
      (let
       ((f.332 sum-row.139)
        (row.337
         (let ()
          (#0
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota iota.334)
              (#0
               (#0
                (loop-block (frame-shape 10)
                 (map () (iota (iota.336 : iota.334))
                  (let ((iota.286 iota.336)) iota.286))
                 (body-matcher map-result.335) (map-result (map-result.335))
                 (consumer (values))))))
             (body-matcher map-result.333) (map-result (map-result.333))
             (consumer (values))))))))
       (let ((f.140 f.332) (row.287 row.337))
        (let ((row.338 row.287))
         (#0
          (#0
           (loop-block (frame-shape 1000000)
            (map ((row.340 row.338))
             (let ((row.288 row.340))
              (let ((f.341 (values)) (op.342 (values)) (arr.343 row.288))
               (let ((f.190 f.341) (op.276 op.342) (arr.289 arr.343))
                (let
                 ((f.344 (values)) (f.345 op.276)
                  (init.351
                   (let ((f.346 (values)) (arr.347 arr.289))
                    (let ((f.317 f.346) (arr.323 arr.347))
                     (let
                      ((f.348 (values)) (contiguous-subarray-array.349 arr.323)
                       (contiguous-subarray-index.350 (frame 0)))
                      (let
                       ((f.322 f.348)
                        (contiguous-subarray-array.324
                         contiguous-subarray-array.349)
                        (contiguous-subarray-index.325
                         contiguous-subarray-index.350))
                       (contiguous-subarray contiguous-subarray-array.324
                        contiguous-subarray-index.325 (shape 10) (shape)))))))
                  (arr.357
                   (let ((f.352 (values)) (arr.353 arr.289))
                    (let ((f.280 f.352) (arr.290 arr.353))
                     (let
                      ((f.354 (values)) (contiguous-subarray-array.355 arr.290)
                       (contiguous-subarray-index.356 (frame 1)))
                      (let
                       ((f.285 f.354)
                        (contiguous-subarray-array.291
                         contiguous-subarray-array.355)
                        (contiguous-subarray-index.292
                         contiguous-subarray-index.356))
                       (contiguous-subarray contiguous-subarray-array.291
                        contiguous-subarray-index.292 (shape 10) (shape 9))))))))
                 (let
                  ((f.236 f.344) (f.277 f.345) (init.326 init.351)
                   (arr.293 arr.357))
                  (let ((init.358 init.326))
                   (let ((init.327 init.358))
                    (let ((up-ranked-f.359 (values)))
                     (let ((up-ranked-f.264 up-ranked-f.359))
                      (let
                       ((f.360 (values)) (reduce-f-arg.361 up-ranked-f.264)
                        (reduce-zero-arg.367
                         (let ((f.362 (values)) (v.363 init.327))
                          (let ((f.302 f.362) (v.328 v.363))
                           (let ((make.364 (values)))
                            (let ((make.312 make.364))
                             (let ((f.365 make.312) (v.366 v.328))
                              (let ((f.313 f.365) (v.329 v.366))
                               (let () (let () v.329)))))))))
                        (reduce-array-arg.368 arr.293))
                       (let
                        ((f.262 f.360) (reduce-f-arg.265 reduce-f-arg.361)
                         (reduce-zero-arg.330 reduce-zero-arg.367)
                         (reduce-array-arg.294 reduce-array-arg.368))
                        (let ((reduce-arg.375 reduce-array-arg.294))
                         (#1
                          (loop-block (frame-shape 9)
                           (map ((reduce-arg.376 reduce-arg.375))
                            (values reduce-arg.376))
                           (body-matcher (reduce-arg.369)) (map-result ())
                           (consumer
                            (reduce-zero reduce-zero-arg.330
                             (reduce-arg1.295 reduce-arg2.298 reduce-arg.369)
                             (let
                              ((f.370 f.277) (arg0.371 reduce-arg1.295)
                               (arg1.372 reduce-arg2.298))
                              (let
                               ((f.278 f.370) (arg0.296 arg0.371)
                                (arg1.299 arg1.372))
                               (let ((arg0.373 arg0.296) (arg1.374 arg1.299))
                                (let ((arg0.297 arg0.373) (arg1.300 arg1.374))
                                 (+ arg0.297 arg1.300))))))))))))))))))))))
            (body-matcher map-result.339) (map-result (map-result.339))
            (consumer (values))))))))))
    Result of stage Fuse and Simplify:
    (let
     ((contiguous-subarray-index.350 (frame 0))
      (contiguous-subarray-index.356 (frame 1)))
     (#0
      (#0
       (loop-block (frame-shape 1000000)
        (map () (iota iota.334)
         (let
          ((map-result.408
            (#0
             (loop-block (frame-shape 10)
              (map () (iota (iota.336 : iota.334)) iota.336)
              (body-matcher map-result.335) (map-result (map-result.335))
              (consumer (values))))))
          (let
           ((reduce-arg.403
             (contiguous-subarray (#0 map-result.408)
              contiguous-subarray-index.356 (shape 10) (shape 9))))
           (#1
            (loop-block (frame-shape 9)
             (map ((reduce-arg.376 reduce-arg.403)) reduce-arg.376)
             (body-matcher reduce-arg.369) (map-result ())
             (consumer
              (reduce-zero
               (contiguous-subarray (#0 map-result.408)
                contiguous-subarray-index.350 (shape 10) (shape))
               (reduce-arg1.295 reduce-arg2.298 reduce-arg.369)
               (+ reduce-arg1.295 reduce-arg2.298))))))))
        (body-matcher map-result.339) (map-result (map-result.339))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((contiguous-subarray-index.350 (frame 0))
      (contiguous-subarray-index.356 (frame 1)))
     (#0
      (kernel (blocks 320) (threads 32)
       (map-kernel (frame-shape 1000000) () (iota iota.334)
        (body-matcher map-result.339) (map-result (map-result.339))
        (let
         ((map-result.408
           (#0
            (loop-block (frame-shape 10)
             (map () (iota (iota.336 : iota.334)) iota.336)
             (body-matcher map-result.335) (map-result (map-result.335))
             (consumer (values))))))
         (let
          ((reduce-arg.403
            (contiguous-subarray (#0 map-result.408)
             contiguous-subarray-index.356 (shape 10) (shape 9))))
          (#1
           (loop-block (frame-shape 9)
            (map ((reduce-arg.376 reduce-arg.403)) reduce-arg.376)
            (body-matcher reduce-arg.369) (map-result ())
            (consumer
             (reduce-zero
              (contiguous-subarray (#0 map-result.408)
               contiguous-subarray-index.350 (shape 10) (shape))
              (reduce-arg1.295 reduce-arg2.298 reduce-arg.369)
              (+ reduce-arg1.295 reduce-arg2.298)))))))))))
    Result of stage Alloc:
    (malloc-let
     ((map-mem.434 (Tuple ()) device)
      (map-mem.430
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.428
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.427
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.425 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.423 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.350
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.423) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.423)))
       (contiguous-subarray-index.356
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.425) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.425))))
      (#0
       (begin
        (kernel (blocks 320) (threads 32)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.429 map-mem.428))
            (iota iota.334)
            (do-expr
             (let
              ((map-result.408
                (#0
                 (loop (frame-shape 10)
                  (map ((map-mem.431 map-mem.430)) (iota (iota.336 : iota.334))
                   (let ((expr-result.432 iota.336))
                    (begin (putmem expr-result.432 (#0 map-mem.431))
                     expr-result.432)))
                  (body-matcher map-result.335) (map-result (map-result.335))
                  (map-result-mem-interim map-mem.430)
                  (map-result-mem-final map-mem.430) (consumer (values))))))
              (let
               ((reduce-arg.403
                 (index
                  (#0
                   (let ((expr-result.433 map-result.408))
                    (values (#0 expr-result.433))))
                  contiguous-subarray-index.356 (shape 10) (shape 9))))
               (#1
                (let
                 ((expr-result.437
                   (loop (frame-shape 9)
                    (map
                     ((reduce-arg.376 reduce-arg.403) (map-mem.435 map-mem.434))
                     reduce-arg.376)
                    (body-matcher reduce-arg.369) (map-result ())
                    (map-result-mem-interim map-mem.434)
                    (map-result-mem-final map-mem.434)
                    (consumer
                     (reduce-zero
                      (index
                       (#0
                        (let ((expr-result.436 map-result.408))
                         (values (#0 expr-result.436))))
                       contiguous-subarray-index.350 (shape 10) (shape))
                      (reduce-arg1.295 reduce-arg2.298 reduce-arg.369)
                      (+ reduce-arg1.295 reduce-arg2.298))))))
                 (begin (putmem (#1 expr-result.437) (#0 map-mem.429))
                  expr-result.437))))))))
          (map-result-mem-interim map-mem.428)
          (map-result-mem-final (values map-mem-result.0.427))))
        (getmem (values map-mem-result.0.427))))))
    Result of stage Capture:
    (malloc-let
     ((map-mem.434 (Tuple ()) device)
      (map-mem.430
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 10))))))
       device)
      (map-mem.428
       (Tuple ((Array ((element (Literal IntLiteral)) (shape (shape 1000000))))))
       device)
      (map-mem-result.0.427
       (Array ((element (Literal IntLiteral)) (shape (shape 1000000)))) host)
      (frame-array.425 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host)
      (frame-array.423 (Array ((element (Literal IntLiteral)) (shape (shape 1))))
       host))
     (let
      ((contiguous-subarray-index.350
        (begin
         (begin-do
          (putmem 0
           (index (mem frame-array.423) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.423)))
       (contiguous-subarray-index.356
        (begin
         (begin-do
          (putmem 1
           (index (mem frame-array.425) (offset 0)
            (type' (Atom (Literal IntLiteral))))))
         (getmem frame-array.425))))
      (#0
       (begin
        (kernel captures
         ((expr-captures
           ((((name contiguous-subarray-index) (id 350))
             (Array ((element (Literal IntLiteral)) (shape (shape 1)))))
            (((name contiguous-subarray-index) (id 356))
             (Array ((element (Literal IntLiteral)) (shape (shape 1)))))))
          (mem-captures
           ((((name map-mem) (id 428))
             (Tuple
              ((Array ((element (Literal IntLiteral)) (shape (shape 1000000)))))))
            (((name map-mem) (id 430))
             (Tuple
              ((Array ((element (Literal IntLiteral)) (shape (shape 10)))))))
            (((name map-mem) (id 434)) (Tuple ()))))
          (index-captures ()))
         (blocks 320) (threads 32)
         ((map
           (map-kernel (frame-shape 1000000) ((map-mem.429 map-mem.428))
            (iota iota.334)
            (do-expr
             (let
              ((map-result.408
                (#0
                 (loop (frame-shape 10)
                  (map ((map-mem.431 map-mem.430)) (iota (iota.336 : iota.334))
                   (let ((expr-result.432 iota.336))
                    (begin (putmem expr-result.432 (#0 map-mem.431))
                     expr-result.432)))
                  (body-matcher map-result.335) (map-result (map-result.335))
                  (map-result-mem-interim map-mem.430)
                  (map-result-mem-final map-mem.430) (consumer (values))))))
              (let
               ((reduce-arg.403
                 (index
                  (#0
                   (let ((expr-result.433 map-result.408))
                    (values (#0 expr-result.433))))
                  contiguous-subarray-index.356 (shape 10) (shape 9))))
               (#1
                (let
                 ((expr-result.437
                   (loop (frame-shape 9)
                    (map
                     ((reduce-arg.376 reduce-arg.403) (map-mem.435 map-mem.434))
                     reduce-arg.376)
                    (body-matcher reduce-arg.369) (map-result ())
                    (map-result-mem-interim map-mem.434)
                    (map-result-mem-final map-mem.434)
                    (consumer
                     (reduce-zero
                      (index
                       (#0
                        (let ((expr-result.436 map-result.408))
                         (values (#0 expr-result.436))))
                       contiguous-subarray-index.350 (shape 10) (shape))
                      (reduce-arg1.295 reduce-arg2.298 reduce-arg.369)
                      (+ reduce-arg1.295 reduce-arg2.298))))))
                 (begin (putmem (#1 expr-result.437) (#0 map-mem.429))
                  expr-result.437))))))))
          (map-result-mem-interim map-mem.428)
          (map-result-mem-final (values map-mem-result.0.427))))
        (getmem (values map-mem-result.0.427)))))) |}]
;;
