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
       (((binding ((name f) (id 47))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 45)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 46)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 47))))))
         (args (((id ((name +arg1) (id 45)))) ((id ((name +arg2) (id 46))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 48)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 49)))
         (value
          (AtomAsArray
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 50)))
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
                  ((id ((name +arg1) (id 49)))
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
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((f.51 (values)) (+arg1.52 1) (+arg2.53 2))
     (let ((f.48 f.51) (+arg1.49 +arg1.52) (+arg2.50 +arg2.53))
      (+ +arg1.49 +arg2.50)))
    Result of stage Fuse and Simplify:
    3
    Result of stage Kernelize:
    3
    Result of stage Alloc:
    (mem-let () 3)
    Result of stage Capture:
    (mem-let () 3) |}]
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
     ((binding ((name add) (id 45)))
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding ((name x) (id 46)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding ((name y) (id 47)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args
                ((Ref ((id ((name x) (id 46))))) (Ref ((id ((name y) (id 47))))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name add) (id 45))))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name add) (id 45)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name x) (id 46)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name y) (id 47)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 50)))
                     (value (Primitive ((name (Func Add))))))
                    ((binding ((name +arg1) (id 48)))
                     (value (Ref ((id ((name x) (id 46)))))))
                    ((binding ((name +arg2) (id 49)))
                     (value (Ref ((id ((name y) (id 47)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 50))))))
                     (args
                      (((id ((name +arg1) (id 48))))
                       ((id ((name +arg2) (id 49))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 53)))
            (value (Ref ((id ((name add) (id 45)))))))
           ((binding ((name x) (id 51)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding ((name y) (id 52)))
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 53))))))
            (args (((id ((name x) (id 51)))) ((id ((name y) (id 52))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name add) (id 55)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 56)))
            (value
             (Ref
              ((id ((name add) (id 55)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name x) (id 58)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name y) (id 60)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 57)))
               (value
                (AtomAsArray
                 ((element (Values ((elements ()) (type' ()))))
                  (type' ((element (Tuple ())) (shape ())))))))
              ((binding ((name +arg1) (id 59)))
               (value
                (Ref
                 ((id ((name x) (id 58)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name +arg2) (id 61)))
               (value
                (Ref
                 ((id ((name y) (id 60)))
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
                        ((id ((name +arg1) (id 59)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 61)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))))
                  (type' (Literal IntLiteral)))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((add.62 (values)))
     (let ((add.55 add.62))
      (let ((f.63 add.55) (x.64 5) (y.65 10))
       (let ((f.56 f.63) (x.58 x.64) (y.60 y.65))
        (let ((f.66 (values)) (+arg1.67 x.58) (+arg2.68 y.60))
         (let ((f.57 f.66) (+arg1.59 +arg1.67) (+arg2.61 +arg2.68))
          (+ +arg1.59 +arg2.61)))))))
    Result of stage Fuse and Simplify:
    15
    Result of stage Kernelize:
    15
    Result of stage Alloc:
    (mem-let () 15)
    Result of stage Capture:
    (mem-let () 15) |}]
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
     ((binding ((name id) (id 45)))
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @t) (id 46))) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name e) (id 47)))
                     (bound (ArrayRef ((name @t) (id 46)))))))
                  (body (Ref ((id ((name e) (id 47))))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id ((name id) (id 45))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name id) (id 45)))
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding ((name @t) (id 46))) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name e) (id 47)))
                        (bound (ArrayRef ((name @t) (id 46)))))))
                     (body (Ref ((id ((name e) (id 47)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 49)))
            (value
             (TypeApplication
              ((tFunc (Ref ((id ((name id) (id 45))))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding ((name e) (id 48)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 49))))))
            (args (((id ((name e) (id 48))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (ArrayPrimitive
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 51)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 52)))
            (value
             (Ref
              ((id ((name id) (id 51)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name e) (id 53)))
            (value
             (AtomAsArray
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name e) (id 53)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Nest:
    (let ((id.54 (values)))
     (let ((id.51 id.54))
      (let ((f.55 id.51) (e.56 5)) (let ((f.52 f.55) (e.53 e.56)) e.53))))
    Result of stage Fuse and Simplify:
    5
    Result of stage Kernelize:
    5
    Result of stage Alloc:
    (mem-let () 5)
    Result of stage Capture:
    (mem-let () 5) |}]
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
       (((binding ((name f) (id 47))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 45)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding ((name +arg2) (id 46)))
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
          (((binding ((name +arg1) (id 48)))
            (value (Ref ((id ((name +arg1) (id 45)))))))
           ((binding ((name +arg2) (id 49)))
            (value (Ref ((id ((name +arg2) (id 46)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg2) (id 50)))
               (value (Ref ((id ((name +arg2) (id 49)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 47))))))
               (args
                (((id ((name +arg1) (id 48)))) ((id ((name +arg2) (id 50))))))
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
       (((binding ((name f) (id 51)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))
        ((binding ((name +arg1) (id 52)))
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
        ((binding ((name +arg2) (id 54)))
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
          (((binding ((name +arg1) (id 53)))
            (value
             (Ref
              ((id ((name +arg1) (id 52)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding ((name +arg2) (id 55)))
            (value
             (Ref
              ((id ((name +arg2) (id 54)))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (ArrayPrimitive
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 56)))
               (value
                (Ref
                 ((id ((name +arg2) (id 55)))
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
                        ((id ((name +arg1) (id 53)))
                         (type' ((element (Literal IntLiteral)) (shape ()))))))
                      (type' (Literal IntLiteral))))
                    (ArrayAsAtom
                     ((array
                       (Ref
                        ((id ((name +arg2) (id 56)))
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
     ((f.57 (values)) (+arg1.58 (frame 1 2))
      (+arg2.59 (frame (frame 3 4 5) (frame 6 7 8))))
     (let ((f.51 f.57) (+arg1.52 +arg1.58) (+arg2.54 +arg2.59))
      (let ((+arg1.60 +arg1.52) (+arg2.61 +arg2.54))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((+arg1.63 +arg1.60) (+arg2.64 +arg2.61))
           (let ((+arg1.53 +arg1.63) (+arg2.55 +arg2.64))
            (let ((+arg2.65 +arg2.55))
             (#0
              (#0
               (loop-block (frame-shape 3)
                (map ((+arg2.67 +arg2.65))
                 (let ((+arg2.56 +arg2.67)) (+ +arg1.53 +arg2.56)))
                (body-matcher map-result.66) (map-result (map-result.66))
                (consumer (values))))))))
          (body-matcher map-result.62) (map-result (map-result.62))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let ((+arg1.60 (frame 1 2)) (+arg2.61 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.63 +arg1.60) (+arg2.64 +arg2.61))
         (let ((+arg2.65 +arg2.64))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.67 +arg2.65)) (+ +arg1.63 +arg2.67))
             (body-matcher map-result.66) (map-result (map-result.66))
             (consumer (values)))))))
        (body-matcher map-result.62) (map-result (map-result.62))
        (consumer (values))))))
    Result of stage Kernelize:
    (let ((+arg1.60 (frame 1 2)) (+arg2.61 (frame (frame 3 4 5) (frame 6 7 8))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((+arg1.63 +arg1.60) (+arg2.64 +arg2.61))
         (let ((+arg2.65 +arg2.64))
          (#0
           (#0
            (loop-block (frame-shape 3)
             (map ((+arg2.67 +arg2.65)) (+ +arg1.63 +arg2.67))
             (body-matcher map-result.66) (map-result (map-result.66))
             (consumer (values)))))))
        (body-matcher map-result.62) (map-result (map-result.62))
        (consumer (values))))))
    Result of stage Alloc:
    (mem-let
     ((map-mem.89
       (Malloc (hostOrDevice MallocHost)
        (type'
         (Array
          ((element (Array ((element (Literal IntLiteral)) (size 3)))) (size 2))))))
      (frame-array.82
       (Malloc (hostOrDevice MallocHost)
        (type'
         (Array
          ((element (Array ((element (Literal IntLiteral)) (size 3)))) (size 2))))))
      (frame-array.79
       (Malloc (hostOrDevice MallocHost)
        (type' (Array ((element (Literal IntLiteral)) (size 2)))))))
     (let
      ((+arg1.60
        (begin
         (begin-do
          (putmem 1
           (Index
            (mem
             (Ref (id ((name frame-array) (id 79)))
              (type' (Array ((element (Literal IntLiteral)) (size 2))))))
            (offset 0) (elementType (Literal IntLiteral))
            (type' (Literal IntLiteral))))
          (putmem 2
           (Index
            (mem
             (Ref (id ((name frame-array) (id 79)))
              (type' (Array ((element (Literal IntLiteral)) (size 2))))))
            (offset 1) (elementType (Literal IntLiteral))
            (type' (Literal IntLiteral)))))
         (getmem
          (Ref (id ((name frame-array) (id 79)))
           (type' (Array ((element (Literal IntLiteral)) (size 2))))))))
       (+arg2.61
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (Index
             (mem
              (Index
               (mem
                (Ref (id ((name frame-array) (id 82)))
                 (type'
                  (Array
                   ((element (Array ((element (Literal IntLiteral)) (size 3))))
                    (size 2))))))
               (offset 0)
               (elementType (Array ((element (Literal IntLiteral)) (size 3))))
               (type' (Array ((element (Literal IntLiteral)) (size 3))))))
             (offset 0) (elementType (Literal IntLiteral))
             (type' (Literal IntLiteral))))
           (putmem 4
            (Index
             (mem
              (Index
               (mem
                (Ref (id ((name frame-array) (id 82)))
                 (type'
                  (Array
                   ((element (Array ((element (Literal IntLiteral)) (size 3))))
                    (size 2))))))
               (offset 0)
               (elementType (Array ((element (Literal IntLiteral)) (size 3))))
               (type' (Array ((element (Literal IntLiteral)) (size 3))))))
             (offset 1) (elementType (Literal IntLiteral))
             (type' (Literal IntLiteral))))
           (putmem 5
            (Index
             (mem
              (Index
               (mem
                (Ref (id ((name frame-array) (id 82)))
                 (type'
                  (Array
                   ((element (Array ((element (Literal IntLiteral)) (size 3))))
                    (size 2))))))
               (offset 0)
               (elementType (Array ((element (Literal IntLiteral)) (size 3))))
               (type' (Array ((element (Literal IntLiteral)) (size 3))))))
             (offset 2) (elementType (Literal IntLiteral))
             (type' (Literal IntLiteral)))))
          (begin-do
           (putmem 6
            (Index
             (mem
              (Index
               (mem
                (Ref (id ((name frame-array) (id 82)))
                 (type'
                  (Array
                   ((element (Array ((element (Literal IntLiteral)) (size 3))))
                    (size 2))))))
               (offset 1)
               (elementType (Array ((element (Literal IntLiteral)) (size 3))))
               (type' (Array ((element (Literal IntLiteral)) (size 3))))))
             (offset 0) (elementType (Literal IntLiteral))
             (type' (Literal IntLiteral))))
           (putmem 7
            (Index
             (mem
              (Index
               (mem
                (Ref (id ((name frame-array) (id 82)))
                 (type'
                  (Array
                   ((element (Array ((element (Literal IntLiteral)) (size 3))))
                    (size 2))))))
               (offset 1)
               (elementType (Array ((element (Literal IntLiteral)) (size 3))))
               (type' (Array ((element (Literal IntLiteral)) (size 3))))))
             (offset 1) (elementType (Literal IntLiteral))
             (type' (Literal IntLiteral))))
           (putmem 8
            (Index
             (mem
              (Index
               (mem
                (Ref (id ((name frame-array) (id 82)))
                 (type'
                  (Array
                   ((element (Array ((element (Literal IntLiteral)) (size 3))))
                    (size 2))))))
               (offset 1)
               (elementType (Array ((element (Literal IntLiteral)) (size 3))))
               (type' (Array ((element (Literal IntLiteral)) (size 3))))))
             (offset 2) (elementType (Literal IntLiteral))
             (type' (Literal IntLiteral))))))
         (getmem
          (Ref (id ((name frame-array) (id 82)))
           (type'
            (Array
             ((element (Array ((element (Literal IntLiteral)) (size 3))))
              (size 2)))))))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.63 +arg1.60) (+arg2.64 +arg2.61)
           (map-mem.90
            (Ref (id ((name map-mem) (id 89)))
             (type'
              (Array
               ((element (Array ((element (Literal IntLiteral)) (size 3))))
                (size 2)))))))
          (let ((+arg2.65 +arg2.64))
           (#0
            (#0
             (loop (frame-shape 3)
              (map
               ((+arg2.67 +arg2.65)
                (map-mem.91
                 (Ref (id ((name map-mem) (id 90)))
                  (type' (Array ((element (Literal IntLiteral)) (size 3)))))))
               (let ((expr-result.92 (+ +arg1.63 +arg2.67)))
                (begin
                 (putmem expr-result.92
                  (Ref (id ((name map-mem) (id 91)))
                   (type' (Literal IntLiteral))))
                 expr-result.92)))
              (body-matcher map-result.66) (map-result (map-result.66))
              (map-result-mem
               (Values
                (elements
                 ((Ref (id ((name map-mem) (id 90)))
                   (type' (Array ((element (Literal IntLiteral)) (size 3)))))))
                (type'
                 (Tuple ((Array ((element (Literal IntLiteral)) (size 3))))))))
              (consumer (values)))))))
         (body-matcher map-result.62) (map-result (map-result.62))
         (map-result-mem
          (Values
           (elements
            ((Ref (id ((name map-mem) (id 89)))
              (type'
               (Array
                ((element (Array ((element (Literal IntLiteral)) (size 3))))
                 (size 2)))))))
           (type'
            (Tuple
             ((Array
               ((element (Array ((element (Literal IntLiteral)) (size 3))))
                (size 2))))))))
         (consumer (values)))))))
    Result of stage Capture:
    (mem-let
     ((map-mem.89
       (Malloc (hostOrDevice MallocHost)
        (type'
         (Array
          ((element (Array ((element (Literal IntLiteral)) (size 3)))) (size 2))))))
      (frame-array.82
       (Malloc (hostOrDevice MallocHost)
        (type'
         (Array
          ((element (Array ((element (Literal IntLiteral)) (size 3)))) (size 2))))))
      (frame-array.79
       (Malloc (hostOrDevice MallocHost)
        (type' (Array ((element (Literal IntLiteral)) (size 2)))))))
     (let
      ((+arg1.60
        (begin
         (begin-do
          (putmem 1
           (Index
            (mem
             (Ref (id ((name frame-array) (id 79)))
              (type' (Array ((element (Literal IntLiteral)) (size 2))))))
            (offset 0) (elementType (Literal IntLiteral))
            (type' (Literal IntLiteral))))
          (putmem 2
           (Index
            (mem
             (Ref (id ((name frame-array) (id 79)))
              (type' (Array ((element (Literal IntLiteral)) (size 2))))))
            (offset 1) (elementType (Literal IntLiteral))
            (type' (Literal IntLiteral)))))
         (getmem
          (Ref (id ((name frame-array) (id 79)))
           (type' (Array ((element (Literal IntLiteral)) (size 2))))))))
       (+arg2.61
        (begin
         (begin-do
          (begin-do
           (putmem 3
            (Index
             (mem
              (Index
               (mem
                (Ref (id ((name frame-array) (id 82)))
                 (type'
                  (Array
                   ((element (Array ((element (Literal IntLiteral)) (size 3))))
                    (size 2))))))
               (offset 0)
               (elementType (Array ((element (Literal IntLiteral)) (size 3))))
               (type' (Array ((element (Literal IntLiteral)) (size 3))))))
             (offset 0) (elementType (Literal IntLiteral))
             (type' (Literal IntLiteral))))
           (putmem 4
            (Index
             (mem
              (Index
               (mem
                (Ref (id ((name frame-array) (id 82)))
                 (type'
                  (Array
                   ((element (Array ((element (Literal IntLiteral)) (size 3))))
                    (size 2))))))
               (offset 0)
               (elementType (Array ((element (Literal IntLiteral)) (size 3))))
               (type' (Array ((element (Literal IntLiteral)) (size 3))))))
             (offset 1) (elementType (Literal IntLiteral))
             (type' (Literal IntLiteral))))
           (putmem 5
            (Index
             (mem
              (Index
               (mem
                (Ref (id ((name frame-array) (id 82)))
                 (type'
                  (Array
                   ((element (Array ((element (Literal IntLiteral)) (size 3))))
                    (size 2))))))
               (offset 0)
               (elementType (Array ((element (Literal IntLiteral)) (size 3))))
               (type' (Array ((element (Literal IntLiteral)) (size 3))))))
             (offset 2) (elementType (Literal IntLiteral))
             (type' (Literal IntLiteral)))))
          (begin-do
           (putmem 6
            (Index
             (mem
              (Index
               (mem
                (Ref (id ((name frame-array) (id 82)))
                 (type'
                  (Array
                   ((element (Array ((element (Literal IntLiteral)) (size 3))))
                    (size 2))))))
               (offset 1)
               (elementType (Array ((element (Literal IntLiteral)) (size 3))))
               (type' (Array ((element (Literal IntLiteral)) (size 3))))))
             (offset 0) (elementType (Literal IntLiteral))
             (type' (Literal IntLiteral))))
           (putmem 7
            (Index
             (mem
              (Index
               (mem
                (Ref (id ((name frame-array) (id 82)))
                 (type'
                  (Array
                   ((element (Array ((element (Literal IntLiteral)) (size 3))))
                    (size 2))))))
               (offset 1)
               (elementType (Array ((element (Literal IntLiteral)) (size 3))))
               (type' (Array ((element (Literal IntLiteral)) (size 3))))))
             (offset 1) (elementType (Literal IntLiteral))
             (type' (Literal IntLiteral))))
           (putmem 8
            (Index
             (mem
              (Index
               (mem
                (Ref (id ((name frame-array) (id 82)))
                 (type'
                  (Array
                   ((element (Array ((element (Literal IntLiteral)) (size 3))))
                    (size 2))))))
               (offset 1)
               (elementType (Array ((element (Literal IntLiteral)) (size 3))))
               (type' (Array ((element (Literal IntLiteral)) (size 3))))))
             (offset 2) (elementType (Literal IntLiteral))
             (type' (Literal IntLiteral))))))
         (getmem
          (Ref (id ((name frame-array) (id 82)))
           (type'
            (Array
             ((element (Array ((element (Literal IntLiteral)) (size 3))))
              (size 2)))))))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((+arg1.63 +arg1.60) (+arg2.64 +arg2.61)
           (map-mem.90
            (Ref (id ((name map-mem) (id 89)))
             (type'
              (Array
               ((element (Array ((element (Literal IntLiteral)) (size 3))))
                (size 2)))))))
          (let ((+arg2.65 +arg2.64))
           (#0
            (#0
             (loop (frame-shape 3)
              (map
               ((+arg2.67 +arg2.65)
                (map-mem.91
                 (Ref (id ((name map-mem) (id 90)))
                  (type' (Array ((element (Literal IntLiteral)) (size 3)))))))
               (let ((expr-result.92 (+ +arg1.63 +arg2.67)))
                (begin
                 (putmem expr-result.92
                  (Ref (id ((name map-mem) (id 91)))
                   (type' (Literal IntLiteral))))
                 expr-result.92)))
              (body-matcher map-result.66) (map-result (map-result.66))
              (map-result-mem
               (Values
                (elements
                 ((Ref (id ((name map-mem) (id 90)))
                   (type' (Array ((element (Literal IntLiteral)) (size 3)))))))
                (type'
                 (Tuple ((Array ((element (Literal IntLiteral)) (size 3))))))))
              (consumer (values)))))))
         (body-matcher map-result.62) (map-result (map-result.62))
         (map-result-mem
          (Values
           (elements
            ((Ref (id ((name map-mem) (id 89)))
              (type'
               (Array
                ((element (Array ((element (Literal IntLiteral)) (size 3))))
                 (size 2)))))))
           (type'
            (Tuple
             ((Array
               ((element (Array ((element (Literal IntLiteral)) (size 3))))
                (size 2))))))))
         (consumer (values))))))) |}]
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
     ((binding ((name words) (id 45)))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 46)) 1)))))))))))))))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 46)) 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings ((((name len) (id 47)) Dim)))
         (valueBinding ((name word) (id 48)))
         (box (Ref ((id ((name words) (id 45))))))
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
                       ((Dimension ((const 0) (refs ((((name len) (id 47)) 1)))))
                        (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id ((name word) (id 48)))))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name words) (id 45)))
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
                       ((Add ((const 0) (refs ((((name len) (id 46)) 1)))))))))))))))
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
                       ((Add ((const 0) (refs ((((name len) (id 46)) 1)))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name box) (id 54)))
            (value (Ref ((id ((name words) (id 45)))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 47)))
               (indexValue
                (FromBox (box (Ref ((id ((name box) (id 54)))))) (i 0)))
               (sort Dim))))
            (body
             (Map
              ((args
                (((binding ((name word) (id 48)))
                  (value (BoxValue ((box (Ref ((id ((name box) (id 54))))))))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 53)))
                     (value (Primitive ((name (Func Equal))))))
                    ((binding ((name =arg1) (id 49)))
                     (value (Scalar ((element (Literal (IntLiteral 3)))))))
                    ((binding ((name =arg2) (id 52)))
                     (value
                      (Map
                       ((args
                         (((binding ((name f) (id 51)))
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
                                    ((const 0) (refs ((((name len) (id 47)) 1)))))
                                   (Shape ()))))))
                              (args ((Atom (Literal CharacterLiteral))))))))
                          ((binding ((name arr) (id 50)))
                           (value (Ref ((id ((name word) (id 48)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 51))))))
                           (args (((id ((name arr) (id 50))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 53))))))
                     (args
                      (((id ((name =arg1) (id 49))))
                       ((id ((name =arg2) (id 52))))))
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
       (((binding ((name words) (id 60)))
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
                      ((Add ((const 0) (refs ((((name len) (id 46)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 46))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 46)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 46))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 46)) 1))))))))))))
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
                      ((Add ((const 0) (refs ((((name len) (id 46)) 1)))))))))
                   (type'
                    ((parameters (((binding ((name len) (id 46))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 46)) 1))))))))))))))
                (type'
                 ((element
                   (Sigma
                    ((parameters (((binding ((name len) (id 46))) (bound Dim))))
                     (body
                      ((element (Literal CharacterLiteral))
                       (shape
                        ((Add ((const 0) (refs ((((name len) (id 46)) 1))))))))))))
                  (shape ())))))))
            (type'
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 46))) (bound Dim))))
                 (body
                  ((element (Literal CharacterLiteral))
                   (shape ((Add ((const 0) (refs ((((name len) (id 46)) 1))))))))))))
              (shape ((Add ((const 2) (refs ())))))))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name box) (id 61)))
            (value
             (Ref
              ((id ((name words) (id 60)))
               (type'
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 46))) (bound Dim))))
                    (body
                     ((element (Literal CharacterLiteral))
                      (shape
                       ((Add ((const 0) (refs ((((name len) (id 46)) 1))))))))))))
                 (shape ((Add ((const 2) (refs ())))))))))))))
         (body
          (IndexLet
           ((indexArgs
             (((indexBinding ((name len) (id 47)))
               (indexValue
                (FromBox
                 (box
                  (Ref
                   ((id ((name box) (id 61)))
                    (type'
                     ((element
                       (Sigma
                        ((parameters
                          (((binding ((name len) (id 46))) (bound Dim))))
                         (body
                          ((element (Literal CharacterLiteral))
                           (shape
                            ((Add ((const 0) (refs ((((name len) (id 46)) 1))))))))))))
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
                   (((binding ((name f) (id 55)))
                     (value
                      (AtomAsArray
                       ((element (Values ((elements ()) (type' ()))))
                        (type' ((element (Tuple ())) (shape ())))))))
                    ((binding ((name =arg1) (id 56)))
                     (value
                      (AtomAsArray
                       ((element (Literal (IntLiteral 3)))
                        (type' ((element (Literal IntLiteral)) (shape ())))))))
                    ((binding ((name =arg2) (id 59)))
                     (value
                      (ArrayPrimitive
                       (Map (frameShape ())
                        (args
                         (((binding ((name f) (id 58)))
                           (value
                            (AtomAsArray
                             ((element (Values ((elements ()) (type' ()))))
                              (type' ((element (Tuple ())) (shape ())))))))))
                        (body
                         (ReifyIndex
                          ((index
                            (Dimension
                             ((const 0) (refs ((((name len) (id 47)) 1))))))
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
                              ((id ((name =arg1) (id 56)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name =arg2) (id 59)))
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
     ((words.62 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (let ((words.60 words.62))
      (let ((box.63 words.60))
       (#0
        (#0
         (loop-block (frame-shape 2)
          (map ((box.65 box.63))
           (let ((box.61 box.65))
            (index-let ((len.47 box-index-0 box.61))
             (let ()
              (let ()
               (let
                ((f.66 (values)) (=arg1.67 3)
                 (=arg2.69
                  (let ((f.68 (values)))
                   (let ((f.58 f.68)) (reify-index len.47)))))
                (let ((f.55 f.66) (=arg1.56 =arg1.67) (=arg2.59 =arg2.69))
                 (= =arg1.56 =arg2.59))))))))
          (body-matcher map-result.64) (map-result (map-result.64))
          (consumer (values))))))))
    Result of stage Fuse and Simplify:
    (let
     ((box.63 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.65 box.63))
         (index-let ((len.47 box-index-0 box.65)) (= 3 (reify-index len.47))))
        (body-matcher map-result.64) (map-result (map-result.64))
        (consumer (values))))))
    Result of stage Kernelize:
    (let
     ((box.63 (frame (box (3) (frame 'h' 'e' 'y')) (box (2) (frame 'h' 'i')))))
     (#0
      (#0
       (loop-block (frame-shape 2)
        (map ((box.65 box.63))
         (index-let ((len.47 box-index-0 box.65)) (= 3 (reify-index len.47))))
        (body-matcher map-result.64) (map-result (map-result.64))
        (consumer (values))))))
    Result of stage Alloc:
    (mem-let
     ((map-mem.81
       (Malloc (hostOrDevice MallocHost)
        (type' (Array ((element (Literal BooleanLiteral)) (size 2))))))
      (frame-array.77
       (Malloc (hostOrDevice MallocHost)
        (type' (Array ((element (Literal CharacterLiteral)) (size 2))))))
      (frame-array.72
       (Malloc (hostOrDevice MallocHost)
        (type' (Array ((element (Literal CharacterLiteral)) (size 3))))))
      (frame-array.71
       (Malloc (hostOrDevice MallocHost)
        (type'
         (Array
          ((element
            (Sigma
             ((parameters (((binding ((name len) (id 46))) (bound Dim))))
              (body (Array ((element (Literal CharacterLiteral)) (size len.46)))))))
           (size 2)))))))
     (let
      ((box.63
        (begin
         (begin-do
          (putmem
           (box (3)
            (begin
             (begin-do
              (putmem 'h'
               (Index
                (mem
                 (Ref (id ((name frame-array) (id 72)))
                  (type' (Array ((element (Literal CharacterLiteral)) (size 3))))))
                (offset 0) (elementType (Literal CharacterLiteral))
                (type' (Literal CharacterLiteral))))
              (putmem 'e'
               (Index
                (mem
                 (Ref (id ((name frame-array) (id 72)))
                  (type' (Array ((element (Literal CharacterLiteral)) (size 3))))))
                (offset 1) (elementType (Literal CharacterLiteral))
                (type' (Literal CharacterLiteral))))
              (putmem 'y'
               (Index
                (mem
                 (Ref (id ((name frame-array) (id 72)))
                  (type' (Array ((element (Literal CharacterLiteral)) (size 3))))))
                (offset 2) (elementType (Literal CharacterLiteral))
                (type' (Literal CharacterLiteral)))))
             (getmem
              (Ref (id ((name frame-array) (id 72)))
               (type' (Array ((element (Literal CharacterLiteral)) (size 3))))))))
           (Index
            (mem
             (Ref (id ((name frame-array) (id 71)))
              (type'
               (Array
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 46))) (bound Dim))))
                    (body
                     (Array ((element (Literal CharacterLiteral)) (size len.46)))))))
                 (size 2))))))
            (offset 0)
            (elementType
             (Sigma
              ((parameters (((binding ((name len) (id 46))) (bound Dim))))
               (body
                (Array ((element (Literal CharacterLiteral)) (size len.46)))))))
            (type'
             (Sigma
              ((parameters (((binding ((name len) (id 46))) (bound Dim))))
               (body
                (Array ((element (Literal CharacterLiteral)) (size len.46)))))))))
          (putmem
           (box (2)
            (begin
             (begin-do
              (putmem 'h'
               (Index
                (mem
                 (Ref (id ((name frame-array) (id 77)))
                  (type' (Array ((element (Literal CharacterLiteral)) (size 2))))))
                (offset 0) (elementType (Literal CharacterLiteral))
                (type' (Literal CharacterLiteral))))
              (putmem 'i'
               (Index
                (mem
                 (Ref (id ((name frame-array) (id 77)))
                  (type' (Array ((element (Literal CharacterLiteral)) (size 2))))))
                (offset 1) (elementType (Literal CharacterLiteral))
                (type' (Literal CharacterLiteral)))))
             (getmem
              (Ref (id ((name frame-array) (id 77)))
               (type' (Array ((element (Literal CharacterLiteral)) (size 2))))))))
           (Index
            (mem
             (Ref (id ((name frame-array) (id 71)))
              (type'
               (Array
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 46))) (bound Dim))))
                    (body
                     (Array ((element (Literal CharacterLiteral)) (size len.46)))))))
                 (size 2))))))
            (offset 1)
            (elementType
             (Sigma
              ((parameters (((binding ((name len) (id 46))) (bound Dim))))
               (body
                (Array ((element (Literal CharacterLiteral)) (size len.46)))))))
            (type'
             (Sigma
              ((parameters (((binding ((name len) (id 46))) (bound Dim))))
               (body
                (Array ((element (Literal CharacterLiteral)) (size len.46))))))))))
         (getmem
          (Ref (id ((name frame-array) (id 71)))
           (type'
            (Array
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 46))) (bound Dim))))
                 (body
                  (Array ((element (Literal CharacterLiteral)) (size len.46)))))))
              (size 2)))))))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((box.65 box.63)
           (map-mem.82
            (Ref (id ((name map-mem) (id 81)))
             (type' (Array ((element (Literal BooleanLiteral)) (size 2)))))))
          (index-let ((len.47 box-index-0 box.65))
           (let ((expr-result.83 (= 3 (reify-dimension-index len.47))))
            (begin
             (putmem expr-result.83
              (Ref (id ((name map-mem) (id 82)))
               (type' (Literal BooleanLiteral))))
             expr-result.83))))
         (body-matcher map-result.64) (map-result (map-result.64))
         (map-result-mem
          (Values
           (elements
            ((Ref (id ((name map-mem) (id 81)))
              (type' (Array ((element (Literal BooleanLiteral)) (size 2)))))))
           (type'
            (Tuple ((Array ((element (Literal BooleanLiteral)) (size 2))))))))
         (consumer (values)))))))
    Result of stage Capture:
    (mem-let
     ((map-mem.81
       (Malloc (hostOrDevice MallocHost)
        (type' (Array ((element (Literal BooleanLiteral)) (size 2))))))
      (frame-array.77
       (Malloc (hostOrDevice MallocHost)
        (type' (Array ((element (Literal CharacterLiteral)) (size 2))))))
      (frame-array.72
       (Malloc (hostOrDevice MallocHost)
        (type' (Array ((element (Literal CharacterLiteral)) (size 3))))))
      (frame-array.71
       (Malloc (hostOrDevice MallocHost)
        (type'
         (Array
          ((element
            (Sigma
             ((parameters (((binding ((name len) (id 46))) (bound Dim))))
              (body (Array ((element (Literal CharacterLiteral)) (size len.46)))))))
           (size 2)))))))
     (let
      ((box.63
        (begin
         (begin-do
          (putmem
           (box (3)
            (begin
             (begin-do
              (putmem 'h'
               (Index
                (mem
                 (Ref (id ((name frame-array) (id 72)))
                  (type' (Array ((element (Literal CharacterLiteral)) (size 3))))))
                (offset 0) (elementType (Literal CharacterLiteral))
                (type' (Literal CharacterLiteral))))
              (putmem 'e'
               (Index
                (mem
                 (Ref (id ((name frame-array) (id 72)))
                  (type' (Array ((element (Literal CharacterLiteral)) (size 3))))))
                (offset 1) (elementType (Literal CharacterLiteral))
                (type' (Literal CharacterLiteral))))
              (putmem 'y'
               (Index
                (mem
                 (Ref (id ((name frame-array) (id 72)))
                  (type' (Array ((element (Literal CharacterLiteral)) (size 3))))))
                (offset 2) (elementType (Literal CharacterLiteral))
                (type' (Literal CharacterLiteral)))))
             (getmem
              (Ref (id ((name frame-array) (id 72)))
               (type' (Array ((element (Literal CharacterLiteral)) (size 3))))))))
           (Index
            (mem
             (Ref (id ((name frame-array) (id 71)))
              (type'
               (Array
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 46))) (bound Dim))))
                    (body
                     (Array ((element (Literal CharacterLiteral)) (size len.46)))))))
                 (size 2))))))
            (offset 0)
            (elementType
             (Sigma
              ((parameters (((binding ((name len) (id 46))) (bound Dim))))
               (body
                (Array ((element (Literal CharacterLiteral)) (size len.46)))))))
            (type'
             (Sigma
              ((parameters (((binding ((name len) (id 46))) (bound Dim))))
               (body
                (Array ((element (Literal CharacterLiteral)) (size len.46)))))))))
          (putmem
           (box (2)
            (begin
             (begin-do
              (putmem 'h'
               (Index
                (mem
                 (Ref (id ((name frame-array) (id 77)))
                  (type' (Array ((element (Literal CharacterLiteral)) (size 2))))))
                (offset 0) (elementType (Literal CharacterLiteral))
                (type' (Literal CharacterLiteral))))
              (putmem 'i'
               (Index
                (mem
                 (Ref (id ((name frame-array) (id 77)))
                  (type' (Array ((element (Literal CharacterLiteral)) (size 2))))))
                (offset 1) (elementType (Literal CharacterLiteral))
                (type' (Literal CharacterLiteral)))))
             (getmem
              (Ref (id ((name frame-array) (id 77)))
               (type' (Array ((element (Literal CharacterLiteral)) (size 2))))))))
           (Index
            (mem
             (Ref (id ((name frame-array) (id 71)))
              (type'
               (Array
                ((element
                  (Sigma
                   ((parameters (((binding ((name len) (id 46))) (bound Dim))))
                    (body
                     (Array ((element (Literal CharacterLiteral)) (size len.46)))))))
                 (size 2))))))
            (offset 1)
            (elementType
             (Sigma
              ((parameters (((binding ((name len) (id 46))) (bound Dim))))
               (body
                (Array ((element (Literal CharacterLiteral)) (size len.46)))))))
            (type'
             (Sigma
              ((parameters (((binding ((name len) (id 46))) (bound Dim))))
               (body
                (Array ((element (Literal CharacterLiteral)) (size len.46))))))))))
         (getmem
          (Ref (id ((name frame-array) (id 71)))
           (type'
            (Array
             ((element
               (Sigma
                ((parameters (((binding ((name len) (id 46))) (bound Dim))))
                 (body
                  (Array ((element (Literal CharacterLiteral)) (size len.46)))))))
              (size 2)))))))))
      (#0
       (#0
        (loop (frame-shape 2)
         (map
          ((box.65 box.63)
           (map-mem.82
            (Ref (id ((name map-mem) (id 81)))
             (type' (Array ((element (Literal BooleanLiteral)) (size 2)))))))
          (index-let ((len.47 box-index-0 box.65))
           (let ((expr-result.83 (= 3 (reify-dimension-index len.47))))
            (begin
             (putmem expr-result.83
              (Ref (id ((name map-mem) (id 82)))
               (type' (Literal BooleanLiteral))))
             expr-result.83))))
         (body-matcher map-result.64) (map-result (map-result.64))
         (map-result-mem
          (Values
           (elements
            ((Ref (id ((name map-mem) (id 81)))
              (type' (Array ((element (Literal BooleanLiteral)) (size 2)))))))
           (type'
            (Tuple ((Array ((element (Literal BooleanLiteral)) (size 2))))))))
         (consumer (values))))))) |}]
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
     ((binding ((name sum-row) (id 45)))
      (value
       (Scalar
        ((element
          (IndexLambda
           ((params (((binding ((name d-1) (id 46))) (bound Dim))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name row) (id 47)))
                     (bound
                      (Arr
                       ((element (Literal IntLiteral))
                        (shape
                         ((Add ((const 1) (refs ((((name d-1) (id 46)) 1)))))))))))))
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
                              ((const 0) (refs ((((name d-1) (id 46)) 1)))))
                             (Shape ()) (Shape ()))))))
                        (args ((Atom (Literal IntLiteral)))))))
                     (args
                      ((Primitive ((name (Func Add))))
                       (Ref ((id ((name row) (id 47))))))))))))))))))))))
      (body
       (TermApplication
        ((func
          (IndexApplication
           ((iFunc (Ref ((id ((name sum-row) (id 45))))))
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
       (((binding ((name sum-row) (id 45)))
         (value
          (Scalar
           ((element
             (IndexLambda
              ((params (((binding ((name d-1) (id 46))) (bound Dim))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name row) (id 47)))
                        (bound
                         (Arr
                          ((element (Literal IntLiteral))
                           (shape
                            ((Add ((const 1) (refs ((((name d-1) (id 46)) 1)))))))))))))
                     (body
                      (Map
                       ((args
                         (((binding ((name f) (id 50)))
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
                                    ((const 0) (refs ((((name d-1) (id 46)) 1)))))
                                   (Shape ()) (Shape ()))))))
                              (args ((Atom (Literal IntLiteral))))))))
                          ((binding ((name reduce-f-arg) (id 48)))
                           (value (Primitive ((name (Func Add))))))
                          ((binding ((name reduce-array-arg) (id 49)))
                           (value (Ref ((id ((name row) (id 47)))))))))
                        (body
                         (TermApplication
                          ((func (Ref ((id ((name f) (id 50))))))
                           (args
                            (((id ((name reduce-f-arg) (id 48))))
                             ((id ((name reduce-array-arg) (id 49))))))
                           (type' ((element (Literal IntLiteral)) (shape ()))))))
                        (frameShape ())
                        (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 52)))
            (value
             (IndexApplication
              ((iFunc (Ref ((id ((name sum-row) (id 45))))))
               (args ((Dimension ((const 9) (refs ())))))))))
           ((binding ((name row) (id 51)))
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
             (((binding ((name row) (id 53)))
               (value (Ref ((id ((name row) (id 51)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 52))))))
               (args (((id ((name row) (id 53))))))
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
       (((binding ((name sum-row) (id 55)))
         (value
          (AtomAsArray
           ((element (Values ((elements ()) (type' ()))))
            (type' ((element (Tuple ())) (shape ())))))))))
      (body
       (ArrayPrimitive
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 56)))
            (value
             (Ref
              ((id ((name sum-row) (id 55)))
               (type' ((element (Tuple ())) (shape ())))))))
           ((binding ((name row) (id 62)))
            (value
             (ArrayPrimitive
              (Map
               (frameShape
                ((Add ((const 1000000) (refs ()))) (Add ((const 10) (refs ())))))
               (args ()) (iotaVar (((name iota) (id 61))))
               (body
                (Ref
                 ((id ((name iota) (id 61)))
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
             (((binding ((name row) (id 63)))
               (value
                (Ref
                 ((id ((name row) (id 62)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape
                     ((Add ((const 1000000) (refs ())))
                      (Add ((const 10) (refs ())))))))))))))
            (body
             (ArrayPrimitive
              (Map (frameShape ())
               (args
                (((binding ((name f) (id 57)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name reduce-f-arg) (id 58)))
                  (value
                   (AtomAsArray
                    ((element (Values ((elements ()) (type' ()))))
                     (type' ((element (Tuple ())) (shape ())))))))
                 ((binding ((name reduce-array-arg) (id 64)))
                  (value
                   (Ref
                    ((id ((name row) (id 63)))
                     (type'
                      ((element (Literal IntLiteral))
                       (shape ((Add ((const 10) (refs ())))))))))))))
               (body
                (ArrayPrimitive
                 (Reduce
                  (arg
                   ((firstBinding ((name reduce-arg1) (id 65)))
                    (secondBinding ((name reduce-arg2) (id 66)))
                    (value
                     (Ref
                      ((id ((name reduce-array-arg) (id 64)))
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
                              ((id ((name reduce-arg1) (id 65)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))
                          (ArrayAsAtom
                           ((array
                             (Ref
                              ((id ((name reduce-arg2) (id 66)))
                               (type'
                                ((element (Literal IntLiteral)) (shape ()))))))
                            (type' (Literal IntLiteral))))))
                        (type' (Literal IntLiteral)))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (d ((const 10) (refs ()))) (itemPad ()) (cellShape ())
                  (associative true) (character Reduce)
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
    (let ((sum-row.67 (values)))
     (let ((sum-row.55 sum-row.67))
      (let
       ((f.68 sum-row.55)
        (row.73
         (let ()
          (#0
           (#0
            (loop-block (frame-shape 1000000)
             (map () (iota iota.70)
              (#0
               (#0
                (loop-block (frame-shape 10)
                 (map () (iota (iota.72 : iota.70))
                  (let ((iota.61 iota.72)) iota.61))
                 (body-matcher map-result.71) (map-result (map-result.71))
                 (consumer (values))))))
             (body-matcher map-result.69) (map-result (map-result.69))
             (consumer (values))))))))
       (let ((f.56 f.68) (row.62 row.73))
        (let ((row.74 row.62))
         (#0
          (#0
           (loop-block (frame-shape 1000000)
            (map ((row.76 row.74))
             (let ((row.63 row.76))
              (let
               ((f.77 (values)) (reduce-f-arg.78 (values))
                (reduce-array-arg.79 row.63))
               (let
                ((f.57 f.77) (reduce-f-arg.58 reduce-f-arg.78)
                 (reduce-array-arg.64 reduce-array-arg.79))
                (let ((reduce-arg.81 reduce-array-arg.64))
                 (#1
                  (loop-block (frame-shape 10)
                   (map ((reduce-arg.82 reduce-arg.81)) (values reduce-arg.82))
                   (body-matcher (reduce-arg.80)) (map-result ())
                   (consumer
                    (reduce (shape) (reduce-arg1.65 reduce-arg2.66 reduce-arg.80)
                     (+ reduce-arg1.65 reduce-arg2.66))))))))))
            (body-matcher map-result.75) (map-result (map-result.75))
            (consumer (values))))))))))
    Result of stage Fuse and Simplify:
    (#0
     (#0
      (loop-block (frame-shape 1000000)
       (map () (iota iota.70)
        (#1
         (loop-block (frame-shape 10) (map () (iota (iota.72 : iota.70)) iota.72)
          (body-matcher reduce-arg.80) (map-result ())
          (consumer
           (reduce (shape) (reduce-arg1.65 reduce-arg2.66 reduce-arg.80)
            (+ reduce-arg1.65 reduce-arg2.66))))))
       (body-matcher map-result.75) (map-result (map-result.75))
       (consumer (values)))))
    Result of stage Kernelize:
    (#0
     (kernel (blocks 80) (threads 128)
      (map-kernel (frame-shape 1000000) () (iota iota.70)
       (body-matcher map-result.75) (map-result (map-result.75))
       (#1
        (loop-block (frame-shape 10) (map () (iota (iota.72 : iota.70)) iota.72)
         (body-matcher reduce-arg.80) (map-result ())
         (consumer
          (reduce (shape) (reduce-arg1.65 reduce-arg2.66 reduce-arg.80)
           (+ reduce-arg1.65 reduce-arg2.66))))))))
    Result of stage Alloc:
    (mem-let
     ((map-mem.121 (Malloc (hostOrDevice MallocDevice) (type' (Tuple ()))))
      (map-mem.116
       (Malloc (hostOrDevice MallocHost)
        (type' (Array ((element (Literal IntLiteral)) (size 1000000)))))))
     (#0
      (begin
       (kernel (blocks 80) (threads 128)
        (map-kernel (frame-shape 1000000)
         ((map-mem.117
           (Ref (id ((name map-mem) (id 116)))
            (type' (Array ((element (Literal IntLiteral)) (size 1000000))))))
          (map-mem.118 (Ref (id ((name map-mem) (id 121))) (type' (Tuple ())))))
         (iota iota.70) (body-matcher map-result.75) (map-result (map-result.75))
         (map-result-mem
          (Values
           (elements
            ((Ref (id ((name map-mem) (id 116)))
              (type' (Array ((element (Literal IntLiteral)) (size 1000000)))))))
           (type'
            (Tuple ((Array ((element (Literal IntLiteral)) (size 1000000))))))))
         ((statement
           (begin-do
            (putmem
             (loop (frame-shape 10)
              (map
               ((map-mem.119
                 (Ref (id ((name map-mem) (id 118))) (type' (Tuple ())))))
               (iota (iota.72 : iota.70)) iota.72)
              (body-matcher reduce-arg.80) (map-result ())
              (map-result-mem
               (Ref (id ((name map-mem) (id 118))) (type' (Tuple ()))))
              (consumer
               (reduce (shape) (reduce-arg1.65 reduce-arg2.66 reduce-arg.80)
                (+ reduce-arg1.65 reduce-arg2.66))))
             (Ref (id ((name map-mem) (id 117))) (type' (Literal IntLiteral))))))
          (sub-maps))))
       (getmem
        (Values
         (elements
          ((Ref (id ((name map-mem) (id 116)))
            (type' (Array ((element (Literal IntLiteral)) (size 1000000)))))))
         (type'
          (Tuple ((Array ((element (Literal IntLiteral)) (size 1000000)))))))))))
    Result of stage Capture:
    (mem-let
     ((map-mem.121 (Malloc (hostOrDevice MallocDevice) (type' (Tuple ()))))
      (map-mem.116
       (Malloc (hostOrDevice MallocHost)
        (type' (Array ((element (Literal IntLiteral)) (size 1000000)))))))
     (#0
      (begin
       (kernel captures ((expr-captures ()) (index-captures ())) (blocks 80)
        (threads 128)
        (map-kernel (frame-shape 1000000)
         ((map-mem.117
           (Ref (id ((name map-mem) (id 116)))
            (type' (Array ((element (Literal IntLiteral)) (size 1000000))))))
          (map-mem.118 (Ref (id ((name map-mem) (id 121))) (type' (Tuple ())))))
         (iota iota.70) (body-matcher map-result.75) (map-result (map-result.75))
         (map-result-mem
          (Values
           (elements
            ((Ref (id ((name map-mem) (id 116)))
              (type' (Array ((element (Literal IntLiteral)) (size 1000000)))))))
           (type'
            (Tuple ((Array ((element (Literal IntLiteral)) (size 1000000))))))))
         ((statement
           (begin-do
            (putmem
             (loop (frame-shape 10)
              (map
               ((map-mem.119
                 (Ref (id ((name map-mem) (id 118))) (type' (Tuple ())))))
               (iota (iota.72 : iota.70)) iota.72)
              (body-matcher reduce-arg.80) (map-result ())
              (map-result-mem
               (Ref (id ((name map-mem) (id 118))) (type' (Tuple ()))))
              (consumer
               (reduce (shape) (reduce-arg1.65 reduce-arg2.66 reduce-arg.80)
                (+ reduce-arg1.65 reduce-arg2.66))))
             (Ref (id ((name map-mem) (id 117))) (type' (Literal IntLiteral))))))
          (sub-maps))))
       (getmem
        (Values
         (elements
          ((Ref (id ((name map-mem) (id 116)))
            (type' (Array ((element (Literal IntLiteral)) (size 1000000)))))))
         (type'
          (Tuple ((Array ((element (Literal IntLiteral)) (size 1000000))))))))))) |}]
;;
