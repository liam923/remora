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
      @> (module PrintResult (Nucleus) (Simplify.Stage (Source.UnitBuilder)))
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
       (((binding ((name f) (id 40))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 38)))
         (value (Scalar ((element (Literal (IntLiteral 1)))))))
        ((binding ((name +arg2) (id 39)))
         (value (Scalar ((element (Literal (IntLiteral 2)))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name f) (id 40))))))
         (args (((id ((name +arg1) (id 38)))) ((id ((name +arg2) (id 39))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 41)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name +arg1) (id 42)))
         (value
          (Scalar
           ((element (Literal (IntLiteral 1)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))
        ((binding ((name +arg2) (id 43)))
         (value
          (Scalar
           ((element (Literal (IntLiteral 2)))
            (type' ((element (Literal IntLiteral)) (shape ())))))))))
      (body
       (PrimitiveCall
        ((op Add)
         (args
          ((Ref
            ((id ((name +arg1) (id 42)))
             (type' ((element (Literal IntLiteral)) (shape ())))))
           (Ref
            ((id ((name +arg2) (id 43)))
             (type' ((element (Literal IntLiteral)) (shape ())))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Simplify:
    (Scalar
     ((element (Literal (IntLiteral 3)))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}]
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
     ((binding ((name add) (id 38)))
      (value
       (Scalar
        ((element
          (TermLambda
           ((params
             (((binding ((name x) (id 39)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
              ((binding ((name y) (id 40)))
               (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
            (body
             (TermApplication
              ((func (Primitive ((name (Func Add)))))
               (args
                ((Ref ((id ((name x) (id 39))))) (Ref ((id ((name y) (id 40))))))))))))))))
      (body
       (TermApplication
        ((func (Ref ((id ((name add) (id 38))))))
         (args
          ((Scalar ((element (Literal (IntLiteral 5)))))
           (Scalar ((element (Literal (IntLiteral 10))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name add) (id 38)))
         (value
          (Scalar
           ((element
             (TermLambda
              ((params
                (((binding ((name x) (id 39)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))
                 ((binding ((name y) (id 40)))
                  (bound (Arr ((element (Literal IntLiteral)) (shape ())))))))
               (body
                (Map
                 ((args
                   (((binding ((name f) (id 43)))
                     (value (Primitive ((name (Func Add))))))
                    ((binding ((name +arg1) (id 41)))
                     (value (Ref ((id ((name x) (id 39)))))))
                    ((binding ((name +arg2) (id 42)))
                     (value (Ref ((id ((name y) (id 40)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 43))))))
                     (args
                      (((id ((name +arg1) (id 41))))
                       ((id ((name +arg2) (id 42))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 46)))
            (value (Ref ((id ((name add) (id 38)))))))
           ((binding ((name x) (id 44)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))
           ((binding ((name y) (id 45)))
            (value (Scalar ((element (Literal (IntLiteral 10)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 46))))))
            (args (((id ((name x) (id 44)))) ((id ((name y) (id 45))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name add) (id 48)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 49)))
            (value
             (Ref
              ((id ((name add) (id 48)))
               (type' ((element (Literal UnitLiteral)) (shape ())))))))
           ((binding ((name x) (id 51)))
            (value
             (Scalar
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))
           ((binding ((name y) (id 53)))
            (value
             (Scalar
              ((element (Literal (IntLiteral 10)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (IntrinsicCall
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 50)))
               (value
                (Scalar
                 ((element (Literal UnitLiteral))
                  (type' ((element (Literal UnitLiteral)) (shape ())))))))
              ((binding ((name +arg1) (id 52)))
               (value
                (Ref
                 ((id ((name x) (id 51)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name +arg2) (id 54)))
               (value
                (Ref
                 ((id ((name y) (id 53)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (PrimitiveCall
              ((op Add)
               (args
                ((Ref
                  ((id ((name +arg1) (id 52)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name +arg2) (id 54)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type' ((element (Literal IntLiteral)) (shape ()))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Simplify:
    (Scalar
     ((element (Literal (IntLiteral 15)))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}]
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
     ((binding ((name id) (id 38)))
      (value
       (Scalar
        ((element
          (TypeLambda
           ((params (((binding ((name @t) (id 39))) (bound Array))))
            (body
             (Scalar
              ((element
                (TermLambda
                 ((params
                   (((binding ((name e) (id 40)))
                     (bound (ArrayRef ((name @t) (id 39)))))))
                  (body (Ref ((id ((name e) (id 40))))))))))))))))))
      (body
       (TermApplication
        ((func
          (TypeApplication
           ((tFunc (Ref ((id ((name id) (id 38))))))
            (args ((Array (Arr ((element (Literal IntLiteral)) (shape ())))))))))
         (args ((Scalar ((element (Literal (IntLiteral 5))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name id) (id 38)))
         (value
          (Scalar
           ((element
             (TypeLambda
              ((params (((binding ((name @t) (id 39))) (bound Array))))
               (body
                (Scalar
                 ((element
                   (TermLambda
                    ((params
                      (((binding ((name e) (id 40)))
                        (bound (ArrayRef ((name @t) (id 39)))))))
                     (body (Ref ((id ((name e) (id 40)))))))))))))))))))))
      (body
       (Map
        ((args
          (((binding ((name f) (id 42)))
            (value
             (TypeApplication
              ((tFunc (Ref ((id ((name id) (id 38))))))
               (args ((Array (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
           ((binding ((name e) (id 41)))
            (value (Scalar ((element (Literal (IntLiteral 5)))))))))
         (body
          (TermApplication
           ((func (Ref ((id ((name f) (id 42))))))
            (args (((id ((name e) (id 41))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (frameShape ())
         (type' (Arr ((element (Literal IntLiteral)) (shape ())))))))
      (frameShape ()) (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))
    Result of stage Inline and Monomorphize:
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name id) (id 44)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ())
         (args
          (((binding ((name f) (id 45)))
            (value
             (Ref
              ((id ((name id) (id 44)))
               (type' ((element (Literal UnitLiteral)) (shape ())))))))
           ((binding ((name e) (id 46)))
            (value
             (Scalar
              ((element (Literal (IntLiteral 5)))
               (type' ((element (Literal IntLiteral)) (shape ())))))))))
         (body
          (Ref
           ((id ((name e) (id 46)))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type' ((element (Literal IntLiteral)) (shape ()))))))
      (type' ((element (Literal IntLiteral)) (shape ())))))
    Result of stage Simplify:
    (Scalar
     ((element (Literal (IntLiteral 5)))
      (type' ((element (Literal IntLiteral)) (shape ()))))) |}]
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
       (((binding ((name f) (id 40))) (value (Primitive ((name (Func Add))))))
        ((binding ((name +arg1) (id 38)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar ((element (Literal (IntLiteral 1)))))
              (Scalar ((element (Literal (IntLiteral 2)))))))))))
        ((binding ((name +arg2) (id 39)))
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
          (((binding ((name +arg1) (id 41)))
            (value (Ref ((id ((name +arg1) (id 38)))))))
           ((binding ((name +arg2) (id 42)))
            (value (Ref ((id ((name +arg2) (id 39)))))))))
         (body
          (Map
           ((args
             (((binding ((name +arg2) (id 43)))
               (value (Ref ((id ((name +arg2) (id 42)))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 40))))))
               (args
                (((id ((name +arg1) (id 41)))) ((id ((name +arg2) (id 43))))))
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
    (IntrinsicCall
     (Map (frameShape ())
      (args
       (((binding ((name f) (id 44)))
         (value
          (Scalar
           ((element (Literal UnitLiteral))
            (type' ((element (Literal UnitLiteral)) (shape ())))))))
        ((binding ((name +arg1) (id 45)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ())))))))))))
        ((binding ((name +arg2) (id 47)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Frame
               ((dimensions (3))
                (elements
                 ((Scalar
                   ((element (Literal (IntLiteral 3)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))
                  (Scalar
                   ((element (Literal (IntLiteral 4)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))
                  (Scalar
                   ((element (Literal (IntLiteral 5)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))))
                (type'
                 ((element (Literal IntLiteral))
                  (shape ((Add ((const 3) (refs ())))))))))
              (Frame
               ((dimensions (3))
                (elements
                 ((Scalar
                   ((element (Literal (IntLiteral 6)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))
                  (Scalar
                   ((element (Literal (IntLiteral 7)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))
                  (Scalar
                   ((element (Literal (IntLiteral 8)))
                    (type' ((element (Literal IntLiteral)) (shape ())))))))
                (type'
                 ((element (Literal IntLiteral))
                  (shape ((Add ((const 3) (refs ())))))))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ((Add ((const 2) (refs ())))))
         (args
          (((binding ((name +arg1) (id 46)))
            (value
             (Ref
              ((id ((name +arg1) (id 45)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 2) (refs ())))))))))))
           ((binding ((name +arg2) (id 48)))
            (value
             (Ref
              ((id ((name +arg2) (id 47)))
               (type'
                ((element (Literal IntLiteral))
                 (shape
                  ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
         (body
          (IntrinsicCall
           (Map (frameShape ((Add ((const 3) (refs ())))))
            (args
             (((binding ((name +arg2) (id 49)))
               (value
                (Ref
                 ((id ((name +arg2) (id 48)))
                  (type'
                   ((element (Literal IntLiteral))
                    (shape ((Add ((const 3) (refs ())))))))))))))
            (body
             (PrimitiveCall
              ((op Add)
               (args
                ((Ref
                  ((id ((name +arg1) (id 46)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name +arg2) (id 49)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
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
    Result of stage Simplify:
    (IntrinsicCall
     (Map (frameShape ((Add ((const 2) (refs ())))))
      (args
       (((binding ((name +arg1) (id 46)))
         (value
          (Frame
           ((dimensions (2))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 1)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 2)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ())))))))))))
        ((binding ((name +arg2) (id 48)))
         (value
          (Frame
           ((dimensions (2 3))
            (elements
             ((Scalar
               ((element (Literal (IntLiteral 3)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 4)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 5)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 6)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 7)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Scalar
               ((element (Literal (IntLiteral 8)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type'
             ((element (Literal IntLiteral))
              (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))))))))
      (body
       (IntrinsicCall
        (Map (frameShape ((Add ((const 3) (refs ())))))
         (args
          (((binding ((name +arg2) (id 49)))
            (value
             (Ref
              ((id ((name +arg2) (id 48)))
               (type'
                ((element (Literal IntLiteral))
                 (shape ((Add ((const 3) (refs ())))))))))))))
         (body
          (PrimitiveCall
           ((op Add)
            (args
             ((Ref
               ((id ((name +arg1) (id 46)))
                (type' ((element (Literal IntLiteral)) (shape ())))))
              (Ref
               ((id ((name +arg2) (id 49)))
                (type' ((element (Literal IntLiteral)) (shape ())))))))
            (type' ((element (Literal IntLiteral)) (shape ()))))))
         (type'
          ((element (Literal IntLiteral)) (shape ((Add ((const 3) (refs ()))))))))))
      (type'
       ((element (Literal IntLiteral))
        (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ()))))))))) |}]
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
     ((binding ((name words) (id 38)))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 39)) 1)))))))))))))))
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
                   (shape ((Add ((const 0) (refs ((((name len) (id 39)) 1))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings (((name len) (id 40))))
         (valueBinding ((name word) (id 41)))
         (box (Ref ((id ((name words) (id 38))))))
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
                       ((Dimension ((const 0) (refs ((((name len) (id 40)) 1)))))
                        (Shape ()))))))
                   (args ((Atom (Literal CharacterLiteral)))))))
                (args ((Ref ((id ((name word) (id 41)))))))))))))))))))
    Result of stage Explicitize:
    (Map
     ((args
       (((binding ((name words) (id 38)))
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
                       ((Add ((const 0) (refs ((((name len) (id 39)) 1)))))))))))))))
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
                       ((Add ((const 0) (refs ((((name len) (id 39)) 1)))))))))))))))))))))))
      (body
       (Unbox
        ((indexBindings (((name len) (id 40))))
         (valueBinding ((name word) (id 41)))
         (box (Ref ((id ((name words) (id 38))))))
         (body
          (Map
           ((args
             (((binding ((name f) (id 46)))
               (value (Primitive ((name (Func Equal))))))
              ((binding ((name =arg1) (id 42)))
               (value (Scalar ((element (Literal (IntLiteral 3)))))))
              ((binding ((name =arg2) (id 45)))
               (value
                (Map
                 ((args
                   (((binding ((name f) (id 44)))
                     (value
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
                                        (((binding ((name t) (id 2)))
                                          (bound Atom))))
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
                                                       (refs
                                                        ((((name d) (id 0)) 1)))))
                                                     (ShapeRef
                                                      ((name @cell-shape) (id 1)))))))))))
                                             (body
                                              (ReifyIndex
                                               ((index
                                                 (Dimension
                                                  ((const 0)
                                                   (refs ((((name d) (id 0)) 1)))))))))))))))))))))))))))
                           (args
                            ((Dimension
                              ((const 0) (refs ((((name len) (id 40)) 1)))))
                             (Shape ()))))))
                        (args ((Atom (Literal CharacterLiteral))))))))
                    ((binding ((name arr) (id 43)))
                     (value (Ref ((id ((name word) (id 41)))))))))
                  (body
                   (TermApplication
                    ((func (Ref ((id ((name f) (id 44))))))
                     (args (((id ((name arr) (id 43))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (frameShape ())
                  (type' (Arr ((element (Literal IntLiteral)) (shape ()))))))))))
            (body
             (TermApplication
              ((func (Ref ((id ((name f) (id 46))))))
               (args
                (((id ((name =arg1) (id 42)))) ((id ((name =arg2) (id 45))))))
               (type' ((element (Literal BooleanLiteral)) (shape ()))))))
            (frameShape ())
            (type' (Arr ((element (Literal BooleanLiteral)) (shape ()))))))))))
      (frameShape ())
      (type'
       (Arr
        ((element (Literal BooleanLiteral))
         (shape ((Add ((const 2) (refs ()))))))))))
    Result of stage Inline and Monomorphize:
    (IntrinsicCall
     (Map (frameShape ()) (args ())
      (body
       (Unbox
        ((indexBindings (((name len) (id 40)))) (boxBindings ())
         (body
          (IntrinsicCall
           (Map (frameShape ())
            (args
             (((binding ((name f) (id 47)))
               (value
                (Scalar
                 ((element (Literal UnitLiteral))
                  (type' ((element (Literal UnitLiteral)) (shape ())))))))
              ((binding ((name =arg1) (id 48)))
               (value
                (Scalar
                 ((element (Literal (IntLiteral 3)))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))
              ((binding ((name =arg2) (id 51)))
               (value
                (IntrinsicCall
                 (Map (frameShape ())
                  (args
                   (((binding ((name f) (id 50)))
                     (value
                      (Scalar
                       ((element (Literal UnitLiteral))
                        (type' ((element (Literal UnitLiteral)) (shape ())))))))))
                  (body
                   (ReifyIndex
                    ((index
                      (Dimension ((const 0) (refs ((((name len) (id 40)) 1))))))
                     (type' ((element (Literal IntLiteral)) (shape ()))))))
                  (type' ((element (Literal IntLiteral)) (shape ())))))))))
            (body
             (PrimitiveCall
              ((op Equal)
               (args
                ((Ref
                  ((id ((name =arg1) (id 48)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))
                 (Ref
                  ((id ((name =arg2) (id 51)))
                   (type' ((element (Literal IntLiteral)) (shape ())))))))
               (type' ((element (Literal BooleanLiteral)) (shape ()))))))
            (type' ((element (Literal BooleanLiteral)) (shape ()))))))
         (type'
          ((element (Literal BooleanLiteral))
           (shape ((Add ((const 2) (refs ()))))))))))
      (type'
       ((element (Literal BooleanLiteral)) (shape ((Add ((const 2) (refs ())))))))))
    Result of stage Simplify:
    (Unbox
     ((indexBindings (((name len) (id 40)))) (boxBindings ())
      (body
       (PrimitiveCall
        ((op Equal)
         (args
          ((Scalar
            ((element (Literal (IntLiteral 3)))
             (type' ((element (Literal IntLiteral)) (shape ())))))
           (ReifyIndex
            ((index (Dimension ((const 0) (refs ((((name len) (id 40)) 1))))))
             (type' ((element (Literal IntLiteral)) (shape ())))))))
         (type' ((element (Literal BooleanLiteral)) (shape ()))))))
      (type'
       ((element (Literal BooleanLiteral)) (shape ((Add ((const 2) (refs ()))))))))) |}]
;;
