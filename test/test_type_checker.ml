open! Base
open Remora

let%expect_test "check sort" =
  let checkAndPrint str =
    match Parser.Unit.IndexParser.parseString str with
    | MOk index ->
      (match TypeChecker.checkSort index with
      | MOk indexTyped ->
        [%sexp_of: Ast.Typed.Index.t] indexTyped
        |> Sexp.to_string_hum
        |> Stdio.print_endline
      | Errors errs ->
        NeList.iter errs ~f:(fun err ->
            Stdio.prerr_endline [%string "Error: %{TypeChecker.errorMessage err.elem}"]))
    | Errors errs ->
      let errsSexp = [%sexp_of: (string * unit) list] (NeList.to_list errs) in
      let errsStr = Sexp.to_string_hum errsSexp in
      Error.raise (Error.of_string [%string "Got errors: `%{errsStr}` parsing `%{str}`"])
  in
  checkAndPrint {| 5 |};
  [%expect {| (Dimension ((const 5) (refs ()))) |}];
  checkAndPrint {| (+ 5 10) |};
  [%expect {| (Dimension ((const 15) (refs ()))) |}];
  checkAndPrint {| (shape 5 10 15) |};
  [%expect
    {|
    (Shape
     ((Add ((const 5) (refs ()))) (Add ((const 10) (refs ())))
      (Add ((const 15) (refs ()))))) |}];
  checkAndPrint {| [5 10 15] |};
  [%expect
    {|
    (Shape
     ((Add ((const 5) (refs ()))) (Add ((const 10) (refs ())))
      (Add ((const 15) (refs ()))))) |}];
  checkAndPrint {| (++ [1 2] [3 4]) |};
  [%expect
    {|
    (Shape
     ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
      (Add ((const 3) (refs ()))) (Add ((const 4) (refs ()))))) |}];
  checkAndPrint {| (++ 1 2 3) |};
  [%expect {| Error: Unexpected sort: expected `Dim`, got `Shape` |}];
  checkAndPrint {| x |};
  [%expect {| Error: Unbound index variable `x` |}];
  checkAndPrint {| (+ [1 2 3] 4) |};
  [%expect {| Error: Unexpected sort: expected `Dim`, got `Shape` |}]
;;

let%expect_test "check kind" =
  let checkAndPrint str =
    match Parser.Unit.TypeParser.parseString str with
    | MOk type' ->
      (match TypeChecker.checkKind type' with
      | MOk typeTyped ->
        [%sexp_of: Ast.Typed.Type.t] typeTyped
        |> Sexp.to_string_hum
        |> Stdio.print_endline
      | Errors errs ->
        NeList.iter errs ~f:(fun err ->
            Stdio.prerr_endline [%string "Error: %{TypeChecker.errorMessage err.elem}"]))
    | Errors errs ->
      let errsSexp = [%sexp_of: (string * unit) list] (NeList.to_list errs) in
      let errsStr = Sexp.to_string_hum errsSexp in
      Error.raise (Error.of_string [%string "Got errors: `%{errsStr}` parsing `%{str}`"])
  in
  checkAndPrint {| int |};
  [%expect {| (Atom (AtomRef ((name int) (id 1)))) |}];
  checkAndPrint {| char |};
  [%expect {| (Atom (AtomRef ((name char) (id 2)))) |}];
  checkAndPrint {| foo |};
  [%expect {| Error: Unbound type variable `foo` |}];
  checkAndPrint {| (Arr int [1 2 3]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape
        ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))
         (Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| (-> (int int) int) |};
  [%expect
    {|
    (Atom
     (Func
      ((parameters
        ((Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))
         (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))))
       (return (Arr ((element (AtomRef ((name int) (id 1)))) (shape ()))))))) |}];
  checkAndPrint {| (Forall (@t) @t) |};
  [%expect
    {|
    (Atom
     (Forall
      ((parameters (((binding ((name @t) (id 7))) (bound Array))))
       (body (ArrayRef ((name @t) (id 7))))))) |}];
  checkAndPrint {| (Arr (Forall (@t) @t) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Forall
         ((parameters (((binding ((name @t) (id 7))) (bound Array))))
          (body (ArrayRef ((name @t) (id 7)))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Forall (t) t) |};
  [%expect
    {|
    (Atom
     (Forall
      ((parameters (((binding ((name t) (id 7))) (bound Atom))))
       (body (Arr ((element (AtomRef ((name t) (id 7)))) (shape ()))))))) |}];
  checkAndPrint {| (Pi (@i) (Arr int @i)) |};
  [%expect
    {|
    (Atom
     (Pi
      ((parameters (((binding ((name @i) (id 7))) (bound Shape))))
       (body
        (Arr
         ((element (AtomRef ((name int) (id 1))))
          (shape ((ShapeRef ((name @i) (id 7))))))))))) |}];
  checkAndPrint {| (Arr (Pi (@i) [int @i]) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Pi
         ((parameters (((binding ((name @i) (id 7))) (bound Shape))))
          (body
           (Arr
            ((element (AtomRef ((name int) (id 1))))
             (shape ((ShapeRef ((name @i) (id 7)))))))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Pi (i) [int i i]) |};
  [%expect
    {|
    (Atom
     (Pi
      ((parameters (((binding ((name i) (id 7))) (bound Dim))))
       (body
        (Arr
         ((element (AtomRef ((name int) (id 1))))
          (shape
           ((Add ((const 0) (refs ((((name i) (id 7)) 1)))))
            (Add ((const 0) (refs ((((name i) (id 7)) 1))))))))))))) |}];
  checkAndPrint {| (Sigma (@i) (Arr int @i)) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters (((binding ((name @i) (id 7))) (bound Shape))))
       (body
        (Arr
         ((element (AtomRef ((name int) (id 1))))
          (shape ((ShapeRef ((name @i) (id 7))))))))))) |}];
  checkAndPrint {| (Arr (Sigma (@i) [int @i]) [1 2]) |};
  [%expect
    {|
    (Array
     (Arr
      ((element
        (Sigma
         ((parameters (((binding ((name @i) (id 7))) (bound Shape))))
          (body
           (Arr
            ((element (AtomRef ((name int) (id 1))))
             (shape ((ShapeRef ((name @i) (id 7)))))))))))
       (shape ((Add ((const 1) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| (Sigma (i) [int i i]) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters (((binding ((name i) (id 7))) (bound Dim))))
       (body
        (Arr
         ((element (AtomRef ((name int) (id 1))))
          (shape
           ((Add ((const 0) (refs ((((name i) (id 7)) 1)))))
            (Add ((const 0) (refs ((((name i) (id 7)) 1))))))))))))) |}];
  checkAndPrint {| (Sigma (@i j) (Arr int [j @i 5 (+ j 10)])) |};
  [%expect
    {|
    (Atom
     (Sigma
      ((parameters
        (((binding ((name @i) (id 7))) (bound Shape))
         ((binding ((name j) (id 8))) (bound Dim))))
       (body
        (Arr
         ((element (AtomRef ((name int) (id 1))))
          (shape
           ((Add ((const 0) (refs ((((name j) (id 8)) 1)))))
            (ShapeRef ((name @i) (id 7))) (Add ((const 5) (refs ())))
            (Add ((const 10) (refs ((((name j) (id 8)) 1))))))))))))) |}]
;;

let%expect_test "check type" =
  let checkAndPrint str =
    match Parser.Unit.parseString str with
    | MOk expr ->
      (match TypeChecker.checkType expr with
      | MOk exprTyped ->
        [%sexp_of: Ast.Typed.Expr.t] exprTyped
        |> Sexp.to_string_hum
        |> Stdio.print_endline;
        [%sexp_of: Ast.Typed.Type.t] (Ast.Typed.Expr.type' exprTyped)
        |> Sexp.to_string_hum
        |> fun typeStr -> [%string "Type: %{typeStr}"] |> Stdio.print_endline
      | Errors errs ->
        NeList.iter errs ~f:(fun err ->
            Stdio.prerr_endline [%string "Error: %{TypeChecker.errorMessage err.elem}"]))
    | Errors errs ->
      let errsSexp = [%sexp_of: (string * unit) list] (NeList.to_list errs) in
      let errsStr = Sexp.to_string_hum errsSexp in
      Error.raise (Error.of_string [%string "Got errors: `%{errsStr}` parsing `%{str}`"])
  in
  checkAndPrint {| 5 |};
  [%expect {|
    (Atom (Literal ((value (IntLiteral 5)))))
    Type: (Atom (AtomRef ((name int) (id 1)))) |}];
  checkAndPrint {| [1 2 3 4 5] |};
  [%expect
    {|
    (Array
     (Arr
      ((dimensions (5))
       (elements
        ((Literal ((value (IntLiteral 1)))) (Literal ((value (IntLiteral 2))))
         (Literal ((value (IntLiteral 3)))) (Literal ((value (IntLiteral 4))))
         (Literal ((value (IntLiteral 5)))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 5) (refs ())))))))) |}];
  checkAndPrint {| [[1 2] [3 4]] |};
  [%expect
    {|
    (Array
     (Arr
      ((dimensions (2 2))
       (elements
        ((Literal ((value (IntLiteral 1)))) (Literal ((value (IntLiteral 2))))
         (Literal ((value (IntLiteral 3)))) (Literal ((value (IntLiteral 4)))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 2) (refs ()))) (Add ((const 2) (refs ())))))))) |}];
  checkAndPrint {| [[1 2 3] [4 5]] |};
  [%expect
    {| Error: Mismatched frame arrays; first array is type `[int 3]`, got `[int 2]` |}];
  checkAndPrint {| [[1 2] "hi"] |};
  [%expect
    {| Error: Mismatched frame arrays; first array is type `[int 2]`, got `[char 2]` |}];
  checkAndPrint {| (+ 1 2) |};
  [%expect {|
    (Array
     (TermApplication
      ((func (Ref ((id ((name +) (id 3))))))
       (args
        ((Arr ((dimensions ()) (elements ((Literal ((value (IntLiteral 1))))))))
         (Arr ((dimensions ()) (elements ((Literal ((value (IntLiteral 2)))))))))))))
    Type: (Array (Arr ((element (AtomRef ((name int) (id 1)))) (shape ())))) |}];
  checkAndPrint {| (+ [1 2 3] [4 5 6]) |};
  [%expect {|
    (Array
     (TermApplication
      ((func (Ref ((id ((name +) (id 3))))))
       (args
        ((Arr
          ((dimensions (3))
           (elements
            ((Literal ((value (IntLiteral 1))))
             (Literal ((value (IntLiteral 2))))
             (Literal ((value (IntLiteral 3))))))))
         (Arr
          ((dimensions (3))
           (elements
            ((Literal ((value (IntLiteral 4))))
             (Literal ((value (IntLiteral 5))))
             (Literal ((value (IntLiteral 6)))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 3) (refs ())))))))) |}];
  checkAndPrint {| [(+ [1 2 3] [4 5 6]) [7 8 9]] |};
  [%expect {|
    (Array
     (Frame
      ((dimensions (2))
       (arrays
        ((TermApplication
          ((func (Ref ((id ((name +) (id 3))))))
           (args
            ((Arr
              ((dimensions (3))
               (elements
                ((Literal ((value (IntLiteral 1))))
                 (Literal ((value (IntLiteral 2))))
                 (Literal ((value (IntLiteral 3))))))))
             (Arr
              ((dimensions (3))
               (elements
                ((Literal ((value (IntLiteral 4))))
                 (Literal ((value (IntLiteral 5))))
                 (Literal ((value (IntLiteral 6))))))))))))
         (Arr
          ((dimensions (3))
           (elements
            ((Literal ((value (IntLiteral 7))))
             (Literal ((value (IntLiteral 8))))
             (Literal ((value (IntLiteral 9)))))))))))))
    Type: (Array
     (Arr
      ((element (AtomRef ((name int) (id 1))))
       (shape ((Add ((const 2) (refs ()))) (Add ((const 3) (refs ())))))))) |}]
;;
