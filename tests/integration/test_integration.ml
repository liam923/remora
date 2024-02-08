open! Base
open Remora

type sshConfig =
  { proxies : string list
  ; remote : string
  ; credentialsFiles : string list
  }
[@@deriving sexp]

let sshConfig =
  Sys.getenv "REMORA_REMOTE_CONFIG"
  |> Option.map ~f:Parsexp.Single.parse_string_exn
  |> Option.map ~f:sshConfig_of_sexp
;;

let escapeForBash =
  String.concat_map ~f:(function
    | '\\' -> "\\\\"
    | '"' -> "\\\""
    | '$' -> "\\$"
    | c -> String.of_char c)
;;

let wrapCommandInSsh ~sshConfig command =
  match sshConfig with
  | None -> command
  | Some { proxies; remote; credentialsFiles } ->
    let proxiesArgs = proxies |> List.map ~f:(fun proxy -> [%string "-J  %{proxy}"]) in
    let remoteArg = [ remote ] in
    let credsArgs = List.map credentialsFiles ~f:(fun f -> [%string "-i %{f}"]) in
    String.concat
      ~sep:" "
      ([ "ssh" ]
       @ credsArgs
       @ proxiesArgs
       @ remoteArg
       @ [ [%string "\"%{escapeForBash command}\""] ])
;;

let compileAndRun program =
  let cCode =
    match Compiler.Default.compileStringToString program with
    | MOk outProgram -> outProgram
    | Errors errors ->
      errors
      |> NeList.to_list
      |> List.map ~f:Compiler.Default.showError
      |> String.concat_lines
      |> failwith
  in
  [ "cd $(mktemp -d)"
  ; [%string "echo \"%{escapeForBash cCode}\" > prog.cu"]
  ; "nvcc -std=c++17 -arch=native -w -o prog prog.cu"
  ; "./prog"
  ]
  |> String.concat ~sep:" ; "
  |> wrapCommandInSsh ~sshConfig
  |> Sys_unix.command_exn
;;

let%expect_test "basic" =
  compileAndRun "[1 2 3 4 5]";
  [%expect {| [1 2 3 4 5] |}]
;;

let%expect_test "simple reduction" =
  compileAndRun "(reduce{int | 99999999 [] []} + (+ 1 iota{ | [100000000]}))";
  [%expect {| 5000000050000000 |}]
;;

let%expect_test "reduce zero" =
  compileAndRun "(reduce-init{int | 100000000 [] []} + 0 (+ 1 iota{ | [100000000]}))";
  [%expect {| 5000000050000000 |}]
;;

let%expect_test "simple boxes" =
  compileAndRun
    {|
    (define words
      (boxes (len) [char len] [2]
        ((3) "hey" )
        ((2) "hi" )))

    (unbox words (word len)
      (= 3 (length{char | len []} word)))
    |};
  [%expect {| [1 0] |}]
;;

let%expect_test "mean" =
  compileAndRun
    {|
    (define (mean{ | l-1} [arr [float (+ l-1 1)]])
      (/. (reduce{float | l-1 [] []} +. arr)
          (int->float (reify-dimension (+ l-1 1)))))

    (mean{ | 999} (int->float (+ 1 iota{ | [1000]})))
    |};
  [%expect {| 500.5 |}]
;;

let%expect_test "nested reduce" =
  compileAndRun
    {|
    (define (add [x [int 10]] [y [int 10]])
      (+ x y))

    (reduce{int | 999 [] [10]} add iota{ | [1000 10]})
    |};
  [%expect
    {| [4995000 4996000 4997000 4998000 4999000 5000000 5001000 5002000 5003000 5004000] |}]
;;

let%expect_test "lift" =
  compileAndRun
    {|
    (define count (reduce{int | 2 [] []} + [1 2 3]))

    (define res
      (lift [d-1 count]
        (replicate{int | [(+ d-1 1)] []} 5)))

    (unbox res (arr d-1)
      (reduce{int | d-1 [] []} + arr))
    |};
  [%expect {| 35 |}]
;;

let%expect_test "matrix multiplication" =
  compileAndRun
    {|
    (define (add{ | @s} [x [int @s]] [y [int @s]])
      (+ x y))

    (define (v*m{ | a b} [x [int a]] [y [int a b]])
      (reduce-init{int | a [] [b]} add{ | [b]} (replicate{int | [b] []} 0) (* x y)))

    (define (m*m{ | a b c} [x [int a b]] [y [int b c]])
      (v*m{ | b c} x y))

    (m*m{ | 10 2000 5} iota{ | [10 2000]} iota{ | [2000 5]})
    |};
  [%expect
    {|
    [[13323335000 13325334000 13327333000 13329332000 13331331000]
     [33313335000 33319334000 33325333000 33331332000 33337331000]
     [53303335000 53313334000 53323333000 53333332000 53343331000]
     [73293335000 73307334000 73321333000 73335332000 73349331000]
     [93283335000 93301334000 93319333000 93337332000 93355331000]
     [113273335000 113295334000 113317333000 113339332000 113361331000]
     [133263335000 133289334000 133315333000 133341332000 133367331000]
     [153253335000 153283334000 153313333000 153343332000 153373331000]
     [173243335000 173277334000 173311333000 173345332000 173379331000]
     [193233335000 193271334000 193309333000 193347332000 193385331000]] |}]
;;

let%expect_test "reverse" =
  compileAndRun {|
    (reverse{char | 5 [3]} ["hey" "sup" "who" "why" "moo"])
    |};
  [%expect {|
    [[m o o]
     [w h y]
     [w h o]
     [s u p]
     [h e y]] |}]
;;

let%expect_test "sum evens" =
  compileAndRun
    {|
    (define values (+ 1 iota{ | [1000]}))
    (define even-values (if{int | } (= (% values 2) 0) values 0))
    (reduce{int | 999 [] []} + even-values)
    |};
  [%expect {|
    250500 |}]
;;

let%expect_test "sum of sums" =
  compileAndRun
    {|
    (define sums (scan-init{int | 1000 [] []} + 0 (+ 1 iota{ | [1000]})))
    (reduce{int | 1000 [] []} + sums)
    |};
  [%expect {|
    167167000 |}]
;;

let%expect_test "sum of sums (larger)" =
  compileAndRun
    {|
    (define sums (scan-init{int | 1000000 [] []} + 0 (+ 1 iota{ | [1000000]})))
    (reduce{int | 1000000 [] []} + sums)
    |};
  [%expect {|
    166667166667000000 |}]
;;

let%expect_test "sum of sums array" =
  compileAndRun
    {|
    (define sums (scan-init{int | 1000 [5] []} + 0 (+ 1 iota{ | [1000 5]})))
    (reduce{int | 1000 [5] []} + sums)
    |};
  [%expect
    {|
    [[833833000 834333500 834834000 835334500 835835000]
     [833833000 834333500 834834000 835334500 835835000]
     [833833000 834333500 834834000 835334500 835835000]
     [833833000 834333500 834834000 835334500 835835000]
     [833833000 834333500 834834000 835334500 835835000]] |}]
;;

let%expect_test "covariance" =
  compileAndRun
    {|
    (define (mean{ | l-1} [arr [float (+ l-1 1)]])
      (/. (reduce{float | l-1 [] []} +. arr)
          (int->float (reify-dimension (+ l-1 1)))))

    (define (covariance{ | l-1} [xs [float (+ 1 l-1)]] [ys [float (+ 1 l-1)]])
      (/. (reduce{float | l-1 [] []} +.
                                  (*. (-. xs (mean{ | l-1} xs))
                                      (-. ys (mean{ | l-1} ys))))
          ; covariance divides by n-1 instead of n
          (int->float (reify-dimension l-1))))

    (covariance{ | 3} [1. 2. 3. 4.] [1. 2. 3. 5.])
    |};
  [%expect {| 2.16667 |}]
;;
