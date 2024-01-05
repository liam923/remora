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
    let proxiesArgs = proxies |> List.map ~f:(fun proxy -> [%string "-J %{proxy}"]) in
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
  ; "nvcc -std=c++17 -w -o prog prog.cu"
  ; "./prog"
  ]
  |> String.concat ~sep:" ; "
  |> wrapCommandInSsh ~sshConfig
  |> Sys_unix.command_exn
;;

let%expect_test "basic" =
  compileAndRun "[1 2 3 4 5]";
  [%expect {| [1, 2, 3, 4, 5] |}]
;;

let%expect_test "simple reduction" =
  compileAndRun "(reduce{int | 99999999 []} + (+ 1 iota{ | [100000000]}))";
  [%expect {| 5000050000 |}]
;;
