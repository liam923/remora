open! Base
(* open Remora *)

(* let () =
  let args = Sys.get_argv () in
  let channel =
    match Array.to_list args with
    | _ :: filename :: _ -> Stdio.In_channel.create filename
    | _ -> Stdio.stdin
  in
  let program = Stdio.In_channel.input_all channel in
  match Compiler.Default.compileStringToString program with
  | MOk outProgram -> Stdio.print_endline outProgram
  | Errors errors ->
    NeList.iter errors ~f:(fun { elem = error; source } ->
      match source with
      | Some { start; finish } ->
        let showPos (pos : Lexing.position) =
          [%string "%{pos.pos_lnum#Int}:%{(pos.pos_cnum - pos.pos_bol)#Int}"]
        in
        Stdio.prerr_endline
          [%string "Error (%{showPos start} - %{showPos finish}): %{error}"]
      | None -> Stdio.prerr_endline [%string "Error: %{error}"]);
    Stdlib.exit 1
;; *)

type foo =
  { hello : string
  ; world : string
  }
[@@deriving sexp]

let () =
  { hello = "hello"; world = "world" }
  |> [%sexp_of: foo]
  |> Sexp.to_string_hum
  |> Stdio.print_endline
;;

let () =
  "((hello hello) (world world))"
  |> Parsexp.Single.parse_string_exn
  |> foo_of_sexp
  |> [%sexp_of: foo]
  |> Sexp.to_string_hum
  |> Stdio.print_endline
;;
