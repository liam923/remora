open! Base
open Remora

let () =
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
    NeList.iter errors ~f:(fun { elem = error; source = _ } ->
        Stdio.prerr_endline [%string "Error: %{error}"]);
    Caml.exit 1
;;
