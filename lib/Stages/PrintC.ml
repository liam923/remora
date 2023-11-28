open! Base
open C

module Buffer = struct
  type lines =
    | Empty
    | Line of string
    | LineSeq of
        { prev : lines
        ; next : lines
        }
    | Indent of lines

  type 'a t =
    { value : 'a
    ; lines : lines
    }

  include Monad.Make (struct
      type nonrec 'a t = 'a t

      let bind { value = inValue; lines = firstLines } ~f =
        let { value = outValue; lines = secondLines } = f inValue in
        { value = outValue; lines = LineSeq { prev = firstLines; next = secondLines } }
      ;;

      let map = `Define_using_bind
      let return value = { value; lines = Empty }
    end)

  let create ~indent { value = (); lines } =
    let indentationSize = String.length indent in
    let rec size indentCount = function
      | Empty -> 0
      | Line line -> (indentationSize * indentCount) + String.length line
      | LineSeq { prev; next } -> size indentCount prev + size indentCount next
      | Indent lines -> size (indentCount + 1) lines
    in
    let buffer = Buffer.create @@ size 0 lines in
    let rec print indentCount = function
      | Empty -> ()
      | Line line ->
        for _ = 0 to indentCount do
          Buffer.add_string buffer indent
        done;
        Buffer.add_string buffer line
      | LineSeq { prev; next } ->
        print indentCount prev;
        print indentCount next
      | Indent lines -> print (indentCount + 1) lines
    in
    print 0 lines;
    Buffer.contents buffer
  ;;

  let empty = { value = (); lines = Empty }
  let printLine line = { value = (); lines = Line line }
  let printEmptyLine = printLine ""
  let indent { value; lines } = { value; lines = Indent lines }
  let all_separated buffers = buffers |> List.intersperse ~sep:printEmptyLine |> all_unit
end

open Buffer
open Buffer.Let_syntax

let showName : name -> string = function
  | UniqueName id -> [%string "%{Identifier.name id}_%{Identifier.uniqueNum id#Int}"]
  | StrName str -> [%string "%{str}_"]
;;

let rec showType : type' -> string = function
  | Char -> "char"
  | Int64 -> "int64_t"
  | Float64 -> "double"
  | Bool -> "boolean"
  | Ptr t -> [%string "*%{showType t}"]
  | TypeRef name -> showName name
;;

let rec showExpr = function
  | Literal (Char c) -> [%string "'%{Char.escaped c}'"]
  | Literal (Int64 i) -> Int.to_string i
  | Literal (Float64 f) -> Float.to_string f
  | Literal (Bool true) -> "true"
  | Literal (Bool false) -> "false"
  | VarRef name -> showName name
  | FieldDeref { value; fieldName } -> [%string "%{showExpr value}.%{showName fieldName}"]
  | ArrayDeref { value; index } -> [%string "%{showExpr value}[%{showExpr index}]"]
  | PtrDeref expr -> [%string "*%{showExpr expr}"]
  | Ternary { cond; then'; else' } ->
    [%string "(%{showExpr cond} ? %{showExpr then'} : %{showExpr else'})"]
  | FunCall { fun'; args } ->
    let argsStr = args |> List.map ~f:showExpr |> String.concat ~sep:", " in
    [%string "%{showName fun'}(%{argsStr})"]
  | KernelLaunch { kernel; blocks; threads; args } ->
    let argsStr = args |> List.map ~f:showExpr |> String.concat ~sep:", " in
    [%string "%{showName kernel}<<<%{blocks#Int}, %{threads#Int}>>>(%{argsStr})"]
  | Binop { op; arg1; arg2 } -> [%string "(%{showExpr arg1} %{op} %{showExpr arg2})"]
;;

let rec printStatement = function
  | Return expr -> printLine [%string "return %{showExpr expr};"]
  | Define { name; type'; value = None } ->
    printLine [%string "%{showType type'} %{showName name};"]
  | Define { name; type'; value = Some rhs } ->
    printLine [%string "%{showType type'} %{showName name} = %{showExpr rhs};"]
  | Assign (lhs, rhs) -> printLine [%string "%{showExpr lhs} = %{showExpr rhs};"]
  | Ite { cond; thenBranch; elseBranch } ->
    let%bind () = printLine [%string "if (%{showExpr cond}) {"] in
    let%bind () = indent @@ printBlock thenBranch in
    let%bind () = printLine [%string "} else {"] in
    let%bind () = indent @@ printBlock elseBranch in
    let%bind () = printLine [%string "}"] in
    return ()
  | Eval expr -> printLine [%string "%{showExpr expr};"]

and printBlock block = block |> List.map ~f:printStatement |> Buffer.all_unit

let printFunDec { name; value = { isKernel; args; returnType; body } } =
  let printArgs args =
    args
    |> List.map ~f:(fun ({ name; type' } : funArg) ->
      [%string "%{showType type'} %{showName name}"])
    |> String.concat ~sep:", "
  in
  let kernel = if isKernel then "__global__ " else "" in
  let%bind () =
    printLine
      [%string "%{kernel}%{showType returnType} %{showName name}(%{printArgs args}) {"]
  in
  let%bind () = indent @@ printBlock body in
  let%bind () = printLine "}" in
  return ()
;;

let printStructDec { name; value = fields } =
  let printField { name; type' } =
    printLine [%string "%{showType type'} %{showName name}"]
  in
  let%bind () = printLine [%string "struct %{showName name} {"] in
  let%bind () = fields |> List.map ~f:printField |> all_unit in
  let%bind () = printLine "}" in
  return ()
;;

let printMain = function
  | None -> empty
  | Some body ->
    let%bind () = printLine "int main() {" in
    let%bind () = indent @@ printBlock body in
    let%bind () = printLine "}" in
    return ()
;;

let printProgram { structDecs; funDecs; main } =
  let%bind () = structDecs |> List.map ~f:printStructDec |> Buffer.all_separated in
  let%bind () = funDecs |> List.map ~f:printFunDec |> Buffer.all_separated in
  let%bind () = printMain main in
  return ()
;;

let printC prog = printProgram prog |> Buffer.create ~indent:"    "

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = C.t
  type output = string
  type error = (SB.source option, string) Source.annotate

  let name = "Print C"
  let run input = CompilerPipeline.S.return (printC input)
end
