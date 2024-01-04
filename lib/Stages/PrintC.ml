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
      | Line line -> (indentationSize * indentCount) + String.length line + 1
      | LineSeq { prev; next } -> size indentCount prev + size indentCount next
      | Indent lines -> size (indentCount + 1) lines
    in
    let buffer = Buffer.create @@ size 0 lines in
    let rec print indentCount = function
      | Empty -> ()
      | Line line ->
        for _ = 1 to indentCount do
          Buffer.add_string buffer indent
        done;
        Buffer.add_string buffer line;
        Buffer.add_string buffer "\n"
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

let showName (name : name) =
  let replacements =
    Map.of_alist_exn
      (module Char)
      [ '-', "_"; '+', "plus"; '*', "mul"; '/', "div"; '.', "_" ]
  in
  let nameStr =
    match name with
    | UniqueName id -> [%string "%{Identifier.name id}_%{Identifier.uniqueNum id#Int}"]
    | StrName str -> str
  in
  String.concat_map nameStr ~f:(fun c ->
    Map.find replacements c |> Option.value ~default:(String.of_char c))
;;

let rec showType : type' -> string = function
  | Char -> "char"
  | Int64 -> "int64_t"
  | Float64 -> "double"
  | Bool -> "bool"
  | Ptr t -> [%string "%{showType t}*"]
  | TypeRef name -> showName name
  | TypeInstantiation { base; args } ->
    let argsStr = args |> List.map ~f:showType |> String.concat ~sep:", " in
    [%string "%{showType base}<%{argsStr}>"]
;;

let rec showExpr = function
  | Literal (CharLiteral c) ->
    let rec to_string_octal i =
      if i < 8
      then Int.to_string i
      else String.concat [ to_string_octal (i / 8); Int.to_string (i % 8) ]
    in
    [%string "'\\%{Char.to_int c |> to_string_octal}'"]
  | Literal (Int64Literal i) -> Int.to_string i
  | Literal (Float64Literal f) -> Float.to_string f
  | Literal (BoolLiteral true) -> "true"
  | Literal (BoolLiteral false) -> "false"
  | VarRef name -> showName name
  | FieldDeref { value; fieldName } -> [%string "%{showExpr value}.%{showName fieldName}"]
  | PtrFieldDeref { value; fieldName } ->
    [%string "%{showExpr value}->%{showName fieldName}"]
  | ArrayDeref { value; index } -> [%string "%{showExpr value}[%{showExpr index}]"]
  | PtrDeref expr -> [%string "*%{showExpr expr}"]
  | Ternary { cond; then'; else' } ->
    [%string "(%{showExpr cond} ? %{showExpr then'} : %{showExpr else'})"]
  | FunCall { fun'; typeArgs; args } ->
    let typeArgsStr =
      match typeArgs with
      | None -> ""
      | Some typeArgs ->
        let typeArgsStr = typeArgs |> List.map ~f:showType |> String.concat ~sep:", " in
        [%string "<%{typeArgsStr}>"]
    in
    let argsStr = args |> List.map ~f:showExpr |> String.concat ~sep:", " in
    [%string "%{showName fun'}%{typeArgsStr}(%{argsStr})"]
  | KernelLaunch { kernel; blocks; threads; args } ->
    let argsStr = args |> List.map ~f:showExpr |> String.concat ~sep:", " in
    [%string "%{showName kernel}<<<%{blocks#Int}, %{threads#Int}>>>(%{argsStr})"]
  | Binop { op; arg1; arg2 } -> [%string "(%{showExpr arg1} %{op} %{showExpr arg2})"]
  | StructConstructor { type'; args } ->
    let argsStr = args |> List.map ~f:showExpr |> String.concat ~sep:", " in
    [%string "(%{showType type'} {%{argsStr}})"]
  | Arr elements ->
    let elementsStr = elements |> List.map ~f:showExpr |> String.concat ~sep:", " in
    [%string "{%{elementsStr}}"]
;;

let rec printStatement = function
  | Return expr -> printLine [%string "return %{showExpr expr};"]
  | Define { name; type'; value = rhs } ->
    let typeStr = type' |> Option.map ~f:showType |> Option.value ~default:"auto" in
    let rhsStr =
      match rhs with
      | None -> ""
      | Some rhs -> [%string " = %{showExpr rhs}"]
    in
    printLine [%string "%{typeStr} %{showName name}%{rhsStr};"]
  | DefineDetail { attributes; name; type'; value = rhs; dims } ->
    let typeStr = type' |> Option.map ~f:showType |> Option.value ~default:"auto" in
    let rhsStr =
      match rhs with
      | None -> ""
      | Some rhs -> [%string " = %{showExpr rhs}"]
    in
    let attributesStr =
      attributes |> List.map ~f:(fun at -> [%string "%{at} "]) |> String.concat
    in
    let dimsStr =
      dims |> List.map ~f:(fun dim -> [%string "[%{showExpr dim}]"]) |> String.concat
    in
    printLine [%string "%{attributesStr}%{typeStr} %{showName name}%{dimsStr}%{rhsStr};"]
  | Assign { lhs; rhs } -> printLine [%string "%{showExpr lhs} = %{showExpr rhs};"]
  | Ite { cond; thenBranch; elseBranch = [] } ->
    let%bind () = printLine [%string "if (%{showExpr cond}) {"] in
    let%bind () = indent @@ printBlock thenBranch in
    let%bind () = printLine "}" in
    return ()
  | Ite { cond; thenBranch; elseBranch } ->
    let%bind () = printLine [%string "if (%{showExpr cond}) {"] in
    let%bind () = indent @@ printBlock thenBranch in
    let%bind () = printLine [%string "} else {"] in
    let%bind () = indent @@ printBlock elseBranch in
    let%bind () = printLine [%string "}"] in
    return ()
  | Eval expr -> printLine [%string "%{showExpr expr};"]
  | StrStatement statement -> printLine statement
  | ForLoop { loopVar; loopVarType; initialValue; cond; loopVarUpdate; body } ->
    let loopVarUpdateStr =
      match loopVarUpdate with
      | IncrementOne -> [%string "++%{showName loopVar}"]
      | Increment expr -> [%string "%{showName loopVar} += %{showExpr expr}"]
    in
    let declStr =
      [%string "%{showType loopVarType} %{showName loopVar} = %{showExpr initialValue}"]
    in
    let%bind () =
      printLine [%string "for (%{declStr}; %{showExpr cond}; %{loopVarUpdateStr}) {"]
    in
    let%bind () = indent @@ printBlock body in
    let%bind () = printLine [%string "}"] in
    return ()
  | SyncThreads -> printLine "__syncthreads();"

and printBlock block = block |> List.map ~f:printStatement |> Buffer.all_unit

let printInclude include' = printLine [%string "#include %{include'}"]

let printFunDec { name; value = { params; returnType; body; funType } } =
  let printParams params =
    params
    |> List.map ~f:(fun ({ name; type' } : funParam) ->
      [%string "%{showType type'} %{showName name}"])
    |> String.concat ~sep:", "
  in
  let attributesStr =
    match funType with
    | Host -> ""
    | Device -> "__device__ "
    | HostOrDevice -> "__host__ __device__ "
    | Kernel -> "__global__ "
  in
  let retTypeStr = Option.value_map returnType ~f:showType ~default:"void" in
  let%bind () =
    printLine
      [%string "%{attributesStr}%{retTypeStr} %{showName name}(%{printParams params}) {"]
  in
  let%bind () = indent @@ printBlock body in
  let%bind () = printLine "};" in
  return ()
;;

let printStructDec { name; value = fields } =
  let printField { name; type' } =
    printLine [%string "%{showType type'} %{showName name};"]
  in
  let%bind () = printLine [%string "struct %{showName name} {"] in
  let%bind () = fields |> List.map ~f:printField |> all_unit in
  let%bind () = printLine "};" in
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

let printProgram { includes; prelude; structDecs; funDecs; main } =
  Buffer.all_separated
    ([ includes |> List.map ~f:printInclude |> Buffer.all_unit
     ; prelude |> List.map ~f:printLine |> Buffer.all_unit
     ]
     @ List.map structDecs ~f:printStructDec
     @ List.map funDecs ~f:printFunDec
     @ [ printMain main ])
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
