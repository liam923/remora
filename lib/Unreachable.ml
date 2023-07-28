open! Base

let raise () = Error.raise (Error.of_string "Unreachable code reacher")
let raiseStr str = Error.raise (Error.of_string str)
