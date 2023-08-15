{
module Make (SourceBuilder : Source.BuilderT) = struct
  open Lexing
  module P = TexpParser.Make(SourceBuilder)

  exception SyntaxError of string * SourceBuilder.source
}

let int = '-'? ['0'-'9'] ['0'-'9']*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let unreservedchar = [^'(' ')' '[' ']' '{' '}' '"' ',' '\'' '`' ';' '|' '\\' ' ' '\t' '\r' '\n' '@']
let symbol = (unreservedchar | '@') unreservedchar*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  | int      { P.INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | '['      { LEFT_SQUARE }
  | ']'      { RIGHT_SQUARE }
  | '{'      { LEFT_CURLY }
  | '}'      { RIGHT_CURLY }
  | '|'      { BAR }
  | ';'      { read_single_line_comment lexbuf }
  | '#' '|'  { read_multi_line_comment lexbuf }
  | symbol   { SYMBOL (Lexing.lexeme lexbuf) }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf, SourceBuilder.make ~start:(Lexing.lexeme_start_p lexbuf) ~finish:(Lexing.lexeme_end_p lexbuf))) }
  | eof      { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf, SourceBuilder.make ~start:(Lexing.lexeme_start_p lexbuf) ~finish:(Lexing.lexeme_end_p lexbuf))) }
  | eof { raise (SyntaxError ("String is not terminated", SourceBuilder.make ~start:(Lexing.lexeme_start_p lexbuf) ~finish:(Lexing.lexeme_end_p lexbuf))) }
  
and read_single_line_comment =
  parse
  | newline { new_line lexbuf; read lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }

and read_multi_line_comment =
  parse
  | '|' '#' { read lexbuf }
  | newline { new_line lexbuf; read_multi_line_comment lexbuf }
  | eof { raise (SyntaxError ("Unterminated comment", SourceBuilder.make ~start:(Lexing.lexeme_start_p lexbuf) ~finish:(Lexing.lexeme_end_p lexbuf))) }
  | _ { read_multi_line_comment lexbuf }

{
end
}
