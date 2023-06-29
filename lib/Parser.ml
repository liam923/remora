open! Base
open MResult
open MResult.Let_syntax

module type S = sig
  type source
  type error = string * source
  type result = (source Ast.Untyped.Expr.t, error) MResult.t

  val parseTexps : source Texp.t Non_empty_list.t -> result
  val parseString : string -> result
  val parseFile : string -> result
end

module Make (SourceBuilder : Source.BuilderT) = struct
  type source = SourceBuilder.source

  module Lexer = TexpLexer.Make (SourceBuilder)
  module Parser = TexpParser.Make (SourceBuilder)

  type error = string * SourceBuilder.source
  type result = (SourceBuilder.source Ast.Untyped.Expr.t, error) MResult.t

  let parseTexps _ = Error.raise (Error.of_string "unimplemented")

  let parseBuffer lexbuf =
    try MOk (Parser.prog Lexer.read lexbuf) with
    | Lexer.SyntaxError (msg, source) -> MResult.err (msg, source)
    | Parser.Error ->
      MResult.err
        ( "Syntax error"
        , SourceBuilder.make ~start:lexbuf.lex_curr_p ~finish:lexbuf.lex_curr_p )
  ;;

  let parseString str =
    let lexbuf = Lexing.from_string ~with_positions:true str in
    let%bind texps = parseBuffer lexbuf in
    parseTexps texps
  ;;

  let parseFile filename =
    let channel = In_channel.open_text filename in
    let lexbuf = Lexing.from_channel ~with_positions:true channel in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let result = parseBuffer lexbuf in
    In_channel.close channel;
    let%bind texps = result in
    parseTexps texps
  ;;
end

module Default = Make (Source.Builder)

module Unit = Make (struct
  type source = unit

  let make ~start:_ ~finish:_ = ()
end)
