open Typer;;
open Parser;;
open Lexer;;
open Scoped_map;;
open Calc_visitor;;


let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    var_map#scope_begin;
    while true do
      let result = Parser.instruction Lexer.lex lexbuf in
        Calc_visitor.scan_ast result;
    done

  with Lexer.Eof ->
    exit 0

