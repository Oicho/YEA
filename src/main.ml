open Typer;;
open Parser;;
open Lexer;;
open Scoped_map;;
open Calc_visitor;;
open Print_visitor;;

let main_loop lexbuf =
   while true do
      let result = Parser.instruction Lexer.lex lexbuf in
        Calc_visitor.scan_ast result;
    done

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    var_map#scope_begin;
    main_loop lexbuf;
   
    
  with Lexer.Eof -> exit 0
