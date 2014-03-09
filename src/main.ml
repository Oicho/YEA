open Parser;;
open Lexer;;
open Typer;;
let rec scan_ast = function
  | Int i -> i
  | Add (a, b) -> scan_ast a + scan_ast b
  | Mul (a, b) -> scan_ast a * scan_ast  b;;

let _ =
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.program Lexer.lex lexbuf in
        print_int (scan_ast result);
        print_newline()
    done

