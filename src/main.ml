open Typer;;
open Parser;;
open Lexer;;

let var_hash =  Hashtbl.create 127542;;


let add a  b=
  match a with
  | Int i->
      begin match b with
      | Int j-> Int (i + j)
      | Float j-> Float ((float_of_int i) +. j)
      end
  | Float i->
      begin match b with
      | Int j-> Float (i +. (float_of_int j))
      | Float j-> Float (i +. j)
      end;;
let times a  b=
  match a with
  | Int i->
      begin match b with
      | Int j-> Int (i * j)
      | Float j-> Float ((float_of_int i) *. j)
      end
  | Float i->
      begin match b with
      | Int j-> Float (i *. (float_of_int j))
      | Float j-> Float (i *. j)
      end;;
let divide a  b=
  match a with
  | Int i->
      begin match b with
      | Int j-> Int (i / j)
      | Float j-> Float ((float_of_int i) /. j)
      end
  | Float i->
      begin match b with
      | Int j-> Float (i /. (float_of_int j))
      | Float j-> Float (i /. j)
      end;;

let result_printer = function
  | Int i -> print_int(i); print_newline()
  | Float j -> print_float(j); print_newline();;


let rec scan_expr = function
  | Var v -> Hashtbl.find var_hash v
  | Num n -> n
  | Add (a, b) -> add (scan_expr a) (scan_expr b)
  | Mul (a, b) -> times (scan_expr a) (scan_expr  b)
  | Div (a, b) -> divide (scan_expr a) (scan_expr b);;
let rec scan_dec =function
  | Assign(st, e) -> Hashtbl.add var_hash st (scan_expr e);;
let rec scan_ast = function
  | Declaration(d) -> scan_dec d
  | Expression(e) -> result_printer(scan_expr e);;
let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.program Lexer.lex lexbuf in
        scan_ast result;
    done
  with Lexer.Eof ->
    exit 0

