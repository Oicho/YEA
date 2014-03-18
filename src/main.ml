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
  | Int i -> print_int(i)
  | Float j -> print_float(j);;


let rec scan_ast = function
  | Var v -> Hashtbl.find var_hash v
  | Num n -> n
  | Assign (id, n) -> let new_val = (scan_ast n) in Hashtbl.add var_hash id new_val; Int(0)
  | Add (a, b) -> add (scan_ast a) (scan_ast b)
  | Mul (a, b) -> times (scan_ast a) (scan_ast  b)
  | Div (a, b) -> divide (scan_ast a) (scan_ast b);;

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.program Lexer.lex lexbuf in
        result_printer (scan_ast result);
        print_newline()
    done
  with Lexer.Eof ->
    exit 0

