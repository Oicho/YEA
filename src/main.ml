open Typer;;
open Parser;;
open Lexer;;

let var_hash =  Hashtbl.create 127542;;
let last_calc = ref (Int(0));;


let bool_of_number = function
  | Int i -> (i != 0)
  | Float f -> (f  != 0.);;

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
  | Last -> !last_calc
  | Num n -> n
  | Incr (str, i, is_pre) -> 
    let prev = Hashtbl.find var_hash str in
    if (is_pre) then
    (
      let new_number = add prev (Int(i)) in
      Hashtbl.add var_hash str new_number;
      new_number
    )
    else
    (
      Hashtbl. add var_hash str (add prev (Int(i)));
      prev
    )
  | Add (a, b) -> add (scan_expr a) (scan_expr b)
  | Mul (a, b) -> times (scan_expr a) (scan_expr  b)
  | Div (a, b) -> divide (scan_expr a) (scan_expr b)
;;

let rec scan_dec =function
  | If_dec (cond, if_body, else_body) -> scan_if cond if_body else_body
  | While_dec (cond, body) -> scan_while cond body
  | Assign(st, e) -> Hashtbl.add var_hash st (scan_expr e)
and scan_instruction = function
  | Declaration d -> scan_dec d
  | Expression e -> result_printer (scan_expr e)
and scan_instruction_block = function
  | Nil -> ()
  | Instr (i, block) -> scan_instruction i; scan_instruction_block block
and scan_while cond body =
  while bool_of_number (scan_expr cond) do
    scan_instruction_block body;
  done
and scan_if cond if_body else_body= 
  if bool_of_number (scan_expr cond) then
    scan_instruction_block if_body
  else 
    scan_instruction_block else_body
;;




let rec scan_ast = function
  | Declaration(d) -> scan_dec d
  | Expression(e) -> last_calc := scan_expr e; result_printer !last_calc;;
let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.instruction Lexer.lex lexbuf in
        scan_ast result;
    done
  with Lexer.Eof ->
    exit 0

