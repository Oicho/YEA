type number = Int of int | Float of float;;

type expr = 
    Var of string
  | Last
  | Incr of string * int *bool
  | Num of number
  | Add of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
and dec = 
    Assign of string * expr 
  | If_dec of expr * instruction_block * instruction_block
  | While_dec of expr * instruction_block
and instruction =  Declaration of dec| Expression of expr
and instruction_block = Nil| Instr of instruction * instruction_block;;