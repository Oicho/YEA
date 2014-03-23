type number = Int of int | Float of float;;
type expr = 
    Var of string
  | Last
  | Num of number
  | Add of expr * expr
  | Mul of expr * expr
  | Div of expr * expr;;
type dec = Assign of string * expr;;
type program =  Nil| Declaration of dec| Expression of expr;;
