type number = Int of int | Float of float;;
type expr = Var of string
  | Num of number
  | Add of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Assign of string * expr;;