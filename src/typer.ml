type number = Int of int | Float of float;;
type expr = Num of number | Add of expr * expr | Mul of expr * expr | Div of expr * expr;;