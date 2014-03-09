{
open Parser
}


rule lex = parse
  | [' ' '\t'] {lex lexbuf }
  | ['0' - '9']+ as s { INT(int_of_string s) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '/' { DIVIDE }
  | '*' { TIMES }
  | '(' { OPEN }
  | ')'{ CLOSE }
  | '\n' { EOE }
