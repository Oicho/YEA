{
open Parser
exception Eof
}


rule lex = parse
  | [' ' '\t'] {lex lexbuf }
  | ['0' - '9']+ as s { INT(int_of_string s) }
  | ['0' - '9']+'.'['0' - '9']+ as s { FLOAT(float_of_string s) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '/' { DIVIDE }
  | '*' { TIMES }
  | "+=" {PLUSEQ}
  | "-=" {MINUSEQ}
  | "*=" {TIMESEQ}
  | "/=" {DIVEQ}
  | "++" { PPLUS }
  | "--" { MMINUS }
  | '(' { OPEN }
  | ')'{ CLOSE }
  | '=' { EQ }
  | '\n' { EOE }
  | ';' { EOE }
  | eof { raise Eof }
  | "last" { LAST }
  | "quit" { raise Eof }
  | ['a' - 'z' 'A' - 'Z' '_'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_']* as c { ID(c) }
