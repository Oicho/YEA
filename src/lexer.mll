{
open Parser
exception Eof
}
let digit = ['0' - '9']
rule lex = parse
  | [' ' '\t'] {lex lexbuf }
  | digit+ as s { INT(int_of_string s) }
  | digit+ '.' digit+ as s { FLOAT(float_of_string s) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '/' { DIVIDE }
  | '*' { TIMES }
  | "if" { IF }
  | '(' { LPAREN }
  | '{' { LBRACE }
  | ')' { RPAREN }
  | '}' { RBRACE }
  | "&&" { AND }
  | "||" { OR }
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
  | "//" { one_line_comment lexbuf }
  | "/*" { comment 0 lexbuf }
and comment level  = parse
  | "*/" { if level = 0 then lex lexbuf
             else comment (level - 1) lexbuf}
  | "/*" {comment (level + 1) lexbuf}
  | _ { comment level lexbuf }
  | eof {raise Eof}
and one_line_comment = parse
  | ['\n' '\r'] { lex lexbuf }
  | _ { one_line_comment lexbuf }