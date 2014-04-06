%{
open Typer
%}


%token <int> INT
%token <float> FLOAT
%token <string> ID
%token PLUS MINUS TIMES DIVIDE EOE LAST EQ IF LPAREN RPAREN AND OR
%token LBRACE RBRACE ELSE WHILE DONE FI
%token PLUSEQ DIVEQ MINUSEQ TIMESEQ PPLUS MMINUS
%token EOF

%start instruction
%type <Typer.instruction> instruction
%type <Typer.dec> dec

%nonassoc PLUSEQ DIVEQ MINUSEQ TIMESEQ
%nonassoc EQ
%left AND OR
%left PLUS MINUS PPLUS MMINUS
%left TIMES DIVIDE  EOE
%nonassoc IFX
%nonassoc UMINUS
%nonassoc ELSE

%%

instruction:
  | EOE instruction {$2}
  | expr EOE { Expression($1) }
  | dec EOE { Declaration($1) };

instruction_block:
    instruction instruction_block {Instr($1, $2)}
  | /* nothing */ {Nil};


number:
  | FLOAT { Num (Float $1) }
  | INT { Num (Int $1) };

if_block:
  | IF LPAREN expr RPAREN  instruction_block else_block {
    If_dec($3, $5, $6)};
else_block:
    FI {Nil}
  |  ELSE instruction_block FI {$2};

while_block:
  WHILE LPAREN expr RPAREN  instruction_block DONE {
    While_dec($3, $5)};

dec:
  | if_block {$1}
  | while_block {$1}
  | ID EQ expr {Assign($1, $3)}
  | ID PLUSEQ expr {Assign($1, Add(Var($1), $3))}
  | ID MINUSEQ expr {Assign($1, Add(Var($1), Mul (Num(Int(-1)), $3)))}
  | ID TIMESEQ expr {Assign($1, Mul(Var($1), $3))}
  | ID DIVEQ expr {Assign($1, Div(Var($1), $3))};

expr:
  | LAST { Last }
  | number {$1}
  | ID { Var($1) }
  | expr PLUS expr {Add($1, $3) }
  | expr MINUS expr {Add($1, Mul( Num (Int (-1)), $3))}
  | expr TIMES expr { Mul($1, $3) }
  | expr DIVIDE expr { Div($1, $3) }
  | ID PPLUS { Incr($1, 1, false) }
  | PPLUS ID { Incr($2, 1, true) }
  | ID MMINUS { Incr($1, -1, false) }
  | MMINUS ID { Incr($2, -1, true) }
  | LPAREN expr RPAREN { $2 }
  | MINUS expr %prec UMINUS{ Mul ( Num (Int (-1)) , $2) };

%%
