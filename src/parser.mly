%{
open Typer
%}


%token <int> INT
%token <float> FLOAT
%token <string> ID
%token PLUS MINUS TIMES DIVIDE OPEN CLOSE EOE LAST EQ IF LPAREN RPAREN AND OR
%token LBRACE RBRACE
%token PLUSEQ DIVEQ MINUSEQ TIMESEQ PPLUS MMINUS
%token EOF

%start program
%type <Typer.program> program
%type <Typer.dec> dec

%nonassoc PLUSEQ DIVEQ MINUSEQ TIMESEQ
%nonassoc EQ
%left AND OR
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS 
%%

program:
    EOE program {$2}
  | expr EOE { Expression($1) }
  | dec EOE { Declaration($1) };

/*instruction_block:
  EOE instruction_block {$2};*/

number:
  | FLOAT { Num (Float $1) }
  | INT { Num (Int $1) };

/*if_block:
  | IF LPAREN expr RPAREN program*/

dec:
  | ID EQ expr {Assign($1, $3)}
  | ID PLUSEQ expr {Assign($1, Add(Var($1), $3))}
  | ID MINUSEQ expr {Assign($1, Add(Var($1), Mul (Num(Int(-1)), $3)))}
  | ID TIMESEQ expr {Assign($1, Mul(Var($1), $3))}
  | ID DIVEQ expr {Assign($1, Div(Var($1), $3))};

expr:
  | LAST { Last }
  | number {$1}
  | ID { Var($1) }
  | ID PPLUS { Incr($1, 1, false) }
  | PPLUS ID { Incr($2, 1, true) }
  | ID MMINUS { Incr($1, -1, false) }
  | MMINUS ID { Incr($2, -1, true) }
  | OPEN expr CLOSE { $2 }
  | expr PLUS expr {Add($1, $3) }
  | expr MINUS expr {Add($1, Mul( Num (Int (-1)), $3))}
  | expr TIMES expr { Mul($1, $3) }
  | expr DIVIDE expr { Div($1, $3) }
  | MINUS expr %prec UMINUS{ Mul ( Num (Int (-1)) , $2) };

%%
