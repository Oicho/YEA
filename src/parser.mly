%{
open Typer
%}


%token <int> INT
%token <float> FLOAT
%token <string> ID
%token PLUS MINUS TIMES DIVIDE OPEN CLOSE EOE LAST EQ
%token EOF

%start program
%type <Typer.expr> program
%nonassoc EQ
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS 
%%

program:
  expr EOE{ $1 };

number:
  | FLOAT { Num (Float $1) }
  | INT { Num (Int $1) };

expr:
  | ID EQ expr {Assign($1, $3)}
  | number {$1}
  | ID { Var($1) }
  | OPEN expr CLOSE { $2 }
  | expr PLUS expr {Add($1, $3) }
  | expr MINUS expr {Add($1, Mul( Num (Int (-1)), $3))}
  | expr TIMES expr { Mul($1, $3) }
  | MINUS expr %prec UMINUS{ Mul ( Num (Int (-1)) , $2) };

%%
