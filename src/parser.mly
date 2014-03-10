%{
open Typer
%}


%token <int> INT
%token <float> FLOAT
%token PLUS MINUS TIMES DIVIDE OPEN CLOSE EOE

%start program
%type <Typer.expr> program

%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS
%%

program:
  expr EOE{ $1 };

expr:
  | INT { Num (Int $1) }
  | FLOAT { Num (Float $1) }
  | OPEN expr CLOSE { $2 }
  | expr PLUS expr {Add($1, $3) }
  | expr MINUS expr {Add($1, Mul( Num (Int (-1)), $3))}
  | expr TIMES expr { Mul($1, $3) }
  | MINUS expr %prec UMINUS{ Mul ( Num (Int (-1)) , $2) };

%%
