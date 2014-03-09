%{
open Typer
%}


%token <int> INT
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
  | INT { Int $1 }
  | OPEN expr CLOSE { $2 }
  | expr PLUS expr {Add($1, $3) }
  | expr MINUS expr {Add($1, Mul(Int (-1), $3))}
  | expr TIMES expr { Mul($1, $3) }
  | MINUS expr %prec UMINUS{ Mul (Int (-1) , $2) };

%%
