%{
open Typer
%}


%token <int> INT
%token <float> FLOAT
%token <string> ID
%token PLUS MINUS TIMES DIVIDE OPEN CLOSE EOE LAST EQ
%token EOF

%start program
%type <Typer.program> program
%type <Typer.dec> dec

%nonassoc EQ
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS 
%%

program:
    EOE {Nil}
  | expr EOE { Expression($1) }
  | dec EOE { Declaration($1) };

number:
  | FLOAT { Num (Float $1) }
  | INT { Num (Int $1) };
dec:
  | ID EQ expr {Assign($1, $3)};
expr:
  | LAST { Last }
  | number {$1}
  | ID { Var($1) }
  | OPEN expr CLOSE { $2 }
  | expr PLUS expr {Add($1, $3) }
  | expr MINUS expr {Add($1, Mul( Num (Int (-1)), $3))}
  | expr TIMES expr { Mul($1, $3) }
  | MINUS expr %prec UMINUS{ Mul ( Num (Int (-1)) , $2) };

%%
