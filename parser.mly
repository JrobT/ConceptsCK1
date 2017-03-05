(* File parser.mly *)
%{
    open Reader
}%

%token START FINISH EOL EOF
%token <string> TOKEN
%token <int> NUM
%token ASSIGN
%token APPEND PREPEND
%token LEFTANGLE RIGHTANGLE
%token COMMA SEMICOLON

%start main             /* the entry point */
%type <unit> program

%%

main:
    expr EOL { $1 }
;

expr:
    INT                             { $1 }
 |  LEFTANGLE expr RIGHTANGLE       { $2 }
 |  APPEND expr                     { 
