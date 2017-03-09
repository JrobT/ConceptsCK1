(* File parser.mly, by Alex Everett & Jack Trute *) 
%{
    open Reader
%}

%token START FINISH EOL EOF
%token <int> INT
%token <string> STRING

%start main
%type <Reader.term> main
%%

main:
    expr EOF { $1 }
;

expr:
    STRING { STR $1 }
;
