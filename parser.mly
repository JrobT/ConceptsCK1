%{
    open Reader
%}
%token START FINISH EOL EOF
%token <int> NUM
%token <string> STRING
%start main             /* the entry point */
%type <Reader.term> main
%%

main:
    expr EOF { $1 }
;

expr:
        STRING          { STR $1 }
    ;
