(* File parser.mly *)
%{
    open Reader
}%

%token START FINISH EOL EOF
%token <int> NUM
%token SEMICOLON

%start main             /* the entry point */
%type <unit> program

%%

main:
    expr EOL { $1 }
;

expr:
        INT                             { $1 }
    ;
