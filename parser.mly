(* File parser.mly *)
%token START FINISH EOL EOF
%token <string> TOKEN
%token <int> NUM
%token ASSIGN
%token APPEND PREPEND
%token LIST
%token LEFTANGLE RIGHTANGLE
%token COMMA SEMICOLON

%start main             /* the entry point */
%type <unit> program

%%

main:
    ajStart statements ajEnd EOL { raise End_of_file }
;

ajStart:
|   START { start () }
;

ajFinish:
|   FINISH { finish () }
;

statements:
| { }
| statement SEMICOLON statements { }
;

APPEND 
statement:
| TOKEN ASSIGN expression { assignToVar $1 $3 }
| READ LEFTANGLE lTokens RIGHTANGLE { readInput $3 }
| WRITE LEFTANGLE lExpr RIGHTANGLE { writeOutput $3 }
| APPEND LEFTANGLE s COMMA c RIGHTANGLE { append $3 $5 }
| PREPEND LEFTANGLE s COMMA c RIGHTANGLE { prepend $3 $5 }
| LIST LEFTANGLE l COMMA s RIGHTANGLE { list $3 $5 }
;

lExpr:
| expression { [$1] }
| expression COMMA lExpr { $1 :: $3 }
;

lTokens:
| TOKEN { [$1] }
| TOKEN COMMA lTokens { $1 :: $3 }
;

append:
| APPEND TOKEN 
