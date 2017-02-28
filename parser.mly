(* File parser.mly *)
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
    ajStart statements ajEnd EOF { raise End_of_file }
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

statement:
| TOKEN ASSIGN expression { assignToVar $1 $3 }
| READ LEFTANGLE lTokens RIGHTANGLE { readInput $3 }
| WRITE LEFTANGLE lExpr RIGHTANGLE { writeOutput $3 }
;

lTokens:
| TOKEN { [$1] }
| TOKEN COMMA lExpr { $1 :: $3 }
;

lExpr:
| expression { [$1] }
| expression COMMA lExpr { $1 :: $3 }
;

(* need operators that:
    * Append a letter to a string
    * Prepend a letter to the front of the string
    * Maybe one to list strings together
*)
