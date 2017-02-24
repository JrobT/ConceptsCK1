(* File parser.mly *)
%token START FINISH EOL EOF
%token <string> TOKEN
%token <int> NUM
%token ASSIGN
%token LEFTPAREN RIGHTPAREN
%token SEMICOLON

%start main             /* the entry point */
%type <unit> program

%%

main:
   expr EOL                { (* Build a list of $1 *) }
;

program:
|   begin_stmt statements end_stmt EOF { raise End_of_file }
;

begin_stmt:
|   BEGIN { generate_begin () }
;

end_stmt:
|   END { generate_end () }
;

statements:
| { }
| statement SEMICOLON statements { }
;

statement:
| TOKEN ASSIGN expression { generate_assign $1 $3 }
;

