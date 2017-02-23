(* File parser.mly *)
%token EOL
%token <string> LINE
%start main             /* the entry point */
%type <int> INT
%%
main:
   expr EOL                { (* Build a list of $1 *) }
;
expr:
 expr RIGHT           { Right ($1) }
 | expr UP            { Up ($1) }
 | expr DOWN          { Down ($1) }
 | expr LEFT          { Left ($1)  }
;

