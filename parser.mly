/* File parser.mly */
%token <int> INT
%token RIGHT UP DOWN LEFT
%token EOL
%start main             /* the entry point */
%type <int> main
%%
main:
   expr EOL                { $1 }
;
expr:
 expr RIGHT           { Right ($1) }
 | expr UP            { Up ($1) }
 | expr DOWN          { Down ($1) }
 | expr LEFT          { Left ($1)  }
;

