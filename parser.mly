%{
    open Reader
%}

%token <int> INT
%token <bool> BOOLEAN
%token <string> LANGUAGE
%token <string> STRING
%token <string> IDENT
%token <string> EMPTYWORD
%token COMMENT
%token BEGIN FINISH
%token PLUS MINUS LESS GREATER EQUALS
%token COND THEN ELSE
%token LPAREN RPAREN
%token HEAD TAIL UNION APPEND CONS
%token SORT UNIQ CAP SIZE
%token FUNCT IN FARROW VAR EQUAL
%token CONCAT LENGTH
%token EOF

%start parsermain
%type <Reader.term> parsermain
%%

parsermain:
    | term EOF { $1 }
;

term:
    | INT                                               { Integer $1 }
    | LANGUAGE                                          { Language $1 }
    | STRING                                            { String $1 }
    | IDENT                                             { Var $1 }
    | COMMENT term                                      { $2 }
    | BEGIN term FINISH term                            { Begin ($2, $4) }
    | BEGIN term FINISH                                 { $2 }
    | LPAREN term PLUS term RPAREN                      { PlusTerm ($2, $4) }
    | LPAREN term MINUS term RPAREN                     { MinusTerm ($2, $4) }
    | LPAREN term LESS term RPAREN                      { LessTerm ($2, $4) }
    | LPAREN term GREATER term RPAREN                   { GreaterTerm ($2, $4) }
    | LPAREN term EQUALS term RPAREN                    { EqualsTerm ($2, $4) }
    | LPAREN COND term THEN term ELSE term RPAREN       { CondTerm ($3, $5, $7) }
    | LPAREN FUNCT IDENT IDENT FARROW term IN term RPAREN { Funct ($3, $4, $6, $8) }
    | LPAREN FUNCT IDENT IDENT IDENT FARROW term IN term RPAREN { Funct1 ($3, $4, $5, $7, $9) }
    | LPAREN FUNCT IDENT IDENT IDENT IDENT FARROW term IN term RPAREN { Funct2 ($3, $4, $5, $6, $8, $10) }
    | LPAREN term term RPAREN                    { Apply ($2, $3) }
    | LPAREN term term term RPAREN               { Apply2 ($2, $3, $4) }
    | LPAREN term term term term RPAREN          { Apply3 ($2, $3, $4, $5) }
    | LPAREN VAR IDENT EQUAL term IN term RPAREN { VarTerm ($3, $5, $7) }
    | LPAREN HEAD term RPAREN       	         { Headterm $3 }
    | LPAREN TAIL term RPAREN                    { Tailterm $3 }
    | LPAREN UNION  term term RPAREN             { Unionterm  ($3, $4) }
    | LPAREN APPEND term term RPAREN             { Appendterm ($3, $4) }
    | LPAREN CONS term term RPAREN               { Consterm ($3, $4) }
    | LPAREN CONCAT term term RPAREN             { Concatterm ($3, $4) }
    | LPAREN LENGTH term RPAREN                  { Lengthterm $3 }
    | LPAREN SIZE term RPAREN                    { Sizeterm $3 }
    | LPAREN SORT term RPAREN                    { Sortterm $3 }
    | LPAREN UNIQ term RPAREN                    { Uniqterm $3 }
    | LPAREN CAP term term RPAREN                { Capterm ($3, $4) }
;
