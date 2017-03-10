(* File parser.mly, by Alex Everett & Jack Trute *) 
%{
    open Reader
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token COMMENT
%token START END
%token LPAREN RPAREN
%token APPEND CONS UNION
%token FUNCT IN FARROW VAR
%token CONCAT LENGTH
%token EOF

%start parsermain
%type <Reader.term> parsermain
%%

parsermain:
    | term EOF { $1 }
;

term:
    | INT                              { Integer $1 }
    | STRING                           { String $1 }
    | IDENT                            { Var $1 }
    | COMMENT term                     { $2 }
    | START term END term              { Start ($2, $4) }
    | START term END                   { $2 }
    | LPAREN FUNCT IDENT IDENT FARROW term IN term RPAREN { Funct ($3, $4, $6, $8) }
    | LPAREN FUNCT IDENT IDENT IDENT ARROW term IN term RPAREN { Funct1 ($3, $4, $5, $7, $9) }
    | LPAREN FUNCT IDENT IDENT IDENT IDENT ARROW term IN term RPAREN { Funct2 ($3, $4, $5, $6, $8, $10) }
    | LPAREN expr expr expr RPAREN               { Apply ($2, $3, $4) }
    | LPAREN expr expr expr expr RPAREN          { Apply1 ($2, $3, $4, $5) }
    | LPAREN VAR IDENT EQUAL expr IN expr RPAREN { VarExpr ($3, $5, $7) }
    | LPAREN APPEND term term RPAREN   { AppendTerm ($3, $4) }
    | LPAREN CONS term term RPAREN     { ConsTerm ($3, $4)   }
    | LPAREN CONCAT term term RPAREN   { ConcatTerm ($3, $4) }
    | LPAREN LENGTH term RPAREN        { LengthTerm $3       }
;
