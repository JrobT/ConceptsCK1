(* File lexer.mll, by Alex Everett & Jack Trute *)
{

open Parser
open Functions

}

let blank = [' ' '\n' '\t']
let digits = ['0'-'9']+ as lxm
let word = 
    '"'['a'-'z''A'-'Z''0'-'9']*'"'
let ident =
    ['a'-'z''A'-'Z''0'-'9']+ as lxm
let comment = '(''*'_*'*'')'

rule lexermain = parse
    | blank           { lexermain lexbuf }
    | digits          { INT(int_of_string lxm) }
    | word as lxm     { STRING(lxm) }
    | comment         { COMMENT }
    | '('             { LPAREN }
    | ')'             { RPAREN }
    | "start"         { START }
    | "end"           { END }
    | "len"           { LENGTH }
    | "concat"        { CONCAT }
    | "funct"         { FUNCT }
    | "->"            { ARROW }
    | "in"            { IN }
    | "var"           { VAR }
    | "union"         { UNION }
    | "append"        { APPEND }
    | ident           { IDENT(lxm) }
    | eof             { EOF }
