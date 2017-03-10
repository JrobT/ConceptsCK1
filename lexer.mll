(* File lexer.mll, by Alex Everett & Jack Trute *)
{

open Parser
open Functions

}

let blank = [' ' '\n' '\t']
let digits = ['0'-'9']+ as lxm
let word = alpha*
let stringTerm = 
    '"'['a'-'z''A'-'Z''0'-'9']*'"'
let identTerm =
    ['a'-'z''A'-'Z''0'-'9']+ as lxm
let comment = '(''*'_*'*'')'

rule lexermain = parse
    | blank           { lexermain lexbuf }         (* skip blanks *)
    | digits {
        (* parse an Int *)
        INT(int_of_string lxm)
    }
    | stringTerm as lxm {
        (* parse a String *)
        STRING(lxm)
    }
    | comment {
        (* parse a comment *)
        COMMENT
    }
    | '('             { LPAREN }
    | ')'             { RPAREN }
    | "start"         { START }
    | "end"           { END }
    | "len"           { LENGTH }
    | "concat"        { CONCAT }
    | "funct"         { FUNCT }
    | "->"            { ARROW }
    | "in"            { IN }
    | "union"         { UNION }
    | "append"        { APPEND }
    | identTerm       { IDENT(lxm) }
    | EOF             { EOF }
