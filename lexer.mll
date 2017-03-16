(* File lexer.mll, by Alex Everett & Jack Trute *)
{

open Parser

}

let blank = [' ' '\n' '\t']
let digits = ['0'-'9']+ as lxm
let lang =
    '{'(' '*(['a'-'z']+|':')' '*','' '*)*(['a'-'z']+|':')?' '*'}' as lxm
let word = 
    ('"'['a'-'z''A'-'Z''0'-'9']*'"')|':' as lxm
let ident =
    ['a'-'z''A'-'Z''0'-'9']+ as lxm
let comment = '(''*'_*'*'')'

rule lexermain = parse
    | blank                         { lexermain lexbuf }    
    | digits                        { INT(int_of_string lxm) }
    | lang                          { LANGUAGE(lxm) }
    | word                          { STRING(lxm) }
    | comment                       { COMMENT }
    | '('                           { LPAREN }
    | ')'                           { RPAREN }
    | '<'                           { LESS }
    | '>'                           { GREATER }
    | '='                           { EQUAL }
    | '+'                           { PLUS }
    | '-'                           { MINUS }
    | "=="                          { EQUALS }
    | "begin"                       { BEGIN }
    | "fi"                          { FINISH }
    | "cond"                        { COND }
    | "then"                        { THEN }
    | "else"                        { ELSE }
    | "len"                         { LENGTH }
    | "cons"                        { CONS }
    | "concat"                      { CONCAT }
    | "funct"                       { FUNCT }
    | "->"                          { FARROW }
    | "in"                          { IN }
    | "var"                         { VAR }
    | "union"                       { UNION }
    | ":"                           { EMPTYWORD("") }
    | "head"                        { HEAD }
    | "tail"                        { TAIL }
    | "sort"                        { SORT }
    | "uniq"                        { UNIQ }
    | "cap"                         { CAP }
    | "append"                      { APPEND }
    | ident                         { IDENT(lxm) }
    | "$"['1'-'9']+ as lxm          { LANGUAGE(read_line lxm) }
    | "$last_line"                  { INT(get_last_line 10) }
    | eof                           { EOF }
