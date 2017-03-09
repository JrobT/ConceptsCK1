(* File lexer.mll, by Alex Everett & Jack Trute *)
{

open Parser           (* The type token is defined in parser.mli *)

let line = ref 1      (* current token line number *)

exception SyntaxError of string
exception EOF

}

let blank = [' ' '\r' '\t']
let alpha = ['a'-'z']
let digit = ['0'-'9']
let digits = digit*
let word = alpha*

rule token = parse
      blank           { token lexbuf }        (* skip blanks *)
    | '\n'            { EOL }                 (* record & deal with new line *)
    | eof             { EOF }                 (* no more tokens *)
    | digits as lxm {
        (* parse an Int *)
        INT (int_of_string lxm)
    }
    | word as lxm {
        (* parse a String *)
        STRING(lxm)
    }
