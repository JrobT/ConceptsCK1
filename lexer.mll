(* File lexer.mll *)
{

open Parser           (* The type token is defined in parser.mli *)

let line = ref 1      (* current token line number *)

exception SyntaxError of string
exception EOF


}

let blank = [' ' '\r' '\t']
let alpha = ['a'-'z']
let digit = ['0'-'9']
let number = digit*
let word = alpha (alpha | digit | '_')*

rule token = parse
      blank           { token lexbuf }        (* skip blanks *)
    | '\n'            { EOL }   (* record & deal with new line *)
    | eof             { EOF }                 (* no more tokens *)
    | number as num {
        (* parse number *)
        NUM (int_of_string num)
    }
    | word as lxm {STRING(lxm)}
