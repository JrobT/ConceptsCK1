(* File lexer.mll *)
{

open Parser   (* The type token is defined in parser.mli *)

let lineNum = ref 1   (* current token line number *)

exception SyntaxError of string
exception EOF

let syntaxError err = raise (SyntaxError (err ^ " on line " ^ (string_of_int !lineNum)))

}

let blank = [' ' '\r' '\t']

let alpha = ['a'-'z']
let word  = alpha*

let digit = ['0'-'9']
let number = digit*

rule token = parse
      blank           { token lexbuf }        (* skip blanks *)
    | ":="            { ASSIGN }              (* assignment token *)
    | ';'             { SEMICOLON }           (* end line token *)
    | '\n'            { incr lineNum; EOL }   (* record & deal with new line *)
    | _               { syntaxError "Couldn't identify the token" }
    | eof             { raise EOF }           (* no more tokens *)
    | digits as d {
            ( * parse literal *)
            LITERAL (int_of_string d)
    }
    | word as lxm {
            let l = String.lowercase lxm in
            LINE -> lxm (* NOT SURE ABOUT THIS *) 
    }
