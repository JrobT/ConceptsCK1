(* File lexer.mll *)
{

open Parser           (* The type token is defined in parser.mli *)

let line = ref 1      (* current token line number *)

exception SyntaxError of string
exception EOF

let syntaxError err = raise (SyntaxError (err ^ " on line " ^ (string_of_int !lineNum)))

let keywords = [
    "begin", BEGIN; "end", END; "read", READ; "append", APPEND;
    "prepend", PREPEND; "list", LIST;
]

}

let blank = [' ' '\r' '\t']
let alpha = ['a'-'z']
let digit = ['0'-'9']
let number = digit*
let word = alpha (alpha | digit | '_')*

rule token = parse
      blank           { token lexbuf }        (* skip blanks *)
    | ":="            { ASSIGN }              (* assignment token *)
    | '('             { LEFTPAREN }
    | ')'             { RIGHTPAREN }
    | ';'             { SEMICOLON }           (* end line token *)
    | '\n'            { incr lineNum; EOL }   (* record & deal with new line *)
    | _               { syntaxError "Token doesn't exist" }
    | eof             { EOF }                 (* no more tokens *)
    | number as num {
        (* parse number *)
        NUM (int_of_string num)
    }
    | word as lxm {
        let l = String.lowercase lxm in
        try List.assoc l keywords
        with Not_found -> TOKEN lxm
    }

