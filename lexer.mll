(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)

exception SyntaxError of string

}

let letters = ['a'-'z']

rule token = parse
      [' ' '\t']     { token lexbuf }     (* skip blanks *)
    | ['\n' ]  {  }
    | letters+ as lxm { STR(string lxm) }
