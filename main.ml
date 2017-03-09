(* File main.ml, by Alex Everett & Jack Trute *) 
open Lexer
open Parser
open Arg
open Printf

let parseProgram c = 
            try let lexbuf = Lexing.from_channel c in  
                        main token lexbuf 
                            with Parsing.Parse_error -> failwith "Parse failure!" ;;
flush stdout
