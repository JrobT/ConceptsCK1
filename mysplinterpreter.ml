(* File main.ml, by Alex Everett & Jack Trute *) 
open Lexer
open Parser
open Arg
open Printf
open Reader

(* Parse the program using parsermain and lexermain *)
let parseProgram c = 
            try let lexbuf = Lexing.from_channel c in  
                        parsermain lexermain lexbuf 
            with Parsing.Parse_error -> failwith "Parse failure!" ;;

(* Accept a program and input from command line *)
let arg = ref stdin in
  let prog p = arg := open_in p in
    let use = "mysplinterpreter PROGRAM_FILE" in
      parse [] prog use ;
      let parsed = parseProgram !arg in
        let result = eval parsed in
          print_result result ;
          print_newline() ;
flush stdout
