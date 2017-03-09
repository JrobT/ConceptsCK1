(* File Reader.ml, by Alex Everett & Jack Trute *)

(* I don't think all these are needed *)
exception LookupError;;
exception TypeError;;
exception UnboundVariableError;;
exception Terminated;;
exception StuckTerm;;
exception NonBaseTypeResult;;

type term = 
        STR of string
        (*INT of int ??*)

(* jokes if this works, it seems bollocks *)
let print_res res = match res with
    | (STR s) -> print_int s ; print_string " is a STRING" 
                | _ -> raise NonBaseTypeResult
