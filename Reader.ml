exception LookupError ;;
exception TypeError ;;
exception UnboundVariableError;;
exception Terminated ;;
exception StuckTerm ;;
exception NonBaseTypeResult;;

type term = 
        STR of string

let print_res res = match res with
    | (STR s) -> print_int s ; print_string " is a STRING" 
                | _ -> raise NonBaseTypeResult
