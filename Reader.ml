(* File reader.ml, by Alex Everett & Jack Trute *)
open Printf
open Functions

type term = 
    | Integer of int
    | Boolean of bool
    | Language of string
    | String of string
    | Var of string
    | Start of term * term
    | Sequence of expr * expr
    | If of expr * expr * expr
    | Funct of string * string * term * term
    | Funct1 of string * string * string * term * term
    | Funct2 of string * string * string * string * term * term
    | VarTerm of string * term * term
    | Apply of term * term
    | Apply2 of term * term * term
    | Apply3 of term * term * term * term
    | AppendTerm of term * term
    | UnionTerm of term * term
    | ConsTerm of term * term
    | ConcatTerm of term * term
    | LengthTerm of term

let rec lookup env v = match env with
    | [] -> failwith ("Doesn't exist " ^ v)
    | (vname, vvalue) :: rest -> if v = vname
                                     then vvalue
                                     else lookup rest v

let rec lookup2 env v  = match env with
   | [] -> failwith ("Doesn't exist " ^ v)
   | (vname1, vvalue1) :: (vname2, vvalue2) :: rest -> if v = vname1
                                    then (vvalue1, vvalue2)
                                    else lookup2 rest v
   | (_, _) :: [] -> failwith ("Doesn't exist " ^ v)


let rec lookup3 env v  = match env with
   | [] -> failwith ("Doesn't exist " ^ v)
   | (vname1, vvalue1) :: (vname2, vvalue2) :: (vname3, vvalue3) :: rest -> if v = vname1
                                    then (vvalue1, vvalue2, vvalue3)
                                    else lookup3 rest v
   | (_, _) :: [] -> failwith ("Doesn't exist " ^ v)
   | (_, _) :: (_, _) :: [] -> failwith ("Doesn't exist " ^ v)


exception TypeErrException
exception EmptyListException

let rec eval funct_env arg_env term =
    let to_lang x =
        let xEval = eval funct_env arg_env x
        in (match xEval with
              | Language x' -> x'
              | _ -> raise TypeErrException)
        in let to_string x =
            let xEval = eval funct_env arg_env x
            in (match xEval with
              | String x' -> x'
              | _ -> raise TypeErrException)
        in let to_int x =
            let xEval = eval funct_env arg_env x
            in (match xEval with
              | Integer x' -> x'
              | _ -> raise TypeErrException)
    in match term with
        | (Integer i) -> Integer i      
        | (String c) -> String c
        | (Start (x, y)) ->
            (Sequence ((eval funct_env arg_env x), (eval funct_env arg_env y)))
        | (If (cond, trueTerm, falseTerm)) ->
            let condEval = eval funct_env arg_env cond
            in (match condEval with
                  | (Boolean b) ->
                      eval func_env arg_env (if b then trueTerm else falseTerm)
                  | _ -> raise TypeErrException)
        | (Funct (name, argName, body, inTerm)) ->
            eval ((name, (argName, body)) :: func_env) arg_env inTerm
        | (Funct1 (name, arg0Name, arg1Name, body, inTerm)) ->
            eval ((name, (arg0Name, body)) :: (name, (arg1Name, body)) :: func_env) arg_env inTerm
        | (Funct2 (name, arg0Name, arg1Name, arg2Name, body, inTerm)) ->
            eval ((name, (arg0Name, body)) :: (name, (arg1Name, body)) :: (name, (arg2Name, body)) :: funct_env) arg_env inTerm
        | (VarTerm (name, value, inTerm)) ->
            eval funct_env ((name, (evaluator func_env arg_env value)) :: arg_env) inTerm
        | (Apply (funct, arg)) ->
            let argEval = eval funct_env arg_env arg
                in (match funct with
                     | (Var f) ->
                        (match lookup func_env f with
                             | (argName, body) ->
                                eval func_env ((argName, argEval) :: arg_env) body)
                             | _ -> raise TypeErrException)
         | (Apply1 (funct, arg0, arg1)) ->
             let argEval0 = eval funct_env arg_env arg0
             in let argEval1 = eval funct_env arg_env arg1
                 in (match funct with
                      | (Var f) ->
                         (match (lookup2 func_env f) with
                              | ((argName0, body0), (argName1, body1)) ->
                                 evaluator func_env ((argName0, argEval0) :: (argName1, argEval1) :: arg_env) body0)
                              | _ -> raise TypeErrException)
          | (Apply2 (funct, arg0, arg1, arg2)) ->
              let argEval0 = eval funct_env arg_env arg0
              in let argEval1 = eval funct_env arg_env arg1
              in let argEval2 = eval funct_env arg_env arg2
                in (match funct with
                       | (Var f) ->
                          (match (lookup3 funct_env f) with
                               | ((argName0, body0), (argName1, body1), (argName2, body2)) ->
                                  eval funct_env ((argName0, argEval0) :: (argName1, argEval1) :: (argName2, argEval2) :: arg_env) body0)
                               | _ -> raise TypeErrException)
        | (AppendTerm (x, y)) ->
            let x' = set_to_list (to_lang x)
            in let y' = to_string y
            in Language (list_to_set (remove_colons (List.sort compare (uniq (append x' (remove_quotes y'))))))
        | (UnionTerm (x, y)) ->
            let x' = set_to_list (to_lang x)
            in let y' = set_to_list (to_lang y)
            in Language (list_to_set (List.sort compare (union x' y')))
        | (UniqTerm x) ->
            Language (list_to_set (uniq (set_to_list (to_lang x))))
        | (ConcatTerm (x, y)) ->
            let x' = remove_quotes (to_string x)
            in let y' = remove_quotes (to_string y)
            in String (list_to_string (remove_colon_from_list (string_to_list (x' ^ y'))))
        | (LengthTerm x) ->
            (Integer (List.length (remove_quotes_from_list(string_to_list (to_string x)))))
;;

let eval term = evaluator [] [] term ;;

let rec print_result result = match result with
    | (Integer i) -> print_int i
    | (Language l) -> print_string l
    | (String s) -> print_string s
    | (Boolean b) -> if b then print_string "true" else print_string "false"
    | (Sequence (a, b)) -> print_result a; print_string "\n";print_result b
    | _ -> raise TypeErrException
