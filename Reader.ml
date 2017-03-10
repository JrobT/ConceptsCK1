(* File reader.ml, by Alex Everett & Jack Trute *)
open Printf
open Functions

type term = 
    | Integer of int
    | String of string
    | "->"                      { FARROW }
    | "in"                      { IN }
 Start of term * term
    | Funct of string * string * term * term
    | Funct1 of string * string * string * term * term
    | Funct2 of string * string * string * string * term * term
    | Apply of term * term
    | Apply2 of term * term * term
    | Apply3 of term * term * term * term
    | Append of term * term
    | Union of term * term
    | Cons of term * term
    | Concat of term * term
    | Length of term

exception TypeErrorException
exception EmptyListException
exception UnrecognisedTypeException

let rec evaluator func_env arg_env term =
    let to_lang x =
        let xEval = evaluator func_env arg_env x
        in (match xEval with
              | Language x' -> x'
              | _ -> raise TypeErrorException)
        in let to_string x =
            let xEval = evaluator func_env arg_env x
            in (match xEval with
              | String x' -> x'
              | _ -> raise TypeErrorException)
        in let to_int x =
            let xEval = evaluator func_env arg_env x
            in (match xEval with
              | Integer x' -> x'
              | _ -> raise TypeErrorException)
    in match term with
        | (Integer i) -> Integer i      
        | (String c) -> String c
        | (StartExpr (x, y)) ->
            (Sequence ((evaluator func_env arg_env x), (evaluator func_env arg_env y)))
        | (IfExpr (cond, trueExpr, falseExpr)) ->
            let condEval = evaluator func_env arg_env cond
            in (match condEval with
                  | (Boolean b) ->
                      evaluator func_env arg_env (if b then trueExpr else falseExpr)
                  | _ -> raise TypeErrorException)
        | (FuncExpr1 (name, argName, body, inExpr)) ->
            evaluator ((name, (argName, body)) :: func_env) arg_env inExpr
        | (FuncExpr2 (name, arg0Name, arg1Name, body, inExpr)) ->
            evaluator ((name, (arg0Name, body)) :: (name, (arg1Name, body)) :: func_env) arg_env inExpr
        | (FuncExpr3 (name, arg0Name, arg1Name, arg2Name, body, inExpr)) ->
            evaluator ((name, (arg0Name, body)) :: (name, (arg1Name, body)) :: (name, (arg2Name, body)) :: func_env) arg_env inExpr
        | (VarExpr (name, value, inExpr)) ->
            evaluator func_env ((name, (evaluator func_env arg_env value)) :: arg_env) inExpr
        | (AppExpr1 (func, arg)) ->
            let argEval = evaluator func_env arg_env arg
                in (match func with
                     | (Var f) ->
                        (match lookup func_env f with
                             | (argName, body) ->
                                evaluator func_env ((argName, argEval) :: arg_env) body)
                             | _ -> raise TypeErrorException)
         | (AppExpr2 (func, arg0, arg1)) ->
             let argEval0 = evaluator func_env arg_env arg0
             in let argEval1 = evaluator func_env arg_env arg1
                 in (match func with
                      | (Var f) ->
                         (match (lookup2 func_env f) with
                              | ((argName0, body0), (argName1, body1)) ->
                                 evaluator func_env ((argName0, argEval0) :: (argName1, argEval1) :: arg_env) body0)
                              | _ -> raise TypeErrorException)
          | (AppExpr3 (func, arg0, arg1, arg2)) ->
              let argEval0 = evaluator func_env arg_env arg0
              in let argEval1 = evaluator func_env arg_env arg1
              in let argEval2 = evaluator func_env arg_env arg2
                in (match func with
                       | (Var f) ->
                          (match (lookup3 func_env f) with
                               | ((argName0, body0), (argName1, body1), (argName2, body2)) ->
                                  evaluator func_env ((argName0, argEval0) :: (argName1, argEval1) :: (argName2, argEval2) :: arg_env) body0)
                               | _ -> raise TypeErrorException)
        | (HeadExpr x) ->
            let x' = set_to_list (to_lang x)
            in let head list =
                match list with
                | [] -> raise EmptyListException
                | h :: t -> "\"" ^ h ^ "\""
            in String (head x')
        | (TailExpr x) ->
            let x' = set_to_list (to_lang x)
            in let tail list =
                match list with
                | [] -> raise EmptyListException
                | h :: t -> t
            in Language (list_to_set (tail x'))
        | (AppendExpr (x, y)) ->
            let x' = set_to_list (to_lang x)
            in let y' = to_string y
            in Language (list_to_set (remove_colons (List.sort compare (uniq (append x' (remove_quotes y'))))))
        | (UnionExpr (x, y)) ->
            let x' = set_to_list (to_lang x)
            in let y' = set_to_list (to_lang y)
            in Language (list_to_set (List.sort compare (union x' y')))
        | (ConsExpr (x, y)) ->
            let x' = remove_quotes (to_string x)
            in Language (list_to_set (x' :: set_to_list (to_lang y)))
        | (SizeExpr x) ->
            Integer (List.length(set_to_list(to_lang x)))
        | (SortExpr x) ->
            Language (list_to_set (List.sort compare (set_to_list (to_lang x))))
        | (UniqExpr x) ->
            Language (list_to_set (uniq (set_to_list (to_lang x))))
        | (CapExpr (x, y)) ->
            let x' = (set_to_list (to_lang x))
            in let y' = to_int y
            in Language (list_to_set (cap y' x'))
        | (KleeneExpr (x, y)) ->
            let x' = (remove_quotes (to_string x))
            in let y' = to_int y
            in Language (list_to_set (remove_colons (List.sort compare (kleene x' y'))))
        | (ConcatExpr (x, y)) ->
            let x' = remove_quotes (to_string x)
            in let y' = remove_quotes (to_string y)
            in String (list_to_string (remove_colon_from_list (string_to_list (x' ^ y'))))
        | (LengthExpr x) ->
            (Integer (List.length (remove_quotes_from_list(string_to_list (to_string x)))))
;;

let eval term = evaluator [] [] term ;;

let rec print_result result = match result with
    | (Integer i) -> print_int i
    | (Language l) -> print_string l
    | (String s) -> print_string s
    | (Boolean b) -> if b then print_string "true" else print_string "false"
    | (Sequence (a, b)) -> print_result a; print_string "\n";print_result b
    | _ -> raise UnrecognisedTypeException
