(* File reader.ml, by Alex Everett & Jack Trute *)
open Printf
open Functions

type term =
    | Var of string
    | Integer of int
    | Boolean of bool
    | Language of string
    | String of string
    | Start of term * term
    | Sequence of term * term
    | PlusTerm of term * term
    | MinusTerm of term * term
    | LessTerm of term * term
    | GreaterTerm of term * term
    | EqualsTerm of term * term
    | Begin of term * term
    | CondTerm of term * term * term
    | Funct of string * string * term * term
    | Funct1 of string * string * string * term * term
    | Funct2 of string * string * string * string * term * term
    | VarTerm of string * term * term
    | Apply of term * term
    | Apply2 of term * term * term
    | Apply3 of term * term * term * term
    | Headterm of term
    | Tailterm of term
    | Appendterm of term * term
    | Unionterm of term * term
    | Consterm of term * term
    | Sizeterm of term
    | Sortterm of term
    | Uniqterm of term
    | Capterm of term * term
    | Concatterm of term * term
    | Lengthterm of term

let rec cap i list =
    match (i, list) with
        | 0, h :: t -> []
        | _, [] -> list
        | _, h :: t -> h :: cap (i - 1) t
;;

let rec uniq l =
  let rec aux l n =
    match l with
    | [] -> []
    | h :: t -> if n = h then aux t n else h :: (aux t n)
    in match l with
         | [] -> []
         | h :: t -> h :: (aux (uniq t) h)
;;

let rec append list letter =
    match list with
    | [] -> []
    | h :: t -> List.sort compare ((h ^ letter) :: append t letter)
;;

let union set1 set2 =
    List.sort compare (uniq (List.merge compare set1 set2))
;;

let string_to_list s =
  let rec aux i list =
    if i < 0 then list else aux (i - 1) (s.[i] :: list) in
  aux (String.length s - 1) []
;;

let list_to_string l =
  let result = String.create (List.length l) in
  let rec aux count = function
  | [] -> result
  | c :: l -> String.set result count c; aux (count + 1) l in
  aux 0 l
;;

let rec cat l =
    match l with
        | [] -> []
        | ',' :: t -> []
        | '}' :: t -> []
        | ' ' :: t -> []
        | h :: t -> h :: (cat t)
;;

let findword l =
    list_to_string (cat l)
;;

let rec sublist i l =
    match (i, l) with
        | (_, []) -> []
        | (0, _) -> l
        | (_, h :: t) -> sublist (i - 1) t
;;

let rec compress l1 l2 =
    match l2 with
        | [] -> l1
        | '}' :: t -> l1
        | '{' :: t -> compress l1 t
        | ' ' :: t -> compress l1 t
        | ',' :: t -> compress l1 t
        | h :: t -> let w = findword l2 in compress (w :: l1) (sublist (String.length w) t)
;;

let set_to_list s =
    List.rev (compress [] (string_to_list s))
;;

let remove_colon_from_list list =
    match list with
        | [] -> []
        | h :: t -> if (t = [':'])
                        then h :: []
                        else if (t = [])
                        then h :: t
                        else if h = ':' then t else h :: t
;;

let rec remove_colons list =
    match list with
        | [] -> []
        | h :: t -> list_to_string (remove_colon_from_list (string_to_list h)) :: remove_colons t
;;

let list_to_set list =
    let rec aux list =
        match list with
        | [] -> "}"
        | [x] -> x ^ "}"
        | h :: t ->  h ^ ", " ^ (aux t)
    in "{" ^ (aux list)
;;

let rec remove_quotes_from_list l =
    match l with
    | [] -> []
    | h :: t -> if h = '\"' then remove_quotes_from_list t else h :: remove_quotes_from_list t
;;

let remove_quotes str = list_to_string (remove_quotes_from_list (string_to_list str));;

let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
         try Some (input_line channel) with End_of_file -> None);;

exception EmptyList

let rec last_element = function
    | [x] -> x
    | _ :: t -> last_element t
    | [] -> raise EmptyList
;;

let remove_dollar_from_list list =
  match list with
    | [] -> []
    | h :: t -> t
;;

let remove_dollar str = list_to_string (remove_dollar_from_list (string_to_list str))
;;

let lines = line_stream_of_channel stdin;;

let get_line line_number =
    last_element (Stream.npeek (int_of_string (remove_dollar line_number)) lines)
;;

let get_last_line last_line =
    int_of_string (last_element (Stream.npeek last_line lines))
;;

let rec lookup env v = match env with
    | [] -> failwith ("Can't find: " ^ v)
    | (vname, vvalue) :: rest -> if v = vname
                                     then vvalue
                                     else lookup rest v

let rec lookup2 env v  = match env with
   | [] -> failwith ("Can't find: " ^ v)
   | (vname1, vvalue1) :: (vname2, vvalue2) :: rest -> if v = vname1
                                    then (vvalue1, vvalue2)
                                    else lookup2 rest v
   | (_, _) :: [] -> failwith ("cannot find var: " ^ v)


let rec lookup3 env v  = match env with
   | [] -> failwith ("Can't find: " ^ v)
   | (vname1, vvalue1) :: (vname2, vvalue2) :: (vname3, vvalue3) :: rest -> if v = vname1
                                    then (vvalue1, vvalue2, vvalue3)
                                    else lookup3 rest v
   | (_, _) :: [] -> failwith ("Can't find: " ^ v)
   | (_, _) :: (_, _) :: [] -> failwith ("Can't find: " ^ v)

exception TypeException
exception EmptyException

let rec evaluator func_env arg_env term =
    let to_lang x =
        let evalF = evaluator func_env arg_env x
        in (match evalF with
              | Language x' -> x'
              | _ -> raise TypeException)
        in let to_string x =
            let evalF = evaluator func_env arg_env x
            in (match evalF with
              | String x' -> x'
              | _ -> raise TypeException)
        in let to_int x =
            let evalF = evaluator func_env arg_env x
            in (match evalF with
              | Integer x' -> x'
              | _ -> raise TypeException)
    in match term with
        | (Var v) -> lookup arg_env v
        | (Integer i) -> Integer i
        | (Language l) -> Language l
        | (String c) -> String c
        | (Boolean b) -> Boolean b
        | (Sequence (a, b)) -> Sequence (a, b)
        | (PlusTerm (x, y) ) ->
            let x' = to_int x in
            let y' = to_int y in
                (Integer (x' + y'))
        | (MinusTerm (x, y) ) ->
            let x' = to_int x in
            let y' = to_int y in
                (Integer (x' - y'))
        | (LessTerm (x, y)) ->
            let x' = to_int x
            in let y' = to_int y
            in Boolean (x' < y')
        | (GreaterTerm (x, y)) ->
            let x' = to_int x
            in let y' = to_int y
            in Boolean (x' > y')
        | (EqualsTerm (x, y)) ->
            let x' = to_int x
            in let y' = to_int y
            in Boolean (x' = y')
        | (Begin (x, y)) ->
            (Sequence ((evaluator func_env arg_env x), (evaluator func_env arg_env y)))
        | (CondTerm (cond, trueTerm, falseTerm)) ->
            let condEval = evaluator func_env arg_env cond
            in (match condEval with
                  | (Boolean b) ->
                      evaluator func_env arg_env (if b then trueTerm else falseTerm)
                  | _ -> raise TypeException)
        | (Funct (name, argName, body, inTerm)) ->
            evaluator ((name, (argName, body)) :: func_env) arg_env inTerm
        | (Funct1 (name, arg0Name, arg1Name, body, inTerm)) ->
            evaluator ((name, (arg0Name, body)) :: (name, (arg1Name, body)) :: func_env) arg_env inTerm
        | (Funct2 (name, arg0Name, arg1Name, arg2Name, body, inTerm)) ->
            evaluator ((name, (arg0Name, body)) :: (name, (arg1Name, body)) :: (name, (arg2Name, body)) :: func_env) arg_env inTerm
        | (VarTerm (name, value, inTerm)) ->
            evaluator func_env ((name, (evaluator func_env arg_env value)) :: arg_env) inTerm
        | (Apply (func, arg)) ->
            let argEval = evaluator func_env arg_env arg
                in (match func with
                     | (Var f) ->
                        (match lookup func_env f with
                             | (argName, body) ->
                                evaluator func_env ((argName, argEval) :: arg_env) body)
                             | _ -> raise TypeException)
         | (Apply2 (func, arg0, arg1)) ->
             let argEval0 = evaluator func_env arg_env arg0
             in let argEval1 = evaluator func_env arg_env arg1
                 in (match func with
                      | (Var f) ->
                         (match (lookup2 func_env f) with
                              | ((argName0, body0), (argName1, body1)) ->
                                 evaluator func_env ((argName0, argEval0) :: (argName1, argEval1) :: arg_env) body0)
                              | _ -> raise TypeException)
          | (Apply3 (func, arg0, arg1, arg2)) ->
              let argEval0 = evaluator func_env arg_env arg0
              in let argEval1 = evaluator func_env arg_env arg1
              in let argEval2 = evaluator func_env arg_env arg2
                in (match func with
                       | (Var f) ->
                          (match (lookup3 func_env f) with
                               | ((argName0, body0), (argName1, body1), (argName2, body2)) ->
                                  evaluator func_env ((argName0, argEval0) :: (argName1, argEval1) :: (argName2, argEval2) :: arg_env) body0)
                               | _ -> raise TypeException)
        | (Headterm x) ->
            let x' = set_to_list (to_lang x)
            in let head list =
                match list with
                | [] -> raise EmptyException
                | h :: t -> "\"" ^ h ^ "\""
            in String (head x')
        | (Tailterm x) ->
            let x' = set_to_list (to_lang x)
            in let tail list =
                match list with
                | [] -> raise EmptyException
                | h :: t -> t
            in Language (list_to_set (tail x'))
        | (Appendterm (x, y)) ->
            let x' = set_to_list (to_lang x)
            in let y' = to_string y
            in Language (list_to_set (remove_colons (List.sort compare (uniq (append x' (remove_quotes y'))))))
        | (Unionterm (x, y)) ->
            let x' = set_to_list (to_lang x)
            in let y' = set_to_list (to_lang y)
            in Language (list_to_set (List.sort compare (union x' y')))
        | (Consterm (x, y)) ->
            let x' = remove_quotes (to_string x)
            in Language (list_to_set (x' :: set_to_list (to_lang y)))
        | (Sizeterm x) ->
            Integer (List.length(set_to_list(to_lang x)))
        | (Sortterm x) ->
            Language (list_to_set (List.sort compare (set_to_list (to_lang x))))
        | (Uniqterm x) ->
            Language (list_to_set (uniq (set_to_list (to_lang x))))
        | (Capterm (x, y)) ->
            let x' = (set_to_list (to_lang x))
            in let y' = to_int y
            in Language (list_to_set (cap y' x'))
        | (Concatterm (x, y)) ->
            let x' = remove_quotes (to_string x)
            in let y' = remove_quotes (to_string y)
            in String (list_to_string (remove_colon_from_list (string_to_list (x' ^ y'))))
        | (Lengthterm x) ->
            (Integer (List.length (remove_quotes_from_list(string_to_list (to_string x)))))
;;

let eval term = evaluator [] [] term ;;

let rec print_result result = match result with
    | (Integer i) -> print_int i
    | (Language l) -> print_string l
    | (String s) -> print_string s
    | (Boolean b) -> if b then print_string "true" else print_string "false"
    | (Sequence (a, b)) -> print_result a; print_string "\n";print_result b
    | _ -> raise TypeException
