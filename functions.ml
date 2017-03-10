(* File functions.ml, by Alex Everett & Jack Trute *)

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

let rec cat l =
    match l with
        | [] -> []
        | ',' :: t -> []
        | '}' :: t -> []
        | ' ' :: t -> []
        | h :: t -> h :: (cat t)
;;

let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
         try Some (input_line channel) with End_of_file -> None);;

let remove_dollar str = list_to_string (remove_dollar_from_list (string_to_list str))
;;

let lines = line_stream_of_channel stdin;;
