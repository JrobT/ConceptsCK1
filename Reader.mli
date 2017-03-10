(* File reader.mli, by Alex Everett & Jack Trute *)

type term =
    | Integer of int
    | String of string
    | Start of term * term
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

val eval : term -> term
val print_result : term -> unit

