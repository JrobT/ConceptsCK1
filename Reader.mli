(* File reader.mli, by Alex Everett & Jack Trute *)

type term = 
    | Integer of int
    | Boolean of bool
    | Language of string
    | String of string
    | Var of string
    | Start of term * term
    | Sequence of term * term
    | If of term * term * term
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

val eval : term -> term
val print_result : term -> unit

