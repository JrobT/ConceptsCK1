(* File reader.mli, by Alex Everett & Jack Trute *)

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

val eval : term -> term
val print_result : term -> unit

