type token =
  | INT of (int)
  | BOOLEAN of (bool)
  | LANGUAGE of (string)
  | STRING of (string)
  | IDENT of (string)
  | COMMENT
  | START
  | END
  | PLUS
  | MINUS
  | LESSTHAN
  | GREATERTHAN
  | EQUALS
  | IF
  | THEN
  | ELSE
  | LPAREN
  | RPAREN
  | APPEND
  | CONS
  | UNION
  | FUNCT
  | IN
  | FARROW
  | VAR
  | CONCAT
  | LENGTH
  | EOF

val parsermain :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Reader.term
