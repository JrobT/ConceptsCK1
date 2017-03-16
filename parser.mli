type token =
  | INT of (int)
  | BOOLEAN of (bool)
  | LANGUAGE of (string)
  | STRING of (string)
  | IDENT of (string)
  | EMPTYWORD of (string)
  | COMMENT
  | BEGIN
  | FINISH
  | PLUS
  | MINUS
  | LESS
  | GREATER
  | EQUALS
  | COND
  | THEN
  | ELSE
  | LPAREN
  | RPAREN
  | HEAD
  | TAIL
  | UNION
  | APPEND
  | CONS
  | SORT
  | UNIQ
  | CAP
  | SIZE
  | FUNCT
  | IN
  | FARROW
  | VAR
  | EQUAL
  | CONCAT
  | LENGTH
  | EOF

val parsermain :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Reader.term
