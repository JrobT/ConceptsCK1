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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Reader
# 36 "parser.ml"
let yytransl_const = [|
  262 (* COMMENT *);
  263 (* START *);
  264 (* END *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* LESSTHAN *);
  268 (* GREATERTHAN *);
  269 (* EQUALS *);
  270 (* IF *);
  271 (* THEN *);
  272 (* ELSE *);
  273 (* LPAREN *);
  274 (* RPAREN *);
  275 (* APPEND *);
  276 (* CONS *);
  277 (* UNION *);
  278 (* FUNCT *);
  279 (* IN *);
  280 (* FARROW *);
  281 (* VAR *);
  282 (* CONCAT *);
  283 (* LENGTH *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOLEAN *);
  259 (* LANGUAGE *);
  260 (* STRING *);
  261 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\002\000\004\000\
\003\000\009\000\010\000\011\000\005\000\006\000\008\000\005\000\
\005\000\005\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\006\000\003\000\004\000\005\000\000\000\
\000\000\000\000\020\000\000\000\007\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\016\000\017\000\
\000\000\000\000\000\000\018\000\013\000\000\000\000\000\000\000\
\000\000\000\000\014\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\000\000\000\000\010\000\000\000\011\000\
\012\000"

let yydgoto = "\002\000\
\011\000\012\000"

let yysindex = "\255\255\
\061\255\000\000\000\000\000\000\000\000\000\000\000\000\061\255\
\061\255\054\255\000\000\005\000\000\000\000\255\061\255\061\255\
\006\255\007\255\061\255\061\255\061\255\000\000\061\255\061\255\
\061\255\011\255\008\255\061\255\005\255\061\255\000\000\009\255\
\010\255\001\255\061\255\012\255\000\000\081\255\000\000\000\000\
\002\255\061\255\253\254\000\000\000\000\013\255\014\255\061\255\
\016\255\061\255\000\000\061\255\017\255\061\255\015\255\018\255\
\061\255\019\255\000\000\061\255\025\255\000\000\027\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\250\255"

let yytablesize = 280
let yytable = "\001\000\
\009\000\013\000\014\000\021\000\022\000\041\000\047\000\023\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\034\000\
\031\000\032\000\033\000\050\000\035\000\036\000\037\000\038\000\
\042\000\048\000\039\000\040\000\043\000\044\000\051\000\046\000\
\059\000\000\000\000\000\049\000\062\000\052\000\054\000\057\000\
\060\000\053\000\064\000\055\000\065\000\056\000\000\000\058\000\
\000\000\000\000\061\000\000\000\000\000\063\000\003\000\004\000\
\005\000\006\000\007\000\008\000\009\000\003\000\004\000\005\000\
\006\000\007\000\008\000\009\000\000\000\000\000\010\000\000\000\
\015\000\016\000\000\000\017\000\000\000\010\000\018\000\019\000\
\020\000\003\000\004\000\005\000\006\000\007\000\008\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\010\000\045\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\000\000\000\000\000\000\000\000\000\009\000"

let yycheck = "\001\000\
\000\000\008\000\009\000\010\000\000\000\005\001\005\001\008\001\
\015\000\016\000\005\001\005\001\019\000\020\000\021\000\005\001\
\023\000\024\000\025\000\023\001\013\001\028\000\018\001\030\000\
\024\001\024\001\018\001\018\001\035\000\018\001\018\001\038\000\
\018\001\255\255\255\255\042\000\018\001\024\001\023\001\023\001\
\023\001\048\000\018\001\050\000\018\001\052\000\255\255\054\000\
\255\255\255\255\057\000\255\255\255\255\060\000\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\255\255\255\255\017\001\255\255\
\019\001\020\001\255\255\022\001\255\255\017\001\025\001\026\001\
\027\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\018\001\255\255\255\255\255\255\255\255\023\001"

let yynames_const = "\
  COMMENT\000\
  START\000\
  END\000\
  PLUS\000\
  MINUS\000\
  LESSTHAN\000\
  GREATERTHAN\000\
  EQUALS\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LPAREN\000\
  RPAREN\000\
  APPEND\000\
  CONS\000\
  UNION\000\
  FUNCT\000\
  IN\000\
  FARROW\000\
  VAR\000\
  CONCAT\000\
  LENGTH\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  BOOLEAN\000\
  LANGUAGE\000\
  STRING\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 25 "parser.mly"
               ( _1 )
# 236 "parser.ml"
               : Reader.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 29 "parser.mly"
                                       ( Integer _1 )
# 243 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 30 "parser.mly"
                                       ( Language _1 )
# 250 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 31 "parser.mly"
                                       ( String _1 )
# 257 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 32 "parser.mly"
                                       ( Var _1 )
# 264 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 33 "parser.mly"
                                       ( Boolean _1 )
# 271 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 34 "parser.mly"
                                       ( _2 )
# 278 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 35 "parser.mly"
                                       ( Start (_2, _4) )
# 286 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 36 "parser.mly"
                                       ( _2 )
# 293 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 37 "parser.mly"
                                                          ( Funct (_3, _4, _6, _8) )
# 303 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 38 "parser.mly"
                                                                ( Funct1 (_3, _4, _5, _7, _9) )
# 314 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 39 "parser.mly"
                                                                      ( Funct2 (_3, _4, _5, _6, _8, _10) )
# 326 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 40 "parser.mly"
                                                 ( Apply2 (_2, _3, _4) )
# 335 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 41 "parser.mly"
                                                 ( Apply3 (_2, _3, _4, _5) )
# 345 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 42 "parser.mly"
                                                  ( VarTerm (_3, _5, _7) )
# 354 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 43 "parser.mly"
                                       ( AppendTerm (_3, _4) )
# 362 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 44 "parser.mly"
                                       ( ConsTerm (_3, _4)   )
# 370 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 45 "parser.mly"
                                       ( ConcatTerm (_3, _4) )
# 378 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 46 "parser.mly"
                                       ( LengthTerm _3       )
# 385 "parser.ml"
               : 'term))
(* Entry parsermain *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parsermain (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Reader.term)
