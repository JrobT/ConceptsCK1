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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Reader
# 44 "parser.ml"
let yytransl_const = [|
  263 (* COMMENT *);
  264 (* BEGIN *);
  265 (* FINISH *);
  266 (* PLUS *);
  267 (* MINUS *);
  268 (* LESS *);
  269 (* GREATER *);
  270 (* EQUALS *);
  271 (* COND *);
  272 (* THEN *);
  273 (* ELSE *);
  274 (* LPAREN *);
  275 (* RPAREN *);
  276 (* HEAD *);
  277 (* TAIL *);
  278 (* UNION *);
  279 (* APPEND *);
  280 (* CONS *);
  281 (* SORT *);
  282 (* UNIQ *);
  283 (* CAP *);
  284 (* SIZE *);
  285 (* FUNCT *);
  286 (* IN *);
  287 (* FARROW *);
  288 (* VAR *);
  289 (* EQUAL *);
  290 (* CONCAT *);
  291 (* LENGTH *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOLEAN *);
  259 (* LANGUAGE *);
  260 (* STRING *);
  261 (* IDENT *);
  262 (* EMPTYWORD *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\002\000\004\000\003\000\
\005\000\005\000\005\000\005\000\005\000\008\000\009\000\010\000\
\011\000\004\000\005\000\006\000\008\000\004\000\004\000\005\000\
\005\000\005\000\005\000\004\000\004\000\004\000\004\000\005\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\003\000\004\000\005\000\000\000\000\000\
\000\000\033\000\000\000\006\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\000\000\000\022\000\023\000\000\000\000\000\
\000\000\030\000\031\000\000\000\029\000\000\000\000\000\000\000\
\028\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\
\000\000\024\000\025\000\026\000\032\000\000\000\000\000\000\000\
\027\000\009\000\010\000\011\000\012\000\013\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\000\000\
\000\000\000\000\000\000\014\000\000\000\000\000\000\000\021\000\
\000\000\000\000\015\000\000\000\016\000\017\000"

let yydgoto = "\002\000\
\010\000\011\000"

let yysindex = "\004\000\
\134\255\000\000\000\000\000\000\000\000\000\000\134\255\134\255\
\091\255\000\000\007\000\000\000\255\254\134\255\134\255\134\255\
\134\255\134\255\134\255\134\255\134\255\134\255\134\255\014\255\
\015\255\134\255\134\255\065\255\000\000\134\255\008\255\009\255\
\016\255\134\255\134\255\134\255\017\255\018\255\134\255\020\255\
\022\255\000\255\134\255\032\255\134\255\134\255\134\255\134\255\
\134\255\045\255\000\000\134\255\000\000\000\000\035\255\036\255\
\037\255\000\000\000\000\038\255\000\000\251\254\134\255\040\255\
\000\000\041\255\042\255\043\255\046\255\052\255\000\000\126\255\
\063\255\000\000\000\000\000\000\000\000\001\255\134\255\051\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\066\255\
\134\255\056\255\134\255\059\255\134\255\000\000\071\255\134\255\
\072\255\134\255\082\255\000\000\073\255\134\255\085\255\000\000\
\134\255\086\255\000\000\088\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\251\255"

let yytablesize = 287
let yytable = "\078\000\
\008\000\012\000\013\000\028\000\001\000\090\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\050\000\052\000\
\051\000\079\000\062\000\053\000\055\000\056\000\057\000\091\000\
\063\000\060\000\054\000\058\000\059\000\064\000\061\000\066\000\
\067\000\068\000\069\000\070\000\072\000\003\000\073\000\004\000\
\005\000\006\000\065\000\007\000\008\000\074\000\075\000\076\000\
\077\000\080\000\081\000\082\000\083\000\084\000\009\000\071\000\
\085\000\003\000\088\000\004\000\005\000\006\000\086\000\007\000\
\008\000\092\000\045\000\046\000\047\000\048\000\049\000\089\000\
\093\000\000\000\009\000\095\000\094\000\097\000\096\000\099\000\
\098\000\100\000\101\000\003\000\103\000\004\000\005\000\006\000\
\106\000\007\000\008\000\108\000\104\000\102\000\105\000\107\000\
\109\000\014\000\110\000\000\000\009\000\000\000\015\000\016\000\
\017\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\000\000\000\000\025\000\000\000\026\000\027\000\003\000\000\000\
\004\000\005\000\006\000\000\000\007\000\008\000\003\000\000\000\
\004\000\005\000\006\000\000\000\007\000\008\000\000\000\009\000\
\087\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000\
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
\000\000\008\000\008\000\008\000\008\000\008\000\008\000\000\000\
\008\000\008\000\000\000\008\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000"

let yycheck = "\005\001\
\000\000\007\000\008\000\009\000\001\000\005\001\000\000\009\001\
\014\000\015\000\016\000\017\000\018\000\019\000\020\000\021\000\
\022\000\023\000\005\001\005\001\026\000\027\000\028\000\016\001\
\030\000\031\001\005\001\019\001\034\000\035\000\036\000\031\001\
\033\001\039\000\019\001\019\001\019\001\043\000\019\001\045\000\
\046\000\047\000\048\000\049\000\050\000\001\001\052\000\003\001\
\004\001\005\001\019\001\007\001\008\001\019\001\019\001\019\001\
\019\001\063\000\019\001\019\001\019\001\019\001\018\001\019\001\
\019\001\001\001\072\000\003\001\004\001\005\001\019\001\007\001\
\008\001\079\000\010\001\011\001\012\001\013\001\014\001\017\001\
\030\001\255\255\018\001\089\000\019\001\091\000\031\001\093\000\
\030\001\019\001\096\000\001\001\098\000\003\001\004\001\005\001\
\102\000\007\001\008\001\105\000\019\001\030\001\030\001\019\001\
\019\001\015\001\019\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\255\255\255\255\032\001\255\255\034\001\035\001\001\001\255\255\
\003\001\004\001\005\001\255\255\007\001\008\001\001\001\255\255\
\003\001\004\001\005\001\255\255\007\001\008\001\255\255\018\001\
\019\001\255\255\255\255\255\255\255\255\255\255\255\255\018\001\
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
\255\255\009\001\010\001\011\001\012\001\013\001\014\001\255\255\
\016\001\017\001\255\255\019\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\030\001"

let yynames_const = "\
  COMMENT\000\
  BEGIN\000\
  FINISH\000\
  PLUS\000\
  MINUS\000\
  LESS\000\
  GREATER\000\
  EQUALS\000\
  COND\000\
  THEN\000\
  ELSE\000\
  LPAREN\000\
  RPAREN\000\
  HEAD\000\
  TAIL\000\
  UNION\000\
  APPEND\000\
  CONS\000\
  SORT\000\
  UNIQ\000\
  CAP\000\
  SIZE\000\
  FUNCT\000\
  IN\000\
  FARROW\000\
  VAR\000\
  EQUAL\000\
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
  EMPTYWORD\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 27 "parser.mly"
               ( _1 )
# 281 "parser.ml"
               : Reader.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 31 "parser.mly"
                                                        ( Integer _1 )
# 288 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 32 "parser.mly"
                                                        ( Language _1 )
# 295 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "parser.mly"
                                                        ( String _1 )
# 302 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 34 "parser.mly"
                                                        ( Var _1 )
# 309 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 35 "parser.mly"
                                                        ( _2 )
# 316 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 36 "parser.mly"
                                                        ( Begin (_2, _4) )
# 324 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 37 "parser.mly"
                                                        ( _2 )
# 331 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 38 "parser.mly"
                                                        ( PlusTerm (_2, _4) )
# 339 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 39 "parser.mly"
                                                        ( MinusTerm (_2, _4) )
# 347 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 40 "parser.mly"
                                                        ( LessTerm (_2, _4) )
# 355 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 41 "parser.mly"
                                                        ( GreaterTerm (_2, _4) )
# 363 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 42 "parser.mly"
                                                        ( EqualsTerm (_2, _4) )
# 371 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 43 "parser.mly"
                                                        ( CondTerm (_3, _5, _7) )
# 380 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 44 "parser.mly"
                                                          ( Funct (_3, _4, _6, _8) )
# 390 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 45 "parser.mly"
                                                                ( Funct1 (_3, _4, _5, _7, _9) )
# 401 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 46 "parser.mly"
                                                                      ( Funct2 (_3, _4, _5, _6, _8, _10) )
# 413 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 47 "parser.mly"
                                                 ( Apply (_2, _3) )
# 421 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 48 "parser.mly"
                                                 ( Apply2 (_2, _3, _4) )
# 430 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 49 "parser.mly"
                                                 ( Apply3 (_2, _3, _4, _5) )
# 440 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 50 "parser.mly"
                                                 ( VarTerm (_3, _5, _7) )
# 449 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 51 "parser.mly"
                                              ( Headterm _3 )
# 456 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 52 "parser.mly"
                                                 ( Tailterm _3 )
# 463 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 53 "parser.mly"
                                                 ( Unionterm  (_3, _4) )
# 471 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 54 "parser.mly"
                                                 ( Appendterm (_3, _4) )
# 479 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 55 "parser.mly"
                                                 ( Consterm (_3, _4) )
# 487 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 56 "parser.mly"
                                                 ( Concatterm (_3, _4) )
# 495 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 57 "parser.mly"
                                                 ( Lengthterm _3 )
# 502 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 58 "parser.mly"
                                                 ( Sizeterm _3 )
# 509 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 59 "parser.mly"
                                                 ( Sortterm _3 )
# 516 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 60 "parser.mly"
                                                 ( Uniqterm _3 )
# 523 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 61 "parser.mly"
                                                 ( Capterm (_3, _4) )
# 531 "parser.ml"
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
