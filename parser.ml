type token =
  | VAR of (string)
  | INT of (int)
  | ADD
  | MUL
  | DIV
  | MIN
  | LT
  | GT
  | LEQ
  | GEQ
  | CMPEQ
  | NOT
  | AND
  | OR
  | LPAREN
  | RPAREN
  | RB
  | LB
  | EQ
  | SC
  | COM
  | MAIN
  | IF
  | WHILE
  | ELSE
  | RETURN
  | VARS
  | EOF

open Parsing;;
let yytransl_const = [|
  259 (* ADD *);
  260 (* MUL *);
  261 (* DIV *);
  262 (* MIN *);
  263 (* LT *);
  264 (* GT *);
  265 (* LEQ *);
  266 (* GEQ *);
  267 (* CMPEQ *);
  268 (* NOT *);
  269 (* AND *);
  270 (* OR *);
  271 (* LPAREN *);
  272 (* RPAREN *);
  273 (* RB *);
  274 (* LB *);
  275 (* EQ *);
  276 (* SC *);
  277 (* COM *);
  278 (* MAIN *);
  279 (* IF *);
  280 (* WHILE *);
  281 (* ELSE *);
  282 (* RETURN *);
  283 (* VARS *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\005\000\005\000\006\000\006\000\007\000\007\000\
\007\000\007\000\007\000\007\000\008\000\008\000\009\000\009\000\
\010\000\011\000\011\000\012\000\012\000\013\000\013\000\013\000\
\014\000\002\000\002\000\003\000\000\000"

let yylen = "\002\000\
\003\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\002\000\003\000\002\000\003\000\001\000\003\000\004\000\002\000\
\005\000\007\000\005\000\003\000\002\000\003\000\001\000\002\000\
\003\000\001\000\003\000\002\000\003\000\002\000\003\000\004\000\
\003\000\000\000\002\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\045\000\000\000\000\000\000\000\000\000\
\000\000\000\000\043\000\000\000\036\000\000\000\000\000\041\000\
\000\000\001\000\000\000\037\000\000\000\003\000\000\000\000\000\
\000\000\038\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\000\000\000\000\000\017\000\000\000\
\015\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\000\032\000\039\000\000\000\044\000\019\000\
\000\000\000\000\000\000\018\000\000\000\000\000\028\000\033\000\
\000\000\005\000\007\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\000\000\020\000\023\000\000\000\
\000\000\022\000\000\000\000\000\027\000\029\000\000\000\000\000\
\030\000\026\000"

let yydgoto = "\002\000\
\004\000\005\000\010\000\031\000\039\000\066\000\032\000\092\000\
\033\000\034\000\014\000\008\000\016\000\006\000"

let yysindex = "\023\000\
\038\255\000\000\025\255\000\000\020\255\038\255\002\255\034\255\
\040\255\053\000\000\000\037\255\000\000\044\255\042\255\000\000\
\045\255\000\000\055\255\000\000\245\254\000\000\076\255\076\255\
\076\255\000\000\048\255\052\255\076\255\055\255\214\255\095\255\
\056\255\095\255\034\255\000\000\035\255\076\255\000\000\057\255\
\000\000\000\000\099\000\076\255\076\255\232\255\054\255\076\255\
\076\255\076\255\076\255\076\255\076\255\076\255\076\255\076\255\
\076\255\076\255\000\000\000\000\000\000\059\255\000\000\000\000\
\138\255\063\255\250\255\000\000\113\000\127\000\000\000\000\000\
\041\255\000\000\000\000\041\255\006\255\006\255\006\255\006\255\
\006\255\041\255\041\255\000\000\076\255\000\000\000\000\062\255\
\062\255\000\000\069\255\058\255\000\000\000\000\068\255\062\255\
\000\000\000\000"

let yyrindex = "\000\000\
\065\255\000\000\000\000\000\000\000\000\065\255\000\000\000\000\
\000\000\000\000\000\000\253\254\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\081\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\119\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\083\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\147\255\000\000\000\000\163\255\020\000\035\000\050\000\065\000\
\080\000\179\255\195\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\088\255\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\096\000\000\000\233\255\000\000\021\000\000\000\183\255\
\229\255\000\000\019\000\000\000\069\000\000\000"

let yytablesize = 399
let yytable = "\041\000\
\042\000\043\000\012\000\037\000\060\000\046\000\062\000\038\000\
\048\000\049\000\050\000\051\000\034\000\065\000\067\000\093\000\
\034\000\013\000\057\000\058\000\069\000\070\000\098\000\001\000\
\073\000\074\000\075\000\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\040\000\022\000\036\000\003\000\007\000\
\023\000\009\000\021\000\022\000\049\000\050\000\024\000\023\000\
\047\000\025\000\064\000\015\000\018\000\024\000\017\000\012\000\
\025\000\019\000\026\000\020\000\035\000\065\000\044\000\095\000\
\027\000\028\000\045\000\029\000\030\000\021\000\022\000\037\000\
\061\000\072\000\023\000\084\000\040\000\022\000\086\000\091\000\
\024\000\023\000\096\000\025\000\097\000\094\000\042\000\024\000\
\025\000\025\000\025\000\027\000\028\000\025\000\029\000\021\000\
\022\000\031\000\021\000\025\000\023\000\011\000\025\000\063\000\
\025\000\090\000\024\000\000\000\000\000\025\000\025\000\025\000\
\000\000\025\000\000\000\000\000\000\000\027\000\028\000\000\000\
\029\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\000\000\002\000\002\000\000\000\002\000\000\000\
\000\000\000\000\002\000\002\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\004\000\057\000\058\000\
\004\000\004\000\004\000\004\000\004\000\004\000\085\000\004\000\
\004\000\000\000\004\000\000\000\000\000\006\000\004\000\004\000\
\006\000\006\000\006\000\006\000\006\000\006\000\000\000\006\000\
\006\000\000\000\006\000\000\000\000\000\013\000\006\000\006\000\
\013\000\013\000\013\000\013\000\013\000\013\000\000\000\013\000\
\013\000\000\000\013\000\000\000\000\000\014\000\013\000\013\000\
\014\000\014\000\014\000\014\000\014\000\014\000\000\000\014\000\
\014\000\000\000\014\000\000\000\000\000\000\000\014\000\014\000\
\048\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\000\000\057\000\058\000\000\000\000\000\000\000\000\000\
\000\000\059\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\000\000\057\000\058\000\000\000\000\000\
\000\000\000\000\000\000\071\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\000\000\057\000\058\000\
\000\000\000\000\000\000\000\000\000\000\087\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\000\000\
\002\000\002\000\008\000\008\000\008\000\008\000\008\000\002\000\
\000\000\000\000\000\000\008\000\000\000\000\000\000\000\008\000\
\008\000\009\000\009\000\009\000\009\000\009\000\000\000\000\000\
\000\000\000\000\009\000\000\000\000\000\000\000\009\000\009\000\
\010\000\010\000\010\000\010\000\010\000\000\000\000\000\000\000\
\000\000\010\000\000\000\000\000\000\000\010\000\010\000\011\000\
\011\000\011\000\011\000\011\000\000\000\000\000\000\000\000\000\
\011\000\000\000\000\000\000\000\011\000\011\000\012\000\012\000\
\012\000\012\000\012\000\000\000\000\000\000\000\000\000\012\000\
\000\000\000\000\000\000\012\000\012\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\000\000\057\000\
\058\000\000\000\068\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\000\000\057\000\058\000\000\000\
\088\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\000\000\057\000\058\000\000\000\089\000"

let yycheck = "\023\000\
\024\000\025\000\001\001\015\001\032\000\029\000\034\000\019\001\
\003\001\004\001\005\001\006\001\016\001\037\000\038\000\089\000\
\020\001\016\001\013\001\014\001\044\000\045\000\096\000\001\000\
\048\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\001\001\002\001\019\000\001\001\015\001\
\006\001\022\001\001\001\002\001\004\001\005\001\012\001\006\001\
\030\000\015\001\016\001\018\001\000\000\012\001\015\001\001\001\
\015\001\021\001\017\001\016\001\016\001\085\000\015\001\091\000\
\023\001\024\001\015\001\026\001\027\001\001\001\002\001\015\001\
\017\001\020\001\006\001\017\001\001\001\002\001\016\001\018\001\
\012\001\006\001\025\001\015\001\017\001\017\001\022\001\012\001\
\001\001\002\001\015\001\023\001\024\001\006\001\026\001\001\001\
\002\001\017\001\016\001\012\001\006\001\006\000\015\001\035\000\
\017\001\085\000\012\001\255\255\255\255\015\001\023\001\024\001\
\255\255\026\001\255\255\255\255\255\255\023\001\024\001\255\255\
\026\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\013\001\014\001\255\255\016\001\255\255\
\255\255\255\255\020\001\021\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\003\001\013\001\014\001\
\006\001\007\001\008\001\009\001\010\001\011\001\021\001\013\001\
\014\001\255\255\016\001\255\255\255\255\003\001\020\001\021\001\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\013\001\
\014\001\255\255\016\001\255\255\255\255\003\001\020\001\021\001\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\013\001\
\014\001\255\255\016\001\255\255\255\255\003\001\020\001\021\001\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\013\001\
\014\001\255\255\016\001\255\255\255\255\255\255\020\001\021\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\013\001\014\001\255\255\255\255\255\255\255\255\
\255\255\020\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\013\001\014\001\255\255\255\255\
\255\255\255\255\255\255\020\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\013\001\014\001\
\255\255\255\255\255\255\255\255\255\255\020\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\013\001\014\001\007\001\008\001\009\001\010\001\011\001\020\001\
\255\255\255\255\255\255\016\001\255\255\255\255\255\255\020\001\
\021\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\255\255\255\255\016\001\255\255\255\255\255\255\020\001\021\001\
\007\001\008\001\009\001\010\001\011\001\255\255\255\255\255\255\
\255\255\016\001\255\255\255\255\255\255\020\001\021\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\255\255\255\255\
\016\001\255\255\255\255\255\255\020\001\021\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\255\255\016\001\
\255\255\255\255\255\255\020\001\021\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\013\001\
\014\001\255\255\016\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\014\001\255\255\
\016\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\013\001\014\001\255\255\016\001"

let yynames_const = "\
  ADD\000\
  MUL\000\
  DIV\000\
  MIN\000\
  LT\000\
  GT\000\
  LEQ\000\
  GEQ\000\
  CMPEQ\000\
  NOT\000\
  AND\000\
  OR\000\
  LPAREN\000\
  RPAREN\000\
  RB\000\
  LB\000\
  EQ\000\
  SC\000\
  COM\000\
  MAIN\000\
  IF\000\
  WHILE\000\
  ELSE\000\
  RETURN\000\
  VARS\000\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fdecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'main) in
    Obj.repr(
# 15 "parser.mly"
                               ( {Ast.func = _1; Ast.main = _2} )
# 283 "parser.ml"
               : Ast.prg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 18 "parser.mly"
                        ( Ast.Var _1 )
# 290 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 19 "parser.mly"
                        ( Ast.Int _1 )
# 297 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 20 "parser.mly"
                        ( Ast.BinOp(_1,"+",_3) )
# 305 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 21 "parser.mly"
                        ( Ast.BinOp(_1,"*",_3) )
# 313 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 22 "parser.mly"
                        ( Ast.BinOp(_1,"-",_3) )
# 321 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 23 "parser.mly"
                        ( Ast.BinOp(_1,"/",_3) )
# 329 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 24 "parser.mly"
                        ( Ast.BinOp(_1,"<",_3) )
# 337 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
                        ( Ast.BinOp(_1,">",_3) )
# 345 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 26 "parser.mly"
                        ( Ast.BinOp(_1,"<=",_3) )
# 353 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 27 "parser.mly"
                        ( Ast.BinOp(_1,">=",_3) )
# 361 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 28 "parser.mly"
                        ( Ast.BinOp(_1,"==",_3) )
# 369 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 29 "parser.mly"
                        ( Ast.BinOp(_1,"&&",_3) )
# 377 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 30 "parser.mly"
                        ( Ast.BinOp(_1,"||",_3) )
# 385 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 31 "parser.mly"
                        ( Ast.UniOp("-",_2) )
# 392 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 32 "parser.mly"
                        ( Ast.UniOp("!",_2) )
# 399 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 34 "parser.mly"
      ( Ast.Call (_1,_2) )
# 407 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
                        ( _2 )
# 414 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                        ( [] )
# 420 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'elist) in
    Obj.repr(
# 40 "parser.mly"
                        ( _2 )
# 427 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 44 "parser.mly"
                        ( _1::[] )
# 434 "parser.ml"
               : 'elist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'elist) in
    Obj.repr(
# 45 "parser.mly"
                        ( _1::_3 )
# 442 "parser.ml"
               : 'elist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                        ( Ast.Assign (_1,_3) )
# 450 "parser.ml"
               : 'stm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                        ( Ast.Expr _1 )
# 457 "parser.ml"
               : 'stm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmblock) in
    Obj.repr(
# 52 "parser.mly"
      ( Ast.If (_3, _5, []) )
# 465 "parser.ml"
               : 'stm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmblock) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmblock) in
    Obj.repr(
# 54 "parser.mly"
      ( Ast.If (_3, _5, _7) )
# 474 "parser.ml"
               : 'stm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmblock) in
    Obj.repr(
# 56 "parser.mly"
      ( Ast.While (_3, _5) )
# 482 "parser.ml"
               : 'stm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                        ( Ast.Return _2 )
# 489 "parser.ml"
               : 'stm))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
                        ( [] )
# 495 "parser.ml"
               : 'stmblock))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmlist) in
    Obj.repr(
# 61 "parser.mly"
                        ( _2 )
# 502 "parser.ml"
               : 'stmblock))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stm) in
    Obj.repr(
# 65 "parser.mly"
                        ( _1::[] )
# 509 "parser.ml"
               : 'stmlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmlist) in
    Obj.repr(
# 66 "parser.mly"
                        ( _1::_2 )
# 517 "parser.ml"
               : 'stmlist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vlist) in
    Obj.repr(
# 70 "parser.mly"
                        ( _2 )
# 524 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "parser.mly"
                        ( _1::[] )
# 531 "parser.ml"
               : 'vlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vlist) in
    Obj.repr(
# 75 "parser.mly"
                        ( _1::_3 )
# 539 "parser.ml"
               : 'vlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
                        ( [] )
# 545 "parser.ml"
               : 'fparam))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vlist) in
    Obj.repr(
# 80 "parser.mly"
                        ( _2 )
# 552 "parser.ml"
               : 'fparam))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                        ( ([],[]) )
# 558 "parser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmlist) in
    Obj.repr(
# 85 "parser.mly"
                        ( ([],_2) )
# 565 "parser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'vars) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmlist) in
    Obj.repr(
# 86 "parser.mly"
                        ( (_2,_3) )
# 573 "parser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fparam) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 90 "parser.mly"
      (
        let (v,b) = _3 in
        {
          Ast.fname   = _1;
          Ast.fparams = _2;
          Ast.fvars   = v;
          Ast.fbody   = b;
        }
      )
# 590 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
                        ( [] )
# 596 "parser.ml"
               : 'fdecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecls) in
    Obj.repr(
# 103 "parser.mly"
                        ( _1::_2 )
# 604 "parser.ml"
               : 'fdecls))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 108 "parser.mly"
      (
        let (v,b) = _4 in
        {
          Ast.mainvars = v;
          Ast.mainbody = b;
        }
      )
# 617 "parser.ml"
               : 'main))
(* Entry prg *)
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
let prg (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prg)
