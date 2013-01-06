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

val prg :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prg
