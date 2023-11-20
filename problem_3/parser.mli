type token =
  | INT of (
# 5 "parser.mly"
        int
# 6 "parser.mli"
)
  | VAR of (
# 6 "parser.mly"
        string
# 11 "parser.mli"
)
  | PLUS
  | MINUS
  | TIMES
  | LT
  | AND
  | OR
  | NOT
  | EQ
  | EQQ
  | NEQQ
  | GT
  | LE
  | GE
  | FORALL
  | DOT
  | LPAREN
  | RPAREN
  | LCURL
  | RCURL
  | LSQR
  | RSQR
  | SEMI
  | IF
  | ELSE
  | WHILE
  | PRE
  | POST
  | INV
  | MALLOC
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Implang.stmt
