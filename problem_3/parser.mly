%{
open Implang ;;
%}

%token <int> INT
%token <string> VAR
%token PLUS MINUS TIMES LT AND OR NOT EQ  EQQ NEQQ GT LE GE FORALL DOT IMPLIES
%token LPAREN RPAREN LCURL RCURL LSQR RSQR SEMI IF ELSE WHILE PRE POST INV MALLOC 
%token EOF
%left AR
%left OR 
%left AND
%left LT GT LE GE EQQ NEQQ
%left PLUS MINUS            /* lowest precedence */
%left TIMES                 /* medium precedence */
%nonassoc NOT               /* highest precedence */
%nonassoc MALL
%start main                 /* the entry point */
%type <Implang.stmt> main
%%


main:
    prog EOF                { $1 }
;

prog : stmt prog { Seq($1, $2) }
      | { Skip }
;
expr:
    INT                     { Num($1) }
  | VAR                     { Var($1) }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { Binary(Plus, $1 , $3) }
  | expr MINUS expr         { Binary(Minus, $1 , $3) }
  | expr TIMES expr         { Binary(Times, $1 , $3) }
  | expr AND expr           { Binary(And, $1, $3) }
  | expr OR expr            { Binary(Or, $1, $3) }
  | expr LT expr            { Binary(Lt, $1, $3) }
  | expr GT expr            { Binary(Lt, $3, $1) }
  | expr GE expr            { Unary(Not, Binary(Lt, $1, $3)) }
  | expr LE expr            { Unary(Not, Binary(Lt, $3, $1)) }
  | expr EQQ expr           { Binary(Eq, $1, $3) }
  | expr NEQQ expr          { Unary(Not, Binary(Eq, $1, $3)) }
  | expr IMPLIES expr       { Implies($1,$3) }
  | NOT expr %prec NOT      { Unary(Not, $2) }
  | TIMES expr              { Deref($2) } 
  | MALLOC expr %prec MALL  { Malloc($2)}
  | FORALL expr DOT expr    { Forall($2,$4) }
  | VAR LSQR expr RSQR      { Arr($1, $3) } 
;

  
stmt :   VAR EQ expr  SEMI                                                      { Assign($1, $3) }
       | VAR LSQR expr RSQR EQ expr SEMI                                        { ArrAssign($1, $3, $6) } 
       | TIMES expr EQ expr SEMI                                                { DerefAssign($2, $4) }
       | PRE LPAREN expr RPAREN SEMI                                            { Pre($3) }
       | POST LPAREN expr RPAREN SEMI                                           { Post($3) }
       | IF LPAREN expr RPAREN LCURL prog RCURL ELSE LCURL prog RCURL           { Ifthen($3, $6, $10) }
       | WHILE LPAREN expr RPAREN LCURL INV LPAREN expr RPAREN SEMI prog RCURL  { Whileloop($3, $8, $11) }
;


