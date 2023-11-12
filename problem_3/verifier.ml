open Implang
open Z3

let z3ExprString (expr:Z3.Expr.expr):string = (Z3.Expr.to_string expr)

let rec curryBinaryZ3Fn (fn:Z3.context->Z3.Expr.expr->Z3.Expr.expr->Z3.Expr.expr) (ctx:Z3.context) (expr_list:Z3.Expr.expr list):Z3.Expr.expr =
  match expr_list with 
    []        -> failwith "Invalid Expression List as input to Binary Expression Function"
   | [hd;hd'] -> fn ctx hd hd'
   | hd::tl   -> fn ctx hd (curryBinaryZ3Fn fn ctx tl) 
  
let rec getPreCondition (stmt:stmt):expr =
  match stmt with
   Seq(stmt', _) -> 
    (match stmt' with
      Pre(expr) -> expr
    | _         -> failwith "Program doesn't contain a pre-condition!")
  | Pre(expr)   -> expr
  | _ -> failwith "Program doesn't contain a pre-condition!"

let rec getPostCondition (stmt:stmt):expr option=
    match stmt with
     Seq(stmt', stmt'') -> 
      (match stmt'' with
        Post(expr) -> Some(expr)
      | _          -> (match getPostCondition stmt'' with 
                        Some(post_condition) -> Some(post_condition)
                      | _  -> (match getPostCondition stmt' with 
                                Some(post_condition) -> Some(post_condition)
                              | _ -> None
                            )))
    | Post(expr)   -> Some(expr)
    | _ -> None

let rec makeList (n:int) (constant:'a):'a list = 
  match n with 
    0 -> []
  | _ -> constant::(makeList (n-1) constant)

let rec scanForModifiedVariables (stmt:stmt): string list = 
  match stmt with 
    Seq(stmt',stmt'') -> (scanForModifiedVariables stmt')@(scanForModifiedVariables stmt'')
  | Skip              -> []
  | Post(_)           -> []
  | Pre(_)            -> []
  | Assign(identifier,expr) -> [identifier]
  | Ifthen(_,if_stmt,else_stmt) -> (scanForModifiedVariables if_stmt)@(scanForModifiedVariables else_stmt)
  | Whileloop(_,_,stmt')        -> scanForModifiedVariables stmt'
  | _ -> failwith ("Unsupported statement"^(stmtToStr stmt))

let rec unopToPredicate (ctx:Z3.context) (unary_operator:unop):Z3.Expr.expr -> Z3.Expr.expr = 
    match unary_operator with 
      Not -> Z3.Boolean.mk_not ctx 

let binopToPredicate (ctx:Z3.context) (binary_operator:binop):Z3.Expr.expr list -> Z3.Expr.expr = 
    match binary_operator with 
          Plus  ->  Z3.Arithmetic.mk_add ctx
        | Minus ->  Z3.Arithmetic.mk_sub ctx
        | Times ->  Z3.Arithmetic.mk_mul ctx
        | Lt    ->  (curryBinaryZ3Fn Z3.Arithmetic.mk_lt  ctx)
        | And   ->  Z3.Boolean.mk_and ctx
        | Or    ->  Z3.Boolean.mk_or ctx
        | Eq    ->  (curryBinaryZ3Fn Z3.Boolean.mk_eq ctx)

let rec exprToPredicate (ctx:Z3.context) (expr:expr):Z3.Expr.expr =
  match expr with 
     Num(integer) -> Z3.Arithmetic.Integer.mk_numeral_i ctx integer
   | Var(identifier) -> Z3.Arithmetic.Integer.mk_const_s ctx identifier
   | Unary(unary_op,expr') -> (unopToPredicate ctx unary_op) (exprToPredicate ctx expr')
   | Binary(binary_op,expr',expr'') -> (binopToPredicate ctx binary_op) [(exprToPredicate ctx expr');(exprToPredicate ctx expr'')]
   | _ -> failwith ("unsupported expression " ^ (exprToStr expr))

let rec stmtToPredicate (ctx:Z3.context) (stmt:stmt) (expr:Z3.Expr.expr):Z3.Expr.expr = 
  match stmt with 
    Skip    -> expr
  | Post(_) -> expr
  | Pre(_)  -> expr
  | Assign(identifier, expr') ->  Z3.Expr.substitute expr [(Z3.Arithmetic.Integer.mk_const_s ctx identifier)] [(exprToPredicate ctx expr')]
  | Seq(stmt',stmt'')         ->  stmtToPredicate ctx stmt' (stmtToPredicate ctx stmt'' expr)
  | Ifthen(condition,if_stmt,else_stmt) -> 
      let condition_expr = exprToPredicate ctx condition in
      let if_expr   = (Z3.Boolean.mk_and ctx [condition_expr;(stmtToPredicate ctx if_stmt expr)]) in 
      let else_expr = (Z3.Boolean.mk_and ctx [(Z3.Boolean.mk_not ctx condition_expr);(stmtToPredicate ctx else_stmt expr)]) in 
      (Z3.Boolean.mk_or ctx [if_expr;else_expr])
  | Whileloop(condition,invariant, stmt') -> (
      let modified_variables = scanForModifiedVariables stmt' in
      let condition:Z3.Expr.expr  = exprToPredicate ctx condition in 
      let invariant:Z3.Expr.expr = exprToPredicate ctx invariant in 
      let false_condition = Z3.Boolean.mk_implies ctx (Z3.Boolean.mk_not ctx condition) expr in 
      let true_condition = Z3.Boolean.mk_implies ctx condition (stmtToPredicate ctx stmt' invariant) in 
      let quantified_expr = Z3.Boolean.mk_implies ctx invariant (Z3.Boolean.mk_and ctx [true_condition;false_condition]) in 
      Z3.Boolean.mk_and ctx [invariant;Z3.Quantifier.expr_of_quantifier (Z3.Quantifier.mk_forall ctx (makeList (List.length modified_variables) (Z3.Arithmetic.Integer.mk_sort ctx)) (Z3.Symbol.mk_strings ctx modified_variables) quantified_expr None [] [] None None)]
    )
  | _ -> failwith ("Unsupported statement"^(stmtToStr stmt))