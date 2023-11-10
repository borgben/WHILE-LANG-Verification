open Implang
open Z3

let z3ExprString (expr:Z3.Expr.expr):string = (Z3.Expr.to_string expr)

let rec curryBinaryZ3Fn (fn:Z3.context->Z3.Expr.expr->Z3.Expr.expr->Z3.Expr.expr) (ctx:Z3.context) (expr_list:Z3.Expr.expr list) =
  match expr_list with 
    []     -> failwith "Invalid Expression List as input to Binary Expression Function"
   | [hd;hd'] -> fn ctx hd hd'
   | hd::tl  -> fn ctx hd (curryBinaryZ3Fn fn ctx tl) 
  
let rec getPreCondition (stmt:stmt) =
  match stmt with
   Seq(stmt', _) -> 
    (match stmt' with
      Pre(expr) -> expr
    | _         -> failwith "Program doesn't contain a pre-condition!")
  | Pre(expr)   -> expr
  | _ -> failwith "Program doesn't contain a pre-condition!"

let rec getPostCondition (stmt:stmt) =
    match stmt with
     Seq(stmt', stmt'') -> 
      (match stmt'' with
        Post(expr) -> Some(expr)
      | _          -> (match getPostCondition stmt'' with 
                        Some(post_condition) -> Some(post_condition)
                      | _  -> (match getPostCondition stmt' with 
                                Some(post_condition) -> Some(post_condition)
                              | _ -> None
                            )
          )
      )
    | Post(expr)   -> Some(expr)
    | _ -> None

let rec unopToPredicate (ctx:Z3.context) (unary_operator:unop) = 
    match unary_operator with 
      Not -> Z3.Boolean.mk_not ctx 

let binopToPredicate (ctx:Z3.context) (binary_operator:binop) = 
    match binary_operator with 
          Plus  ->  Z3.Arithmetic.mk_add ctx
        | Minus ->  Z3.Arithmetic.mk_sub ctx
        | Times ->  Z3.Arithmetic.mk_mul ctx
        | Lt    ->  (curryBinaryZ3Fn Z3.Arithmetic.mk_lt  ctx)
        | And   ->  Z3.Boolean.mk_and ctx
        | Or    ->  Z3.Boolean.mk_or ctx
        | Eq    ->  (curryBinaryZ3Fn Z3.Boolean.mk_eq ctx)

let rec exprToPredicate (ctx:Z3.context) (expr:expr) =
  match expr with 
     Num(integer) -> Z3.Arithmetic.Integer.mk_numeral_i ctx integer
   | Var(identifier) -> Z3.Arithmetic.Integer.mk_const_s ctx identifier
   | Unary(unary_op,expr') -> (unopToPredicate ctx unary_op) (exprToPredicate ctx expr')
   | Binary(binary_op,expr',expr'') -> (binopToPredicate ctx binary_op) [(exprToPredicate ctx expr');(exprToPredicate ctx expr'')]
   | _ -> failwith ("unsupported expression " ^ (exprToStr expr))



let rec stmtToPredicate (ctx:Z3.context) (stmt:stmt) (expr:Z3.Expr.expr) = 
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
  | _ -> failwith ("Unsupported statement"^(stmtToStr stmt))


