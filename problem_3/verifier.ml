open Implang
open Z3

let addArrayMapping (array_map:(expr ExprMap.t) StringMap.t) (identifier:string) (idx:Implang.expr) (subst_expr:expr):(expr ExprMap.t) StringMap.t = 
  match (StringMap.find_opt identifier array_map) with 
    None           -> StringMap.add identifier (ExprMap.singleton idx subst_expr) array_map 
  | Some(expr_map) -> StringMap.add identifier (ExprMap.add idx subst_expr expr_map) array_map

(* let z3ExprString (expr:expr):string = (Z3.Expr.to_string expr) *)

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
    Seq(stmt',stmt'')           -> (scanForModifiedVariables stmt')@(scanForModifiedVariables stmt'')
  | Skip                        -> []
  | Post(_)                     -> []
  | Pre(_)                      -> []
  | Assign(identifier,expr)     -> [identifier]
  | Ifthen(_,if_stmt,else_stmt) -> (scanForModifiedVariables if_stmt)@(scanForModifiedVariables else_stmt)
  | Whileloop(_,_,stmt')        -> scanForModifiedVariables stmt'
  | _                           -> failwith ("Unsupported statement"^(stmtToStr stmt))

let rec exprToIdentifier (expr:Implang.expr):string= 
  match expr with
    Var(_) -> exprToStr expr 
  | Arr(_) -> exprToStr expr 
  | _ -> failwith ("Invalid expression in forall statement '" ^ exprToStr(expr) ^ "' must be a variable or array identifier." )

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

(*TODO: Test the Forall case. *)
let rec exprToZ3Predicate (ctx:Z3.context) (expr:expr):Z3.Expr.expr =
  match expr with 
    Num(integer)                                  -> Z3.Arithmetic.Integer.mk_numeral_i ctx integer
  | Var(identifier)                               -> Z3.Arithmetic.Integer.mk_const_s ctx identifier
  | Unary(unary_op,expr')                         -> (unopToPredicate ctx unary_op) (exprToZ3Predicate ctx expr')
  | Binary(binary_op,expr',expr'')                -> (binopToPredicate ctx binary_op) [(exprToZ3Predicate ctx expr');(exprToZ3Predicate ctx expr'')]
  | Arr(identifier,expr')                         -> Z3.Arithmetic.Integer.mk_const_s ctx (exprToStr (Arr(identifier,expr')))
  | Forall(quantified_variable, quantified_expr)  -> (
      let quantified_variable = (Z3.Symbol.mk_string ctx (exprToIdentifier quantified_variable)) in 
      let quantified_expr = exprToZ3Predicate ctx quantified_expr in
      Z3.Quantifier.expr_of_quantifier (Z3.Quantifier.mk_forall ctx [(Z3.Arithmetic.Integer.mk_sort ctx)] [quantified_variable] quantified_expr None [] [] None None)
    )
  | Implies(expr',expr'')                         -> (Z3.Boolean.mk_implies ctx (exprToZ3Predicate ctx expr') (exprToZ3Predicate ctx expr''))
  | _                                             -> failwith ("Unsupported expression " ^ (exprToStr expr))

let rec variablesToForAllExpr (vars:string list) (expr:expr):expr =
  match vars with 
   hd::tl -> Forall(Var(hd),variablesToForAllExpr tl expr )
  | []    -> expr                          

(*TODO: Make Array Map a Reference*)
let rec stmtToPredicate (array_map:(expr ExprMap.t) StringMap.t) (stmt:stmt) (post_predicate:expr):expr= 
  match stmt with 
      Skip                                ->  post_predicate
    | Post(_)                             ->  post_predicate
    | Pre(_)                              ->  post_predicate
    | Assign(identifier, expr')           ->  substExpr array_map post_predicate (Var(identifier)) expr'
    | Seq(stmt',stmt'')                   ->  stmtToPredicate array_map stmt' (stmtToPredicate array_map stmt'' post_predicate)
    | Ifthen(condition,if_stmt,else_stmt) ->  ( 
        let if_expr   = Binary(And,condition, stmtToPredicate array_map if_stmt post_predicate) in
        let else_expr = Binary(And,Unary(Not,condition),(stmtToPredicate array_map else_stmt post_predicate)) in 
        Binary(Or,if_expr,else_expr)
      )
    | Whileloop(condition,invariant, stmt') -> (
        let modified_variables:string list = scanForModifiedVariables stmt' in
        let false_condition = Implies(Unary(Not,condition),post_predicate) in 
        let true_condition  = Implies(condition,(stmtToPredicate array_map stmt' invariant)) in 
        let quantified_expr = Implies(invariant,(Binary(And,true_condition,false_condition))) in 
        Binary(And,invariant,variablesToForAllExpr modified_variables quantified_expr)
      )
    | ArrAssign(identifier, idx_expr, expr') -> (
        let array_map = addArrayMapping array_map identifier idx_expr expr' in
        substExpr array_map post_predicate (Num(1)) (Num(1))
      )
    | _ -> failwith ("Unsupported statement"^(stmtToStr stmt))