open Implang
open Z3

let addArrayMapping (ctx:context) (array_map:Z3.Expr.expr StringMap.t) (identifier:string) (idx:Z3.Expr.expr) (subst_expr:Z3.Expr.expr):Z3.Expr.expr StringMap.t = 
  match (StringMap.find_opt identifier array_map) with 
    None           -> StringMap.add identifier (Z3.Z3Array.mk_const_s ctx identifier (Z3.Arithmetic.Integer.mk_sort ctx) (Z3.Arithmetic.Integer.mk_sort ctx)) array_map 
  | Some(z3_array) -> StringMap.add identifier (Z3.Z3Array.mk_store ctx idx subst_expr z3_array) array_map

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

let rec scanForModifiedVariables (stmt:stmt): expr list = 
  match stmt with 
    Seq(stmt',stmt'')                -> (scanForModifiedVariables stmt')@(scanForModifiedVariables stmt'')
  | Skip                             -> []
  | Post(_)                          -> []
  | Pre(_)                           -> []
  | Assign(identifier,expr)          -> [Var(identifier)]
  | Ifthen(_,if_stmt,else_stmt)      -> (scanForModifiedVariables if_stmt)@(scanForModifiedVariables else_stmt)
  | Whileloop(_,_,stmt')             -> scanForModifiedVariables stmt'
  | ArrAssign(identifier,idx,expr')  -> [Arr(identifier,idx)] 
  | _                                -> failwith ("Unsupported statement"^(stmtToStr stmt))

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
let rec exprToZ3Predicate (ctx:Z3.context) (array_map:Z3.Expr.expr StringMap.t) (expr:expr):Z3.Expr.expr =
  match expr with 
    Num(integer)                                  -> Z3.Arithmetic.Integer.mk_numeral_i ctx integer
  | Var(identifier)                               -> Z3.Arithmetic.Integer.mk_const_s ctx identifier
  | Unary(unary_op,expr')                         -> (unopToPredicate ctx unary_op) (exprToZ3Predicate ctx array_map expr')
  | Binary(binary_op,expr',expr'')                -> (binopToPredicate ctx binary_op) [(exprToZ3Predicate ctx array_map expr');(exprToZ3Predicate ctx array_map expr'')]
  | Arr(identifier,expr')                         -> Z3.Z3Array.mk_select ctx (StringMap.find identifier array_map) (exprToZ3Predicate ctx array_map expr')
  | Forall(quantified_variable, quantified_expr)  -> (
      let quantified_variable = (Z3.Symbol.mk_string ctx (exprToIdentifier quantified_variable)) in 
      let quantified_expr = exprToZ3Predicate ctx array_map quantified_expr in
      Z3.Quantifier.expr_of_quantifier (Z3.Quantifier.mk_forall ctx [(Z3.Arithmetic.Integer.mk_sort ctx)] [quantified_variable] quantified_expr None [] [] None None)
    )
  | Implies(expr',expr'')                         -> (Z3.Boolean.mk_implies ctx (exprToZ3Predicate ctx array_map expr') (exprToZ3Predicate ctx array_map expr''))
  | _                                             -> failwith ("Unsupported expression " ^ (exprToStr expr))

let rec variablesToForAllExpr (vars:expr list) (expr:expr):expr =
  match vars with 
   hd::tl -> Forall(hd,variablesToForAllExpr tl expr )
  | []    -> expr                          

(* let rec stmtToPredicate (array_map_ref:((expr ExprMap.t) StringMap.t) ref) (stmt:stmt) (post_predicate:expr):expr= 
  match stmt with 
      Skip                                ->  post_predicate
    | Post(_)                             ->  post_predicate
    | Pre(_)                              ->  post_predicate
    | Assign(identifier, expr')           ->  substExpr array_map_ref post_predicate (Var(identifier)) expr'
    | Seq(stmt',stmt'')                   ->  stmtToPredicate array_map_ref stmt' (stmtToPredicate array_map_ref stmt'' post_predicate)
    | Ifthen(condition,if_stmt,else_stmt) ->  ( 
        let if_expr   = Binary(And,condition, stmtToPredicate array_map_ref if_stmt post_predicate) in
        let else_expr = Binary(And,Unary(Not,condition),(stmtToPredicate array_map_ref else_stmt post_predicate)) in 
        Binary(Or,if_expr,else_expr)
      )
    | Whileloop(condition,invariant, stmt') -> (
        let modified_variables:expr list = scanForModifiedVariables stmt' in
        let false_condition = Implies(Unary(Not,condition),post_predicate) in 
        let true_condition  = Implies(condition,(stmtToPredicate array_map_ref stmt' invariant)) in 
        let quantified_expr = Implies(invariant,(Binary(And,true_condition,false_condition))) in 
        Binary(And,invariant,variablesToForAllExpr modified_variables quantified_expr)
      )
    | ArrAssign(identifier, idx_expr, expr') -> (
        let array_map_ref = ref (addArrayMapping (!array_map_ref) identifier idx_expr expr') in
        substExpr array_map_ref post_predicate (Num(1)) (Num(1))
      )
    | _ -> failwith ("Unsupported statement"^(stmtToStr stmt)) *)

let rec stmtToPredicate (ctx:Z3.context) (array_map:(Z3.Expr.expr) StringMap.t) (stmt:stmt) (expr:Z3.Expr.expr):Z3.Expr.expr = 
      match stmt with 
        Skip    -> expr
      | Post(_) -> expr
      | Pre(_)  -> expr
      | Assign(identifier, expr') ->  Z3.Expr.substitute expr [(Z3.Arithmetic.Integer.mk_const_s ctx identifier)] [(exprToZ3Predicate ctx array_map expr')]
      | Seq(stmt',stmt'')         ->  stmtToPredicate ctx array_map stmt' (stmtToPredicate ctx array_map stmt'' expr)
      | Ifthen(condition,if_stmt,else_stmt) -> 
          let condition_expr = exprToZ3Predicate ctx array_map condition in
          let if_expr   = (Z3.Boolean.mk_and ctx [condition_expr;(stmtToPredicate ctx array_map if_stmt expr)]) in 
          let else_expr = (Z3.Boolean.mk_and ctx [(Z3.Boolean.mk_not ctx condition_expr);(stmtToPredicate ctx array_map else_stmt expr)]) in 
          (Z3.Boolean.mk_or ctx [if_expr;else_expr])
      | Whileloop(condition,invariant, stmt') -> (
          let modified_variables = scanForModifiedVariables stmt' in
          let condition:Z3.Expr.expr  = exprToZ3Predicate ctx array_map condition in 
          let invariant:Z3.Expr.expr = exprToZ3Predicate ctx array_map invariant in 
          let false_condition = Z3.Boolean.mk_implies ctx (Z3.Boolean.mk_not ctx condition) expr in 
          let true_condition = Z3.Boolean.mk_implies ctx condition (stmtToPredicate ctx array_map stmt' invariant) in 
          let quantified_expr = Z3.Boolean.mk_implies ctx invariant (Z3.Boolean.mk_and ctx [true_condition;false_condition]) in 
          Z3.Boolean.mk_and ctx [invariant;Z3.Quantifier.expr_of_quantifier (Z3.Quantifier.mk_forall ctx (makeList (List.length modified_variables) (Z3.Arithmetic.Integer.mk_sort ctx)) (Z3.Symbol.mk_strings ctx modified_variables) quantified_expr None [] [] None None)]
        )
      | ArrAssign(identifier, idx_expr, expr') -> (
          let array_map' = addArrayMapping array_map identifier (exprToZ3Predicate ctx array_map idx_expr) (exprToZ3Predicate ctx array_map expr') in
          Z3.Expr.substitute expr [StringMap.find identifier array_map] [StringMap.find identifier array_map']
        )
      | _ -> failwith ("Unsupported statement"^(stmtToStr stmt))