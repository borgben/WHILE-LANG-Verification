#!/usr/bin/env ocaml
#use "topfind";;
#require "z3";;
open Implang
open Z3

(* let rec verifyProg ast:stmt = 
  match ast with
  |  Seq(stmt',stmt'') ->  *)

type vc = VC of stmt * Z3.Expr.expr

let rec curryBinaryZ3Fn (fn:context->Z3.Expr.expr->Z3.Expr.expr>Z3.Expr.expr) (ctx:context) (expr_list:expr list) =
  match expr_list with 
    []     -> failwith "Invalid Expression List as input to Binary Expression Function"
   | hd:hd' -> fn ctx hd hd'
   | hd:tl  -> fn ctx hd (curryBinaryZ3Fn fn ctx tl) 
  
let rec getPreCondition (stmt:stmt) =
  match stmt with
   Seq(stmt', _) -> 
    match stmt' with
      Pre(expr) -> expr
    | _         -> failwith "Program doesn't contain a pre-condition!"
  | Pre(expr)   -> expr
  | _ -> failwith "Program doesn't contain a pre-condition!"

let rec getPostCondition (stmt:stmt) =
    match stmt with
     Seq(_, stmt'') -> 
      match stmt'' with
        Post(expr) -> expr
      | _          -> getPostCondition stmt''
    | Post(expr)   -> expr
    | _ -> failwith "Program doesn't contain a post-condition!"


let rec exprToPredicate (ctx:context) (expr:expr) =
  match expr with 
     Num(integer) -> Z3.Arithmetic.Integer.mk_numeral_i ctx integer
   | Var(identifier) -> Z3.Arithmetic.Integer.mk_const_s ctx identifier
   | Unary(unary_op,expr') -> (unopToPredicate ctx unary_op) (exprToPredicate ctx expr')
   | Binary(binary_op,expr',expr'') -> (binopToPredicate ctx binary_op) [(exprToPredicate ctx expr');(exprToPredicate ctx expr'')]
   | _ -> failwith ("unsupported expression " ^ (exprToStr))

let rec unopToPredicate (ctx:context) (unary_operator:unop) = 
  match unary_operator with 
    Not -> Z3.Boolean.mk_not ctx 

let binopToPredicate (ctx:context) (binary_operator:unop) = 
  match binary_operator with 
    Plus  ->  Z3.Arithmetic.Integer.mk_add ctx
  | Minus ->  Z3.Arithmetic.Integer.mk_sub ctx
  | Times ->  Z3.Arithmetic.Integer.mk_mul ctx
  | Lt    ->  (curryBinaryZ3Fn Z3.Arithmetic.Integer.mk_lt  ctx)
  | And   ->  (curryBinaryZ3Fn Z3.Boolean.mk_and ctx)
  | Or    ->  (curryBinaryZ3Fn Z3.Boolean.mk_or ctx) 
  | Eq    ->  (curryBinaryZ3Fn Z3.Boolean.mk_eq ctx)

let stmtToPredicate (ctx:context) (stmt:stmt) = 
  match stmt with 
    Skip -> ctx
  | Post(_) -> ctx
  | Pre(_)  -> ctx
  | Assign(identifier, expr) ->  Z

