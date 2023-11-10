
open Implang;;
open Z3;;
open Verifier;;


let lexbuf = Lexing.from_channel stdin ;;

let result = Parser.main Lexer.token lexbuf;;
let ctx =Z3.mk_context [("model", "true"); ("proof", "false")];;
let post_condition = exprToPredicate (ctx) (Verifier.getPostCondition result);;
let verify_expr = Verifier.stmtToPredicate ctx result post_condition;;

print_string (z3ExprString verify_expr);;
