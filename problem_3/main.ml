open Implang;;
open Z3;;
open Verifier;;

let lexbuf = Lexing.from_channel stdin ;;
let result = Parser.main Lexer.token lexbuf;;
let ctx =Z3.mk_context [("model", "true"); ("proof", "false")];;
let solver = Z3.Solver.mk_solver ctx None;;
let array_map_ref = ref Implang.StringMap.empty
let pre_condition  = (Verifier.getPreCondition result)
let post_condition = (match (Verifier.getPostCondition result) with Some(post_condition) -> post_condition | None -> failwith("Program Does Not Include a Post Condition !"));;
let verification_condition = Z3.Boolean.mk_not ctx (exprToZ3Predicate ctx (Verifier.stmtToPredicate array_map_ref result post_condition));;
Z3.Solver.add solver ([(exprToZ3Predicate ctx pre_condition);verification_condition]);;
Z3.Solver.check solver [];;
print_endline (match Z3.Solver.get_model solver with Some(model) -> Z3.Model.to_string model | None -> failwith "Unsat");;
