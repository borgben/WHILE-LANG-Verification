open Printf;;

type binop = Plus | Minus | Times | And | Or | Lt | Eq ;;

type unop = Not ;;

type expr = 
			Binary of binop * expr * expr 
	  | Unary of unop * expr 
	  | Arr of string * expr
	  | Malloc of expr
	  | Deref of expr
	  | Var of string 
	  | Num of int
		| Forall of expr * expr
		| Implies of expr * expr

	module StringMap = Map.Make(String)
	module ExprMap = Map.Make(struct 
			type t = expr 
			let compare x y = if x = y then 1 else 0 
		end)
		

type stmt = 
      Skip 
    | Post of expr
	  | Pre of expr 
    | Assign of string * expr 
    | ArrAssign of string * expr * expr
	  | DerefAssign of expr * expr
	  | Seq of stmt * stmt
	  | Ifthen of expr * stmt * stmt
	  | Whileloop of expr * expr* stmt

let binopToStr op = 
	match op with 
		| Plus -> "+" 
		| Minus -> "-" 
		| Times -> "*" 
		| And -> "&" 
		| Or -> "|" 
		| Lt -> "<"  
		| Eq -> "=="

let unopToStr op = match op with | Not -> "!" 

let rec exprToStr e = 
	match e with 
	| Num(a)              -> string_of_int (a)
	| Var v               -> v
	| Unary( op, e )      -> "(" ^ unopToStr(op) ^ exprToStr(e)  ^ ")"
	| Binary(op, e1, e2)  -> "(" ^ exprToStr(e1) ^ binopToStr(op) ^ exprToStr(e2) ^ ")"
	| Arr(base, idx)      -> base ^ "[" ^ exprToStr(idx) ^ "]"
	| Malloc(size)        -> "malloc(" ^ exprToStr(size) ^ ")"
	| Deref(ptr)          -> "*(" ^ exprToStr(ptr) ^ ")"
	| Forall(expr,expr')  -> "V" ^exprToStr(expr)^". "^exprToStr(expr')
	| Implies(expr,expr') -> exprToStr(expr)^" => "^exprToStr(expr')

let rec stmtToStr c = 
	match c with 
  | Skip -> "/*skip*/\n"
  | Pre(e) -> "Pre( " ^ exprToStr(e) ^ ");\n"
	| Post(e) -> "Post(" ^ exprToStr(e) ^ ");\n"
	| Assign(lhs, rhs) -> lhs ^ "= " ^ exprToStr(rhs) ^ "; \n"
	| ArrAssign(base, idx, rhs) -> base ^ "[" ^ exprToStr(idx) ^ "] = " ^ exprToStr(rhs) ^ ";\n";
	| DerefAssign(ptr, rhs) -> "*(" ^ exprToStr(ptr) ^ ")=" ^ exprToStr(rhs) ^ ";\n";
	| Seq(c1, c2) -> stmtToStr(c1) ^ stmtToStr(c2)
	| Ifthen(e, c1, c2) -> "if( " ^ exprToStr(e) ^ "){ \n" ^ stmtToStr(c1) ^ "} else { \n" ^ stmtToStr(c2) ^ "} \n" 
	| Whileloop(e, inv, c1) -> "while(" ^ exprToStr(e) ^ "){\n" ^ "Inv(" ^ exprToStr(inv) ^ "); \n" ^ stmtToStr(c1) ^ "}\n"

let rec substExpr (array_map:(expr ExprMap.t) StringMap.t) (base_expr:expr) (og_expr:expr) (subst_expr:expr):expr = 
	match base_expr with 
		Num(_)               -> base_expr
	| Var(_)               -> if base_expr = og_expr then subst_expr else base_expr
	| Unary(op,expr)       -> Unary(op,(substExpr array_map expr og_expr subst_expr))
	| Binary(op,expr,expr')-> Binary(op,(substExpr array_map expr og_expr subst_expr),(substExpr array_map expr' og_expr subst_expr))
	| Arr(identifier,expr) -> (
		  let update_identifier id is_updated = match is_updated with Some(updated_expr) -> updated_expr | None -> id in 
			let expr_map:expr ExprMap.t = StringMap.find identifier array_map in  
			let updated_arr_expr  = (substExpr array_map expr og_expr subst_expr) in 
			Arr(identifier ,update_identifier updated_arr_expr (ExprMap.find_opt updated_arr_expr expr_map))
		)
	| Forall(expr,expr')   -> if expr = og_expr then base_expr else Forall(expr,substExpr array_map expr' og_expr subst_expr)
	| Implies(expr,expr')  -> Implies(substExpr array_map expr og_expr subst_expr,substExpr array_map expr' og_expr subst_expr)
	| _                    -> failwith ("Invalid Expression "^exprToStr(base_expr)^" !")                  