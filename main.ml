(* type 'a mult_node = T of 'a * 'a mult_node;; *)

(* ANI: identifier = identifier, typ = type *)
type identifier = IDENT of int * string

(* name of formal, type of formal *)
and formals = FORMAL of identifier * identifier

(* INT(line_no, value) *)
and case_element = CE of identifier * identifier * expr

and let_binding = LB_NO_INIT of identifier * identifier
	| LB_INIT of identifier * identifier * expr

and expr = INT of int * int
	| STRING of int * string
	| ASSIGN of int * identifier * expr
	| DYN_DISPATCH of expr * identifier * expr list
	| STAT_DISPATCH of expr * identifier * identifier * expr list
	| SELF_DISPATCH of identifier * expr list
	| IF_ELSE of int * expr * expr * expr
	| WHILE of int * expr * expr
	| BLOCK of int * expr list
	| NEW of int * identifier
	| ISVOID of int * expr
	| PLUS of int * expr * expr
	| MINUS of int * expr * expr
	| TIMES of int * expr * expr
	| DIVIDE of int * expr * expr
	| LT of int * expr * expr
	| LE of int * expr * expr
	| EQ of int * expr * expr
	| NOT of int * expr
	| NEGATE of int * expr
	| TRUE of int
	| FALSE of int
	| LET of int * let_binding list * expr
	| CASE of expr * case_element list
	| IDENTIFIER of int * identifier

and feature = ATTRIBUTE of identifier * identifier * expr option
	| METHOD of identifier * identifier * formals list * expr

(* name of class, optional inheritance, number of features, features list *)
and clas = CLASS of identifier * identifier option * int * feature list

(* number of classes, class list *)
and program = PROGRAM of int * clas list;;


(**********  BUILD THE AST FROM FILE  *************)
let rec binding_list lines num_bindings = match num_bindings with
	0 -> (lines, [])
|	_ -> match lines with
			"let_binding_init" :: line_no :: ident :: type_line_no :: type_name :: lines ->
				let (rem_lines, expr) = build_expr lines in
				let (rem_lines, binding_nodes) = binding_list rem_lines (num_bindings-1) in
				let line_num = int_of_string line_no
				and type_line_num = int_of_string type_line_no in
				(rem_lines, LB_INIT( IDENT(line_num, ident), IDENT(type_line_num, type_name), expr) :: binding_nodes)
		| "let_binding_no_init" :: line_no :: ident :: type_line_no :: type_name :: lines ->
				let (rem_lines, binding_nodes) = binding_list lines (num_bindings-1) in
				let line_num = int_of_string line_no
				and type_line_num = int_of_string type_line_no in
				(rem_lines, LB_NO_INIT( IDENT(line_num, ident), IDENT(type_line_num, type_name)) :: binding_nodes ) 
		|  _ -> (lines, [])

and expr_list lines num_exprs = match num_exprs with
	0 -> (lines, [])
|	_ -> 
	let (rem_lines, expr) = build_expr lines in
	let (rem_lines, expr_list) = expr_list rem_lines (num_exprs-1) in
	(rem_lines, expr :: expr_list)



and build_expr lines = match lines with
	line_no :: "integer" :: num :: rem_lines -> 
		(rem_lines, INT((int_of_string line_no), (int_of_string num)))
|	line_no :: "string" :: word :: rem_lines ->
		(rem_lines, STRING( (int_of_string line_no), word))
|   line_no :: "assign" :: iden_line_no :: name :: lines  ->
		let (rem_lines, expr) = build_expr lines in
		let line_num = int_of_string line_no
		and iden_line_num = int_of_string iden_line_no in
		( rem_lines, ASSIGN( line_num, IDENT(iden_line_num, name), expr) )
| 	line_no :: "let" :: num_bindings :: lines ->
		let (rem_lines, bindings) = binding_list lines (int_of_string num_bindings) in
		let (rem_lines, expr) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		( rem_lines, LET(line_num, bindings, expr) )
| 	line_no :: "if" :: lines ->
		let (rem_lines, expr) = build_expr lines in
		let (rem_lines, expr1) = build_expr rem_lines in
		let (rem_lines, expr2) = build_expr rem_lines in
		(rem_lines, IF_ELSE( (int_of_string line_no), expr, expr1, expr2 ) )
| 	line_no :: "le" :: lines ->
		let (rem_lines, expr) = build_expr lines in
		let (rem_lines, expr2) = build_expr rem_lines in
		(rem_lines, LE( (int_of_string line_no), expr, expr2 ) )
| 	line_no :: "identifier" :: ident_line_no :: ident :: lines ->
		let line_num = int_of_string line_no
		and ident_line_num = int_of_string ident_line_no in
		(lines, IDENTIFIER( line_num, IDENT( ident_line_num, ident) ) )
|   line_no :: "while" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let (rem_lines, expr2) = build_expr rem_lines
		and line_num = int_of_string line_no in
		(rem_lines, WHILE(line_num, expr, expr2) )
| 	line_no :: "block" :: num_exprs :: lines ->
		let (rem_lines, exprs) = expr_list lines (int_of_string num_exprs) in
		let line_num = int_of_string line_no in
		(rem_lines, BLOCK( line_num, exprs ))
|   line_no :: "plus" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, PLUS(line_num, lhs, rhs ) )
|   line_no :: "minus" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, MINUS(line_num, lhs, rhs ) )
|   line_no :: "times" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, TIMES( line_num, lhs, rhs ))
|   line_no :: "divide" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, DIVIDE( line_num, lhs, rhs ))
|   line_no :: "lt" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, LT( line_num, lhs, rhs ))
|   line_no :: "le" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, LE( line_num, lhs, rhs ))
|   line_no :: "eq" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, EQ( line_num, lhs, rhs ))
|   line_no :: "not" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let line_num = int_of_string line_no in
		(rem_lines, NOT( line_num, expr ))
|   line_no :: "negate" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let line_num = int_of_string line_no in
		(rem_lines, NOT( line_num, expr ))
|   line_no :: "isvoid" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let line_num = int_of_string line_no in
		(rem_lines, ISVOID( line_num, expr ))
| 	line_no :: "new" :: ident_no :: ident :: lines ->
		let line_num = int_of_string line_no 
		and ident_line_num = int_of_string ident_no in
		(lines, NEW( line_num, IDENT(ident_line_num, ident)))
|   line_no :: "true" :: lines -> 
		let line_num = int_of_string line_no in
		(lines, TRUE(line_num))
|   line_no :: "false" :: lines -> 
		let line_num = int_of_string line_no in
		(lines, FALSE(line_num))
(* |   line_no :: "dynamic_dispatch" :: lines  -> 
		let (rem_lines, expr) = build_expr lines in
		match rem_lines with
			line_no :: ident :: rem_lines -> match rem_lines with 
				num_exprs :: lines -> 
					let (rem_lines, expr_list) = expr_list lines in *)
|   _ -> ([], STRING(0, "DIFFERENT EXPRESSION"))
;;

let rec formal_list lines num_formals = match num_formals with 
	0 -> (lines, [])
|   _ -> match lines with
			line_no :: name :: type_line_no :: type_name :: rem_lines -> begin
				let (rem, formals) = formal_list rem_lines (num_formals - 1) in
				let line_num = int_of_string line_no
				and type_line_num = int_of_string type_line_no in
				(rem, FORMAL( IDENT(line_num, name), IDENT(type_line_num, type_name) ) :: formals ) 
			end
		|	[] -> ([], [])
;;

let rec feature_list lines num_features = match num_features with
	0 -> (lines, [])
|   _ -> match lines with
			"attribute_no_init" :: line_no :: name :: type_line_no :: t :: lines -> begin
					let (rem, feature_nodes) = feature_list lines (num_features-1) in
					let (line_num, type_line_num) = ( (int_of_string line_no), (int_of_string type_line_no)) in
					(rem, ATTRIBUTE(IDENT(line_num, name), IDENT(type_line_num, t), None) :: feature_nodes );
				end
		|   "attribute_init" :: line_no :: name :: type_line_no :: type_name :: lines -> begin
					let (rem_lines, expr) = build_expr lines in
					let (rem, feature_nodes) = feature_list rem_lines (num_features-1) in
					let line_num = int_of_string line_no
					and type_line_num = int_of_string type_line_no in
					(rem, ATTRIBUTE( IDENT(line_num, name), IDENT(type_line_num, type_name), Some(expr)) :: feature_nodes );
				end
		|   "method" :: line_no :: name :: num_formals :: type_line_no :: type_name :: lines -> begin
					(* METHOD WITH 0 FORMALS -- NEED ANOTHER RULE FOR METHODS WITH FORMALS *)

					(* Get the list of formals *)
					let (lines, formals_nodes) = formal_list lines (int_of_string num_formals) in
					(* Get this methods expression and the remaining lines after that *)
					let (rem_lines, expr) = build_expr lines in

					(* Get the rest of the features in this class and the remaining lines after those *)
					let (rem, feature_nodes) = feature_list rem_lines (num_features-1)
					and line_num = int_of_string line_no
					and type_line_num = int_of_string type_line_no in
					(rem, METHOD( IDENT(line_num, name), IDENT(type_line_num, type_name), [], expr) :: feature_nodes )
				end
		| 	_ -> ([], [])
;;

let rec class_list lines num_classes = match num_classes with
	0 -> []
|	_ -> match lines with
			line_no :: name :: "no_inherits" :: num_features :: lines -> begin
				let (rem_lines, feat_nodes) = feature_list lines (int_of_string num_features) in
				let line_num = int_of_string line_no in
				CLASS( IDENT(line_num, name), None, (int_of_string num_features), feat_nodes ) :: (class_list rem_lines (num_classes-1));
			end
		|  line_no :: name :: "inherits" :: type_line_no :: type_name :: num_features :: lines -> begin
				let (rem_lines, feat_nodes) = feature_list lines (int_of_string num_features) in
				let line_num = int_of_string line_no
				and type_line_num = int_of_string type_line_no in
				CLASS( IDENT(line_num, name), Some(IDENT(type_line_num, type_name)), (int_of_string num_features), feat_nodes) :: (class_list rem_lines (num_classes-1));
			end
		|   _ -> []
;;

let ast lst = match lst with
	num_classes :: lines -> PROGRAM((int_of_string num_classes) , (class_list lines (int_of_string num_classes) ) )
|   [] -> PROGRAM(0, []);;




(****************  PRINTING HELPER METHODS *****************)
let print_list lst = List.iter (fun a -> print_string (a ^ "\n")) lst;;

let print_identifier ident = match ident with
	IDENT(line_no, str) -> Printf.printf "%d\n%s\n" line_no str
|   _ -> print_string "NOT IDENTIFIER"
;;

let rec print_bindings bindings = match bindings with
	[] -> ()
|	LB_NO_INIT(ident, typ) :: tl  -> 
				print_string "let_binding_no_init\n";
				print_identifier ident;
				print_identifier typ;
				print_bindings tl;
|	LB_INIT(ident, typ, expr) :: tl ->
				print_string "let_binding_init\n";
				print_identifier ident;
				print_identifier typ;
				print_expr expr;
				print_bindings tl;
|   _ -> ()

and print_expr expr = match expr with
	INT(line_no, value) -> Printf.printf "%d\ninteger\n%d\n" line_no value
|	STRING (line_no, str) -> Printf.printf "%d\nstring\n%s\n" line_no str
|   ASSIGN (line_no, ident, expr) -> 
					Printf.printf "%d\nassign\n" line_no;
					print_identifier ident;
					print_expr expr;
| 	LET (line_no, bindings, expr) ->
					Printf.printf "%d\nlet\n%d\n" line_no (List.length bindings);
					print_bindings bindings;
					print_expr expr;
|   IF_ELSE (line_no, expr1, expr2, expr3) -> 
					Printf.printf "%d\nif\n" line_no;
					print_expr expr1;
					print_expr expr2;
					print_expr expr3;
|   WHILE (line_no, expr, expr1) -> 
					Printf.printf "%d\nwhile\n" line_no;
					print_expr expr;
					print_expr expr1;
| 	BLOCK (line_no, exprs) ->
					Printf.printf "%d\nblock\n%d\n" line_no (List.length exprs);
					List.iter print_expr exprs;
|   IDENTIFIER (line_no, ident) -> 
					Printf.printf "%d\nidentifier\n" line_no;
					print_identifier ident;
|   PLUS (line_no, lhs, rhs) -> 
					Printf.printf "%d\nplus\n" line_no;
					print_expr lhs;
					print_expr rhs;
|   MINUS (line_no, lhs, rhs) -> 
					Printf.printf "%d\nminus\n" line_no;
					print_expr lhs;
					print_expr rhs;
|   TIMES (line_no, lhs, rhs) -> 
					Printf.printf "%d\ntimes\n" line_no;
					print_expr lhs;
					print_expr rhs;
|   DIVIDE (line_no, lhs, rhs) -> 
					Printf.printf "%d\ndivide\n" line_no;
					print_expr lhs;
					print_expr rhs;
| 	LT (line_no, lhs, rhs) ->
					Printf.printf "%d\nlt\n" line_no;
					print_expr lhs;
					print_expr rhs;
|   LE (line_no, expr1, expr2) -> 
					Printf.printf "%d\nle\n" line_no;
					print_expr expr1;
					print_expr expr2;
|   EQ (line_no, expr1, expr2) -> 
					Printf.printf "%d\neq\n" line_no;
					print_expr expr1;
					print_expr expr2;
|   NOT (line_no, expr) -> 
					Printf.printf "%d\nnot\n" line_no;
					print_expr expr;
|   NEGATE (line_no, expr) -> 
					Printf.printf "%d\nnegate\n" line_no;
					print_expr expr;
|   ISVOID (line_no, expr) -> 
					Printf.printf "%d\nisvoid\n" line_no;
					print_expr expr;
|   NEW (line_no, ident) -> 
					Printf.printf "%d\nnew\n" line_no;
					print_identifier ident;
|   TRUE (line_no) ->
					Printf.printf "%d\ntrue\n" line_no; 
|   FALSE (line_no) ->
					Printf.printf "%d\nfalse\n" line_no;
;;

let rec print_feature_list feat_list = match feat_list with
	[] -> ()
|   hd :: tl -> match hd with
		ATTRIBUTE(ident, typ, None) -> 
							print_string "attribute_no_init\n";
							print_identifier ident;
							print_identifier typ;
							print_feature_list tl;
	|   ATTRIBUTE(ident, typ, Some(expr)) ->
	 						print_string "attribute_init\n";
							print_identifier ident;
							print_identifier typ;
							print_expr expr;
							print_feature_list tl;
	|   METHOD(name, typ, formals, expr) -> 
							print_string "method\n";
							print_identifier name;
							Printf.printf "%d\n" (List.length formals);
							print_identifier typ;
							print_expr expr;
							print_feature_list tl;
	|   _ -> print_string "NOT ATTRIBUTE"
;;

let rec print_class_list class_list = match class_list with
	[] -> ()
|   hd :: tl -> match hd with
		CLASS(ident, None, num_features, feat_list) -> 
									print_identifier ident;
									print_string "no_inherits\n";
									Printf.printf "%d\n" num_features;
									print_feature_list feat_list;
									print_class_list tl;
	|   CLASS(ident, Some(inher), num_features, feat_list) -> 
									print_identifier ident;
									print_string "inherits\n";
									print_identifier inher;
									Printf.printf "%d\n" num_features;
									print_feature_list feat_list;
									print_class_list tl;
	|   _ -> print_string "NOT A CLASS"
;;

let rec print_ast ast = match ast with
	PROGRAM(num_classes, class_list) -> 
				Printf.printf "%d\n" num_classes;
				print_class_list class_list;
|   _ -> print_string "NOT A PROGRAM"
;;

let filename = match Array.length (Sys.argv) with
		2 -> Array.get Sys.argv 1
	|   _ -> failwith "Please provide a single file command line arguments"
;;


let lines = ref [] in
let in_file = open_in filename in
try
	while true do
		let line = input_line in_file in
		lines := line :: !lines;
	done
with
| _ -> begin
	let cool_input = (List.rev !lines) in
	(* print_list cool_input; *)
	let p = ast cool_input in
	print_ast p;
end
	