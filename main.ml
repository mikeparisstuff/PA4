(* type 'a mult_node = T of 'a * 'a mult_node;; *)

(* ANI: identifier = identifier, typ = type *)
type identifier = IDENT of int * string;;

(* name of formal, type of formal *)
type formals = FORMAL of identifier * identifier;;

(* INT(line_no, value) *)
type case_element = CE of identifier * identifier * expr;;

type let_binding = LB_NO_INIT of identifier * identifier
	| LB_INIT of identifier * identifier * expr;;

type expr = INT of int * int
	| STRING of int * string
	| ASSIGN of int * identifier * expr
	| DYN_DISPATCH of expr * identifier * expr list
	| STAT_DISPATCH of expr * identifier * identifier * expr list
	| SELF_DISPATCH of identifier * expr list
	| IF_ELSE of expr * expr * expr
	| WHILE of expr * expr
	| BLOCK of expr list
	| NEW of identifier
	| ISVOID of expr
	| PLUS of expr * expr
	| MINUS of expr * expr
	| TIMES of expr * expr
	| DIVIDE of expr * expr
	| LT of expr * expr
	| LE of expr * expr
	| EQ of expr * expr
	| NOT of expr
	| NEGATE of expr
	| BOOL of string
	| LET of let_binding list * expr
	| CASE of expr * case_element list;;

type feature = ATTRIBUTE of identifier * identifier * expr option
	| METHOD of identifier * identifier * formals list * expr;;

(* name of class, optional inheritance, number of features, features list *)
type clas = CLASS of identifier * identifier option * int * feature list;;

(* number of classes, class list *)
type program = PROGRAM of int * clas list;;


(**********  BUILD THE AST FROM FILE  *************)
let rec build_expr lines = match lines with
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
		let (rem_lines, bindings) = binding_list lines in
		let (rem_lines, expr) = build_expr rem_lines in
		let line_num = int_of_string line_no
		and num_bindings = int_of_string num_bindings in
|   _ -> ([], STRING(0, "DIFFERENT EXPRESSION"))

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

let rec print_expr expr = match expr with
	INT(line_no, value) -> Printf.printf "%d\ninteger\n%d\n" line_no value
|	STRING (line_no, str) -> Printf.printf "%d\nstring\n%s\n" line_no str
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
	