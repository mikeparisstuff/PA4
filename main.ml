(* type 'a mult_node = T of 'a * 'a mult_node;; *)

(* ANI: identifier = identifier, typ = type *)
type identifier = IDENT of int * string;;
(* type typ = TYPE of int * string;; *)
type formals = FORMAL of string * identifier;;
type expr = INT of int
	| STRING of string;;

type feature = ATTRIBUTE of identifier * identifier * expr option;;
	(* | METHOD of identifier * identifier * formals list * expr;; *)

type clas = CLASS of identifier * identifier option * feature list;;
type program = PROGRAM of clas list;;

(* int = line_no, string = identifier name *)


(**********  BUILD THE AST FROM FILE  *************)
let rec feature_list lines num_features = match num_features with
	0 -> (lines, [])
|   _ -> match lines with
			"attribute_no_init" :: line_no :: name :: type_line_no :: t :: rem_lines -> begin
					let (rem, feature_nodes) = feature_list rem_lines (num_features-1) in
					let (line_num, type_line_num) = ( (int_of_string line_no), (int_of_string type_line_no)) in
					(rem, ATTRIBUTE(IDENT(line_num, name), IDENT(type_line_num, t), None) :: feature_nodes );
				end
		| 	_ -> ([], [])
;;

let rec class_list lines num_classes = match num_classes with
	0 -> []
|	_ -> match lines with
			line_no :: name :: "no_inherits" :: num_features :: lines -> begin
				let (rem_lines, ast_nodes) = feature_list lines (int_of_string num_features) in
				let line_num = int_of_string line_no in
				CLASS( IDENT(line_num, name), None, ast_nodes ) :: (class_list rem_lines (num_classes-1));
			end
		|   _ -> []
;;

let ast lst = match lst with
	num_classes :: lines -> PROGRAM( (class_list lines (int_of_string num_classes) ) )
|   [] -> PROGRAM( []);;

let print_list lst = List.iter (fun a -> print_string (a ^ "\n")) lst;;

(****************  PRINTING HELPER METHODS *****************)
let print_identifier ident = match ident with
	IDENT(line_no, str) -> Printf.printf "%d\n%s\n" line_no str
|   _ -> print_string "NOT IDENTIFIER"
;;

let rec print_feature_list feat_list = match feat_list with
	[] -> ()
|   hd :: tl -> match hd with
		ATTRIBUTE(ident, typ, expr) -> print_identifier ident;
										print_identifier typ;
										print_feature_list tl;
	|   _ -> print_string "NOT ATTRIBUTE"
;;

let rec print_class_list class_list = match class_list with
	[] -> ()
|   hd :: tl -> match hd with
		CLASS(ident, None, feat_list) -> print_identifier ident;
									print_string "no_inherits\n";
									print_feature_list feat_list;
									print_class_list tl;
	|   _ -> print_string "NOT A CLASS"
;;

let rec print_ast ast = match ast with
	PROGRAM(class_list) -> print_class_list class_list
|   _ -> print_string "NOT A PROGRAM"
;;

(* let rec print_ast ast = match ast with
| 	PROGRAM(clas_list) -> List.iter print_ast clas_list
|   CLASS(t, i, feat_list) -> print_ast t
							  List.iter print_ast feat_list
|   ATTRIBUTE(ident, typ, exp) -> begin print_ast ident; print_ast typ; end
|	IDENT(line_no, s) -> print_string s
|   STRING(s) -> print_string s
|	INT(i) -> Printf.printf "%d" i
| 	_ -> print_string "COULD NOT FIND MATCH"
(* |	T(line_no, t) -> print_string t *)
;; *)

let filename = match Array.length (Sys.argv) with
		2 -> Array.get Sys.argv 1
	|   _ -> failwith "Please provide a single file command line arguments"
;;

(* print_string (filename ^ "\n");; *)
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
	(* match p with 
	PROGRAM(class_list) -> match class_list with
			hd :: rest -> match hd with
				CLASS(i1, i2, f) -> match i1 with
					IDENT(line_no, name) -> Printf.printf "%s\n" name
				|   _ -> print_string "NO MATCH 3"
			|   _ -> print_string "NO MATCH 2"
	|   _ -> print_string "NO MATCH" *)
end
	