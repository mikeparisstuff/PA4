(* type 'a mult_node = T of 'a * 'a mult_node;; *)

(* ANI: identifier = identifier, typ = type *)
type identifier = I of int * string;;
type typ = T of int * string;;
type formals = F of string * typ;;
type expr = E of int;;

type feature = ANI of identifier * typ 
	| AI of identifier * typ * expr
	| M of identifier * typ * formals list * expr;;

type clas = NI of typ * feature list;;
type program = P of string * clas list;;


(* int = line_no, string = identifier name *)




let print_list lst = List.iter (fun a -> print_string (a ^ "\n")) lst;;

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
	print_list cool_input;
end


(**********  BUILD THE AST FROM FILE  *************)

let ast lst = match lst with
	num_classes :: lines -> P("Program", [](* (class_list lines num_classes []) *) )
|   _ -> P("Program", []);;

(* let rec class_list lst num_classes = match num_classes with
	0 -> []
|	_ -> match lst with
			lin_no :: class_name :: "inherits" :: num_features :: lines -> 
		|   lin_no :: class_name :: "no_inherits" :: num_feature :: lines ->  *)



	(* match List.length classes with
		num_classes -> []
	|   _ -> match lst with
			lin_no :: class_name :: "inherits" :: num_features :: lines -> class_list T("inherits", (feature_list lines num_features [])) :: classes )
		|   lin_no :: class_name :: "no_inherits" :: num_feature :: lines ->  *)
	

