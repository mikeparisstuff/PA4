type 'a mult_tree = T of 'a * 'a mult_tree list;;


let print_list lst = List.iter (fun a -> print_string (a ^ "\n")) lst;;

let rec match_elem elem = match elem with
	L _ -> "string elem";;

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

let rec build_ast ast lst = match lst with
