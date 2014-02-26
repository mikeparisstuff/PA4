type node = ( string * string list ) list;;



let rec graph lines grph = 
	match lines with
		[] -> grph
	|   dependency :: action :: tl -> begin
		match (List.mem_assoc action grph, List.mem_assoc dependency grph) with
			| (true, true) -> graph tl (( action , dependency :: (List.assoc action grph)) :: List.remove_assoc action grph)
			| (true, false) -> graph tl ((dependency, []) :: ( action , dependency :: (List.assoc action grph)) :: List.remove_assoc action grph)
			| (false, true) -> graph tl (( action , dependency :: []) :: grph)
			| (false, false) -> graph tl ((dependency, []) :: ( action , dependency :: []) :: grph)
		end;;

let rec vertex_of_in_degree_zero grph lst= 
	try
		match (List.hd grph, lst) with 
		| ((key, incoming), s) -> if (List.length incoming) = 0 
				then
					vertex_of_in_degree_zero (List.tl grph) (key :: s)
				else
					vertex_of_in_degree_zero (List.tl grph) s
	with _ -> begin 
		if (List.length lst) = 0 then begin print_string ("cycle\n"); failwith "Cycle"; end
		else List.nth (List.sort (fun a b -> compare a b) lst) 0
	end;;

let rec remove_elem lst elem =
	 match (List.tl lst) with
	 | [] -> if (List.hd lst) = elem then [] else List.hd lst :: []
	 | _ -> if (List.hd lst) = elem then List.tl lst 
			else (List.hd lst) :: remove_elem (List.tl lst) elem
;;

let rec decrement_incoming grph for_value = 
	match List.tl grph with
	| [] -> 
		if (List.mem for_value (snd (List.hd grph))) then
			((fst (List.hd grph)), remove_elem (snd (List.hd grph)) for_value) :: []
		else
			List.hd grph :: []
	| s -> 
		if (List.mem for_value (snd (List.hd grph))) then begin
			let (key, incoming) = List.hd grph in
			(key, (remove_elem incoming for_value)) :: decrement_incoming (List.tl grph) for_value;
			end
		else
			(List.hd grph) :: decrement_incoming (List.tl grph) for_value
;;

let rec take_vertex_out_of_game g vert = 
	match (List.tl g) with
	| [] -> if (fst (List.hd g)) = vert then (fst (List.hd g), ["a";"a";"a";"a";"a";"a";"a";"a";"a"]) :: []
		else (List.hd g) :: []
	| s -> if (fst (List.hd g)) = vert then ((fst (List.hd g)), ["a";"a";"a";"a";"a";"a";"a";"a";"a"]) :: (List.tl g)
		else (List.hd g) :: take_vertex_out_of_game (List.tl g) vert
	;;

let rec topo_sort grph lst num = 
	match num with
		| 0 -> lst
		| n -> begin
			let vertex = vertex_of_in_degree_zero grph [] in
			let g = take_vertex_out_of_game grph vertex in
			topo_sort (decrement_incoming g vertex) (vertex :: lst) (num-1);
		end;;


let print_list lst = List.iter (fun a -> print_string (a ^ "\n")) lst;;

let rec print_graph grph = match grph with 
	| [] -> ()
	| (a, b) :: tl -> print_string ("A:" ^ a ^ "\n"); print_list b; print_graph tl
;;


let lines = ref [] in
try
	while true do
		let line = (read_line ()) in

		lines := line :: !lines;

		(* print_string("Line: " ^ line ^ "\n"); *)

	done
with
| _ -> begin

	let g = graph !lines [] in
	let sorted = topo_sort g [] (List.length g) in
	print_list (List.rev sorted);
	(* print_graph g *)

end
