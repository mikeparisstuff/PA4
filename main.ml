(************************ TOPOLOGICAL SORT  *******************************)

module StrSet = Set.Make(String);;
(* input_list is a list of [string; string] elements *)
let topoSort input_list = 
    let initial_graph = input_list in
    let flat = List.flatten initial_graph in
    (*  use a set for counting uniques *)
    let num_unique = StrSet.cardinal (List.fold_left (fun acc elt -> StrSet.add elt acc) 
                                                     StrSet.empty
                                                     flat) in

    let cmp a b = compare a b in
    let find_start graph = List.sort (fun a b -> compare a b) 
        (let remove_dups lst = List.fold_left  (* here we sort the list then remove
            duplicate elements using fold *)
                        (fun acc elem -> 
                            let not_eqs h e a = if h = e then a else e :: a in
                            match acc with
                            | []     -> [elem]
                            | [hd]   -> not_eqs hd elem acc
                            | hd::tl -> not_eqs hd elem acc) 
                        [] 
                        (List.sort cmp lst) in
        let has_incoming = remove_dups (List.map (fun elem -> List.hd elem) graph) in
        let has_outgoing = remove_dups (List.map (fun elem -> List.nth elem 1) graph) in
        let within k l = List.fold_left (fun acc elem -> acc || elem = k) false l in
        (List.filter 
                (fun elem -> not (within elem has_incoming))
                has_outgoing)
    ) in

    let g_orphans = ref [] in

    let remove_elem target graph =
        let new_g = List.filter (fun edge -> not ((List.nth edge 1) = target)) graph in
        g_orphans := List.filter (fun elem -> not (target = elem)) !g_orphans;
        let orphans =
           let removed = List.map 
                (fun edge -> List.hd edge) 
                (List.filter (fun edge -> (List.nth edge 1) = target) graph) 
           in
           List.filter
                (fun possible -> 
                    List.fold_left 
                        (fun acc edge -> acc && not ((List.nth edge 1) =
                            possible) && not ((List.hd edge) = possible))
                        true 
                        new_g) 
                removed 
        in
        g_orphans := !g_orphans @ orphans; (* g_orphans is a reference so we
        must dereference the pointer *)
        let s = List.sort cmp (!g_orphans @ (find_start new_g)) in
        (s, new_g) 
    in

    let rec topo_sort (s, graph) = match s with (* note recursions + appending e
    and hd *)
        | []     -> []
        | [e]    -> e :: (topo_sort (remove_elem e graph))
        | hd::tl -> hd :: (topo_sort (remove_elem hd graph)) 
    in
    let out = topo_sort ((find_start initial_graph), initial_graph) in
    if (List.length out) == num_unique then 
        out 
    else 
        []
;;


(***********************  PA4 ******************************)

(* type 'a mult_node = T of 'a * 'a mult_node;; *)
module ClassMap = Map.Make(String);;
module ImplMap = Map.Make(String);;
module FeatMap = Map.Make(String);;

(* ANI: identifier = identifier, typ = type *)
type identifier = IDENT of int * string

and cm = CM of clas list

(*                    name of formal, type of formal *)
and formals = FORMAL of identifier * identifier

(* INT(line_no, value) *)
and case_element = CE of identifier * identifier * expr
                  (*            name,         typ *)
and let_binding = LB_NO_INIT of identifier * identifier
                  (*  name,       typ          some expr *)
	| LB_INIT of identifier * identifier * expr

and expr = INT of int * int
	| STRING of int * string
	| ASSIGN of int * identifier * expr 
	| DYN_DISPATCH of int * expr * identifier * expr list
	| STAT_DISPATCH of int * expr * identifier * identifier * expr list
	| SELF_DISPATCH of int * identifier * expr list
	| IF_ELSE of int * expr * expr * expr
	| WHILE of int * expr * expr
	| BLOCK of int * expr list
	| NEW of int * identifier
	| ISVOID of int * expr
	| PLUS of int * expr * expr | MINUS of int * expr * expr
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
	| CASE of int * expr * case_element list
	| IDENTIFIER of int * identifier

                        (* fname,        type,       no_init or init with expr *)
and feature = ATTRIBUTE of identifier * identifier * expr option
                    (* name,     return type,  () or list,   not-opt *)
	| METHOD of identifier * identifier * formals list * expr

(*                  name of class, optional inheritance, number of features, features list *)
and clas = CLASS of identifier * identifier option * int * feature list

(* number of classes, class list *)
and program = PROGRAM of int * clas list;;

let failure line_no message = 
            print_endline (Printf.sprintf ("ERROR: %d: Type-Check: %s") line_no message);
            (* To fool our compilier! *)
            ignore(exit 0)

(****************************  BUILD THE AST FROM FILE  ************************************)
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

and build_identifier lines = match lines with
	line_no :: ident :: lines ->
		(lines, IDENT((int_of_string line_no), ident))
|	_ -> 
		failure 0 "badtree";
        (lines, IDENT(0, ""))

and case_element_list lines num_elems = match num_elems with
	0 -> (lines, [])
|	_ -> match lines with
		line_no :: name :: typ_line_no :: typ :: lines ->
			(* Printf.printf "Found case elem on line: %s with name: %s and type: %s\n" line_no name typ; *)
			let (rem_lines, expr) = build_expr lines in
			let (rem_lines, elem_list) = case_element_list rem_lines (num_elems -1) in
			let line_num = int_of_string line_no
			and typ_line_num = int_of_string typ_line_no in
			(rem_lines, CE( IDENT(line_num, name), IDENT(typ_line_num, typ), expr ) :: elem_list)
        | _ -> 
        	failure 0 "badtree"; 
        	(lines, [])

and build_expr lines = match lines with
    line_no :: "integer" :: num :: rem_lines -> 
		(rem_lines, INT((int_of_string line_no), (int_of_string num)))
|   line_no :: "string" :: word :: rem_lines ->
		(rem_lines, STRING( (int_of_string line_no), word))
|   line_no :: "assign" :: iden_line_no :: name :: lines  ->
		let (rem_lines, expr) = build_expr lines in
		let line_num = int_of_string line_no
		and iden_line_num = int_of_string iden_line_no in
		( rem_lines, ASSIGN( line_num, IDENT(iden_line_num, name), expr) )
|   line_no :: "let" :: num_bindings :: lines ->
		let (rem_lines, bindings) = binding_list lines (int_of_string num_bindings) in
		let (rem_lines, expr) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		( rem_lines, LET(line_num, bindings, expr) )
|   line_no :: "case" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let num_elems  = List.hd rem_lines in
        let rem = List.tl rem_lines in
		let (rem_lines, case_elements) = case_element_list rem (int_of_string num_elems) in
		( rem_lines, CASE( (int_of_string line_no), expr, case_elements ))
|   line_no :: "if" :: lines ->
		let (rem_lines, expr) = build_expr lines in
		let (rem_lines, expr1) = build_expr rem_lines in
		let (rem_lines, expr2) = build_expr rem_lines in
		(rem_lines, IF_ELSE( (int_of_string line_no), expr, expr1, expr2 ) )
|   line_no :: "identifier" :: ident_line_no :: ident :: lines ->
		let line_num = int_of_string line_no
		and ident_line_num = int_of_string ident_line_no in
		(lines, IDENTIFIER( line_num, IDENT( ident_line_num, ident) ) )
|   line_no :: "while" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let (rem_lines, expr2) = build_expr rem_lines
		and line_num = int_of_string line_no in
		(rem_lines, WHILE(line_num, expr, expr2) )
|   line_no :: "block" :: num_exprs :: lines ->
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
		(rem_lines, NEGATE( line_num, expr ))
|   line_no :: "isvoid" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let line_num = int_of_string line_no in
		(rem_lines, ISVOID( line_num, expr ))
|   line_no :: "new" :: ident_no :: ident :: lines ->
		let line_num = int_of_string line_no 
		and ident_line_num = int_of_string ident_no in
		(lines, NEW( line_num, IDENT(ident_line_num, ident)))
|   line_no :: "true" :: lines -> 
		let line_num = int_of_string line_no in
		(lines, TRUE(line_num))
|   line_no :: "false" :: lines -> 
		let line_num = int_of_string line_no in
		(lines, FALSE(line_num))
|   line_no :: "self_dispatch" :: meth_line_no :: meth :: num_exprs :: lines -> 
		let (rem_lines, exprs) = expr_list lines (int_of_string num_exprs) in
		let line_num = int_of_string line_no
		and ident_line_num = int_of_string meth_line_no in
		(rem_lines, SELF_DISPATCH( line_num, IDENT(ident_line_num, meth), exprs ) )
|   line_no :: "dynamic_dispatch" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let (rem_lines, ident) = build_identifier rem_lines in
		let num_exprs = List.hd rem_lines in
		let rem_lines = List.tl rem_lines in
		let (rem_lines, exprs) = expr_list rem_lines (int_of_string num_exprs) in
		let line_num = int_of_string line_no in
		(rem_lines, DYN_DISPATCH(line_num, expr, ident, exprs))
|   line_no :: "static_dispatch" :: lines -> 

		let (rem_lines, expr) = build_expr lines in
		let (rem_lines, ident1) = build_identifier rem_lines in
		let (rem_lines, ident2) = build_identifier rem_lines in
		let num_exprs = List.hd rem_lines in
        let rem_lines = List.tl rem_lines in
		let (rem_lines, exprs) = expr_list rem_lines (int_of_string num_exprs) in
		let line_num = int_of_string line_no in
		(rem_lines, STAT_DISPATCH(line_num, expr, ident1, ident2, exprs))
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
                |	_ -> failure 0 "badtree"; ([], [])
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
		|   "method" :: line_no :: name :: num_formals :: lines -> 		
					let (lines, formals_nodes) = formal_list lines (int_of_string num_formals) in
					let (rem_lines, typ_ident) = build_identifier lines in
					let (rem_lines, expr) = build_expr rem_lines in

					(* Get the rest of the features in this class and the remaining lines after those *)
					let (rem, feature_nodes) = feature_list rem_lines (num_features-1) in
					let line_num = int_of_string line_no in
					(rem, METHOD( IDENT(line_num, name), typ_ident, formals_nodes, expr) :: feature_nodes )
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



(************************* CLASS MAP BUILDING ***************************)
let ai typ = IDENT(0, typ)

let obj = Some(ai "Object") ;;
(* predefined classes in cool *)
let predefd = [CLASS(ai "Bool", obj, 0, []); CLASS(ai "Int", obj, 0, []); 
              CLASS(ai "Object", None, 0, []); CLASS(ai "String", obj, 0, []);
              CLASS(ai "IO", obj, 0, [])] ;;

let base_impl_classes = [
	(* Bool *)
	CLASS(ai "Bool", obj, 0, []);
	(* Int *)
	CLASS(ai "Int", obj, 0, []);
	(* IO *)
	CLASS(ai "IO", obj, 4, [
		METHOD(IDENT(0, "out_string"), IDENT(0, "SELF_TYPE"), [FORMAL(IDENT(0, "x"), IDENT(0, "String"))], STRING(0, "internal"));
		METHOD(IDENT(0, "out_int"), IDENT(0, "SELF_TYPE"), [FORMAL(IDENT(0, "x"), IDENT(0, "Int"))], STRING(0, "internal"));
		METHOD(IDENT(0, "in_string"), IDENT(0, "String"), [], STRING(0, "internal"));
		METHOD(IDENT(0, "in_int"), IDENT(0, "Int"), [], STRING(0, "internal"))
	]);
	(* Object *)
	CLASS(ai "Object", None, 3, [
		METHOD(IDENT(0, "abort"), IDENT(0, "Object"), [], STRING(0, "internal"));
		METHOD(IDENT(0, "type_name"), IDENT(0, "String"), [], STRING(0, "internal"));
		METHOD(IDENT(0, "copy"), IDENT(0, "SELF_TYPE"), [], STRING(0, "internal"))
	]);
	(* IO *)
	CLASS(ai "String", obj, 3, [
		METHOD(IDENT(0, "length"), IDENT(0, "Int"), [], STRING(0, "internal"));
		METHOD(IDENT(0, "concat"), IDENT(0, "String"), [FORMAL(IDENT(0, "s"), IDENT(0, "String"))], STRING(0, "internal"));
		METHOD(IDENT(0, "substr"), IDENT(0, "String"), [FORMAL(IDENT(0, "i"), IDENT(0, "Int")); FORMAL(IDENT(0, "l"), IDENT(0, "Int"))], STRING(0, "internal"))
	])

];;

let rec cm_attribute feat_list = match feat_list with
    [] -> []

|   ATTRIBUTE( name, typ, Some(expr)) :: tl ->
		(* Add this attribute and print as initializer*)
                ATTRIBUTE(name, typ, Some(expr)) :: (cm_attribute tl)

|   ATTRIBUTE( name, typ, None) :: tl -> 
		(* Add this attribute and print as no_initializer *)
                ATTRIBUTE(name, typ, None) :: (cm_attribute tl)
|   _ :: tl -> 
		(* Ignore everything else *)
		cm_attribute tl

and cm_class_list ast = match ast with
	CLASS( name, Some(typ), num_feats, feats) ->
	       CLASS(name, Some(typ), num_feats, (cm_attribute feats))

    |   CLASS( name, None, num_feats, feats) -> 
               CLASS(name, obj, num_feats, (cm_attribute feats))


and add_check_names class_list = 
                let class_names = List.map (fun c -> 
                                                let CLASS(ident, _, _, _) = c in
                                                let IDENT(ln, name) = ident in name)
                                           class_list in
                (* check for main definition *)
                if not (List.mem "Main" class_names) then failure 0 "class Main not found";

                (* check for duplicate classes *)
                let uniques = [] in
                ignore(List.fold_left (fun uniques cls ->
                                let CLASS(IDENT(ln, name), _, _, _) = cls in
                                if List.mem name uniques then failure ln "Class name is duplicated";
                                name::uniques
                ) uniques class_list);

                (* check for classes that overwrite or inherit from Int, String, or Bool *)
                let non_inheritables = ["Int"; "String"; "Bool"] in
                List.iter (fun cls ->
                			let CLASS(IDENT(ln, name), Some(IDENT(tln, typ)), _, _) = cls in
                			if List.mem name non_inheritables then failure ln "Cannot redefine class Int, String, or Bool"
                			else if List.mem typ non_inheritables then failure tln "Cannot inherit from class Int, String, or Bool";
               	) class_list;

                (* check for non-existent classes that are inherited *) 
                let known_classes = (List.map (fun cls -> 
                				let CLASS(IDENT(ln, name), _, _, _) = cls in
                				name) class_list) @ ["IO"; "Object"] in
                List.iter (fun cls ->
                			let CLASS(IDENT(ln, name), Some(IDENT(tln, typ)), _, _) = cls in
                			(* CHECK IF THIS SHOULD FAIL ON LINE OF NAME OR TYP *)
                			if not (List.mem typ known_classes) then failure ln "Inheriting from unbound class";
                ) class_list;

                class_list @ predefd

and class_map ast = 
    let PROGRAM(num_classes, class_list) = ast in

    (* Add this class node to the class map *)
    let class_list = add_check_names (List.map cm_class_list class_list) in
    (* at this point all classnames should be unique *)
    let sorted_list = List.sort (fun a b -> match (a, b) with 
                                                (CLASS(name, _, _, _), CLASS(name2, _, _, _)) -> match (name, name2) with
                                                	(IDENT(c,d), IDENT(x,y)) -> compare d y
                                            ) class_list in
    (* still need to add inherited attrs, but first! topo-sorting *)
    (* the list of edges to pass into topo *)
    let edges_to_sort = List.map (fun cls -> let CLASS(IDENT(_, name), Some(IDENT(_, inherits)), _, _) = cls in
                                             [name; inherits]) 
                                   (* filtering out Object before we sort *)
                                 (List.filter (fun cls -> let CLASS(IDENT(_, name), _, _, _) = cls in
                                                          not (name = "Object")) class_list)
    in
    let order = topoSort edges_to_sort in
    if (List.length order) < 1 then failure 0 "Class hierarchy contains an inheritance cycle";
    (* here we can add attributes to class node *)
    let order = List.map (fun str -> List.find (fun c_node -> let CLASS(IDENT(_, name), _, _, _) = c_node in
                                                              str = name) 
                                     sorted_list) order in

    (* function to check if attributes that are inherited are redefined in subclass *)
    let attr_redef_check attr_list ih_attr_list = 
        List.iter (fun attr -> 
                       let ATTRIBUTE(IDENT(ln, name), _, _) = attr in
                       List.iter (fun ih_attr -> 
                                      let ATTRIBUTE(IDENT(_, pname), _, _) = ih_attr in
                                      if pname = name then
                                         failure ln "Class redefines an inherited attribute";
                                  )
                                  ih_attr_list;
                       (* effecient! and how! *)
                       List.iter (fun attr -> 
                                      let ATTRIBUTE(IDENT(oln, other_name), _, _) = attr in
                                      if other_name = name && not(oln = ln) then 
                                          failure oln "Class defines an attribute twice! yikes!";
                                 ) 
                                 attr_list;
                  ) 
                  attr_list in



    let cmap = List.fold_left (fun cm cls -> match cls with
                                   CLASS(IDENT(_, name), Some(IDENT(_, inherits)), _, attr_list) -> 
                                        let ih_attr_list = ClassMap.find inherits cm in
                                        attr_redef_check attr_list ih_attr_list;
                                        ClassMap.add name (ih_attr_list @ attr_list) cm
                                 | CLASS(IDENT(_, name), None, _, attr_list) ->
                                        ClassMap.add name attr_list cm
                        )
                        ClassMap.empty
                        order in
    (* ClassMap.iter (fun k b -> print_endline k) cmap; *)
    cmap

and impl_attribute feat_list = match feat_list with
	[] -> []
|   METHOD(name, typ, forms, expr) :: tl -> 
			METHOD(name, typ, forms, expr) :: (impl_attribute tl)
|   _ :: tl -> impl_attribute tl

and impl_class_list ast = match ast with
	CLASS( name, Some(typ), num_feats, feats) ->
	       CLASS(name, Some(typ), num_feats, (impl_attribute feats))

    |   CLASS( name, None, num_feats, feats) -> 
               CLASS(name, obj, num_feats, (impl_attribute feats))	

and add_impl_bases_and_check class_list = 
	class_list @ base_impl_classes 

(* checks if two formals lists are the same identifer + type *)
and formals_equal fml1 fml2 = 
                  (* FML *)
    let both = List.combine fml1 fml2 in
    let bad_matches = List.filter (fun (x, y) ->
                                    let FORMAL(IDENT(_, a), IDENT(_, b)) = x in
                                    let FORMAL(IDENT(_, c), IDENT(_, d)) = y in
                                    not(a = c && b = d)
                                  )
                                  both in
    1 > List.length bad_matches

(* builds a FeatMap which is a map of method name METHOD pairs *)
and make_method_map ih_attr_map meth_list = 
            List.fold_left (fun fm feat -> 
                                let METHOD(IDENT(ln, fname), IDENT(_, typ), formals, _) = feat in
                                if FeatMap.mem fname fm then begin
                                    let METHOD(_, IDENT(_, ih_typ), inherited_formals, _) = FeatMap.find fname fm in
                                    if not(formals_equal formals inherited_formals) then 
                                        failure ln "Redefinition of formals breaks class";
                                    if not(typ = ih_typ) then
                                        failure ln "You cannot change the type of the expression";
                                end;
                                FeatMap.add fname feat fm
                            )
                            FeatMap.empty
                            meth_list

and impl_map ast = 
	let PROGRAM(num_classes, class_list) = ast in

	(* Class List is now a list of class nodes with all their method nodes *)
	let class_list = add_impl_bases_and_check (List.map impl_class_list class_list) in
	let sorted_list = List.sort (fun a b -> match (a, b) with 
                                                (CLASS(name, _, _, _), CLASS(name2, _, _, _)) -> match (name, name2) with
                                                	(IDENT(c,d), IDENT(x,y)) -> compare d y
                                            ) class_list in

	(* still need to add inherited attrs, but first! topo-sorting *)
        (* the list of edges to pass into topo *)
	let edges_to_sort = List.map (fun cls -> let CLASS(IDENT(_, name), Some(IDENT(_, inherits)), _, _) = cls in
                                             [name; inherits]) 
                                   (* filtering out Object before we sort *)
                                 (List.filter (fun cls -> let CLASS(IDENT(_, name), _, _, _) = cls in
                                                          not (name = "Object")) class_list)
	in 
	let order = topoSort edges_to_sort in
	if (List.length order) < 1 then failure 0 "Class hierarchy contains an inheritance cycle";

	(* replace string of Classes with Class Nodes *)
        let order = List.map (fun str -> List.find (fun c_node -> let CLASS(IDENT(_, name), _, _, _) = c_node in
                                                              str = name) 
                                     sorted_list) order in

    


        (* The imap is map of maps, IE class maps to method name which maps to METHOD struct *)
        let imap = List.fold_left (fun cm cls -> match cls with
                                       CLASS(IDENT(_, name), Some(IDENT(_, inherits)), _, attr_list) -> 
                                            let ih_attr_map = ImplMap.find inherits cm in
                                            (* for each str in ih_attr_map do something *)
                                            ImplMap.add name (make_method_map ih_attr_map attr_list) cm
                                     (* When class is object, first case *)
                                     | CLASS(IDENT(_, name), None, _, attr_list) ->
                                            ImplMap.add name (make_method_map FeatMap.empty attr_list) cm
                            )
                            ImplMap.empty
                            order in

        if not (FeatMap.mem "main" (ImplMap.find "Main" imap)) then 
            failure 0 "Must have a main method in yo Main class";
 
    (* Create impl map *)
    imap
;;

(******************************* TYPE CHECKING METHODS *****************************)
(* let check_if_inherits_int ast  *)



(*******************************  PRINTING HELPER METHODS *******************************)

let print_list lst = List.iter (fun a -> print_string (a ^ "\n")) lst;;

let print_identifier ident oc = match ident with
	IDENT(line_no, str) -> 
			Printf.fprintf oc "%d\n%s\n" line_no str;
;;

let rec print_bindings bindings oc = match bindings with
	[] -> ()
|	LB_NO_INIT(ident, typ) :: tl  -> 
				Printf.fprintf oc "let_binding_no_init\n";
				print_identifier ident oc;
				print_identifier typ oc;
				print_bindings tl oc;
|	LB_INIT(ident, typ, expr) :: tl ->
				Printf.fprintf oc "let_binding_init\n";
				print_identifier ident oc;
				print_identifier typ oc;
				print_expr oc expr;
				print_bindings tl oc;

and print_case_elements elems oc = match elems with
	[] -> ()
|   hd :: tl -> match hd with 
		CE (a, b, e) ->
			print_identifier a oc;
			print_identifier b oc;
			print_expr oc e;
			print_case_elements tl oc

and print_expr oc expr = match expr with
	INT(line_no, value) -> Printf.fprintf oc "%d\ninteger\n%d\n" line_no value
|	STRING (line_no, str) -> Printf.fprintf oc "%d\nstring\n%s\n" line_no str
|   DYN_DISPATCH (line_no, e, meth,  exprs) ->
					Printf.fprintf oc "%d\ndynamic_dispatch\n" line_no;
					print_expr oc e;
					print_identifier meth oc;
					Printf.fprintf oc "%d\n" (List.length exprs);
					List.iter (print_expr oc) exprs;
|   STAT_DISPATCH (line_no, e, typ, meth, exprs) -> 
					Printf.fprintf oc "%d\nstatic_dispatch\n" line_no;
					print_expr oc e;
					print_identifier typ oc;
					print_identifier meth oc;
					Printf.fprintf oc "%d\n" (List.length exprs);
					List.iter (print_expr oc) exprs;
|   SELF_DISPATCH (line_no, meth, exprs) -> 
					Printf.fprintf oc "%d\nself_dispatch\n" line_no;
					print_identifier meth oc;
					Printf.fprintf oc "%d\n" (List.length exprs);
					List.iter (print_expr oc) exprs;
|   CASE (line_no, expr, elems) -> 
					Printf.fprintf oc "%d\ncase\n" line_no;
					print_expr oc expr;
					Printf.fprintf oc "%d\n" (List.length elems);
					print_case_elements elems oc;
|   ASSIGN (line_no, ident, expr) -> 
					Printf.fprintf oc "%d\nassign\n" line_no;
					print_identifier ident oc;
					print_expr oc expr;
|   LET (line_no, bindings, expr) ->
					Printf.fprintf oc "%d\nlet\n%d\n" line_no (List.length bindings);
					print_bindings bindings oc;
					print_expr oc expr;
|   IF_ELSE (line_no, expr1, expr2, expr3) -> 
					Printf.fprintf oc "%d\nif\n" line_no;
					print_expr oc expr1;
					print_expr oc expr2;
					print_expr oc expr3;
|   WHILE (line_no, expr, expr1) -> 
					Printf.fprintf oc "%d\nwhile\n" line_no;
					print_expr oc expr;
					print_expr oc expr1;
|   BLOCK (line_no, exprs) ->
					Printf.fprintf oc "%d\nblock\n%d\n" line_no (List.length exprs);
					List.iter (print_expr oc) exprs;
|   IDENTIFIER (line_no, ident) -> 
					Printf.fprintf oc "%d\nidentifier\n" line_no;
					print_identifier ident oc;
|   PLUS (line_no, lhs, rhs) -> 
					Printf.fprintf oc "%d\nplus\n" line_no;
					print_expr oc lhs;
					print_expr oc rhs;
|   MINUS (line_no, lhs, rhs) -> 
					Printf.fprintf oc "%d\nminus\n" line_no;
					print_expr oc lhs;
					print_expr oc rhs;
|   TIMES (line_no, lhs, rhs) -> 
					Printf.fprintf oc "%d\ntimes\n" line_no;
					print_expr oc lhs;
					print_expr oc rhs;
|   DIVIDE (line_no, lhs, rhs) -> 
					Printf.fprintf oc "%d\ndivide\n" line_no;
					print_expr oc lhs;
					print_expr oc rhs;
|   LT (line_no, lhs, rhs) ->
					Printf.fprintf oc "%d\nlt\n" line_no;
					print_expr oc lhs;
					print_expr oc rhs;
|   LE (line_no, expr1, expr2) -> 
					Printf.fprintf oc "%d\nle\n" line_no;
					print_expr oc expr1;
					print_expr oc expr2;
|   EQ (line_no, expr1, expr2) -> 
					Printf.fprintf oc "%d\neq\n" line_no;
					print_expr oc expr1;
					print_expr oc expr2;
|   NOT (line_no, expr) -> 
					Printf.fprintf oc "%d\nnot\n" line_no;
					print_expr oc expr;
|   NEGATE (line_no, expr) -> 
					Printf.fprintf oc "%d\nnegate\n" line_no;
					print_expr oc expr;
|   ISVOID (line_no, expr) -> 
					Printf.fprintf oc "%d\nisvoid\n" line_no;
					print_expr oc expr;
|   NEW (line_no, ident) -> 
					Printf.fprintf oc "%d\nnew\n" line_no;
					print_identifier ident oc;
|   TRUE (line_no) ->
					Printf.fprintf oc "%d\ntrue\n" line_no; 
|   FALSE (line_no) ->
					Printf.fprintf oc "%d\nfalse\n" line_no;
;;

let print_formal oc formal = match formal with
	FORMAL(a, b) -> 
		print_identifier a oc;
                print_identifier b oc;
;;


let rec print_feature_list feat_list oc = match feat_list with
	[] -> ()
|   hd :: tl -> match hd with
            ATTRIBUTE(ident, typ, None) -> 
							Printf.fprintf oc "attribute_no_init\n";
							print_identifier ident oc;
							print_identifier typ oc;
							print_feature_list tl oc;
	|   ATTRIBUTE(ident, typ, Some(expr)) ->
	 						Printf.fprintf oc "attribute_init\n";
							print_identifier ident oc;
							print_identifier typ oc;
							print_expr oc expr;
							print_feature_list tl oc;
	|   METHOD(name, typ, formals, expr) -> 
							Printf.fprintf oc "method\n";
							print_identifier name oc;
							Printf.fprintf oc "%d\n" (List.length formals);
							List.iter (print_formal oc) formals;
							print_identifier typ oc;
							print_expr oc expr;
							print_feature_list tl oc;
;;

let rec print_class_list class_list oc = match class_list with
	[] -> ()
        |   hd :: tl -> match hd with
            CLASS(ident, None, num_features, feat_list) -> 
                                                        print_identifier ident oc;
                                                        Printf.fprintf oc "no_inherits\n";
                                                        Printf.fprintf oc "%d\n" num_features;
                                                        print_feature_list feat_list oc;
                                                        print_class_list tl oc;
	|   CLASS(ident, Some(inher), num_features, feat_list) -> 
                                                        print_identifier ident oc;
                                                        Printf.fprintf oc "inherits\n";
                                                        print_identifier inher oc;
                                                        Printf.fprintf oc "%d\n" num_features;
                                                        print_feature_list feat_list oc;
                                                        print_class_list tl oc;
;;


(**************************** CLASS MAP PRINTING ***************************)
let print_cm_attr oc feat = match feat with
        ATTRIBUTE(ident, typ, None) -> 
							Printf.fprintf oc "no_initializer\n";
                                                        let IDENT(ln, name) = ident in
                                                        let IDENT(ln, typ) = typ in
                                                        Printf.fprintf oc "%s\n" name;
                                                        Printf.fprintf oc "%s\n" typ;
    |   ATTRIBUTE(ident, typ, Some(expr)) ->
	 						Printf.fprintf oc "initializer\n";
                                                        let IDENT(ln, name) = ident in
                                                        let IDENT(ln, typ) = typ in
                                                        Printf.fprintf oc "%s\n" name;
                                                        Printf.fprintf oc "%s\n" typ;
                                                        print_expr oc expr;
    (* if method do nothing! *)
    |   _ -> ()
    ;;

let print_class_map class_map oc = 
    Printf.fprintf oc "class_map\n";
    Printf.fprintf oc "%d\n" (ClassMap.cardinal class_map);
    ClassMap.iter (fun cname feat_list ->
                        Printf.fprintf oc "%s\n"cname ;
                        Printf.fprintf oc "%d\n" (List.length feat_list);
                        List.iter (fun feat -> print_cm_attr oc feat) feat_list; 
                  ) class_map;
;;

let rec print_ast ast oc = match ast with
	PROGRAM(num_classes, class_list) -> 
				Printf.fprintf oc "%d\n" num_classes;
				print_class_list class_list oc;
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

	(* Open output file, returning a channel to write to *)
	let f_l = Str.split (Str.regexp "[.]") filename in
	let file = (List.hd f_l) ^ ".cl-type" in
	let oc = open_out file in
        let classMap = class_map p in 
        let implMap = impl_map p in
        (* since print_class_map has side-effects we must ignore it *)
        ignore(print_class_map classMap oc);
	(* print_ast p oc;  *)
	close_out oc;
end
	
