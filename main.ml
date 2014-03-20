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

let print_list lst = List.iter (fun a -> print_string (a ^ "\n")) lst;;
(***********************  PA4 ******************************)

(* type 'a mult_node = T of 'a * 'a mult_node;; *)
module ClassMap = Map.Make(String);;
module ImplMap = Map.Make(String);;
module FeatMap = Map.Make(String);;
module ObjMap = Map.Make(String);;
module ParentMap = Map.Make(String);;
module MethodMap = Map.Make(String);;

(* ANI: identifier = identifier, typ = type *)
type identifier = IDENT of int * string


(*                    name of formal, type of formal *)
and formals = FORMAL of identifier * identifier

(* EXPRESSION TYPE DEF; NOTE optional type notation at last pos *)

and case_element = CE of identifier * identifier * expr * string option

(* LB is a special case *)
                  (*            name,         typ *)
and let_binding = LB_NO_INIT of identifier * identifier
                  (*  name,       typ          some expr *)
	| LB_INIT of identifier * identifier * expr

(* INT(line_no, value) *)
and expr = INT of int * int * string option
	| STRING of int * string * string option
	| ASSIGN of int * identifier * expr  * string option
	| DYN_DISPATCH of int * expr * identifier * expr list * string option
	| STAT_DISPATCH of int * expr * identifier * identifier * expr list * string option
	| SELF_DISPATCH of int * identifier * expr list * string option
	| IF_ELSE of int * expr * expr * expr * string option
	| WHILE of int * expr * expr * string option
	| BLOCK of int * expr list * string option
	| NEW of int * identifier * string option
	| ISVOID of int * expr * string option
	| PLUS of int * expr * expr * string option 
        | MINUS of int * expr * expr * string option
	| TIMES of int * expr * expr * string option
	| DIVIDE of int * expr * expr * string option
	| LT of int * expr * expr * string option
	| LE of int * expr * expr * string option
	| EQ of int * expr * expr * string option
	| NOT of int * expr * string option
	| NEGATE of int * expr * string option
	| TRUE of int * string option
	| FALSE of int * string option
	| LET of int * let_binding list * expr * string option
	| CASE of int * expr * case_element list * string option
	| IDENTIFIER of int * identifier * string option
        | INTERNAL

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
			(rem_lines, CE( IDENT(line_num, name), IDENT(typ_line_num, typ), expr, None ) :: elem_list)
        | _ -> 
        	failure 0 "badtree"; 
        	(lines, [])

and build_expr lines = match lines with
    line_no :: "integer" :: num :: rem_lines -> 
		(rem_lines, INT((int_of_string line_no), (int_of_string num), None))
|   line_no :: "string" :: word :: rem_lines ->
		(rem_lines, STRING((int_of_string line_no), word, None))
|   line_no :: "assign" :: iden_line_no :: name :: lines  ->
		let (rem_lines, expr) = build_expr lines in
		let line_num = int_of_string line_no
		and iden_line_num = int_of_string iden_line_no in
		( rem_lines, ASSIGN(line_num, IDENT(iden_line_num, name), expr, None) )
|   line_no :: "let" :: num_bindings :: lines ->
		let (rem_lines, bindings) = binding_list lines (int_of_string num_bindings) in
		let (rem_lines, expr) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		( rem_lines, LET(line_num, bindings, expr, None) )
|   line_no :: "case" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let num_elems  = List.hd rem_lines in
        let rem = List.tl rem_lines in
		let (rem_lines, case_elements) = case_element_list rem (int_of_string num_elems) in
		( rem_lines, CASE( (int_of_string line_no), expr, case_elements, None))
|   line_no :: "if" :: lines ->
		let (rem_lines, expr) = build_expr lines in
		let (rem_lines, expr1) = build_expr rem_lines in
		let (rem_lines, expr2) = build_expr rem_lines in
		(rem_lines, IF_ELSE( (int_of_string line_no), expr, expr1, expr2, None ) )
|   line_no :: "identifier" :: ident_line_no :: ident :: lines ->
		let line_num = int_of_string line_no
		and ident_line_num = int_of_string ident_line_no in
		(lines, IDENTIFIER( line_num, IDENT( ident_line_num, ident), None ) )
|   line_no :: "while" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let (rem_lines, expr2) = build_expr rem_lines
		and line_num = int_of_string line_no in
		(rem_lines, WHILE(line_num, expr, expr2, None) )
|   line_no :: "block" :: num_exprs :: lines ->
		let (rem_lines, exprs) = expr_list lines (int_of_string num_exprs) in
		let line_num = int_of_string line_no in
		(rem_lines, BLOCK( line_num, exprs, None))
|   line_no :: "plus" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, PLUS(line_num, lhs, rhs, None ) )
|   line_no :: "minus" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, MINUS(line_num, lhs, rhs, None ) )
|   line_no :: "times" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, TIMES( line_num, lhs, rhs, None))
|   line_no :: "divide" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, DIVIDE( line_num, lhs, rhs, None ))
|   line_no :: "lt" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, LT( line_num, lhs, rhs, None ))
|   line_no :: "le" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, LE( line_num, lhs, rhs, None ))
|   line_no :: "eq" :: lines -> 
		let (rem_lines, lhs) = build_expr lines in
		let (rem_lines, rhs) = build_expr rem_lines in
		let line_num = int_of_string line_no in
		(rem_lines, EQ( line_num, lhs, rhs, None ))
|   line_no :: "not" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let line_num = int_of_string line_no in
		(rem_lines, NOT( line_num, expr, None ))
|   line_no :: "negate" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let line_num = int_of_string line_no in
		(rem_lines, NEGATE( line_num, expr, None ))
|   line_no :: "isvoid" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let line_num = int_of_string line_no in
		(rem_lines, ISVOID( line_num, expr, None ))
|   line_no :: "new" :: ident_no :: ident :: lines ->
		let line_num = int_of_string line_no 
		and ident_line_num = int_of_string ident_no in
		(lines, NEW( line_num, IDENT(ident_line_num, ident), None))
|   line_no :: "true" :: lines -> 
		let line_num = int_of_string line_no in
		(lines, TRUE(line_num, None))
|   line_no :: "false" :: lines -> 
		let line_num = int_of_string line_no in
		(lines, FALSE(line_num, None))
|   line_no :: "self_dispatch" :: meth_line_no :: meth :: num_exprs :: lines -> 
		let (rem_lines, exprs) = expr_list lines (int_of_string num_exprs) in
		let line_num = int_of_string line_no
		and ident_line_num = int_of_string meth_line_no in
		(rem_lines, SELF_DISPATCH( line_num, IDENT(ident_line_num, meth), exprs, None ) )
|   line_no :: "dynamic_dispatch" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let (rem_lines, ident) = build_identifier rem_lines in
		let num_exprs = List.hd rem_lines in
		let rem_lines = List.tl rem_lines in
		let (rem_lines, exprs) = expr_list rem_lines (int_of_string num_exprs) in
		let line_num = int_of_string line_no in
		(rem_lines, DYN_DISPATCH(line_num, expr, ident, exprs, None))
|   line_no :: "static_dispatch" :: lines -> 
		let (rem_lines, expr) = build_expr lines in
		let (rem_lines, ident1) = build_identifier rem_lines in
		let (rem_lines, ident2) = build_identifier rem_lines in
		let num_exprs = List.hd rem_lines in
                let rem_lines = List.tl rem_lines in
		let (rem_lines, exprs) = expr_list rem_lines (int_of_string num_exprs) in
		let line_num = int_of_string line_no in
		(rem_lines, STAT_DISPATCH(line_num, expr, ident1, ident2, exprs, None))
|   _ -> failure 0 "bad tree"; ([], STRING(0, "", None))
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



(************************* CLASS MAP & IMPL MAP BUILDING ***************************)
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
            METHOD(IDENT(0, "in_int"), IDENT(0, "Int"), [], INTERNAL);
		METHOD(IDENT(0, "in_string"), IDENT(0, "String"), [], INTERNAL);
		METHOD(IDENT(0, "out_int"), IDENT(0, "SELF_TYPE"), [FORMAL(IDENT(0, "x"), IDENT(0, "Int"))], INTERNAL);
                METHOD(IDENT(0, "out_string"), IDENT(0, "SELF_TYPE"), [FORMAL(IDENT(0, "x"), IDENT(0, "String"))], INTERNAL);
	]);
	(* Object *)
	CLASS(ai "Object", None, 3, [
		METHOD(IDENT(0, "abort"), IDENT(0, "Object"), [], INTERNAL);
                METHOD(IDENT(0, "copy"), IDENT(0, "SELF_TYPE"), [], INTERNAL);
		METHOD(IDENT(0, "type_name"), IDENT(0, "String"), [], INTERNAL);
	]);
	(* IO *)
	CLASS(ai "String", obj, 3, [
		METHOD(IDENT(0, "concat"), IDENT(0, "String"), [FORMAL(IDENT(0, "s"), IDENT(0, "String"))], INTERNAL);
		METHOD(IDENT(0, "length"), IDENT(0, "Int"), [], INTERNAL);
		METHOD(IDENT(0, "substr"), IDENT(0, "String"), [FORMAL(IDENT(0, "i"), IDENT(0, "Int")); FORMAL(IDENT(0, "l"), IDENT(0, "Int"))],
                INTERNAL);
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
    let str_order = topoSort edges_to_sort in
    if (List.length str_order) < 1 then failure 0 "Class hierarchy contains an inheritance cycle";
    let valid_types = str_order @ ["SELF_TYPE"] in
    (* here we can add attributes to class node *)
    let order = List.map (fun str -> List.find (fun c_node -> let CLASS(IDENT(_, name), _, _, _) = c_node in
                                                              str = name) 
                                     sorted_list) str_order in

    (* function to check if attributes that are inherited are redefined in subclass *)
    (* also checks for self and if used types exist *)
    let attr_redef_check attr_list ih_attr_list = 
        List.iter (fun attr -> 
                       let ATTRIBUTE(IDENT(ln, name), IDENT(_,typ), _) = attr in
                       if not (List.mem typ valid_types) then
                           failure ln "Attribute uses non-existent type";
                       if name = "self" then
                           failure ln "Attribute cannot use self as an identifier";
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

(* checks if two formals lists have all the same signatures *)
(* if not return false *)
and formals_equal fml1 fml2 = 
                  (* FML *)
    try
        let both = List.combine fml1 fml2 in
        let bad_matches = List.filter (fun (x, y) ->
                                    let FORMAL(IDENT(_, a), IDENT(_, b)) = x in
                                    let FORMAL(IDENT(_, c), IDENT(_, d)) = y in
                                    not(b = d)
                                  )
                                  both in
        0 = List.length bad_matches
    with Invalid_argument reason -> false

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
	let str_order = topoSort edges_to_sort in
        let valid_types = str_order @ ["SELF_TYPE"] in

	(* replace string of Classes with Class Nodes *)
        let order = List.map (fun str -> List.find (fun c_node -> let CLASS(IDENT(_, name), _, _, _) = c_node in
                                                              str = name) 
                                     sorted_list) str_order in

            
        (* checks to see if SELF_TYPE is used and throws if it is *)
        (* also checks to see if types exist *)
        let check_formals valid_types formals = 
            List.iter (fun formal -> 
                            let FORMAL(IDENT(_, name), IDENT(ln, typ)) = formal in
                            if not (List.mem typ valid_types) then
                                failure ln "Formal uses a type that has not been defined";
                            if typ = "SELF_TYPE" then
                                failure ln "Cannot use SELF_TYPE as a formal";
                            if name = "self" then
                                failure ln "Cannot use self as an identifer in a formal";
                      )
                      formals 
        in


        (* builds a FeatMap which is a map of method name METHOD pairs *)
        let make_method_map ih_attr_list meth_list cls = 

			        let replace_or_add f_l add_fname add_feat cls = begin
						let (was_replaced, f_l) = List.fold_left (fun acc elt ->
							let (cur_fname, v) = elt in
							let (already_added, acc_list) = acc in
							if cur_fname = add_fname then
								(true, (acc_list @ [(add_fname, (add_feat, cls))]))
							else
								(already_added, (acc_list @ [elt]))
						) (false, []) f_l in
						if not was_replaced then (f_l @ [(add_fname, (add_feat, cls))]) else f_l
					end
					in

                    List.fold_left (fun fl feat -> 
                                        let METHOD(IDENT(ln, fname), IDENT(tln, typ), formals, _) = feat in
                                        (* check if type returned is in our list of valid types *)
                                        if not (List.mem typ valid_types) then 
                                            failure ln "Method returns non-existent type";
                                        check_formals valid_types formals;
                                        if List.mem_assoc fname fl then begin
                                            let (pfeat, pcls)  = List.assoc fname fl in
                                            let METHOD(_, IDENT(_, ih_typ), inherited_formals, _) = pfeat in
                                            if cls = pcls then
                                                failure ln "Redefining method in same class";
                                            if not(formals_equal formals inherited_formals) then 
                                                failure ln "Redefinition of formals breaks method";
                                            if not(typ = ih_typ) then
                                                failure ln "Return type of method redefined";
                                        end;
                                        (* (method name, feat, cls) tells us that this feature was defined in this class *)
                                        replace_or_add fl fname feat cls
                                        (* fl @ [(fname, (feat, cls))]  *)
                                   )
                                    ih_attr_list
                                    meth_list
         in

        (* The imap is map of maps, IE class maps to method name which maps to METHOD struct *)
        let imap = List.fold_left (fun cm cls -> match cls with
                                       CLASS(IDENT(_, name), Some(IDENT(_, inherits)), _, attr_list) -> 
                                            let ih_attr_list = ImplMap.find inherits cm in
                                            (* for each str in ih_attr_map do something *)
                                            ImplMap.add name (make_method_map ih_attr_list attr_list name) cm
                                     (* When class is object, first case *)
                                     | CLASS(IDENT(_, name), None, _, obj_list) ->
                                            (*  only add object methods on object *)
                                             ImplMap.add name (make_method_map [] obj_list "Object") cm
                            )
                            ImplMap.empty
                            order in


        if not (List.mem_assoc "main" (ImplMap.find "Main" imap)) then 
            failure 0 "Must have a main method in yo Main class";
    	let (main_method, _) = List.assoc "main" (ImplMap.find "Main" imap) in
    	let METHOD(_,_, forms, _) = main_method in
    	if List.length forms != 0 then failure 0 "Parameterless main method not found";
 
    (* Create impl map *)
    imap

and parent_map ast = 
	let PROGRAM(num_classes, class_list) = ast in
	(* At this point we have looked for inherting non existent classes so just run through class_list *)
	let pmap = List.fold_left (fun acc elt -> match elt with
		CLASS(IDENT(_, cls_name), Some(IDENT(_, inhers)), _, _) -> 
			(* Printf.printf "Adding parent relation from %s to %s\n" cls_name inhers; *)
			ParentMap.add cls_name inhers acc
		|	_ -> acc (* Only class that does not inherit is Object so don't worry about it *)
	) ParentMap.empty (add_check_names (List.map cm_class_list class_list)) in
	pmap

and make_m i_map = 
	(* map(class_name) -> assoc_list (f_name, [signature;...;]) *)
	let m = ImplMap.fold (fun cls_name spec_feat_list acc -> 
		(* This exposes class_name -> feat_assoc_list *)
		let sig_list = List.fold_left (fun acc (feat_name, (feat, defining_cls)) -> 
			let METHOD(IDENT(_, name), IDENT(_, typ), formals_l, _) = feat in
			let signature = List.fold_left (fun acc form -> 
				let FORMAL(IDENT(_, name), IDENT(_, typ)) = form in
				acc @ [typ]
			) [] formals_l in
			(* Must also add the return type as the last type *)
			let signature = signature @ [typ] in
			acc @ [( name, signature)]

		) [] spec_feat_list in
		MethodMap.add cls_name sig_list acc
	) i_map MethodMap.empty in
	m
;;


(******************************* TYPE CHECK **********************************)
(* We are essentially tracing the path from each node to the root node Object
	and then tracing back the two paths to see where they diverge. This point of
	divergence is the least common anscestor in the inheritance heirarchy *)
let check_typ_name typ = 
	let l = Str.split (Str.regexp "[.]") typ in
	if (List.length l) > 1 then
		List.nth l 1
	else
		typ
;;
let rec trace_to_root pM cls= 
	match cls with
		"Object" -> 
			(* We have finished tracing to the root *)
			["Object"]
	|	c ->
        let cls = check_typ_name cls in
			(* We want the result to have the root element first *)
			c :: (trace_to_root pM (ParentMap.find cls pM))
;;

(* LUB! it, either fails or returns the correct type *)
let lub pM class1 class2 = 
    
	let (trace1, trace2) = (trace_to_root pM class1, trace_to_root pM class2) in
	let our_lub = List.fold_left (fun acc elt -> if (List.mem elt trace2 && acc = "") then elt else acc) "" trace1 in
	our_lub;;


        (* does the right thing at every level *)
let rec type_check ast environ = 
        (*objEnv, classMap, implMap, parentMap*)
    let (oE, cM, mM, pM) = environ in
    (* if params we use backwards alphabet to pass into ret tree *)
    
    let PROGRAM(z, c_l) = ast in 
    PROGRAM(z, List.map (fun clas -> 
                        let CLASS(IDENT(_, cname), _, _, _) = clas in
                        (* let oE = ObjMap.add "self" cname oE in *)

                        let oE = List.fold_left (fun acc elt -> match elt with
                        	ATTRIBUTE(IDENT(_, name), IDENT(_, typ), _) ->
                        		(ObjMap.add name typ acc)
                        |   _ -> acc
                        ) oE (ClassMap.find cname cM) in

                        (* Object Environment, Method Env, Class Name *)
                        let environ = (oE, mM, cname, pM) in
                        (* let environ = (ObjMap.add "THIS_CLASS" cname oE, cM, iM, pM) in *)
                        class_type_check clas environ) 
                        c_l) 

and class_type_check ast environ = 
    let CLASS(z, y, x, feat_list) = ast in
    let a_feat_list = List.map (fun feat -> feat_type_check feat environ) feat_list in                                    
    CLASS(z, y, x, a_feat_list)

and feat_type_check ast environ = 
    let (oE, mM, cC, pM) = environ in
    match ast with
        ATTRIBUTE(name_ident, typ_ident, None) -> (* NO INIT *)
            ATTRIBUTE(name_ident, typ_ident, None) 

      | ATTRIBUTE(IDENT(z, name), IDENT(z2, typ), Some(expr)) -> (* ATTR INIT *)
      		let typ = if typ = "SELF_TYPE" then ("SELF_TYPE." ^ cC) else typ in
             let (ret_t, a_expr) = expr_type_check expr environ in 

             if not (is_subclass pM ret_t typ) then failure z "Attribute initiazed to expression of type that does not conform to static type";
             (* let cur_cls = ObjMap.find "THIS_CLASS" oE in *)
             (* if not(lub pM cname ret_t) failure ln "LUB DIED"; *)
             
             (* TODO type check attr_no_init and attr_init *)

             ATTRIBUTE(IDENT(z, name), IDENT(z2, typ), Some(a_expr))
      | METHOD(IDENT(z, name), IDENT(y, typ), formals_l, expr) ->
              let oE = List.fold_left (fun acc fml -> 
                                        let FORMAL(IDENT(_, fn), IDENT(_, ftyp)) = fml in
                                        ObjMap.add fn ftyp acc
                                      ) oE formals_l in 
              let environ = (oE, mM, cC, pM) in
              let (ret_t, a_expr) = expr_type_check expr environ in

              let t0 = if typ = "SELF_TYPE" then ("SELF_TYPE." ^ cC) else typ in
              (* Printf.printf "In class: %s ,  t0 = %s : typ = %s\n" cC t0 typ; *)
              if not (is_subclass pM ret_t t0) then failure z "Return type of method does not confrom to declared type";
              METHOD(IDENT(z, name), IDENT(y, typ), formals_l, a_expr)

and is_subclass pM t1 t2 =
	let check typ1 typ2 = 
			(* Printf.printf "Comparing: %s %s\n" t1 t2; *)
			if typ1 = typ2 then true else
			if typ1 = "Object" then false else begin
				try
					is_subclass pM (ParentMap.find typ1 pM) typ2
				with Not_found -> Printf.printf "Could not find parent\n"; false
			end
	in

	let self_split_check1 = Str.split (Str.regexp "[.]") t1 in
	let self_split_check2 = Str.split (Str.regexp "[.]") t2 in
	match ((List.length self_split_check1), (List.length self_split_check2)) with
		(2,2) -> if (List.nth self_split_check1 1) = (List.nth self_split_check2 1) then true else false
	|	(1,2) -> 
			let t2 = List.nth self_split_check2 1 in
			check t1 t2
	|	(2,1) ->
			let t1 = List.nth self_split_check1 1 in
			check t1 t2
	|	(1,1) ->
			check t1 t2

and type_check_expr_list expr_list environ= 
	let typs = List.fold_left (fun acc elt -> 
        			let (t_n, a_en) = expr_type_check elt environ in
        			acc @ [(t_n, a_en)]
        		) [] expr_list in
	typs

(* handle each case for an expression and return annotated ast + ret_tup type*)
(* does glorious typechecking *)
and expr_type_check ast environ = 
    let (oE, mM, cC, pM) = environ in
    (* TODO: use parentmap to do logic *) 
    (* let is_subclass pM t1 t2 = true in *)
    (* currying *)
    let is_subclass = is_subclass pM in

    match ast with
    		IDENTIFIER(z, IDENT(z1, name), _) -> 
		   		if name = "self" then
		   			let typ_name = "SELF_TYPE." ^ cC in
		   			(typ_name, IDENTIFIER(z, IDENT(z1, name), Some(typ_name )))
		   		else begin
		   			try
		   				let t = ObjMap.find name oE in
		   				(t, IDENTIFIER(z, IDENT(z1, name), Some(t) ))
		   			with
		   			| Not_found -> failure z ("Unbound identifier " ^ name); ("", TRUE(0, None))
		   		end
        |	ASSIGN(z, iden, expr, _) -> begin
            let IDENT(_, name) = iden in
            try 
                let t0 = ObjMap.find name oE in
                let (t1, a_expr) = expr_type_check expr environ in
                if not(is_subclass t1 t0) then failure z "t1 is not a subclass of t0";
                (t1, ASSIGN(z, iden, a_expr, Some(t1)))
            with
                | Not_found -> failure z ("Assignment on unbound iden " ^ name);
                               ("", TRUE(0, None))
            end
        |   TRUE(z, _) ->
            ("Bool", TRUE(z, Some("Bool")))
        |   FALSE(z,  _) ->
            ("Bool", FALSE(z, Some("Bool")))
        |   INT(z, y, _) -> 
            ("Int", INT(z, y, Some("Int")))
        |   STRING(z, y, _) -> 
            ("String", STRING(z, y, Some("String")))
        |   NEW(z, y, _) ->
            let IDENT(_, t) = y in
            if (* (ObjMap.find "THIS_CLASS" oE) *) "SELF_TYPE" = t then 
            	let self_typ_str = "SELF_TYPE." ^ cC in
               (self_typ_str, NEW(z, y, Some(self_typ_str)))  
            else
               (t, NEW(z, y, Some(t)))
        |   DYN_DISPATCH(z, e0, IDENT(z1, meth_name), expr_list, _) -> begin
        		let (t_0, a_e0) = expr_type_check e0 environ in
        		let typs = type_check_expr_list expr_list environ in
        		let t_0p = if t_0 = ("SELF_TYPE." ^ cC) then cC else t_0 in
				
				(* Get out Tn+1 and compare with our actual parameter types *)
				try
					let m_typ_list = List.assoc meth_name (MethodMap.find t_0p mM) in
	        		let m_rev = List.rev m_typ_list in
	        		let m_ret_typ = List.hd m_rev in 
	        		let m_typ_list = List.rev (List.tl m_rev) in
	        		List.iter2 (fun t tp -> 
	        			let (tn, _) = t in
	        			if not (is_subclass tn tp) then failure z "Given formals do not conform to method signature";
	        		) typs m_typ_list;

	        		let tnp1 = if m_ret_typ = "SELF_TYPE" then t_0 else m_ret_typ in
	        		let a_e_list = List.map (fun elt -> 
	        			let (t_n, a_en) = elt in
	        			a_en
	        		) typs in
	        		(tnp1, DYN_DISPATCH(z, a_e0, IDENT(z1, meth_name), a_e_list, Some(tnp1)))
				with
				| Not_found -> failure z1 ("unknown method " ^ meth_name); ("", TRUE(0,None))
                | Invalid_argument(_) -> failure z "Invalid number of arguments"; ("", TRUE(0, None))
			end

        |   SELF_DISPATCH(z, IDENT(z1, meth_name), expr_list, _) -> begin
        		let t_0 = "SELF_TYPE." ^ cC in
        		let typs = type_check_expr_list expr_list environ in
        		let t_0p = if t_0 = ("SELF_TYPE." ^ cC) then cC else t_0 in
				
				(* Get out Tn+1 and compare with our actual parameter types *)
				try
					let m_typ_list = List.assoc meth_name (MethodMap.find cC mM) in
	        		let m_rev = List.rev m_typ_list in
	        		let m_ret_typ = List.hd m_rev in 
	        		let m_typ_list = List.rev (List.tl m_rev) in
	        		List.iter2 (fun t tp -> 
	        			let (tn, _) = t in
	        			if not (is_subclass tn tp) then failure z "Given formals do not conform to method signature";
	        		) typs m_typ_list;

	        		let tnp1 = if m_ret_typ = "SELF_TYPE" then t_0 else m_ret_typ in
	        		let a_e_list = List.map (fun elt -> 
	        			let (t_n, a_en) = elt in
	        			a_en
	        		) typs in
	        		(tnp1, SELF_DISPATCH(z, IDENT(z1, meth_name), a_e_list, Some(tnp1)))
				with
				| Not_found -> failure z1 ("unknown method " ^ meth_name); ("", TRUE(0, None))
                | Invalid_argument(_) -> failure z "Invalid number of arguments"; ("", TRUE(0, None))
					
        	end

        |   STAT_DISPATCH(z, e0, IDENT(z1, typ), IDENT(z2, meth_name), expr_list, _) -> begin
        		let (t_0, a_e0) = expr_type_check e0 environ in
        		let typs = List.fold_left (fun acc elt -> 
        			let (t_n, a_en) = expr_type_check elt environ in
        			acc @ [(t_n, a_en)]
        		) [] expr_list in

        		if not (is_subclass t_0 typ) then failure z "Static dispatch called on non-subclass";

        		try
        			let m_typ_list = List.assoc meth_name (MethodMap.find cC mM) in
	        		let m_rev = List.rev m_typ_list in
	        		let m_ret_typ = List.hd m_rev in 
	        		let m_typ_list = List.rev (List.tl m_rev) in
	        		List.iter2 (fun t tp -> 
	        			let (tn, _) = t in
	        			if not (is_subclass tn tp) then failure z "Given formals do not conform to method signature";
	        		) typs m_typ_list;
	        		let ret_t = if m_ret_typ = "SELF_TYPE" then t_0 else m_ret_typ in
	        		let typs = List.map (fun elt -> 
	        			let (t_n, a_en) = elt in
	        			a_en
	        		) typs in
	        		(ret_t, STAT_DISPATCH(z, a_e0, IDENT(z1, typ), IDENT(z2, meth_name), typs, Some(ret_t)))    
        		with
        		| Not_found -> failure z1 ("unknown method " ^ meth_name); ("", TRUE(0, None))
                | Invalid_argument(_) -> failure z "Invalid number of arguments"; ("", TRUE(0, None))
        	end 
        |   IF_ELSE(z, cond, e_then, e_else, _) ->
            let (cond_T, a_cond) = expr_type_check cond environ in
            if not(cond_T = "Bool") then failure z "If clause must have expression of type Bool";
            let (then_T, a_then) = expr_type_check e_then environ in
            let (else_T, a_else) = expr_type_check e_else environ in
            let ret_t = lub pM then_T else_T in
            (ret_t, IF_ELSE(z, a_cond, a_then, a_else, Some(ret_t)))
        |   BLOCK(z, e_list, _) ->
            
            let a_e_list = List.map (fun expr -> 
                                        let (typ, a_expr) = expr_type_check expr environ in
                                        a_expr
                                    )
                                    e_list in
            let (f_typ, _) = expr_type_check (List.hd (List.rev a_e_list)) environ in
            (f_typ, BLOCK(z, a_e_list, Some(f_typ)))

        |   LET(z, lb_list, expr2, _) ->
                (* TODO deal with multiple LBs in one *)
                let (binding_tuple_list, environ) = List.fold_left (fun (btl, environ) lb -> 
                	let (oE, mM, cC, pM) = environ in
	                match lb with 
	                    LB_NO_INIT(IDENT(_, e0), IDENT(_, typ)) ->
	                    let t0 = if typ = "SELF_TYPE" then ("SELF_TYPE." ^ cC) (* ObjMap.find "THIS_CLASS" oE *) else typ in
	                    let environ = (ObjMap.add e0 t0 oE, mM, cC, pM) in
	                    (btl @ [lb], environ )
	                    (* let (t1, a_expr2) = expr_type_check expr2 environ in
	                    (t1, LET(z, )) *)
	                |   LB_INIT(name_id, typ_id, l_expr) ->
	                        let IDENT(_, name) = name_id in
	                        let IDENT(_, typ) = typ_id in
	                        let typ = if typ = "SELF_TYPE" then ("SELF_TYPE." ^ cC) else typ in
	                        let (l_typ, al_expr) = expr_type_check l_expr environ in
	                        if not(is_subclass l_typ typ) then failure z "let expr must return sublcass";
	                        let environ = (ObjMap.add name typ oE, mM, cC, pM) in
	                        (btl @ [ LB_INIT(name_id, typ_id, al_expr)], environ )
	                        (* let (t1, a_expr2) = expr_type_check expr2 environ in
	                        (t1, LET(z, [LB_INIT(name_id, typ_id, al_expr)], a_expr2, Some(t1))) *)
                ) ([], environ) lb_list in
				let (t1, a_expr2) = expr_type_check expr2 environ in
				(t1, LET(z, binding_tuple_list, a_expr2, Some(t1)))
		|   CASE(z, e0, cases, _) -> 

				let (t0, a_e0) = expr_type_check e0 environ in
				let case_type_info = List.map (fun elt -> 
					let CE(IDENT(z, name), IDENT(z1, typ), expr, _) = elt in
					(* Are we allowed to have SELF_TYPE in case expressions? *)
					(* Also need check for identical case statements here *)
					if typ = "SELF_TYPE" then failure z "Case Elements cannot have type SELF_TYPE";
					let environ = ( (ObjMap.add name typ oE), mM, cC, pM  ) in 
					let (t_n, a_en) = expr_type_check expr environ in
					(t_n, CE(IDENT(z, name), IDENT(z1, typ), a_en, Some(t_n)))
				) cases in

				List.fold_left (fun acc elt -> 
					let (_, CE(IDENT(lnum, _), IDENT(_, tn), _, _)) = elt in
					(* Printf.printf "Looking at case element of type: %s\n" tn; *)
					if List.mem tn acc then failure lnum "Case element type bound twice";
					tn :: acc
				) [] case_type_info;

				let (start_acc, _) = List.hd case_type_info in
				let final_typ = List.fold_left ( fun acc elt -> 
					let (t_n, _) = elt in
					let lubbed = lub pM acc t_n in
					lubbed
				) start_acc (List.tl case_type_info) in
				let annotated_cases = List.map (fun elt -> 
					let (_, ce) = elt in
					ce
				) case_type_info in
				(final_typ, CASE(z, a_e0, annotated_cases, Some(final_typ)))
		| 	WHILE(z, e1, e2, _) ->
				let (t1, a_e1) = expr_type_check e1 environ in
				if not (t1 = "Bool") then failure z "While predicate must have type Bool";	
				let (t2, a_e2) = expr_type_check e2 environ in
				("Object", WHILE(z, a_e1, a_e2, Some("Object")))
		|   ISVOID(z, e1, _) ->
				let (t_e1, a_e1) = expr_type_check e1 environ in
				("Bool", ISVOID(z, a_e1, Some("Bool")))
		|   NOT(z, e1, _) ->
				let (t_e1, a_e1) = expr_type_check e1 environ in
				if not (t_e1 = "Bool") then failure z "Not expressions must be of Type Bool";
				("Bool", NOT(z, a_e1, Some("Bool")))
		|   NEGATE(z, e1, _) ->
				let (t_e1, a_e1) = expr_type_check e1 environ in
				if not (t_e1 = "Int") then failure z "Negate expressions must be of Type Int";
				("Int", NEGATE(z, a_e1, Some("Int")))
		|   PLUS(z, e1, e2, _) -> 
				let (t_e1, a_e1) = expr_type_check e1 environ in
				let (t_e2, a_e2) = expr_type_check e2 environ in
				if not (t_e1 = "Int" && t_e2 = "Int") then failure z "Plus must have dos integer arguments";
				("Int", PLUS(z, a_e1, a_e2, Some("Int")))
		|   MINUS(z, e1, e2, _) -> 
				let (t_e1, a_e1) = expr_type_check e1 environ in
				let (t_e2, a_e2) = expr_type_check e2 environ in
				if not (t_e1 = "Int" && t_e2 = "Int") then failure z "Minus must have dos integer arguments";
				("Int", MINUS(z, a_e1, a_e2, Some("Int")))
		|   TIMES(z, e1, e2, _) -> 
				let (t_e1, a_e1) = expr_type_check e1 environ in
				let (t_e2, a_e2) = expr_type_check e2 environ in
				if not (t_e1 = "Int" && t_e2 = "Int") then failure z "Times must have dos integer arguments";
				("Int", TIMES(z, a_e1, a_e2, Some("Int")))
		|   DIVIDE(z, e1, e2, _) -> 
				let (t_e1, a_e1) = expr_type_check e1 environ in
				let (t_e2, a_e2) = expr_type_check e2 environ in
				if not (t_e1 = "Int" && t_e2 = "Int") then failure z "Divide must have dos integer arguments";
				("Int", DIVIDE(z, a_e1, a_e2, Some("Int")))
        |   EQ(z, e1, e2, _)  ->
                let (t_e1, a_e1) = expr_type_check e1 environ in
                let (t_e2, a_e2) = expr_type_check e2 environ in
                let isb = ["Int"; "String"; "Bool"] in
                if List.mem t_e1 isb && not ( t_e1 = t_e2 ) then 
                    failure z "Cannot compare ints strings and bools with not typ";
                ("Bool", EQ(z, a_e1, a_e2, Some("Bool")))
        |   LT(z, e1, e2, _)  ->
                let (t_e1, a_e1) = expr_type_check e1 environ in
                let (t_e2, a_e2) = expr_type_check e2 environ in
                let isb = ["Int"; "String"; "Bool"] in
                if List.mem t_e1 isb && not ( t_e1 = t_e2 ) then 
                    failure z "Cannot compare ints strings and bools with not typ";
                ("Bool", LT(z, a_e1, a_e2, Some("Bool")))
        |   LE(z, e1, e2, _)  ->
                let (t_e1, a_e1) = expr_type_check e1 environ in
                let (t_e2, a_e2) = expr_type_check e2 environ in
                let isb = ["Int"; "String"; "Bool"] in
                if List.mem t_e1 isb && not ( t_e1 = t_e2 ) then 
                    failure z "Cannot compare ints strings and bools with not typ";
                ("Bool", LE(z, a_e1, a_e2, Some("Bool")))


        (* and so on *)

;;


(*******************************  PRINTING HELPER METHODS *******************************)


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
		CE (a, b, e, Some(typ)) ->
           (*             Printf.fprintf oc "%s\n" typ; *)
			print_identifier a oc;
			print_identifier b oc;
			print_expr oc e;
			print_case_elements tl oc

and check_typ typ = 
	let l = Str.split (Str.regexp "[.]") typ in
	if (List.length l) > 1 then
		List.nth l 0
	else
		typ

and print_expr oc expr = match expr with
    INT(line_no, value, Some(typ)) ->
    				let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\ninteger\n%d\n" line_no typ value;
|   STRING (line_no, str, Some(typ)) -> 
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nstring\n%s\n" line_no typ str;
|   DYN_DISPATCH (line_no, e, meth, exprs, Some(typ)) ->
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\ndynamic_dispatch\n" line_no typ;
					print_expr oc e;
					print_identifier meth oc;
					Printf.fprintf oc "%d\n" (List.length exprs);
					List.iter (print_expr oc) exprs;
|   STAT_DISPATCH (line_no, e, dec_typ, meth, exprs, Some(typ)) -> 
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nstatic_dispatch\n" line_no typ;
					print_expr oc e;
					print_identifier dec_typ oc;
					print_identifier meth oc;
					Printf.fprintf oc "%d\n" (List.length exprs);
					List.iter (print_expr oc) exprs;
|   SELF_DISPATCH (line_no, meth, exprs, Some(typ)) -> 
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nself_dispatch\n" line_no typ;
					print_identifier meth oc;
					Printf.fprintf oc "%d\n" (List.length exprs);
					List.iter (print_expr oc) exprs;
|   CASE (line_no, expr, elems, Some(typ)) -> 
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\ncase\n" line_no typ;
					print_expr oc expr;
					Printf.fprintf oc "%d\n" (List.length elems);
					print_case_elements elems oc;
|   ASSIGN (line_no, ident, expr, Some(typ)) -> 
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nassign\n" line_no typ;
					print_identifier ident oc;
					print_expr oc expr;
|   LET (line_no, bindings, expr, Some(typ)) ->
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nlet\n%d\n" line_no typ (List.length bindings);
					print_bindings bindings oc;
					print_expr oc expr;
|   IF_ELSE (line_no, expr1, expr2, expr3, Some(typ)) -> 
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nif\n" line_no typ;
					print_expr oc expr1;
					print_expr oc expr2;
					print_expr oc expr3;
|   WHILE (line_no, expr, expr1, Some(typ)) -> 
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nwhile\n" line_no typ;
					print_expr oc expr;
					print_expr oc expr1;
|   BLOCK (line_no, exprs, Some(typ)) ->
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nblock\n%d\n" line_no typ (List.length exprs);
					List.iter (print_expr oc) exprs;
|   IDENTIFIER (line_no, ident, Some(typ)) -> 
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nidentifier\n" line_no typ;
					print_identifier ident oc;
|   PLUS (line_no, lhs, rhs, Some(typ)) -> 
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nplus\n" line_no typ;
					print_expr oc lhs;
					print_expr oc rhs;
|   MINUS (line_no, lhs, rhs, Some(typ)) -> 
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nminus\n" line_no typ;
					print_expr oc lhs;
					print_expr oc rhs;
|   TIMES (line_no, lhs, rhs, Some(typ)) -> 
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\ntimes\n" line_no typ;
					print_expr oc lhs;
					print_expr oc rhs;
|   DIVIDE (line_no, lhs, rhs, Some(typ)) -> 
					let typ = check_typ typ in
                    Printf.fprintf oc "%d\n%s\ndivide\n" line_no typ;
					print_expr oc lhs;
					print_expr oc rhs;
|   LT (line_no, lhs, rhs, Some(typ)) ->
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nlt\n" line_no typ;
					print_expr oc lhs;
					print_expr oc rhs;
|   LE (line_no, expr1, expr2, Some(typ)) -> 
					let typ = check_typ typ in
					Printf.fprintf oc "%d\n%s\nle\n" line_no typ;
					print_expr oc expr1;
					print_expr oc expr2;
|   EQ (line_no, expr1, expr2, Some(typ)) -> 
					let typ = check_typ typ in
                                        Printf.fprintf oc "%d\n%s\neq\n" line_no typ;
					print_expr oc expr1;
					print_expr oc expr2;
|   NOT (line_no, expr, Some(typ)) -> 
					let typ = check_typ typ in
                                        Printf.fprintf oc "%d\n%s\nnot\n" line_no typ;
					print_expr oc expr;
|   NEGATE (line_no, expr, Some(typ)) -> 
					let typ = check_typ typ in
                                        Printf.fprintf oc "%d\n%s\nnegate\n" line_no typ;
					print_expr oc expr;
|   ISVOID (line_no, expr, Some(typ)) -> 
					let typ = check_typ typ in
                                        Printf.fprintf oc "%d\n%s\nisvoid\n" line_no typ;
					print_expr oc expr;
|   NEW (line_no, ident, Some(typ)) -> 
					let typ = check_typ typ in
                                        Printf.fprintf oc "%d\n%s\nnew\n" line_no typ;
					print_identifier ident oc;
|   TRUE (line_no, Some(typ)) ->
					let typ = check_typ typ in
                                        Printf.fprintf oc "%d\n%s\ntrue\n" line_no typ;
|   FALSE (line_no, Some(typ)) ->
					let typ = check_typ typ in
                                        Printf.fprintf oc "%d\n%s\nfalse\n" line_no typ;
|   _ -> Printf.printf "We failed... BOOOOOO\n";
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
                        Printf.fprintf oc "%s\n" cname ;
                        Printf.fprintf oc "%d\n" (List.length feat_list);
                        List.iter (fun feat -> print_cm_attr oc feat) feat_list; 
                  ) class_map;
;;

(*********************** IMPLEMENTATION MAP PRINTING *******************)
let print_impl_map impl_map oc =
    Printf.fprintf oc "implementation_map\n";
    Printf.fprintf oc "%d\n" (ImplMap.cardinal impl_map);
    ImplMap.iter (fun cname method_list ->
                      Printf.fprintf oc "%s\n" cname;
                      Printf.fprintf oc "%d\n" (List.length method_list);
                      List.iter (fun mtup -> 
                                      let (mname, (meth, cls)) = mtup in 
                                      Printf.fprintf oc "%s\n" mname;
                                      let METHOD(_, _, formals, _) = meth in
                                      Printf.fprintf oc "%d\n" (List.length formals);
                                      List.iter (fun formal -> 
                                                    let FORMAL(IDENT(_, name), _) = formal in
                                                    Printf.fprintf oc "%s\n" name;)
                                                formals;
                                      Printf.fprintf oc "%s\n" cls;
                                      match meth with 
                                        (* INTERNAL HANDLINING *)
                                         METHOD(_, IDENT(_, ret_t), formals, INTERNAL) -> 
                                             Printf.fprintf oc "0\n%s\ninternal\n%s.%s\n" ret_t cls mname;
                                       | METHOD(_, _, formals, expr) -> begin
                                            print_expr oc expr;                              
                                           end
                                         
                                    )
                                    method_list;
                 )
                 impl_map
;;
    
let print_parent_map parent_map oc =
	Printf.fprintf oc "parent_map\n%d\n" (ParentMap.cardinal parent_map);
	ParentMap.iter (fun cls inhers -> 
		Printf.fprintf oc "%s\n%s\n" cls inhers
	) parent_map
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
    let parentMap = parent_map p in
    let method_env = make_m implMap in
    
    (* MethodMap.iter (fun cls m_list -> 
    	Printf.printf "Class: %s\n" cls;
    	List.iter (fun elt -> 
    		let (name, sig_list) = elt in
    		Printf.printf "\tmethod: %s\n" name;
    		List.iter (fun e ->  Printf.printf "\t\t%s\n" e ) sig_list;
    	) m_list;
    ) method_env; *)


    let environ = (ObjMap.empty, classMap, method_env, parentMap) in 
    let annotated_ast = type_check p environ in
    let implMap = impl_map annotated_ast in
    let classMap = class_map annotated_ast in

        (* since print_class_map has side-effects we must ignore it *)
        ignore(print_class_map classMap oc);
        ignore(print_impl_map implMap oc);
        ignore(print_parent_map parentMap oc);
		ignore(print_ast annotated_ast oc); 
	close_out oc;

end

