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
