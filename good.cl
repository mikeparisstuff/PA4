(* Questions:
   I need a better way to use inheritance to use my abstract inheritance interface
   good way todo nothing in cool?
*)
class Main inherits IO {
    main() : Object {
        let 
         initial_graph : List <- new Nil,
         done : Bool <- false
        in {
            while not done loop {
              let into : String <- in_string () in {
              if into = "" then
                done <- true
              else
                let outof : String <- in_string() in {
                initial_graph <- initial_graph.insert((new Edge).init(into, outof));
                }
              fi ; };
            } pool ;
            -- here we need to determine the starting point for our graph
            let vertices : List <- find_all_vertices(initial_graph),
            start_nodes : List <- augment_s(new Nil, initial_graph, vertices),
            -- now that we have it we can recursively step through the sort
            order : List <- rec_step(start_nodes, initial_graph)
            in 
               if not order.length() = vertices.length() then
                  out_string("cycle\n")
               else 
                   order.print_list()
               fi;
        }
    };

    rec_step(queued : List, graph : List) : List { -- s is our too remove list; graph is a list of edge pairs
      case queued of
        s : List => 
          let cur : Elem <- s.get_elem(), -- a Str Elem
            vertices : List <- find_all_vertices(graph)
          in  
            -- remove edges from graph
            let new_graph : List <- prune_edges(cur, graph)
            in {
                vertices <- vertices.remove(cur);
                s <- s.remove(cur);
                s <- augment_s(s, new_graph, vertices);
                -- IMPORTANT STEP: This is where the magic happens
                (new Cons).init(cur, rec_step(s, new_graph)); 
            };
        nil : Nil => new Nil;
      esac
    };

    find_all_vertices(graph : List) : List { -- returns Str Elem, removes the current s from the set of vertices
       let graph_itr : Iterator <- (new Iterator).init(graph),
        vertices : List <- new Nil
       in { 
            while graph_itr.has_next() loop {
                let edge : Elem <- graph_itr.next() in {
                    vertices <- vertices.set_insert((new Str).init(edge.get_into()));
                    vertices <- vertices.set_insert((new Str).init(edge.get_outof()));
                };
            } pool;
          vertices;
       }
    };

    prune_edges(cur : Elem, graph : List) : List { -- returns a new graph with edges removed
       let graph_itr : Iterator <- (new Iterator).init(graph),
       new_graph : List <- graph.rcopy() -- A list of edges
       in {
            while graph_itr.has_next() loop { -- building up to_remove in this step
                let edge : Elem <- graph_itr.next() in {
                    if edge.get_outof() = cur.get_val() then
                        new_graph <- new_graph.remove(edge)
                    else 0 fi; -- do nothing
                };
            } pool;
           new_graph;
       }
    };
    
    augment_s(s : List, new_graph : List, old_vertices : List) : List { -- generates a new s list from orphans and the new_g and old s
       let vertex_itr : Iterator <- (new Iterator).init(old_vertices)
      in {
        while vertex_itr.has_next() loop {
            let vertex : Elem <- vertex_itr.next() in 
                if new_graph.test_orphan(vertex) then -- tests to see if the vertex has any incoming edges
                    s <- s.set_insert(vertex)
                else 0 fi;
        } pool;
            s; -- the augmented sorted order
        }
    };
};

Class Iterator {
    current : List;
    
    init(start: List) : Iterator { {current <- start; self;}};

    has_next() : Bool {
        case current of 
            x : Cons => true;
            y : Nil => false;
        esac
    };

    -- advances the iterator after returning the current element
    next() : Elem {
        if has_next() then 
            let e : Elem <- current.get_elem() in {
                current <- current.get_xcdr(); 
                e;
            }
        else { abort(); new Str; } fi  -- A spot we should never reach
    };
};

-- An abstract interface for elements in a list
Class Elem inherits IO {
    equals(other : Elem) : Bool { {abort(); false; } };

    less_than(target : Elem) : Bool { { abort(); false; } };

    print() : Object { { abort (); ""; } };

    get_into() : String { {abort(); "";} }; 

    get_outof() : String { get_into() }; 

    get_val() : String { get_into() }; 
};

Class Str inherits Elem {
    val : String;
    
    get_val() : String { val }; 

    print() : Object {
        {
       out_string(val); 
       out_string("\n"); 
        }
    };

    init(nv : String) : Str {
      {
        val <- nv;
        self;
      }
    };
    
    less_than(target : Elem) : Bool {
        if val < target.get_val() then true
        else false fi
    };

    equals(s : Elem) : Bool {
        val = s.get_val()
    };
};

Class Edge inherits Elem {
    into  : String;
    outof : String;
    
    init(ninto : String, noutof : String) : Edge {
        {
            into  <- ninto;
            outof <- noutof;
            self;
        }
    }; 
    
    get_into() : String { into }; 

    get_outof() : String { outof }; 

    less_than(target : Elem) : Bool {
         if into < target.get_into() then
            (if outof < target.get_outof() then true
              else false fi)
         else
            false
         fi
    };

    equals(other: Elem) : Bool {
        if other.get_into() = into then
            (if other.get_outof() = outof then true 
            else false fi)
        else false
        fi
    };

    print() : Object {
        {
         out_string("==edge==\n");
         out_string(into);
         out_string("\n");
         out_string(outof);
         out_string("\n");
        }
    };
};


Class List inherits IO { 
           
        (* cons appends returns a new list with 'hd' as the first
         * element and this list (self) as the rest of the list *) 
	cons(s : Elem) : Cons { 
	  let new_cell : Cons <- new Cons in
		new_cell.init(s, self)
	};

        rcopy() : List { {abort(); new Nil;} };

	insert(s: Elem) : List { self };

	set_insert(s: Elem) : List { self };

	print_list() : Object { abort() };

        contains(target : Elem) : Bool { {abort(); false; } };

        test_orphan(strElem : Elem) : Bool { {abort(); false; } }; -- A hack to check the into field
        
        get_xcdr() : List { {abort(); new Nil;}};

        get_elem() : Elem { {out_string("get_elem : "); abort(); new Str;}};

        length() : Int { {abort(); 0;}};

        remove(elem : Elem) : List { {abort(); new Nil; } };
} ;

Class Cons inherits List { -- a Cons cell is a non-empty list 
        elem : Elem;                 -- A psuedo generic type
	xcdr : List;            -- xcdr is the rest of the list

        get_xcdr() : List { xcdr };

        get_elem() : Elem { elem };

        length() : Int { 1 + xcdr.length() };

        rcopy() : List { (new Cons).init(elem, xcdr.rcopy()) };

	init(new_elem : Elem, tl : List) : Cons {
	  {
            elem <- new_elem;
	    xcdr <- tl;
	    self;
	  }
	};
	set_insert(elem: Elem) : List { 
            if not contains(elem) then
                insert(elem)
            else
                self
            fi
        };
	  
        (* insert() does insertion sort (using a reverse comparison) *) 
	insert(s : Elem) : List {
            {
		if not (elem.less_than(s)) then          -- sort in reverse order
			(new Cons).init(s, self)
		else
			(new Cons).init(elem, xcdr.insert(s))
		fi;
            }
	};

	print_list() : Object {
		{
                     elem.print();
		     xcdr.print_list();
		}
	};
    
        contains(s : Elem) : Bool {
            if elem.equals(s) then true
            else 
                xcdr.contains(s)
            fi
        };

        test_orphan(strElem : Elem) : Bool {  -- A hack to check the into field
            if elem.get_into() = strElem.get_val() then
                false
            else
                xcdr.test_orphan(strElem)
            fi
        };

        remove(target : Elem) : List { 
            if elem.equals(target) then 
                xcdr
            else {
                xcdr <- xcdr.remove(target);
                self; 
            } fi
        };
} ;

Class Nil inherits List { -- Nil is an empty list 

	insert(s : Elem) : List { (new Cons).init(s,self) }; 

	set_insert(s : Elem) : List { (new Cons).init(s,self) }; 

	print_list() : Object { true }; -- do nothing 

        contains(s : Elem) : Bool { false };

        test_orphan(s : Elem) : Bool { true }; -- if we never found the element then it must be an orphan
        length() : Int { 0 };

        remove(target : Elem) : List { self };
        
        rcopy() : List { new Nil };
} ;
