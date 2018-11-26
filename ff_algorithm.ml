open Graph
    
type path = id list
type capacity = int
type value = int
type flow = int
type residual_graph = (capacity * value) graph

(* Return a path going from source to sink but the order of nodes is reversed. *)
let rec find_path_bis graph path source sink =
 
	(* if the sink is achieved, the path is returned *)
	if (sink=source) then (source::path) 

	(* else, a path is searched *)
	else
		let arcs_src = out_arcs graph source in
		(* loop with the out arcs of source node *)
		let rec loop arcs_src =	
			match arcs_src with
				| [] -> [] (* no path found *)
				| (id, (_,value))::rest -> 
					(* if the value of the arc is null *)
                    (* or if the node reached is already in the path,  *)
                    (* the loop is continued *)
					if (value = 0) || (List.exists (fun x -> (x=id)) path) then loop rest
					(* else a path through this node is searched *)
					else let aux = find_path_bis graph (source::path) id sink in
								match aux with
									| [] -> loop rest (* no path, the research is continued *)
									| aux -> aux (* a path has been found ! *)
		in loop arcs_src

(* Return a path going from source to sink *)
let find_path graph path source sink =
	List.rev (find_path_bis graph path source sink)

let rec find_min_arc residual_graph path acu =
	match path with
		| [] 
		| _::[] -> acu
		| id1::id2::rest -> 
			let cost = find_arc residual_graph id1 id2 in
				match cost with
					| None -> raise Not_found
					| Some (_,cost) -> if (cost<acu) then find_min_arc residual_graph (id2::rest) cost 
									else find_min_arc residual_graph (id2::rest) acu

(*let convert_Nodepath_to_Arcpath path = match path with
	| [] -> []
	| id2::id1::rest -> 
	| _ -> []
*)

let update_residual_graph flow_graph path min =
    (* create modified arcs *)
    let rec loop path graph = 
		match path with
			| [] -> graph
			| id1::id2::rest -> 
				let residual_arc = find_arc residual_graph id1 id2 in
					match residual_arc with
						| None -> raise Not_found
						| Some (capacity, value) -> 
							let new_graph = add_arc graph id1 id2 (capacity, value+min) in
							let new_graph2 = add_arc new_graph id2 id1 (capacity, value-min) in
							loop (id2::rest) new_graph2
    in loop path flow_graph
        


(*let rec create_residual_graph graph1 graph2 = *)


let calculate_max_flow residual_graph sink = 
	let arcs_src = out_arcs residual_graph sink in
		let rec max = function
 		| [] -> 0
		| (_,label)::rest -> label+max(rest)
		in max arcs_src

let init_residual_graph graph = map graph (fun x -> (x,x))
	
(* arc label of flow_graph : (capacity, value) *)
let ford_fulkerson_algorithm graph source sink =
	let residual_graph = init_residual_graph graph in
	let rec loop residual_graph =
		let path = find_path residual_graph [] source sink in
            match path with
                | [] -> calculate_max_flow residual_graph sink (* There are no more path so we calculate flow *)
                | path ->
		            let min = find_min_arc residual_graph path 1000 in (*max value ?? *)
                    let new_residual_graph = update_residual_graph residual_graph path min) in
                    loop residual_graph
    in loop residual_graph
	

