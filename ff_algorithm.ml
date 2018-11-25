open Graph
    
type path = id list

type flow_graph = ('a*'b) graph


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
				| (id, _)::rest -> 
					(* if the node reached is already in the path, the loop is continued *)
					if (List.exists (fun x -> (x=id)) path) then loop rest
					(* else a path through this node is searched *)
					else let aux = find_path_bis graph (source::path) id sink in
								match aux with
									| [] -> loop rest (* no path, the research is continued *)
									| aux -> aux (* a path has been found ! *)
		in loop arcs_src

(* Return a path going from source to sink *)
let find_path graph path source sink =
	List.rev (find_path_bis graph path source sink)

let rec find_min_arc graph path acu =
	match path with
		| [] -> acu
		| id1::id2::rest -> 
			let cost = find_arc graph id1 id2 in
				match cost with
					| None -> raise Not_found (* à modifier ! *)
					| Some (_,cost) -> if (cost<acu) then find_min_arc graph (id2::rest) cost 
									else find_min_arc graph (id2::rest) acu


let update_flow_graph flow_graph path min =
	let rec loop path graph = 
		match path with
			| [] -> graph
			| id1::id2::rest -> 
				let flow_arc = find_arc flow_graph id1 id2 in
					match flow_arc with
						| None -> raise Not_found
						| Some (capacity, value) -> 
							let new_graph = add_arc graph id1 id2 (capacity, value+min) in
							let new_graph2 = add_arc new_graph id2 id1 (capacity, value-min) in
							loop (id2::rest) new_graph2
				(*let graph_arc = find_arc graph id2 id1 in *)
	in loop path empty_graph


(*let rec create_residual_graph graph1 graph2 = *)


let calculate_max_flow flow_graph source sink =
    (* TO DO *) 0

(* arc label of flow_graph : (capacity, value) *)
let Ford_Fulkerson_Algorithm graph source sink =
	let init_flow_graph = map graph (fun x -> (x,0)) in
	let rec loop flow_graph =
		let path = find_path flow_graph [] source sink in
            match path with
                | [] -> calculate_max_flow flow_graph source sink
                | path ->
		            let min = find_min_arc flow_graph path 1000 in (*max value ?? *)
                    let new_flow_graph = update_flow_graph flow_graph path min) in
                    loop new_flow_graph
    in loop init_flow_graph
	

