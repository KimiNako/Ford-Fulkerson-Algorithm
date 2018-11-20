open Graph
    
type path = id list


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
					| None -> raise Not_found
					| Some (_,cost) -> if (cost<acu) then find_min_arc graph (id2::rest) cost 
									else find_min_arc graph (id2::rest) acu
(*
let update_residual_graph graph residual_graph path min =
	let rec loop path new_graph = 
		match path with
			| [] -> new_graph
			| id1::id2::rest -> 
				let residual_arc = find_arc residual_graph id1 id2 in
				let graph_arc = find_arc graph id2 id1 in (* a finir !!!!!!!!!*) 
	in loop path empty_graph


(*let rec create_residual_graph graph1 graph2 = *)


(* arc label of residual_graph : (capacity, value) *)
let Ford_Fulkerson_Algorithm graph source sink =
	(*let flow_graph = map graph (fun x -> (x,0) in*)
	let residual_graph = map graph (fun x -> (x, x)) in
	let rec loop =
		let path = find_path residual_graph [] source sink in
		let min = find_min_arc residual_graph path 1000 in (*max value ?? *)
	*)	

