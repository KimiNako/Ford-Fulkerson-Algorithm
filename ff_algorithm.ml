open Graph
    
type path = id list


(* Return a path going from source to sink but the order of nodes is reversed. *)
let rec find_path graph path source sink =
 
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
				else let aux = find_path graph (source::path) id sink in
							match aux with
								| [] -> loop rest (* no path, the research is continued *)
								| aux -> aux (* a path has been found ! *)
		in loop arcs_src


(* let Ford_Fulkerson_Algorithm graph source sink *)

