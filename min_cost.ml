open Ff_algorithm
open Graph
open Bipartite_matching
type infile = string
type outfile = string
type capacity = int
type flow = int
type cost = int

(* arc of graph : (id,(capacity,cost))
 * arc of flow_graph : (id,(capacity,label,cost)) 
 *)

(* Return a path, with a minimum cost, going from source to sink but the order of nodes is reversed. *)
let rec find_path_bis graph path source sink total_cost =
 
	(* if the sink is achieved, the path and the minimum cost are returned *)
	if (sink=source) then ((source::path), total_cost)
	(* else, a path is searched *)
	else
		let arcs_src = out_arcs graph source in
		(* loop with the out arcs of source node *)
		let rec loop arcs_src path min_cost =	
			match arcs_src with
				| [] -> 
                    (* if no path found *)
                    if (min_cost=(-1)) then ([], (-1))
                    (* else a path with a minimum cost has been found *)
                    else (path, min_cost)
				| (id, (_,value,cost))::rest ->
					(* if the value of the arc is null *)
                    (* or if the node reached is already in the path,  *)
                    (* the loop is continued *)
					if (value = 0) || (List.exists (fun x -> (x=id)) path) then loop rest path min_cost
					(* else a path through this node is searched *)
					else let (path_aux, min_cost_aux) = find_path_bis graph (source::path) id sink (total_cost+cost) in
								match path_aux with
									| [] -> loop rest path min_cost (* no path, the research is continued *)
									| path_aux -> (* a path has been found ! *)
                                        (* if the cost is minimal, update path and min_cost*)
                                        if (min_cost=(-1) || min_cost_aux<min_cost) then loop rest path_aux min_cost_aux
                                        (* else, continue the loop *)
                                        else loop rest path min_cost
		in loop arcs_src path (-1)

(* find_path graph [] source sink *)
(* Find a path with a minimum cost from the source to the sink *)
(* Return ([],(-1)) if no path found *)
let find_path graph path source sink =
    let (path2, min_cost) = find_path_bis graph path source sink 0 in
	(List.rev(path2), min_cost)


(* Return the smallest label among all labels of a given path from the residual graph *)
(* Find the incrementation of flow *)
let rec find_min_arc residual_graph path acu =
	match path with
		| [] -> acu
		| _::[] -> acu
		| id1::id2::rest -> 
			let cost = find_arc residual_graph id1 id2 in
				match cost with
					| None -> raise Not_found
					| Some (_,label,_) -> if (acu=(-1) || label<acu) then find_min_arc residual_graph (id2::rest) label 
									else find_min_arc residual_graph (id2::rest) acu


(* Decremente the value of the arc from id2 to id1 *)
let decremente flow_graph id1 id2 min =
    let flow_arc = find_arc flow_graph id2 id1 in
        match flow_arc with
            | None -> raise Not_found
			| Some (capacity, value, cost) -> add_arc flow_graph id2 id1 (capacity, value-min, cost)


(* Update flow graph *)
let update_flow_graph flow_graph path min =
    (* update modified arcs *)
    let rec loop path flow_graph = 
		match path with
			| [] -> flow_graph
			| _::[]-> flow_graph
			| id1::id2::rest -> 
				let flow_arc = find_arc flow_graph id1 id2 in
					match flow_arc with
						| None ->
                            (* if no arc found, the arc was in the other direction in the residual graph*)
                            (* so we have to decremente *)
                            Printf.printf(" -> Decrementation in the flow graph\n");
                            let new_graph = decremente flow_graph id1 id2 min in
                                loop (id2::rest) new_graph  
						| Some (capacity, value, cost) -> 
                            (* incremente the value of the arc from id1 to id2 *)
							let new_graph = add_arc flow_graph id1 id2 (capacity, value+min, cost) in
							loop (id2::rest) new_graph

    in loop path flow_graph


(* Update residual graph *)
let update_residual_graph residual_graph path min =
    (* update modified arcs *)
    let rec loop path residual_graph = 
		match path with
			| [] -> residual_graph
			|  _::[]-> residual_graph
			| id1::id2::rest -> 
				let residual_arc = find_arc residual_graph id1 id2 in
					match residual_arc with
						| None -> raise Not_found
						| Some (capacity, value, cost) -> 
                            (* decremente the value on the arc from id1 to id2 *)
							let new_graph = add_arc residual_graph id1 id2 (capacity, value-min, cost) in
                            (* incremente the flow on the arc from id2 to id1 *)
							let new_graph2 = add_arc new_graph id2 id1 (capacity, capacity-value+min, (-cost)) in
							loop (id2::rest) new_graph2

    in loop path residual_graph
        

(* Calculate max flow of a given graph *)
let calculate_max_flow residual_graph sink = 
	let arcs_src = out_arcs residual_graph sink in
		let rec max = function
 		| [] -> 0
		| (id,(capa,label,cost))::rest -> label+(max rest)
		in max arcs_src


(* Initialize flow graph *)
let init_flow_graph graph = map graph (fun (capa,cost) -> (capa,0,cost))

(* Initialize residual graph *)
let init_residual_graph graph = map graph (fun (capa,cost) -> (capa,capa,cost))
	

(* From an assignment graph, return a flow graph with the maximum flow and the minimum cost *)
let busaker_gowen_algorithm (graph, source, sink) =
    let flow_graph = init_flow_graph graph in
	let residual_graph = init_residual_graph graph in 
    let residual_graph_str = 
        Graph.map residual_graph (fun (a,b,c) -> ((string_of_int b)^"/"^(string_of_int a)^" ("^(string_of_int c)^")")) in
	    let rec loop flow_graph residual_graph total_cost =
		    let (path, min_cost) = find_path residual_graph [] source sink in
                match path with
                    | [] ->
                        let max_flow = calculate_max_flow residual_graph sink in (* There are no more path so we calculate flow *)
                        (flow_graph, max_flow, total_cost)
                    | path ->
		                let min_flow = find_min_arc residual_graph path (-1) in 
                        let new_flow_graph = update_flow_graph flow_graph path min_flow in
                        let new_residual_graph = update_residual_graph residual_graph path min_flow in
                        loop new_flow_graph new_residual_graph (total_cost+min_cost*min_flow)
        in loop flow_graph residual_graph 0



let read_node graph line is_student= 
	if is_student then 
	(* Read student nodes *)
		try Scanf.sscanf line "s %s" (fun student -> let newgraph = add_node graph student in add_arc newgraph "s" student (1,0))
		with e -> Printf.printf "Cannot read node in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "create_assignement_graph"
	else
	(* Read school nodes *)
	try Scanf.sscanf line "o %s %d" (fun school capacity-> let newgraph = add_node graph school in add_arc newgraph school "p" (capacity,0))
	with e -> Printf.printf "Cannot read node in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "create_assignement_graph"

(* Associate students to schools *)
let read_arc graph line = 
	try Scanf.sscanf line "p %s %d %s" (fun student wish school -> add_arc graph student school (1,wish)) 
	with e -> Printf.printf "Cannot read arc in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "create_assignement_graph"



(* Read a file which contains bipartite graph and return (int*int) graph that is associated *)
let create_assignement_graph path =
let infile = open_in path in
	let rec loop graph = 
		try
			let line = input_line infile in 
			let graph2 =
				if line ="" then graph
				else match line.[0] with
					| 's'-> read_node graph line true
					| 'o'-> read_node graph line false
					| 'p' -> read_arc graph line
					| _ -> graph
		in loop graph2
		with End_of_file -> graph
	in
	let start_graph = add_node empty_graph "s" in 
	let start = add_node start_graph "p" in
	let final_graph = loop start in
	close_in infile ;
  	final_graph


(* From a file which contains a problem of assignments, find a solution with a the maximum flow and the minimum cost (the outfile is in dot format)*)
let min_cost_max_flow_algorithm infile outfile =
    let graph = create_assignement_graph infile in 
    let problem = (graph, "s", "p") in
    let (flow_graph, max_flow, min_cost) = busaker_gowen_algorithm problem in
    Printf.printf "Obtained : Maximum of assignments : %d \n" max_flow ;
    Printf.printf "Obtained : Minimum cost : %d \n\n" min_cost ;
	let flow_graph_str = Graph.map flow_graph (fun (a,b,c) -> ((string_of_int b)^"/"^(string_of_int a)^" ("^(string_of_int c)^")")) in
    let () = Gfile.export flow_graph_str (outfile^"_mc_flow_graph.gv") in () 


