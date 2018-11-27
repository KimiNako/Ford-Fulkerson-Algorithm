open Graph
    
type path = id list
type capacity = int
type value = int
type max_flow = int
type flow_graph = (capacity * value) graph
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

(* find_path graph [] source sink *)
(* Find a path from the source to the sink *)
(* Return [] if no path found *)
let find_path graph path source sink =
	List.rev (find_path_bis graph path source sink)


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
					| Some (_,cost) -> if (acu==(-1) || cost<acu) then find_min_arc residual_graph (id2::rest) cost 
									else find_min_arc residual_graph (id2::rest) acu

(*let convert_Nodepath_to_Arcpath path = match path with
	| [] -> []
	| id2::id1::rest -> 
	| _ -> []
*)

let update_flow_graph flow_graph path min =
    (* update modified arcs *)
    let rec loop path flow_graph = 
		match path with
			| [] -> flow_graph
			| _::[]-> flow_graph
			| id1::id2::rest -> 
				let flow_arc = find_arc flow_graph id1 id2 in
					match flow_arc with
						| None -> raise Not_found
						| Some (capacity, value) -> 
							let new_graph = add_arc flow_graph id1 id2 (capacity, value+min) in
							loop (id2::rest) new_graph

    in loop path flow_graph


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
						| Some (capacity, value) -> 
							let new_graph = add_arc residual_graph id1 id2 (capacity, value-min) in
							let new_graph2 = add_arc new_graph id2 id1 (capacity, value+min) in
							loop (id2::rest) new_graph2

    in loop path residual_graph
        

let calculate_max_flow residual_graph sink = 
	let arcs_src = out_arcs residual_graph sink in
		let rec max = function
 		| [] -> 0
		| (id,(capa,label))::rest -> label+(max rest)
		in max arcs_src

let init_flow_graph graph = map graph (fun x -> (x,0))

let init_residual_graph graph = map graph (fun x -> (x,x))
	

(* arc label of flow_graph : (capacity[fixed], value) *)


let ford_fulkerson_algorithm (graph, source, sink) =
    let flow_graph = init_flow_graph graph in
	let residual_graph = init_residual_graph graph in
	let rec loop flow_graph residual_graph =
		let path = find_path residual_graph [] source sink in
            match path with
                | [] -> 
                    let max_flow = calculate_max_flow residual_graph sink in (* There are no more path so we calculate flow *)
                    (flow_graph, max_flow)
                | path ->
		            let min = find_min_arc residual_graph path (-1) in 
                    let new_flow_graph = update_flow_graph flow_graph path min in
                    let new_residual_graph = update_residual_graph residual_graph path min in
                    loop new_flow_graph new_residual_graph
    in loop residual_graph
	



(* -----------------------------------------------*)
(* --------------- TESTS -------------------------*)
(*------------------------------------------------*)



let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  in
 	(* Open file *)
  let graph = Gfile.from_file infile in
	let res_graph = init_residual_graph graph in


	(* Test find_path in a residual graph*)
  let res_graph_int = Graph.map res_graph (fun (a,b) -> (int_of_string a,int_of_string b)) in
  	let path2 = find_path res_graph_int [] _source _sink in
			Printf.printf "Path found :";
			let () = List.iter (fun x -> Printf.printf "%s" x) path2 in 
			();
			Printf.printf "\n\n";

	(* Write Residual graph in dot format *)
	let res_graph_str = Graph.map res_graph (fun (a,b) -> (a^","^b)) in
  let () = Gfile.export res_graph_str (outfile^"_res.gv") in ();

	(* Print min of the path *)
	let min = find_min_arc res_graph_int path2 (-1) in 	
	Printf.printf "Path :";
	let () = List.iter (fun x -> Printf.printf "%s" x) path2 in ();
	Printf.printf "\n";
	Printf.printf "Increment : %d" min;
	Printf.printf "\n\n";

	(* Write Updated residual graph in dot format *)
	let res_graph_update = update_residual_graph res_graph_int path2 min in
	let res_graph_update_str = Graph.map res_graph_update (fun (a,b) -> ((string_of_int a)^","^(string_of_int b))) in
  let () = Gfile.export res_graph_update_str (outfile^"_updated_res.gv") in ();

	(* Print flow of the graph *)
	let max_flow = calculate_max_flow res_graph_int _sink in 
	Printf.printf "Flow : %d" max_flow;


(*
(* Test find_min_arc *)
	let res_graph = Graph.empty_graph in
  let res_graph = Graph.add_node res_graph "0" in
  let res_graph = Graph.add_node res_graph "1" in
  let res_graph = Graph.add_node res_graph "2" in
	let res_graph = Graph.add_node res_graph "3" in

	let res_graph = Graph.add_arc res_graph "0" "1" (42,42)  in
  let res_graph = Graph.add_arc res_graph "0" "2" (3,3)  in
  let res_graph = Graph.add_arc res_graph "1" "2" (3,3)  in
	let res_graph = Graph.add_arc res_graph "1" "3" (1,1) in
	let path3 = find_path res_graph [] "0" "3" in
	let min = find_min_arc res_graph path3 -1 in ();

*)
