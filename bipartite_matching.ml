open Ff_algorithm
open Graph
type infile = string
type outfile = string

let read_node graph line is_girl= 
	if is_girl then 
	(* Read girl nodes *)
		try Scanf.sscanf line "s %s" (fun girl -> let newgraph = add_node graph girl in add_arc newgraph "s" girl 1)
		with e -> Printf.printf "Cannot read node in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "create_bipartite_graph"
	else
	(* Read boy nodes *)
	try Scanf.sscanf line "o %s %d" (fun boy capacity-> let newgraph = add_node graph boy in add_arc newgraph boy "p" capacity)
	with e -> Printf.printf "Cannot read node in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "create_bipartite_graph"

(* Associate girls to boys *)
let read_arc graph line = 
	try Scanf.sscanf line "p %s %d %s" (fun girl wish boy -> add_arc graph girl boy wish) 
	with e -> Printf.printf "Cannot read arc in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "create_bipartite_graph"



(* Read a file which contains bipartite graph and return (int*int) graph that is associated *)
let create_bipartite_graph path = 
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


let bipartite_matching_algorithm infile outfile =
    let graph = create_bipartite_graph infile in
    let problem = (graph, "s", "p") in
    let (flow_graph, max_flow) = ford_fulkerson_algorithm problem in
	Printf.printf "Exepected : Maximum of assignments : 3 \n";
    Printf.printf "Obtained : Maximum of assignments : %d \n\n" max_flow ;
	let flow_graph_str = Graph.map flow_graph (fun (a,b) -> ((string_of_int b)^"/"^(string_of_int a))) in
    let () = Gfile.export flow_graph_str (outfile^"_bp_flow_graph.gv") in ()





