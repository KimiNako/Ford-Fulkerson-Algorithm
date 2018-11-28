open Ff_agorithm

type infile = string
type outfile = string

(* Create the graph_file and return its name *)
let create_graph_file infile =

let create_output_file flow_graph outfile =


let bipartite_matching_algorithm infile outfile =
    let graph_file = create_graph_file infile in
    let string_graph = Gfile.from_file graph_file in
    let graph = Graph.map string_graph int_of_string in
    let problem = (graph, "s", "p") in
    let (flow_graph, max_flow) = ford_fulkerson_algorithm problem in
    Printf.printf "Maximum of assignments : %d \n" max_flow ;
	let flow_graph_str = Graph.map flow_graph (fun (a,b) -> ((string_of_int b)^"/"^(string_of_int a))) in
    let () = Gfile.export flow_graph_str (outfile^"_flow_graph.gv") in ();
    let () = create_output_file flow_graph outfile in ()
    
