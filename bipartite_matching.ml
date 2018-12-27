open Ff_algorithm
open Graph
type infile = string
type outfile = string
type path = string

(* Create the graph_file and return its name *)
(* Convert bipartite graph into (V,E) format *)
(*
let create_graph_file path =
	let infile = open_in path

(* ????????? *)
let create_output_file flow_graph outfile =
*)


let read_node graph line is_student= 
	if is_student then 
	(* Read student nodes *)
		try Scanf.sscanf line "s %s" (fun name_student -> let newgraph = add_node graph name_student in add_arc newgraph "s" name_student 1)
		with e -> Printf.printf "Cannot read node in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "create_bipartite_graph"
	else
	(* Read wish nodes *)
	try Scanf.sscanf line "o %s %d" (fun wish capacity-> let newgraph = add_node graph wish in add_arc newgraph wish "p" capacity)
	with e -> Printf.printf "Cannot read node in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "create_bipartite_graph"

(* Associate student to wishes *)
let read_arc graph line = 
	try Scanf.sscanf line "p %s %d %s" (fun name_student rank wish -> add_arc graph name_student wish rank) (* REVIEW THIS LABEL !!!!!!!!!!*)
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

(*
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
    
*)

let bipartite_matching_algorithm path outfile =
    let graph = create_bipartite_graph path in
    (*let problem = (graph, "s", "p") in
    let (flow_graph, max_flow) = ford_fulkerson_algorithm problem in
    Printf.printf "Maximum of assignments : %d \n" max_flow ;
	*)let flow_graph_str = Graph.map graph (fun (a,b) -> ((string_of_int b)^"/"^(string_of_int a))) in
    let () = Gfile.export flow_graph_str (outfile^"_flow_graph.gv") in ()



(* -----------------------------------------------*)
(* --------------- TESTS -------------------------*)
(*------------------------------------------------*)
let () =

  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf "\nUsage: %s infile outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;
  	let path = Sys.argv.(1)
  	and outfile = Sys.argv.(2)
	in bipartite_matching_algorithm path outfile;;


