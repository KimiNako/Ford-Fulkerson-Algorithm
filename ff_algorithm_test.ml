open Graph
open Ff_algorithm

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nFF Usage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = "Graph_examples/"^Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are used in the test of Ford Fulkerson with graph1*)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  in
 	(* Open file *)
  let graph = Gfile.from_file infile in
	let res_graph = init_residual_graph graph in


	(* Test find_path in a residual graph*)
  let res_graph_int = Graph.map res_graph (fun (a,b) -> (int_of_string a,int_of_string b)) in
  let path2 = find_path res_graph_int [] _source _sink in
	Printf.printf "\n------Test : find_path in a residual graph------\n\n";
	Printf.printf "Path found : Debut -> ";
	let () = List.iter (fun x -> Printf.printf "%s -> " x) path2 in ();
	Printf.printf "End\n\n";

	(* Write Residual graph in dot format *)
	Printf.printf "\n------Test : Write Residual graph in dot format------\n";
	Printf.printf "------Check %s_res.gv file in Tests/GV_files repository-----\n\n" outfile;
	let res_graph_str = Graph.map res_graph (fun (a,b) -> (b^"/"^a)) in
  let () = Gfile.export res_graph_str (outfile^"_res.gv") in ();

	(* Print min of the path *)
	let min = find_min_arc res_graph_int path2 (-1) in 	
	Printf.printf "\n------Test : print min of the path------\n\n";
	Printf.printf "Path found : Debut -> ";
	let () = List.iter (fun x -> Printf.printf "%s -> " x) path2 in ();
	Printf.printf "End\n";
	Printf.printf "Expected : Increment : 4\n";
	Printf.printf "Obtained : Increment : %d" min;
	Printf.printf "\n\n";

	(* Write Updated residual graph in dot format *)
	Printf.printf "\n------Test : Write Updated residual graph in dot format------\n";
	Printf.printf "------Check %s_updated_res.gv file in Tests/GV_files repository-----\n\n" outfile;
	let res_graph_update = update_residual_graph res_graph_int path2 min in
	let res_graph_update_str = Graph.map res_graph_update (fun (a,b) -> ((string_of_int b)^"/"^(string_of_int a))) in
  let () = Gfile.export res_graph_update_str (outfile^"_updated_res.gv") in ();

	(* Print flow of the graph *)
	let max_flow = calculate_max_flow res_graph_update _sink in 
	Printf.printf "\n------Test : print flow of the graph after one update------\n\n";
	Printf.printf "Expected : Flow : 4\n";
	Printf.printf "Obtained : Flow : %d\n\n" max_flow;

  (* Test : Ford_Fulkerson_Algorithm with graph1*)
  let graph2 = Graph.map graph int_of_string in
  let problem = (graph2, _source, _sink) in
  let (flow_graph, max_flow) = ford_fulkerson_algorithm problem in
	Printf.printf "\n------Test : Ford_Fulkerson_Algorithm - graph1------\n\n";
	Printf.printf "Expected : Maximum of flow : 23\n";
  Printf.printf "Obtained : Maximum of flow : %d \n\n" max_flow ;
  let flow_graph_str = Graph.map flow_graph (fun (a,b) -> ((string_of_int b)^"/"^(string_of_int a))) in
	Printf.printf "\n------Test : Write Ford_Fulkerson_Algorithm graph in dot format------\n";
	Printf.printf "------Check %s_flow_graph.gv file in Tests/GV_files repository-----\n\n" outfile;
  let () = Gfile.export flow_graph_str (outfile^"_flow_graph.gv") in ();

  (* Test : Ford_Fulkerson_Algorithm with graph2*)
     	(* Open file *)
  let graph = Gfile.from_file (infile^"_2") in
  let graph2 = Graph.map graph int_of_string in
  let problem = (graph2, "S", "P") in
  let (flow_graph, max_flow) = ford_fulkerson_algorithm problem in
	Printf.printf "\n------Test : Ford_Fulkerson_Algorithm - graph 2------\n\n";
	Printf.printf "Expected : Maximum of flow : 14\n";
  Printf.printf "Obtained : Maximum of flow : %d \n\n" max_flow ;
  let flow_graph_str = Graph.map flow_graph (fun (a,b) -> ((string_of_int b)^"/"^(string_of_int a))) in
	Printf.printf "\n------Test : Write Ford_Fulkerson_Algorithm graph in dot format------\n";
	Printf.printf "------Check %s2_flow_graph.gv file in Tests/GV_files repository-----\n\n" outfile;
  let () = Gfile.export flow_graph_str (outfile^"2_flow_graph.gv") in ();
	Printf.printf "\n------Advice : Do \"make dot_ff_algo_test\" in your terminal to visualize these graphs in png format and find them in PNG_files repository------\n\n";


