open Graph
open Ff_algorithm

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

  (* Test map : add "0" at the end of every label *)
  let graph2 = Graph.map graph (fun x -> x^"0") in

  (* Rewrite the graph that has been read. *)
  let () = Gfile.write_file outfile graph in ();

  (* Test export : write graph in a dot format *)
  let () = Gfile.export graph2 (outfile^"_map.gv") in ();
  let () = Gfile.export graph (outfile^".gv") in  ();

  (* Test : Ford_Fulkerson_Algorithm *)
  let graph3 = Graph.map graph int_of_string in
  let problem = (graph3, _source, _sink) in
  let (flow_graph, max_flow) = ford_fulkerson_algorithm problem in
  Printf.printf "Maximum of flow : %d \n" max_flow ;
	let flow_graph_str = Graph.map flow_graph (fun (a,b) -> ((string_of_int a)^","^(string_of_int b))) in
  let () = Gfile.export flow_graph_str (outfile^"_flow_graph.gv") in ();

