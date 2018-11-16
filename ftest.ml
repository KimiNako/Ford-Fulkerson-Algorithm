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
	
	(* Test find_path *)
	let graph_int = Graph.map graph int_of_string in
  let path2 = find_path graph_int [] _source _sink in
	Printf.printf "Path found :";
	let () = List.iter (fun x -> Printf.printf "%s" x) path2 in 
	();
	Printf.printf "\n\n";

