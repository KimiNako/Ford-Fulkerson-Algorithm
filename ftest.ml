open Graph
open Ff_algorithm

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = "Graph_examples/"^Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  in

  (* Open file *)
  let graph = Gfile.from_file infile in

  (* Test map : add "0" at the end of every label *)
	Printf.printf "\n------Test : Map : add \"0\" at the end of every label------\n";
	Printf.printf "------Check %s_map.gv file in Tests/GV_files repository-----\n\n" outfile;
  let graph2 = Graph.map graph (fun x -> x^"0") in

  (* Rewrite the graph that has been read. *)
	Printf.printf "\n------Test : Write_file : rewrite the graph that has been read------\n";
	Printf.printf "------Check %s file in Tests repository------\n\n" outfile;
  let () = Gfile.write_file outfile graph in ();

  (* Test export : write graph in a dot format *)
	Printf.printf "\n------Test : Export : write graph in a dot format------\n";
	Printf.printf "------Check %s.gv and %s_map.gv files in Tests/GV_files repository------\n\n" outfile outfile;
  let () = Gfile.export graph2 (outfile^"_map.gv") in ();
  let () = Gfile.export graph (outfile^".gv") in  ();
	Printf.printf "------Advice : Do \"make dot_ftest\" in your terminal to visualize these graphs in png format and find them in PNG_files repository------\n\n";

