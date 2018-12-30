open Min_cost

let () =

  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf "\nUsage: %s infile outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  	let infile = "Graph_examples/"^Sys.argv.(1)
  	and outfile = Sys.argv.(2)

	in 	
	Printf.printf "\n------Test : Min_cost_algorithm------\n";
	Printf.printf "------Check %s_mc_flow_graph.gv file in Tests/GV_files repository------\n\n" outfile;
	Printf.printf "------Advice : Do \"make dot_mc_test\" in your terminal to visualize the graph in png format and find it in PNG_files repository------\n\n";
	min_cost_max_flow_algorithm infile outfile;
    Printf.printf "Expected : Maximum of assignments : 4 \n";
    Printf.printf "Expected : Minimum cost :  5 \n\n";
