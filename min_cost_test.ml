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

    (* Test 1 : Max_flow_min_cost_algorithm with graph_Better *)
	Printf.printf "\n------Test : Max_flow_min_cost_algorithm - graph_Better-----\n";
	Printf.printf "------Check %s_mc_flow_graph.gv file in Tests/GV_files repository------\n\n" outfile;
	min_cost_max_flow_algorithm infile outfile;
    Printf.printf "Expected : Maximum of assignments : 4 \n";
    Printf.printf "Expected : Minimum cost :  5 \n\n";


    (* Test 2 : Max_flow_min_cost_algorithm with graph_Better2 *)
	Printf.printf "\n------Test : Min_cost_algorithm - graph_Better2-----\n";
	Printf.printf "------Check %s2_mc_flow_graph.gv file in Tests/GV_files repository------\n\n" outfile;
	min_cost_max_flow_algorithm (infile^"2") (outfile^"2");
    Printf.printf "Expected : Maximum of assignments : 3 \n";
    Printf.printf "Expected : Minimum cost :  3 \n\n";

	Printf.printf "------Advice : Do \"make dot_mc_test\" in your terminal to visualize the graphs in png format and find them in PNG_files repository------\n\n";
