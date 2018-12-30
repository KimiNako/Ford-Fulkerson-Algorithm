all: build

build:
	ocamlbuild ftest.byte
	ocamlbuild ff_algorithm_test.byte
	ocamlbuild bipartite_matching_test.byte
	ocamlbuild min_cost_test.byte
	mkdir Tests
	mkdir Tests/GV_files
	mkdir Tests/PNG_files

dot: dot_ftest dot_ff_algo_test dot_bp_test dot_mc_test

dot_ftest: 
	dot -Tpng Tests/GV_files/TEST.gv > Tests/PNG_files/TEST.png
	dot -Tpng Tests/GV_files/TEST_map.gv > Tests/PNG_files/TEST_map.png

dot_ff_algo_test: 
	dot -Tpng Tests/GV_files/TEST_res.gv > Tests/PNG_files/TEST_res.png
	dot -Tpng Tests/GV_files/TEST_updated_res.gv > Tests/PNG_files/TEST_updated_res.png
	dot -Tpng Tests/GV_files/TEST_flow_graph.gv > Tests/PNG_files/TEST_flow_graph.png
	dot -Tpng Tests/GV_files/TEST2_flow_graph.gv > Tests/PNG_files/TEST2_flow_graph.png

dot_bp_test:
	dot -Tpng Tests/GV_files/TEST_bp_flow_graph.gv > Tests/PNG_files/TEST_bp_flow_graph.png


dot_mc_test:
	dot -Tpng Tests/GV_files/TEST_mc_flow_graph.gv > Tests/PNG_files/TEST_mc_flow_graph.png

clear:
	rm -r Tests/
	rm *.byte
