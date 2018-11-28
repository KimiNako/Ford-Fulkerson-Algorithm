all: build

build:
	ocamlbuild ftest.byte
	ocamlbuild ff_algorithm.byte
	mkdir Tests
	mkdir Tests/GV_files
	mkdir Tests/PNG_files

dot: 
	dot -Tpng Tests/GV_files/TEST_res.gv > Tests/PNG_files/TEST_res.png
	dot -Tpng Tests/GV_files/TEST_updated_res.gv > Tests/PNG_files/TEST_updated_res.png
	dot -Tpng Tests/GV_files/TEST_flow_graph.gv > Tests/PNG_files/TEST_flow_graph.png

