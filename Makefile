all: build

build:
	ocamlbuild ftest.byte
	ocamlbuild ff_algorithm.byte

dot: 
	dot -Tpng TEST_res.gv > TEST_res.png
	dot -Tpng TEST_updated_res.gv > TEST_updated_res.png
	dot -Tpng TEST_flow_graph.gv > TEST_flow_graph.png

