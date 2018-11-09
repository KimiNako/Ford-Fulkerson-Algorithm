all : build

build : graph gfile 
	ocamlc -o ftest graph.cmo gfile.cmo ftest.ml	

graph : graph.mli graph.ml
	ocamlc -c graph.mli
	ocamlc -c graph.ml

gfile : gfile.mli graph.ml
	ocamlc -c gfile.mli
	ocamlc -c gfile.ml
