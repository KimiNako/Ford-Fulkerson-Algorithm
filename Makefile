all: build

build:
	ocamlbuild ftest.byte
	ocamlbuild ff_algorithm.byte


