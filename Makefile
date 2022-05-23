.PHONY: test check

build:
	dune build

b:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

p:
	OCAMLRUNPARAM=b dune exec bin/main.exe