ark: 
	ocamlbuild ark.native && ./ark.native

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml ark.out ark
