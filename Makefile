all: ark run

ark: 
	ocamlbuild -r -package llvm ark.native && \
	./ark.native > codegen

run:
	lli codegen

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml ark.out ark _build *.native
