a.out : lexer.mll parser.mly main.ml implang.ml 
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c implang.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlfind ocamlc -c -linkpkg -package z3 implang.cmo verifier.ml
	ocamlfind ocamlc -c -linkpkg -package z3 implang.cmo verifier.cmo main.ml
	ocamlfind ocamlc -o  a.out  -linkpkg -package z3 implang.cmo verifier.cmo lexer.cmo parser.cmo  main.cmo 

clean:
	rm -f a.out implang.cm* lexer.ml lexer.cm* parser.cm* parser.ml parser.mli main.cm* verifier.cm* verifier.o implang.o verifier

