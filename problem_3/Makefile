a.out : lexer.mll parser.mly main.ml implang.ml 
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c implang.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o a.out implang.cmo lexer.cmo parser.cmo  main.cmo 

clean:
	rm -f a.out implang.cm* lexer.ml lexer.cm* parser.cm* parser.ml parser.mli main.cm*

