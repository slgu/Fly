all:
	ocamllex scanner.mll
	ocamlyacc -v parser.mly
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml
	ocamlc -c fly.ml
	ocamlc -o fly scanner.cmo parser.cmo fly.cmo
clean:
	rm fly scanner.ml parser.mli parser.ml parser.output *.cm*
exec:
	rm scanner.ml parser.mli parser.ml parser.output *.cm*
