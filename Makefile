all:
	ocamllex scanner.mll
	ocamlyacc -v parser.mly
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml
	ocamlc -c debug.ml
	ocamlc -c fly.ml
	ocamlc -o fly scanner.cmo parser.cmo debug.cmo fly.cmo
	rm scanner.ml parser.mli parser.ml parser.output *.cm*
debug:
	ocamlc -c ast.ml
	ocamlc -c debug.ml
	ocamlc -o debug ast.cmo debug.cmo
	rm *.cm*
clean:
	rm fly scanner.ml parser.mli parser.ml parser.output *.cm*
exec:
	rm scanner.ml parser.mli parser.ml parser.output *.cm*
