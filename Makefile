parser_dir = parser
ast_dir = ast
check_dir = check
gen = codegen
all:
	ocamllex $(parser_dir)/scanner.mll
	ocamlyacc -v $(parser_dir)/parser.mly
	ocamlc -c $(ast_dir)/ast.ml
	ocamlc -I $(ast_dir) -c $(parser_dir)/parser.mli
	ocamlc -I $(parser_dir) -c $(parser_dir)/scanner.ml
	ocamlc -I $(ast_dir) -I $(parser_dir) -c $(parser_dir)/parser.ml
	ocamlc -I $(ast_dir) -c debug.ml
	ocamlc -I $(parser_dir) -c fly.ml
	ocamlc -I $(ast_dir) -c $(check_dir)/infer.ml
	ocamlc -o fly $(parser_dir)/scanner.cmo $(parser_dir)/parser.cmo debug.cmo fly.cmo
	rm $(parser_dir)/scanner.ml $(parser_dir)/parser.mli $(parser_dir)/parser.ml $(parser_dir)/parser.output */*.cm* *.cm*
debug:
	ocamlc -c ast.ml
	ocamlc -c debug.ml
	ocamlc -o debug ast.cmo debug.cmo
	rm *.cm*
clean:
	rm fly scanner.ml parser.mli parser.ml parser.output *.cm*
exec:
	rm scanner.ml parser.mli parser.ml parser.output *.cm*
