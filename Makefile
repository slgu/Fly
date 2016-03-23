parser_dir = parser
ast_dir = ast
check_dir = check
gen = codegen
debug_dir = debug
all:
	ocamllex $(parser_dir)/scanner.mll
	ocamlyacc -v $(parser_dir)/parser.mly
	ocamlc -c $(ast_dir)/ast.ml
	ocamlc -I $(ast_dir) -c $(ast_dir)/sast.ml
	ocamlc -I $(ast_dir) -c $(parser_dir)/parser.mli
	ocamlc -I $(parser_dir) -c $(parser_dir)/scanner.ml
	ocamlc -I $(ast_dir) -I $(parser_dir) -c $(parser_dir)/parser.ml
	ocamlc -I $(ast_dir) -c $(debug_dir)/debug.ml
	ocamlc -I $(ast_dir) -c $(check_dir)/infer.ml
	ocamlc -I $(parser_dir) -I $(check_dir) -c fly.ml
	ocamlc -o fly $(parser_dir)/scanner.cmo $(parser_dir)/parser.cmo $(debug_dir)/debug.cmo  $(check_dir)/infer.cmo fly.cmo
	rm $(parser_dir)/scanner.ml $(parser_dir)/parser.mli $(parser_dir)/parser.ml $(parser_dir)/parser.output */*.cm* *.cm*
debug:
	ocamlc -c ast.ml
	ocamlc -c debug.ml
	ocamlc -o debug ast.cmo debug.cmo
	rm *.cm*
clean:
	rm fly $(parser_dir)/scanner.ml $(parser_dir)/parser.mli $(parser_dir)/parser.ml $(parser_dir)/parser.output */*.cm* *.cm*
exec:
	rm scanner.ml parser.mli parser.ml parser.output *.cm*
