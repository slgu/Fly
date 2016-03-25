parser_dir = parser
ast_dir = ast
check_dir = check
gen_dir = codegen
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
	ocamlc -o fly $(parser_dir)/scanner.cmo $(parser_dir)/parser.cmo $(ast_dir)/ast.cmo $(ast_dir)/sast.cmo $(debug_dir)/debug.cmo  $(check_dir)/infer.cmo fly.cmo
	rm $(parser_dir)/scanner.ml $(parser_dir)/parser.mli $(parser_dir)/parser.ml $(parser_dir)/parser.output */*.cm* *.cm*
gen:
	ocamllex $(parser_dir)/scanner.mll
	ocamlyacc -v $(parser_dir)/parser.mly
	ocamlc -c $(ast_dir)/ast.ml
	ocamlc -I $(ast_dir) -c $(ast_dir)/sast.ml
	ocamlc -I $(ast_dir) -c $(parser_dir)/parser.mli
	ocamlc -I $(parser_dir) -c $(parser_dir)/scanner.ml
	ocamlc -I $(ast_dir) -I $(parser_dir) -c $(parser_dir)/parser.ml
	ocamlc -I $(ast_dir) -c $(debug_dir)/debug.ml
	ocamlc -I $(ast_dir) -c $(check_dir)/infer2.ml
	ocamlc -I $(ast_dir) -c $(gen_dir)/codegen.ml
	ocamlc -I $(parser_dir) -I $(check_dir) -I $(gen_dir) -c fly_testgen.ml
	ocamlc -o fly $(parser_dir)/scanner.cmo $(parser_dir)/parser.cmo $(debug_dir)/debug.cmo $(ast_dir)/ast.cmo $(check_dir)/infer2.cmo $(gen_dir)/codegen.cmo fly_testgen.cmo
	rm $(parser_dir)/scanner.ml $(parser_dir)/parser.mli $(parser_dir)/parser.ml $(parser_dir)/parser.output */*.cm* *.cm*
	cat test/test1 | ./fly
debug:
	ocamlc -c ast.ml
	ocamlc -c debug.ml
	ocamlc -o debug ast.cmo debug.cmo
	rm *.cm*
clean:
	rm fly $(parser_dir)/scanner.ml $(parser_dir)/parser.mli $(parser_dir)/parser.ml $(parser_dir)/parser.output */*.cm* *.cm*
exec:
	rm scanner.ml parser.mli parser.ml parser.output *.cm*
