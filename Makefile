parser_dir = parser
ast_dir = ast
check_dir = check
gen_dir = codegen
debug_dir = de
all:
	ocamllex $(parser_dir)/scanner.mll
	ocamlyacc -q -v $(parser_dir)/parser.mly
	ocamlc -c $(ast_dir)/ast.ml
	ocamlc -I $(ast_dir) -c $(ast_dir)/sast.ml
	ocamlc -I $(ast_dir) -c $(parser_dir)/parser.mli
	ocamlc -I $(parser_dir) -c $(parser_dir)/scanner.ml
	ocamlc -I $(ast_dir) -I $(parser_dir) -c $(parser_dir)/parser.ml
	ocamlc -I $(ast_dir) -c $(check_dir)/util.ml
	ocamlc -I $(ast_dir) -c $(check_dir)/env.ml
	ocamlc -I $(ast_dir) -c $(debug_dir)/debug.ml
	ocamlc -I $(ast_dir) -c $(check_dir)/checkstruct.ml
	ocamlc -I $(ast_dir) -I $(check_dir) -c $(gen_dir)/buildin.ml
	ocamlc -I $(gen_dir) -I $(debug_dir) -I $(ast_dir) -I $(check_dir) -c $(check_dir)/infer.ml
	ocamlc -I $(ast_dir) -I $(check_dir) -I $(gen_dir) -c $(gen_dir)/codegen.ml
	ocamlc -I $(parser_dir) -I $(check_dir) -I $(gen_dir) -c fly_gen.ml
	ocamlc -o fly $(parser_dir)/scanner.cmo $(parser_dir)/parser.cmo $(ast_dir)/ast.cmo $(ast_dir)/sast.cmo $(debug_dir)/debug.cmo $(check_dir)/util.cmo  $(check_dir)/checkstruct.cmo $(gen_dir)/buildin.cmo $(check_dir)/env.cmo $(check_dir)/infer.cmo  $(gen_dir)/codegen.cmo fly_gen.cmo

install:
	rm -rf /usr/local/include/fly
	cp -r header /usr/local/include/fly

buildin:
	ocamlc -c $(ast_dir)/ast.ml
	ocamlc -I $(ast_dir) -c $(ast_dir)/sast.ml
	ocamlc -I $(ast_dir) -I $(check_dir) -c $(gen_dir)/buildin.ml
sast:
	ocamllex $(parser_dir)/scanner.mll
	ocamlyacc -v $(parser_dir)/parser.mly
	ocamlc -c $(ast_dir)/ast.ml
	ocamlc -I $(ast_dir) -c $(ast_dir)/sast.ml
	ocamlc -I $(ast_dir) -c $(parser_dir)/parser.mli
	ocamlc -I $(parser_dir) -c $(parser_dir)/scanner.ml
	ocamlc -I $(ast_dir) -I $(parser_dir) -c $(parser_dir)/parser.ml
	ocamlc -I $(ast_dir) -c $(debug_dir)/debug.ml
	ocamlc -c $(check_dir)/util.ml
	ocamlc -I $(debug_dir) -I $(ast_dir) -I $(check_dir) -c $(check_dir)/infer.ml
	ocamlc -I $(parser_dir) -I $(check_dir) -c fly.ml
	ocamlc -o fly $(parser_dir)/scanner.cmo $(parser_dir)/parser.cmo $(ast_dir)/ast.cmo $(ast_dir)/sast.cmo $(debug_dir)/debug.cmo $(check_dir)/util.cmo $(check_dir)/infer.cmo fly.cmo
	rm $(parser_dir)/scanner.ml $(parser_dir)/parser.mli $(parser_dir)/parser.ml $(parser_dir)/parser.output */*.cm* *.cm*
gen:
	make
	sudo make install
	cat ${input} | ./fly
	g++ -pthread -std=c++11 tmp.cc
	./a.out
	rm $(parser_dir)/scanner.ml $(parser_dir)/parser.mli $(parser_dir)/parser.ml $(parser_dir)/parser.output */*.cm* *.cm* a.out
debug:
	ocamlc -c $(ast_dir)/ast.ml
	ocamlc -I $(ast_dir) -c $(ast_dir)/sast.ml
	ocamlc -I $(ast_dir) -c $(debug_dir)/debug.ml
	ocamlc -o debug $(ast_dir)/ast.cmo $(ast_dir)/sast.cmo $(debug_dir)/debug.cmo
	rm */*.cm*
bin:
	./fly < $(src)
	g++ -pthread -o $(bin) -std=c++11 tmp.cc
debug_clean:
	rm debug
clean:
	rm fly $(parser_dir)/scanner.ml $(parser_dir)/parser.mli $(parser_dir)/parser.ml $(parser_dir)/parser.output */*.cm* *.cm*
