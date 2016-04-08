open Ast
using namespace std;

(string, ((typ list), typ) list) list

(* to_float, to_int, to_string, sleep, exit *)

let build_in_code =

float to_float(int a){
	return float(a);
}

float to_float(string a) {
	return stof(a);
}

int to_int(string a){
	return stoi(a);
}

int sleep(int seconds){
	std::chrono::seconds duration(seconds);
	std::this_thread::sleep_for(duration);
	return 0;
}

let int_to_float = {
	ttkey = ""; 
	tfname = "to_float"; 
	tformals = [("int", int)]; 
	tbody = []; 
	tret = float;
}

let string_to_float = {
	ttkey = ""; 
	tfname = "to_float"; 
	tformals = [("string", string)]; 
	tbody = []; 
	tret = float;
}

let string_to_int = {
	ttkey = ""; 
	tfname = "to_int"; 
	tformals = [("string", string)]; 
	tbody = []; 
	tret = int;
}

let int_to_string = {
	ttkey = ""; 
	tfname = "to_string"; 
	tformals = [("int", int)]; 
	tbody = []; 
	tret = string;
}

let float_to_string = {
	ttkey = ""; 
	tfname = "to_string"; 
	tformals = [("float", float)]; 
	tbody = []; 
	tret = string;
}



(*

let build_in_func =
    [int_to_string];
    [float_to_string];
        ;
        ;
        ;
        ]

let build_in_class =
    [t_class_decls];
*)
