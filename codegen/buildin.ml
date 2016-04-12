open Ast
open Sast

(* to_float, to_int, to_string, sleep, exit *)

let build_in_code =
"
float to_float(int a){
	return float(a);

float to_float(string a) {
	return stof(a);
}

int to_int(string a){
	return stoi(a);
}

string to_string(int a ){
	return to_string(a);
}
string to_string(float a ){
	return to_string(a);
}

void sleep(int seconds){
	std::chrono::seconds duration(seconds);
	std::this_thread::sleep_for(duration);
}

void exit(int exit_code){
	exit(exit_code);
}
"

let int_to_float = {
	ttkey = "";
	tfname = "to_float";
	tformals = [("int", Int)];
	tbody = [];
	tret = Float;
}

let string_to_float = {
	ttkey = "";
	tfname = "to_float";
	tformals = [("string", String)];
	tbody = [];
	tret = Float;
}

let string_to_int = {
	ttkey = "";
	tfname = "to_int";
	tformals = [("string", String)];
	tbody = [];
	tret = Int;
}

let int_to_string = {
	ttkey = "";
	tfname = "to_string";
	tformals = [("int", Int)];
	tbody = [];
	tret = String;
}

let float_to_string = {
	ttkey = "";
	tfname = "to_string";
	tformals = [("float", Float)];
	tbody = [];
	tret = String;
}

let exit_func = {
	ttkey = "";
	tfname = "exit";
	tformals = [("int", Int)];
	tbody = [];
	tret = Void;
}

let sleep_func = {
	ttkey = "";
	tfname = "sleep";
	tformals = [("int", Int)];
	tbody = [];
	tret = Void;
}

let build_in_func =
    [int_to_string;
    float_to_string;
    string_to_int;
	int_to_string;
	float_to_string;
	exit_func;
	sleep_func]

(*string to_string(void a){} *)
(*string to_string(array a){} *)
(*string to_string(set a ){} *)

(*
let build_in_class =
    [t_class_decls];
*)
