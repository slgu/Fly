open Ast
using namespace std;

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

let exit_func = {
	ttkey = "";
	tfname = "exit";
	tformals = [("int", int)];
	tbody = [];
	tret = void;
}

let sleep_func = {
	ttkey = "";
	tfname = "sleep";
	tformals = [("int", int)];
	tbody = [];
	tret = void;
}

let build_in_func =
    [int_to_string];
    [float_to_string];
    [string_to_int];
	[int_to_string];
	[float_to_string];
	[exit_func];
	[sleep_func];


(*string to_string(void a){} *)
(*string to_string(array a){} *)
(*string to_string(set a ){} *)

(*
let build_in_class =
    [t_class_decls];
*)
