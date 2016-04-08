open Ast
(string, ((typ list), typ) list) list

let build_in_code =
float to_float(int a) {
    return float(a);
}

float to_float(string a) {

}
string to_string(int a ){
	return to_string(a);
}
string to_string(float a ){
	return to_string(a);		
}
string to_string(bool a){
	return to_string(a);	
}
void exit(int exit_code){
	exit(exit_code);
}

(*string to_string(void a){} *)
(*string to_string(array a){} *)
(*string to_string(set a ){} *)


let build_in_func =
    [t_func_decl]
        ;
        ;
        ;
        ;
        ]

let build_in_class =
    [t_class_decls];
