open Ast
(string, ((typ list), typ) list) list

let build_in_code =
"float to_float(int a) {
    return float(a);
}

float to_float(string a) {

}"

let build_in_func =
    [t_func_decl]
        ;
        ;
        ;
        ;
        ]

let build_in_class =
    [t_class_decls];
