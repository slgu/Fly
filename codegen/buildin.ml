open Ast
open Sast

(* to_float, to_int, to_string, sleep, exit *)

let build_in_code =
[
"
#include<fly/func.h>
"
]


let str_is_int = {
    ttkey = "";
    tfname ="str_is_int";
    tformals = [("a", String)];
    tbody = [];
    tret = Bool;
}

let str_split = {
    ttkey = "";
    tfname ="str_split";
    tformals = [("a", String)];
    tbody = [];
    tret = Array(String);
}

let len = {
	ttkey = "";
	tfname = "len";
	tformals = [("a", String)];
	tbody = [];
	tret = Int;
}

let int_to_float = {
	ttkey = "";
	tfname = "_float";
	tformals = [("int", Int)];
	tbody = [];
	tret = Float;
}

let string_to_float = {
	ttkey = "";
	tfname = "_float";
	tformals = [("string", String)];
	tbody = [];
	tret = Float;
}

let print_bool = {
	ttkey = "";
	tfname = "print_bool";
	tformals = [("bool", Bool)];
	tbody = [];
	tret = Void;
}

let print_float = {
	ttkey = "";
	tfname = "print";
	tformals = [("float", Float)];
	tbody = [];
	tret = Void;
}

let print_int = {
	ttkey = "";
	tfname = "print";
	tformals = [("int", Int)];
	tbody = [];
	tret = Void;
}

let print_str = {
	ttkey = "";
	tfname = "print";
	tformals = [("string", String)];
	tbody = [];
	tret = Void;
}

let string_to_int = {
	ttkey = "";
	tfname = "_int";
	tformals = [("string", String)];
	tbody = [];
	tret = Int;
}

let int_to_string = {
	ttkey = "";
	tfname = "_string";
	tformals = [("int", Int)];
	tbody = [];
	tret = String;
}

let float_to_string = {
	ttkey = "";
	tfname = "_string";
	tformals = [("float", Float)];
	tbody = [];
	tret = String;
}

let exit_func = {
	ttkey = "";
	tfname = "_exit";
	tformals = [("int", Int)];
	tbody = [];
	tret = Void;
}

let vect_int_to_string = {
    ttkey = "";
    tfname = "_string";
    tformals = [("vint", Array (Int))];
    tbody = [];
    tret = String;
}

let sleep_func = {
	ttkey = "";
	tfname = "sleep";
	tformals = [("int", Int)];
	tbody = [];
	tret = Void;
}
let exec_func = {
    ttkey = "";
    tfname = "exec";
    tformals = [("string", String);("string", String)];
    tbody = [];
    tret = String;
}
let string_vint_func = {
    ttkey = "";
    tfname = "_vector_int";
    tformals = [("string", String)];
    tbody = [];
    tret = Array (Int);
}
let build_in_func =
    [int_to_string;
    float_to_string;
    string_to_int;
	string_to_float;
	int_to_float;
	exit_func;
	sleep_func;
	print_str;
	print_int;
	print_float;
	print_bool;
    str_split;
    str_is_int;
    len;
    exec_func;
    string_vint_func;
    vect_int_to_string]



let rec match_build_in fname type_list =
	let rec inner_func funcs fname type_list = match funcs with
	| [] -> None
	| (x::y) -> begin match x with
		| {tfname=thisfname;tformals=binds;_}->
			let thistype_list = List.map snd binds
			in if type_list = thistype_list && fname = thisfname
			then Some x
			else inner_func y fname type_list
		end
	in
	inner_func build_in_func fname type_list

let rec check_build_in_name fname =
	List.exists (fun item -> match item with
	| {tfname=thisfname;_} -> if fname = thisfname then true else false) build_in_func

(*string to_string(void a){} *)
(*string to_string(array a){} *)
(*string to_string(set a ){} *)

(* The following defines built-in class and their member functions *)

let connection_is_alive = {
    ttkey = "";
    tfname = "is_alive";
    tformals = [];
    tbody = [];
    tret = Bool;
}

let connection_recv = {
    ttkey = "";
    tfname = "recv";
    tformals = [];
    tbody = [];
    tret = String;
}

let connection_send = {
    ttkey = "";
    tfname = "send";
    tformals = [("msg", String)];
    tbody = [];
    tret = Bool;
}

let connection_close = {
    ttkey = "";
    tfname = "close";
    tformals = [];
    tbody = [];
    tret = Void;
}

let connection = {
    tcname = "connection";
    member_binds = [];
    t_func_decls = [connection_recv; connection_close; connection_send; connection_is_alive];
}

let server_listen = {
    ttkey = "";
    tfname = "listen";
    tformals = [("port", Int)];
    tbody = [];
    tret = Void;
}

let server_accept = {
    ttkey = "";
    tfname = "accept";
    tformals = [];
    tbody = [];
    tret = Class("connection");
}

let server = {
    tcname = "server";
    member_binds = [];
    t_func_decls = [server_listen; server_accept];
}

let client_connect = {
    ttkey = "";
    tfname = "connect";
    tformals = [("server_ip", String);("port", Int)];
    tbody = [];
    tret = Class("connection");
}

let client = {
    tcname = "client";
    member_binds = [];
    t_func_decls = [client_connect];
}

let build_in_class =
    [server; connection; client]

let check_build_in_class cname =
	List.exists (fun item -> match item with
		| {tcname=thiscname;_}-> if cname = thiscname then true else false) build_in_class

let match_build_in_objcall cname fname type_list =
	let match_func tfdecl fname type_list = match tfdecl with
		| {tfname=thisfname;tformals=binds;_} ->
			let thistype_list = List.map snd binds
			in if thisfname = fname && thistype_list = type_list
			then true
			else false
	in
	let rec match_funcs tfdecls fname type_list = match tfdecls with
		| [] -> None
		| (x::y) -> if match_func x fname type_list then Some x else match_funcs y fname type_list
	in
	let rec inner_func classes cname fname type_list = match classes with
		| [] -> None
		| (x::y) ->
			begin
			match x with
			| {tcname=thiscname;t_func_decls=tfdecls;_} ->
				if thiscname = cname
				then match_funcs tfdecls fname type_list
				else inner_func y cname fname type_list
			end
	in
	inner_func build_in_class cname fname type_list


(*get the return type of array, fail if not ok*)
let get_arr_call_ret (thistype:typ) fname expr_types = match thistype with
    | Array x ->
        let expr_len = List.length expr_types
        in
        begin match fname with
        | "push_back" ->
            if expr_len = 1 then
                if [x] = expr_types then Void
                else failwith ("type not consistent: get_arr_call_ret")
            else
                failwith ("push_back not 1 element: get_arr_call_ret")
        | "get_at" ->
            if expr_len = 1 then
                if [Int] = expr_types then x
                else failwith ("type not consistent: get_arr_call_ret")
            else
                failwith ("get_at not 1 element: get_arr_call_ret")
        | "set_at" ->
            if expr_len = 2 then
                if [Int;x] = expr_types then Void
                else failwith ("type not consistent: get_arr_call_ret")
            else
                failwith ("get_at not 1 element: get_arr_call_ret")
        | "size" ->
            if expr_len = 0 then
                Int
            else
                failwith("size should 0 element: get_arr_call_ret")
        | "sync" ->
            if expr_len = 0 then
                Void
            else
                failwith("sync should 0 element: get_arr_call_ret")
        | _ ->
            failwith ("not support build in array function")
        end
    | _ -> failwith ("not array error")

(*get the return type of map, fail if not ok*)
let get_map_call_ret (thistype:typ) fname expr_types = match thistype with
    | Map (x,y) ->
        let expr_len = List.length expr_types
        in
        begin match fname with
        | "insert" ->
            if expr_len = 2 then
                if [x;y] = expr_types then Void
                else failwith ("type not consistent: get_map_call_ret")
            else
                failwith ("insert not 2 element: get_map_call_ret")
        | "get" ->
            if expr_len = 1 then
                if [x] = expr_types then y
                else failwith ("type not consistent: get_map_call_ret")
            else
                failwith ("get_at not 1 element: get_map_call_ret")
        | "size" ->
            if expr_len = 0 then
                Int
            else
                failwith("size should 0 element: get_map_call_ret")
        | "delete" ->
            if expr_len = 1 then
                if [x] = expr_types then Void
                else failwith ("type not consistent: get_map_call_ret")
            else
                failwith("delete should 1 element: get_map_call_ret")
        | "exist" ->
            if expr_len = 1 then
                if [x] = expr_types then Bool
                else failwith  ("type not consistent: get_map_call_ret")
            else
                failwith("exist should be 1 element: get_map_call_ret")
        | "sync" ->
            if expr_len = 0 then
                Void
            else
                failwith("sync should 0 element: get_map_call_ret")
        | _ ->
            failwith ("not support build in map function")
        end
    | _ -> failwith ("not array error")

let build_in_class_code = ["
#include<fly/class.h>
#include<fly/fly.h>
#include<fly/exec.h>
"]
