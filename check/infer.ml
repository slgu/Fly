open Ast
let (func_binds : (string, func_decl) Hashtbl.t) = Hashtbl.create 16
let (class_binds : (string, class_decl) Hashtbl.t) = Hashtbl.create 16

let check_non_exist_func name =
    try
        ignore(Hashtbl.find func_binds name);
        failwith ("exist this bind func:" ^ name)
    with
    | Not_found -> ()

let check_non_exist_class name =
    try
        ignore(Hashtbl.find class_binds name);
        failwith ("exist this bind class:" ^ name)
    with
    | Not_found -> ()

let check_return_of_func func_decl =
    let rec check_return_of_stmts = function
        | [Return (_)] -> ()
        | [] -> ()
        | (Return (_) :: arr) -> failwith("return is not the last statement")
        | (x :: arr) ->
            begin
                match x with
                | If (_, f_branch, s_branch) ->
                    check_return_of_stmts f_branch;
                    check_return_of_stmts s_branch;
                | While (_, branch) ->
                    check_return_of_stmts branch;
                | For (_, _, _, branch) ->
                    check_return_of_stmts branch;
                | Foreach (_, _, branch) ->
                    check_return_of_stmts branch;
                | _ -> ();
                ;
                check_return_of_stmts arr
            end
    in
    match func_decl with
        | {body = stmts;_} ->
            check_return_of_stmts stmts

(* get a name to func_decl binding of *)
(*meanwhile check the name appear once in the binds*)
let bind_name (ast : program) = match ast with
    | Program (cdecl_list, fdecl_list) ->
        begin
        List.iter (fun fdecl ->
            ignore(check_return_of_func fdecl);
            match fdecl with
            | {fname = name ; _} ->
                check_non_exist_func name;
                check_non_exist_class name;
                Hashtbl.add func_binds name fdecl;
                ) fdecl_list;
        List.iter (fun cdecl ->
            match cdecl with
                | {cname=name;_} ->
                    check_non_exist_func name;
                    check_non_exist_class name;
                    Hashtbl.add class_binds name cdecl;
                    ) cdecl_list;
        end


(* create a new env*)
let get_new_env() =
    let (env : (string, typ) Hashtbl.t) = Hashtbl.create 16
    in env

(* a multi-layer env operation *)
let init_level_env() =
    [get_new_env()]

(* append a new level env to level_env*)
let append_new_level level_env =
    get_new_env() :: level_env



(* infer the function result given input params*)
(*let rec infer fdecl env *)
let rec infer_func fdecl level_env =
    None
