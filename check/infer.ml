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

let update_env level_env k v = match level_env with
    | (this_level :: arr) -> Hashtbl.add this_level k v
    | _ -> failwith ("no env internal error")

let rec search_id level_env k = match level_env with
    | [] -> failwith ("variable refered without defined" ^ k)
    | (this_level :: arr) ->
        try
            Hashtbl.find this_level k
        with
        | Not_found -> search_id arr k

let check_type_same type_list cmp_type=
    List.iter (fun item -> if cmp_type = item then () else failwith ("type is not same")) type_list

let check_non_empty = function
    | [] -> failwith ("empty error")
    | (x :: y) -> ()

let check_type_in_arr this_type check_type_list =
    List.exists (fun item -> this_type = item) check_type_list


let built_in_str_type =
    [("int", Int);("bool", Bool);("string", String);
     ("float", Float)]

(* infer the function result given input params*)
(*let rec infer fdecl env *)
let rec infer_func fdecl level_env =
    let rec infer_expr epr = match epr with
        | Literal (_) -> Int
        | BoolLit (_) -> Bool
        | Id (a) -> search_id level_env a
        | Set (expr_list) ->
            let expr_types =
                List.map (fun item -> infer_expr item) expr_list
            in
            begin
                match expr_types with
                    | [] -> Set (Undef)
                    | (x :: y) ->
                        check_type_same expr_types x;
                        Set (x)
            end
        | Map (expr_pair_list) ->
            let expr_pair_types  =
                List.map (fun (litem, ritem) -> (infer_expr litem,
                    infer_expr ritem)) expr_pair_list
            in let expr_k_types = List.map fst expr_pair_types
            and expr_v_types = List.map snd expr_pair_types
            in
            begin
                match expr_k_types, expr_v_types with
                    | [], _ -> Map (Undef, Undef)
                    | _, [] -> Map (Undef, Undef)
                    | (x1 :: y1), (x2 :: y2) ->
                        check_type_same expr_k_types x1;
                        check_type_same expr_v_types x2;
                        Map (x1, x2)
            end
        | Array (expr_list) ->
            let expr_types =
                List.map (fun item -> infer_expr item) expr_list
            in
            begin
                match expr_types with
                    | [] -> Array (Undef)
                    | (x :: y) ->
                        check_type_same expr_types x;
                        Array (x)
                end
        | String (str) -> String
        | Binop (f_expr, bop, s_expr) ->
            let f_expr_type = infer_expr f_expr
                and s_expr_type = infer_expr s_expr
            in
            begin
                match bop with
                | Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
                ->  begin
                    match f_expr_type, s_expr_type with
                    | Int, Int -> Int
                    | Float, Float -> Float
                    | Float, Int -> Float
                    | Int, Float -> Float
                    | _, _ -> failwith ("wrong type binop with each other")
                    end
                | And | Or ->
                    begin
                    match f_expr_type, s_expr_type with
                    | Bool, Bool -> Bool
                    | _, _ -> failwith ("wrong type binop with each other")
                    end
                | SAdd ->
                    begin
                    match f_expr_type, s_expr_type with
                    | String, String -> String
                    | _, _ -> failwith ("wrong type binop with each other")
                    end
                | _ -> failwith ("undefined binop for binop -> <-")
                    (*chan operation*)
            end
        | Assign (varname, epr) ->
            let expr_type = infer_expr epr
            in Int
            (* TODO
        | Unop (epr) ->
        | Call (fname, expr_list) ->
        | ObjCall (cname, fname, expr_list) ->
        | Func (lname, rname, expr) ->
        | ListComprehen (f_expr, varname, s_expr) -> *)
        | _ -> Int
    in
    None
