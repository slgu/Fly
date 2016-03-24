(*infer type and do a static syntax checking*)
open Ast
open Sast
let (func_binds : (string, func_decl) Hashtbl.t) = Hashtbl.create 16
let (class_binds : (string, class_decl) Hashtbl.t) = Hashtbl.create 16

(*typed function bindings*)
let (t_func_binds: (string, t_func_decl) Hashtbl.t) = Hashtbl.create 16
(*typed lambda bindings*)
let (t_lambda_binds: (string, t_lambda_decl) Hashtbl.t) = Hashtbl.create 16

let find_t_func name =
    try
        Some (Hashtbl.find t_func_binds name)
    with
    | Not_found -> None

let find_func name =
    try
        Hashtbl.find func_binds name
    with
    | Not_found -> failwith ("not this function" ^ name)

let find_class name =
    try
        Hashtbl.find class_binds name
    with
    | Not_found -> failwith ("not this class" ^ name)

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
let init_level_env () =
    [get_new_env()]

(* append a new level env to level_env*)
let append_new_level level_env =
    get_new_env() :: level_env

let update_env level_env k v = match level_env with
    | (this_level :: arr) -> Hashtbl.add this_level k v;this_level :: arr
    | _ -> failwith ("no env internal error")

let rec search_id level_env k = match level_env with
    | [] -> failwith ("variable refered without defined" ^ k)
    | (this_level :: arr) ->
        try
            Hashtbl.find this_level k
        with
        | Not_found -> search_id arr k

let back_level level_env = match level_env with
    | [] -> failwith ("no level to be back")
    | (this_level :: arr) -> arr

(*debug a level env, just print out to the screeen*)
let debug_level_env level_env =
    let rec inner_debug level_env cnt = match level_env with
        | [] -> ()
        | (this_level :: arr) -> print_endline ("this level: " ^ (string_of_int cnt));
    in
    inner_debug level_env 0

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
(* return a t_func_decl*)
let rec infer_func fdecl level_env =
    (*level_env is an ref variable modified by infer_expr and infer_stmt*)
    let ref_create_env ()=
        level_env := append_new_level (!level_env)
    in
    let ref_update_env k v =
        level_env := update_env (!level_env) k v
    in
    let ref_search_id k =
        try
            Some (search_id (!level_env) k)
        with
        | _ -> None
    in
    let ref_back_env ()=
        level_env := back_level (!level_env)
    in
    (*return a texpr*)
    let rec infer_expr epr = match epr with
        | Literal x -> TLiteral x
        | BoolLit x -> TBoolLit x
        | Id (a) -> TId (a, search_id (!level_env) a)
        | Float x -> TFloat x
        | Set (expr_list) ->
            let expr_types =
                List.map (fun item -> infer_expr item) expr_list
            in
            begin
                match expr_types with
                    | [] -> TSet (expr_list,Undef)
                    | (x :: y) ->
                        check_type_same expr_types x;
                        TSet (expr_list, x)
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
                    | [], _ -> TMap (expr_pair_list, (Undef, Undef))
                    | _, [] -> TMap (expr_pair_list, (Undef, Undef))
                    | (x1 :: y1), (x2 :: y2) ->
                        check_type_same expr_k_types x1;
                        check_type_same expr_v_types x2;
                        TMap (expr_pair_list, (x1, x2))
            end
        | Array (expr_list) ->
            let expr_types =
                List.map (fun item -> infer_expr item) expr_list
            in
            begin
                match expr_types with
                    | [] -> TArray (expr_list, Undef)
                    | (x :: y) ->
                        check_type_same expr_types x;
                        TArray (expr_list, x)
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
                    (*chan operation*)
                | _ -> failwith ("undefined binop for binop -> <-")
            end
        | Assign (varname, epr) ->
            let expr_type = infer_expr epr
            in let var = ref_search_id varname
            in  begin
                match var with
                | None -> ref_update_env varname expr_type; expr_type
                | Some x -> if expr_type = x then expr_type
                    else failwith ("redefine " ^ varname ^ " with different type")
                end
        | Unop (unop, epr) ->
            let expr_type = infer_expr epr
            in  begin
                match unop with
                | Not -> if expr_type != Bool then failwith ("not with not bool") else Bool
                | Neg -> if expr_type != Int && expr_type != Float then failwith ("neg with not int or float")
                        else expr_type
                end
        | Call (fname, expr_list) ->
            let fdecl = find_func (fname)
            in
                let expr_types = List.map infer_expr expr_list
                in
                (*create new env to infer funcion*)
                let new_func_level_env = init_level_env()
                in begin
                    match fdecl with
                    | {formals = param_list;_} -> (*set env*)
                        let param_len = List.length param_list and true_len = List.length expr_types
                        in if param_len = true_len then (* actual a function call *)
                            let new_func_level_env = List.fold_left2 (fun env param_name typ -> (update_env env param_name typ)) new_func_level_env param_list expr_types
                            in
                            infer_func fdecl (ref new_func_level_env)
                            else (* a clojure which just a function bind less than true parameters*)
                    (*TODO*) Int
                    end
            (* TODO
        | ObjCall (cname, fname, expr_list) ->
        | Func (lname, rname, expr) ->
        | ListComprehen (f_expr, varname, s_expr) -> *)
        | _ -> Int
    in
    let rec infer_stmt smt = match smt with
        | Block stmt_list -> ref_create_env();
            let stmt_list_types = List.map infer_stmt stmt_list
            in let return_type = List.hd (List.rev (stmt_list_types))
            in ref_back_env(); (*back this env*)
            return_type
        | Expr epr ->
            infer_expr epr
        | Return epr ->
            infer_expr epr
        | _ -> Int
    in
    match fdecl with
    | {body = stmt_list;_} ->
        let stmt_list_types = List.map infer_stmt stmt_list
        in List.hd (List.rev (stmt_list_types))

(* when we see a fname and para with type
    we call this function to put a record to a global type info
    and return the t_func_decl
*)
and infer_func_by_name fname type_list =
    let hash_key =
        fname ^ (List.fold_left
            (fun str item -> str "@" item) "" type_list)
    in let hash_value = find_t_func hash_key
    in match hash_value
        | None ->
            let fdecl = find_func fname
            in
                begin
                match fdecl with
                | {formals=param_list;_} ->
                    (*create env and add param type*)
                    let new_func_level_env = List.fold_left2
                    (fun env param_name typ -> (update_env env param_name typ))
                    get_new_env() param_list type_list
                    in infer_func fdecl ref(new_func_level_env)
                end

        | Some x -> x

(* perform static type checking and inferrence*)
let infer_check (ast : program) =
    bind_name ast; (*first bind name*)
    let main_func = find_func "main"
    in let level_env = ref (init_level_env())
    in infer_func main_func level_env
    (* search main function and do a static type infer*)
