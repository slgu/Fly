(*infer type and do a static syntax checking*)
open Ast
open Sast
open Util
open Debug
open Env
open Buildin
let (func_binds : (string, func_decl) Hashtbl.t) = Hashtbl.create 16
(*it is the class_binds*)
let (class_binds : (string, class_decl) Hashtbl.t) = Hashtbl.create 16

(*typed function bindings*)
let (t_func_binds: (string, t_func_decl) Hashtbl.t) = Hashtbl.create 16

(*typed class definition*)
let (t_class_binds: (string, t_class_decl) Hashtbl.t) = Hashtbl.create 16

(*clojure call binds*)
let (clojure_calls: (string, (typ list * typ list) list) Hashtbl.t) = Hashtbl.create 16



let rec get_or_create funcname =
    try
        Hashtbl.find clojure_calls funcname
    with
    | Not_found -> Hashtbl.add clojure_calls funcname []; get_or_create funcname

let rec update_if_no exist_calls f_type_list s_type_list =
    match exist_calls with
    | [] -> [(f_type_list, s_type_list)]
    | (x::y) -> if x = (f_type_list, s_type_list) then (x::y) else
        x :: (update_if_no y f_type_list s_type_list)


let update_clojure_calls funcname f_type_list s_type_list =
    let exist_calls = get_or_create funcname
    in let new_calls = update_if_no exist_calls f_type_list s_type_list
    in Hashtbl.replace clojure_calls funcname new_calls


let find_t_func name =
    try
        Some (Hashtbl.find t_func_binds name)
    with
    | Not_found -> None


let find_func name =
    try
        Hashtbl.find func_binds name
    with
    | Not_found -> failwith ("not this function:" ^ name)


let find_class name =
    try
        Hashtbl.find class_binds name
    with
    | Not_found -> failwith ("not this class:" ^ name)


let find_cfunc cname fname =
    let cdecl = find_class cname
    in match cdecl with
        | {cname=name;member_binds=binds;func_decls=fdecls} ->
            let rec search_func_by_name fdecls fname = begin
                match fdecls with
                | [] -> None
                | (x::y) -> begin match x with
                    | {fname=thisfname;_} -> if thisfname = fname then Some x else search_func_by_name y fname
                end
                end
            in search_func_by_name fdecls fname

let find_t_class name =
    try
        Hashtbl.find t_class_binds name
    with
    | Not_found -> failwith ("not this class:" ^ name)

let init_tclass () =
    let f k v = match v with
        | {cname=name;member_binds=binds;_} ->
            Hashtbl.add t_class_binds k {tcname=name;member_binds=binds;t_func_decls=[]}
    in Hashtbl.iter f class_binds

let update_tclass tcdecl  tfdecl =
    match tcdecl with
    | {tcname=name;member_binds=binds;t_func_decls=tfdecls} ->
        {tcname=name;member_binds=binds;t_func_decls=tfdecl::tfdecls}

let rec search_hash_key tfdecls hash_key = match tfdecls with
    | [] -> None
    | (x :: y) -> begin match x with
        | {ttkey=this_hash_key;_} -> if this_hash_key = hash_key then Some x else search_hash_key y hash_key
        end

let find_t_mfunc cname hashkey =
    let tclass = find_t_class cname
    in match tclass with
        | {tcname=name;member_binds=binds;t_func_decls=tfdecls} ->
            search_hash_key tfdecls hashkey

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

let check_not_void thistype = match thistype with
    | Void -> failwith ("void error")
    | _ -> ()

(*create a copy of the function env*)
let func_level_env () =
    let (env : (string, typ) Hashtbl.t) = Hashtbl.create 16
    in let f k v = match v with
        | {fname=name;_} -> Hashtbl.add env k (Func(name,[]))
    in Hashtbl.iter f func_binds;
    env


(* debug the global function *)
let debug_t_func_binds () =
    let f x y = print_endline x;
        match y with
        | {tret=rtype;_} -> print_endline ("return type:" ^ (type_to_string rtype));
    in
    (* print out all the hash key*)
    Hashtbl.iter f t_func_binds

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

(*just add to function bind for semantic check not codegen*)
let add_build_in_func () =
    let print_func = {fname="print";formals=["a"];body=[]}
    in let build_in_funcs = [print_func]
    in List.iter (
        fun item -> begin
            match item with
            | {fname=name;_} -> Hashtbl.add func_binds name item
        end
        ) build_in_funcs

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

(*check no null*)
(* we don't permit type null to be get evaluated*)
(*T/F*)
let is_null = function
    | Undef -> false
    | _ -> true


(*function from a string to type*)
let string_to_type s = match s with
    | "Int" -> Int
    | "Bool" -> Bool
    | "String" -> String
    | "Float" -> Float
    | _ -> failwith("this type not support")
(* infer the function result given input params*)
(*let rec infer fdecl env *)
(* return a t_func_decl can be inside a class*)
let rec infer_func fdecl hash_key type_list level_env =
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
        | Null x -> TNull (string_to_type x)
        | Id (a) -> TId (a, search_id (!level_env) a)
        | Float x -> TFloat x
        | Set (expr_list) ->
            let texpr_list =
                List.map (fun item -> infer_expr item) expr_list
            in
            let expr_types = List.map get_expr_type_info texpr_list
            in
            begin
                match expr_types with
                    | [] -> TSet (texpr_list,Undef)
                    | (x :: y) ->
                        check_type_same expr_types x;
                        TSet (texpr_list, x)
            end
        | Map (expr_pair_list) ->
            let texpr_pair_list =
                List.map (fun (litem, ritem) -> (infer_expr litem,
                    infer_expr ritem)) expr_pair_list
            in let expr_k_types = List.map
                        (fun item -> get_expr_type_info (fst item)) texpr_pair_list
                and expr_v_types = List.map
                        (fun item -> get_expr_type_info (snd item)) texpr_pair_list
            in
            begin
                match expr_k_types, expr_v_types with
                    | [], _ -> TMap (texpr_pair_list, Map (Undef, Undef))
                    | _, [] -> TMap (texpr_pair_list, Map (Undef, Undef))
                    | (x1 :: y1), (x2 :: y2) ->
                        check_type_same expr_k_types x1;
                        check_type_same expr_v_types x2;
                        TMap (texpr_pair_list, Map(x1, x2))
            end
        | Array (expr_list) ->
            let texpr_list =
                List.map (fun item -> infer_expr item) expr_list
            in
            let expr_types = List.map get_expr_type_info texpr_list
            in
            begin
                match expr_types with
                    | [] -> TArray (texpr_list, Undef)
                    | (x :: y) ->
                        check_type_same expr_types x;
                        TArray (texpr_list, x)
                end
        | String (str) -> TString (str)
        | Binop (f_expr, bop, s_expr) ->
            let
                t_f_expr = infer_expr f_expr
                and t_s_expr = infer_expr s_expr
            in
            let f_expr_type = get_expr_type_info t_f_expr
                and s_expr_type = get_expr_type_info t_s_expr
            in
            begin
                match bop with
                | Add -> begin
                    match f_expr_type, s_expr_type with
                    | Int, Int -> TBinop ((t_f_expr, bop, t_s_expr), Int)
                    | Float, Float -> TBinop ((t_f_expr, bop, t_s_expr), Float)
                    | Float, Int -> TBinop ((t_f_expr, bop, t_s_expr), Float)
                    | Int, Float -> TBinop ((t_f_expr, bop, t_s_expr), Float)
                    | String, String -> TBinop ((t_f_expr, bop, t_s_expr), String)
                    | _, _ -> failwith ("wrong type binop with each other")
                    end
                | Sub | Mult | Div ->  begin
                    match f_expr_type, s_expr_type with
                    | Int, Int -> TBinop ((t_f_expr, bop, t_s_expr), Int)
                    | Float, Float -> TBinop ((t_f_expr, bop, t_s_expr), Float)
                    | Float, Int -> TBinop ((t_f_expr, bop, t_s_expr), Float)
                    | Int, Float -> TBinop ((t_f_expr, bop, t_s_expr), Float)
                    | _, _ -> failwith ("wrong type binop with each other")
                    end
                | Equal | Neq | Less | Leq | Greater | Geq -> begin
                    match f_expr_type, s_expr_type with
                    | Int, Int
                    | Float, Float
                    | Float, Int
                    | Int, Float
                        -> TBinop ((t_f_expr, bop, t_s_expr), Bool)
                    | _, _ -> failwith ("wrong type binop with each other")
                end
                | And | Or ->
                    begin
                    match f_expr_type, s_expr_type with
                    | Bool, Bool -> TBinop ((t_f_expr, bop, t_s_expr), Bool)
                    | _, _ -> failwith ("wrong type binop with each other")
                    end
                | SAdd ->
                    begin
                    match f_expr_type, s_expr_type with
                    | String, String -> TBinop ((t_f_expr, bop, t_s_expr), String)
                    | _, _ -> failwith ("wrong type binop with each other")
                    end
                    (*TODO chan operation*)
                | Mod ->
                    begin
                    match f_expr_type, s_expr_type  with
                    | Int, Int -> TBinop ((t_f_expr, bop, t_s_expr), Int)
                    | _, _ -> failwith ("wrong type binop with each other")
                    end
                | _ -> failwith ("chan not implemented,
                        undefined binop for binop -> <-")
            end
        | Assign (varname, epr) ->
            let tepr = infer_expr epr
            in let expr_type = get_expr_type_info tepr
            in check_not_void expr_type;let var = ref_search_id varname
            in  begin
                match var with
                | None -> ref_update_env varname expr_type;
                    TAssign ((varname, tepr), expr_type)
                | Some x -> if expr_type = x then
                    TAssign ((varname, tepr), expr_type)
                    else failwith ("redefine " ^ varname ^ " with different type")
                end
        | MAssign (varname, mname, epr) ->
            let tepr = infer_expr epr
            in let expr_type = get_expr_type_info tepr
            in check_not_void expr_type;let var = ref_search_id varname
            in begin match var with
                | Some (Class cname) ->
                    let cdecl = find_class cname
                    in let mvartype = get_class_member_type cdecl mname
                    in if mvartype == expr_type then TMAssign ((varname, mname,tepr), mvartype)
                    else failwith ("type not consistent in the obj assign:" ^ (type_to_string mvartype) ^ "," ^ (type_to_string expr_type))
                | None -> failwith ("class obj meber assign without init obj")
                | _ -> failwith (mname ^ " not exist in the class: " ^ varname)
                end
        | Unop (unop, epr) ->
            let tepr = infer_expr epr
            in let expr_type = get_expr_type_info tepr
            in  begin
                match unop with
                | Not -> if expr_type != Bool then failwith ("not with not bool") else
                    TUnop ((unop, tepr),Bool)
                | Neg -> if expr_type != Int && expr_type != Float then failwith ("neg with not int or float")
                        else  TUnop ((unop, tepr), expr_type)
                end
        | Func (param_list, epr) ->
            (*lambda expression*)
            (*we don't evaluate the expression '
            but get the bindings and create new fdecl*)
            let rec get_inner_bindings epr =
                match epr with
                | Id a -> if List.mem a param_list then []
                          else
                           let a_bind = ref_search_id a
                            in begin
                            match a_bind with
                            | Some x -> [(a,x)]
                            | None -> []
                            end
                | Set expr_list ->
                    List.concat (List.map get_inner_bindings expr_list)
                | Map expr_pair_list ->
                    let expr_single_list = List.fold_left (fun arr (item1, item2) -> item2::item1::arr) [] expr_pair_list
                    in List.concat (List.map get_inner_bindings expr_single_list)
                | Array expr_list ->
                    List.concat (List.map get_inner_bindings expr_list)
                | Binop (f_expr, thisop, s_expr) ->
                    let left_binds = get_inner_bindings f_expr
                    and right_binds = get_inner_bindings s_expr
                    in List.append left_binds right_binds
                | Unop (thisnop, thisexpr) ->
                    get_inner_bindings thisexpr
                | Call (name, expr_list) ->
                    if List.mem name param_list then []
                    else
                        let name_bind = ref_search_id name
                        in  begin
                        match name_bind with
                        | None -> List.concat (List.map get_inner_bindings expr_list)
                        | Some x -> (name, x) :: (List.concat (List.map get_inner_bindings expr_list))
                        end
                | _ -> []
            in
            let inner_params_binds = get_inner_bindings epr
            in
            let inner_param_name_binds = List.map (fun (name, thistype) -> name) inner_params_binds
            in
            let new_lambda_name =
                "_" ^ next_random_string()
            in
            let new_lambda_func =
                {
                    fname=new_lambda_name;
                    formals=(List.append inner_param_name_binds param_list);
                    body=[Return epr]
                }
            in
            (*add this func to func_binds*)
            Hashtbl.add func_binds new_lambda_name new_lambda_func;
            let texpr_list = List.map (fun (name, thistype) -> TId (name, thistype)) inner_params_binds
            (*replace with a clojure call*)
            and type_list = List.map (fun (name, thistype) -> thistype) inner_params_binds
            in TCall ((new_lambda_name, texpr_list), Func (new_lambda_name, type_list))
        | Fly (name, expr_list) ->
            let ftype = ref_search_id name
            in
                begin
                match ftype with
                | None -> failwith("unknow refer" ^ name)
                | Some (Func (fname, arr)) -> (*with*)
                    let texpr_list = List.map infer_expr expr_list
                    in let expr_types = List.map get_expr_type_info texpr_list
                    in let fdecl = find_func fname (* find the function*)
                    in let binding_len = List.length arr
                    in
                        begin
                        match fdecl with
                        | {formals = param_list;_} -> (*set env*)
                        let param_len = List.length param_list and true_len = List.length expr_types
                        in if param_len = true_len + binding_len then (* actual a function call *)
                            let rtype = get_func_result (infer_func_by_name fname (List.append arr expr_types))
                            in TFly ((name, texpr_list), Signal rtype)
                            else failwith ("fly with not a true function call ")
                            (* a clojure which just a function bind less than true parameters*)
                        end
                | _ -> failwith  ("not a clojure or function obj when functioncall")
                end
        | Register (signal_name, name, expr_list) ->
            (*check signal name is a signal*)
            let signal_type = ref_search_id signal_name
            in
            begin
            match signal_type with
            | Some (Signal x) ->
                begin
                let ftype = ref_search_id name
                in
                match ftype with
                | None -> failwith ("unknow refer" ^ name)
                | Some (Func (fname, arr)) ->
                    let texpr_list = List.map infer_expr expr_list
                    in let expr_types = List.map get_expr_type_info texpr_list
                    in let fdecl = find_func fname (* find the function*)
                    in let binding_len = List.length arr
                    in
                        begin
                        match fdecl with
                        | {formals = param_list;_} ->
                        let param_len = List.length param_list and true_len = List.length expr_types
                        in if param_len = true_len + binding_len + 1 then
                            (*generate t_funcdecl for this call*)
                            let _ = infer_func_by_name fname (arr @ expr_types @ [x])
                            (*always void for register*)
                            in TRegister ((signal_name, name, texpr_list), x)
                            else failwith ("param num not consistent")
                        end
                | _ -> failwith ("not a clojure or function when register call")
                end
            | _ -> failwith ("no signal type can not c")
            end
        | ObjGen x ->
            let cdecl = find_class x
            in TObjGen (x, Class (get_class_name cdecl))
        | Objid (x, y) ->
            let ctype = ref_search_id x
            in begin
                match ctype with
                | Some (Class cname) ->
                    let cdecl = find_class cname
                    in let mvartype = get_class_member_type cdecl y
                    in TObjid ((x, y), mvartype)
                | None -> failwith ("var used without defined: " ^ x)
                | _ -> failwith ("not class obj can not dot id: " ^ x)
                end
        (*clojure*)
        | Call (name, expr_list) ->
            (* find in bindings*)
            let texpr_list = List.map infer_expr expr_list
            in let expr_types = List.map get_expr_type_info texpr_list
            in let ftype = ref_search_id name
            in
                begin
                match ftype with
                | None ->
                    (*maybe build in no support for clojure build in now*)
                    let test_build_in_func = match_build_in build_in_func name expr_types
                    in begin match test_build_in_func with
                     | None -> failwith ("no refer to " ^ name)
                     | Some tfdecl ->
                        begin
                        match tfdecl with
                        | {tret=rtype;_} ->
                        TCall ((name, texpr_list), rtype)
                        end
                    end
                | Some (Func (fname, arr)) -> (*with*)
                    let fdecl = find_func fname (* find the function*)
                    in let binding_len = List.length arr
                    in
                        begin
                        match fdecl with
                        | {formals = param_list;_} -> (*set env*)
                        let param_len = List.length param_list and true_len = List.length expr_types
                        in
                            (*update clojure calls if not build in*)
                            if check_build_in_name fname then () else update_clojure_calls fname arr expr_types;
                            if param_len = true_len + binding_len then (* actual a function call *)
                            let rtype = get_func_result (infer_func_by_name fname (List.append arr expr_types))
                            in TCall ((name, texpr_list), rtype)
                            else
                            if param_len < true_len + binding_len then
                                failwith ("too many args")
                            else
                            TCall ((name, texpr_list), Func (fname, List.append arr expr_types))
                            (* a clojure which just a function bind less than true parameters*)
                        end
                | _ -> failwith  ("not a clojure or function obj when functioncall")
                end
        (*Haha OBJ CALL!*)
        | ObjCall (varname, fname, expr_list) ->
            (* find in bindings*)
            let ftype = ref_search_id varname
            in (*check class variable*)
                begin
                match ftype with
                | Some (Class cname) ->
                    (*get expr type*)
                    let texpr_list = List.map infer_expr expr_list
                    in let expr_types = List.map get_expr_type_info texpr_list
                    in let somefdecl = find_cfunc cname fname
                    in begin match somefdecl with
                    | None -> failwith ("this member func not found: " ^ cname ^ " " ^ fname)
                    | Some fdecl ->
                        (*check list length, don't consider meberfunc clojure now*)
                        begin match fdecl with
                        | {formals=param_list;_} ->
                        let param_len = List.length param_list and true_len = List.length expr_types
                        in if param_len = true_len then
                            let rtype = get_func_result (infer_cfunc_by_name cname fname expr_types)
                            in TObjCall ((varname, fname, texpr_list), rtype)
                        else
                            failwith ("args number not the same")
                        end
                    end
                | None -> failwith ("var used without defined: " ^ varname)
                | _ -> failwith ("not class obj can not dot id: " ^ varname)
                end
        (* TODO
        | Func (lname, rname, expr) ->
        | ListComprehen (f_expr, varname, s_expr) -> *)
        | _ -> TLiteral 142857
    in
    (* return a tstmt*)
    let rec infer_stmt smt = match smt with
        | Block stmt_list -> ref_create_env();
            let tstmt_list = List.map infer_stmt stmt_list
            in ref_back_env(); (*back this env*)
                TBlock tstmt_list
        | Expr epr ->
            TExpr (infer_expr epr)
        | Return epr ->
            (*TODO may update return type*)
            let tepr = infer_expr epr
            in let tepr_type = get_expr_type_info tepr
            in let tfdecl = Hashtbl.find t_func_binds hash_key
            in let new_tfdecl = compare_and_update tfdecl tepr_type
            in Hashtbl.replace t_func_binds hash_key new_tfdecl;
                TReturn tepr
        | If (judge_expr, f_stmt_list, s_stmt_list) ->
            let judge_t_expr = infer_expr judge_expr
            in
                check_bool (get_expr_type_info judge_t_expr);
                ref_create_env();
                let t_f_stmt_list = List.map infer_stmt f_stmt_list
                in ref_back_env();
                    ref_create_env();
                    let t_s_stmt_list = List.map infer_stmt s_stmt_list
                    in ref_back_env();
                        TIf (judge_t_expr, t_f_stmt_list, t_s_stmt_list)
        | For (init_expr, judge_expr, loop_expr, inner_stmt_list) ->
            (* new env *)
            ref_create_env();
            let init_texpr = infer_expr init_expr
            in let judge_texpr = infer_expr judge_expr
            in let judge_texpr_type = get_expr_type_info judge_texpr
            in begin
            match judge_texpr_type with
            | Bool ->
                let loop_texpr = infer_expr loop_expr
                in let tstmt_list = List.map infer_stmt inner_stmt_list
                in ref_back_env();
                    TFor (init_texpr, judge_texpr, loop_texpr, tstmt_list)
            | _ -> failwith ("judge expr not bool type")
            end
        | While (judge_expr, stmt_list) ->
            let judge_texpr = infer_expr judge_expr
            in let judge_texpr_type = get_expr_type_info judge_texpr
            in begin
            match judge_texpr_type with
            | Bool ->
                ref_create_env();
                let tstmt_list = List.map infer_stmt stmt_list
                in ref_back_env();
                    TWhile (judge_texpr, tstmt_list)
            | _ -> failwith("judge expr not bool type")
            end
        (* TODO complete other cases*)
        | _ -> TBlock []
    in
    match fdecl with
    | {body = stmt_list;formals = param_list;fname = func_name} ->
        (* scan twice to check return type*)
        let _ = List.map (fun item -> try
            infer_stmt item
        with
        | _ -> TExpr (TLiteral 0)) stmt_list
        (*sequencely infer each stmt with level env *)
        in let tstmt_lists = List.map infer_stmt stmt_list
        in let t_param_list = List.map2 (fun item1 item2 -> (item1, item2)) param_list type_list
        in let rtype = get_func_result (Hashtbl.find t_func_binds hash_key)
        in (*if undef then set void*)
            if rtype == Undef then
                {ttkey = hash_key;tfname = func_name;tformals = t_param_list;tbody = tstmt_lists;tret = Void}
            else
                {ttkey = hash_key;tfname = func_name;tformals = t_param_list;tbody = tstmt_lists;tret = rtype}
        (*generate a t func decl*)


(* when we see a fname and para with type
    we call this function to put a record to a global type info
    and return the t_func_decl
*)

and infer_func_by_name fname type_list =
    let hash_key = gen_hash_key fname type_list
    in let hash_value = find_t_func hash_key
    (*this is the place for check in*)
    in let test_build_in_func = match_build_in build_in_func fname type_list
    in match test_build_in_func with
        | Some x -> x
        | None ->
        begin
        match hash_value with
        | None ->
            let fdecl = find_func fname
            in
                begin
                match fdecl with
                | {formals=param_list;_} ->
                    (*first create a binding*)
                    Hashtbl.add t_func_binds hash_key (new_null_tfdecl());
                    (*create func env*)
                    let func_env =
                        func_level_env()
                    in
                    (*create env and add param type*)
                    let new_func_level_env =
                        List.fold_left2 (fun env param_name this_type -> update_env env param_name this_type) (init_level_env()) param_list type_list
                    in
                    let ref_new_func_level_env = ref(List.rev (func_env::new_func_level_env))
                    in let tfdecl = infer_func fdecl hash_key type_list ref_new_func_level_env
                    in
                    (*store in the global hash*)
                    Hashtbl.replace t_func_binds hash_key tfdecl;
                    tfdecl
                end
        | Some x ->
            let rtype = get_func_result x
            in if rtype == Undef then failwith ("no stop recurisve call" ^hash_key)
            else x
        end
(*infer a class member function call*)
and infer_cfunc_by_name cname fname type_list =
    let hash_key = gen_hash_key fname type_list
    in let hash_value = find_t_mfunc cname hash_key
    in match hash_value with
        | None ->
            let somefdecl = find_cfunc cname fname
            in begin match somefdecl with
            | None -> failwith ("no this member func")
            | Some fdecl ->
                (*bind the env*)
                begin match fdecl with
                    | {formals=param_list;_}->
                    (*first create a binding*)
                    Hashtbl.add t_func_binds hash_key (new_null_tfdecl());
                    let tcdecl = find_t_class cname
                    in
                    (*create func env*)
                    let func_env =
                        func_level_env()
                    in
                    (*create env and add param type*)
                    let new_func_level_env =
                        List.fold_left2 (fun env param_name this_type -> update_env env param_name this_type) (init_level_env()) param_list type_list
                    in
                    let new_func_level_env =
                        (*add class member variable*)
                        match tcdecl with
                        | {member_binds=binds;_} ->
                            List.fold_left (fun env (varname, thistype) -> update_env env varname thistype) new_func_level_env binds
                    in
                    let ref_new_func_level_env = ref(List.rev (func_env::new_func_level_env))
                    in let tfdecl = infer_func fdecl hash_key type_list ref_new_func_level_env
                    in
                    let new_tcdecl = update_tclass tcdecl tfdecl
                    in
                        (*replace in the global t_class_binds hash*)
                        Hashtbl.replace t_class_binds cname new_tcdecl;
                        tfdecl
                end
            end
        | Some x ->
            let rtype = get_func_result x
            in if rtype = Undef then failwith ("no class stop recurisve call" ^hash_key)
            else x

let debug_ast_cdecl ast = match ast with
    | Program (cdecls, _) -> List.iter (fun item -> print_endline (debug_cdecl item)) cdecls

(* perform static type checking and inferrence*)
let infer_check (ast : program) =
    add_build_in_func(); (*first add some build in func name*)
    bind_name ast; (*second bind name*)
    init_tclass();
    (*just infer the main function and recur infer all involved functions *)
    let _ =  infer_func_by_name "main" []
    in
    (*
    debug_ast_cdecl ast;
    debug_t_func_binds();
    print_endline (debug_t_fdecl main_fdecl);
    *)
    (t_func_binds, t_class_binds, clojure_calls, func_binds, t_func_binds)
    (* search main function and do a static type infer*)
