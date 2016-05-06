open Ast
open Sast
open Env
open Util
open Infer
open Buildin

type sigbind = {
    vn : string;
    vt : typ;
}

type regibind = {
    vn : string; (* sig var name *)
    vt : typ;    (* Signal(t) *)
    rn : string; (* var name to wait for value *)
}

type fkey_fd_bind = {
    fkey : string;
    fd   : t_func_decl;
}

type objfly_bind = {
    objname : string;
    classname : string;
    fname : string;
    paramt : typ list;
    rtype : typ;
}

let gen_ofly_fkey oname fname =  ""

(* store signal funcs *)
let (objsignal_funcs : (string, objfly_bind) Hashtbl.t) = Hashtbl.create 16

(* store signal funcs *)
let (signal_funcs : (string, string) Hashtbl.t) = Hashtbl.create 16

(* store register funcs *)
let (register_funcs : (string, string) Hashtbl.t) = Hashtbl.create 16


let gen_clojure_class_name funcname type_list =
    funcname ^ (List.fold_left (fun res item -> res ^ "_" ^ (type_to_string item)) "_clojure" type_list)




let gen_clojure_classes clojure_calls func_binds t_func_binds =
    let find_func name =
        try
            Hashtbl.find func_binds name
        with
        | Not_found -> failwith ("not this function:" ^ name)
    in
    let gen_clojure_class fname call_list =
        let (clojure_class_hash : (string, t_class_decl) Hashtbl.t) = Hashtbl.create 16
        in let fdecl = find_func fname
        in let init_tcdecl fname type_list = begin match fdecl with
            | {formals=param_list;_} ->
                let _ = List.length type_list
                in let modify_param_list = List.map (fun item -> "_" ^ item) param_list
                in let binds = zip modify_param_list type_list
                in let clojure_name = gen_clojure_class_name fname type_list
                in {tcname=clojure_name;member_binds=binds;t_func_decls=[]}
            end
        in let rec get_or_init fname type_list =
            let clojure_name = gen_clojure_class_name fname type_list
            in
            try
                Hashtbl.find clojure_class_hash clojure_name
            with
            | _ -> Hashtbl.add clojure_class_hash clojure_name (init_tcdecl fname type_list);get_or_init fname type_list
        in let rec update_if_no tfdecls tfdecl = match tfdecls with
            | [] -> [tfdecl]
            | (x::y) -> begin match x, tfdecl with
                | {ttkey=key1;_}, {ttkey=key2;_} ->
                    if key1 = key2 then y else x :: (update_if_no y tfdecl)
                end
        in let gen_tfdecl fname f_type_list s_type_list =
            let clojure_name = gen_clojure_class_name fname f_type_list
            in let _ = get_or_init fname (List.concat [f_type_list;s_type_list]) and tcdecl = get_or_init fname f_type_list
            in begin match fdecl with
            | {formals=param_list;_} ->
                let l1 = List.length f_type_list
                in let l2 = List.length s_type_list
                in let lt = List.length param_list
                in let f_binds = zip param_list f_type_list
                in let s_param_list = drop_first param_list l1
                in let s_binds = zip s_param_list s_type_list
                in
                    let new_tfdecl =
                    if l1 + l2 = lt then
                        let tfkey = gen_hash_key fname (List.concat [f_type_list;s_type_list])
                        in
                        let tfdecl = Hashtbl.find t_func_binds tfkey
                        in let rtype = get_func_result tfdecl
                        in
                        let ftexprs = List.map (fun (varname, thistype) -> TId("_" ^ varname, thistype)) f_binds
                        in let stexprs = List.map (fun (varname, thistype) -> TId(varname, thistype)) s_binds
                        in let ttexprs = List.concat [ftexprs;stexprs]
                        in let body = [TReturn (TCall ((fname, ttexprs), rtype))]
                        in {
                            ttkey=gen_hash_key "call" s_type_list;
                            tfname="call";
                            tformals=s_binds;
                            tbody = body;
                            tret = rtype
                        }
                    else
                    let res_class_name = gen_clojure_class_name fname (List.concat [f_type_list;s_type_list])
                    in let new_obj_stmt = TExpr (TAssign (("tmp", TObjGen(Class res_class_name, Class res_class_name)), Class res_class_name))
                    in let f_assign_stmts =
                        List.map (fun (varname, thistype) -> TExpr (TMAssign(("tmp", "_" ^ varname, TId (varname, thistype)), thistype))) f_binds
                    in let s_assign_stmts =
                        List.map (fun (varname, thistype) -> TExpr (TMAssign(("tmp", "_" ^ varname, TId (varname, thistype)), thistype))) s_binds
                    in let return_stmt =
                        TReturn (TId ("tmp", Class res_class_name))
                    in {
                            ttkey=gen_hash_key "call" s_type_list;
                            tfname="call";
                            tformals=s_binds;
                            tbody = [new_obj_stmt] @ f_assign_stmts @ s_assign_stmts @[return_stmt];
                            tret = Class res_class_name
                        }
                    in begin match tcdecl with
                        | {tcname=tmp1;member_binds=tmp2;t_func_decls=tfdecls;_} ->
                            let new_tfdecls = update_if_no tfdecls new_tfdecl
                            in Hashtbl.replace clojure_class_hash clojure_name {tcname=tmp1;member_binds=tmp2;t_func_decls=new_tfdecls}
                    end
            end
    in
        List.iter (fun (f_type_list, s_type_list) -> gen_tfdecl fname f_type_list s_type_list) call_list;
        Hashtbl.fold (fun k v arr -> (k,v)::arr) clojure_class_hash []
    in
        List.concat (Hashtbl.fold (fun k v arr -> (gen_clojure_class k v) :: arr) clojure_calls [])





let (fundone : (string, string) Hashtbl.t) = Hashtbl.create 16

let add_hash ht k v =
    Hashtbl.add ht k v

let find_hash ht key =
    try
        Some (Hashtbl.find ht key)
    with
    | Not_found -> None


let remove_hash ht key =
    Hashtbl.remove ht key


let clean_up_hash ht =
    Hashtbl.iter (fun k v -> remove_hash ht k) ht


let rec type_to_func_string = function
    | Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
    | String -> "string"
    | Float -> "float"
    | Signal(x) -> "signal_" ^ (type_to_func_string x)
    | Chan(x) -> "chan_" ^ (type_to_func_string x)
    | Class(x) -> "class_" ^ x
    | Array(x) -> "array_" ^ (type_to_func_string x)
    | Map(x,y) -> "map_" ^ (type_to_func_string x) ^ "_" ^ (type_to_func_string y)
    | Func(_, tlist) -> List.fold_left (fun ret ele -> ret ^ "_" ^ (type_to_func_string ele)) "closure" tlist
    | Lfunc(_) -> raise (Failure ("type_to_func_string not yet support Lfunc")) (* TODO *)
    | Set(_) -> raise (Failure ("type_to_func_string not yet support Set")) (* TODO *)
    | Undef -> raise (Failure ("type_to_func_string not yet support Undef")) (* TODO *)
    | Cfunc(_) -> raise (Failure ("type_to_func_string not yet support Cfunc")) (* TODO *)
    | _ -> raise (Failure ("type_to_func_string not yet support this type")) (* TODO *)

let rec type_to_code_string x = begin match x with
    | Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
    | String -> "string"
    | Float -> "float"
    | Signal(Class(x)) -> "shared_ptr <Signal<" ^ x ^ ">>"
    | Signal(x) -> "shared_ptr <Signal<" ^ (new_type_to_code_string x) ^ ">>"
    | Class x -> "shared_ptr <" ^ x ^ ">"
    | Array x -> "shared_ptr < vector <" ^ (type_to_code_string x) ^ "> >"
    | Map (x, y) -> "shared_ptr < flymap <" ^ (type_to_code_string x) ^ "," ^ (type_to_code_string y) ^ "> >"
    | Chan x -> "shared_ptr <Chan<" ^ (new_type_to_code_string x) ^ ">>"
    | Func (x, type_list) -> "shared_ptr <" ^ (gen_clojure_class_name x type_list) ^ ">"
    | _ -> raise (Failure ("type_to_code_string not yet support this type"))
    end
and new_type_to_code_string x = begin match x with
    | Class x -> x
    | Array x -> "vector <" ^ (type_to_code_string x) ^ ">"
    | Map (x, y) ->  "flymap <" ^ (type_to_code_string x) ^ "," ^ (type_to_code_string y) ^ ">"
    | Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
    | String -> "string"
    | Float -> "float"
    | x -> print_endline (type_to_string x);failwith ("not support for other new_type_to_code_string")
    end

(* take a string list and concatenate them with interleaving space into a single string *)
let rec cat_string_list_with_space sl =
    match sl with
    | [] -> ""
    | hd::tl -> hd ^ " " ^ (cat_string_list_with_space tl)

(* take a string list and concatenate them with interleaving comma into a single string *)
let rec cat_string_list_with_comma sl =
    let tmp = List.fold_left (fun ret ele -> ret ^ ele ^ ",") "" sl in
    let len = (String.length tmp) in
    if len > 0 then (String.sub tmp 0 (len-1)) else tmp

let rec merge_string_list sl = match sl with
    | [] -> ""
    | (x::y) -> x ^ (merge_string_list y)

let get_typelist_from_fm fm =
    List.fold_left
    (fun ret (str_, type_) -> ret @ [type_])
    [] fm

(* take a formal and generate the string *)
let handle_fm formals refenv =
    let fstr =
        List.fold_left
        (fun ret (str_, type_) ->
            ignore(update_env (!refenv) str_ type_);
            ret ^ " " ^ (type_to_code_string type_) ^ " " ^ str_ ^ ",") "" formals in
    let len = (String.length fstr) in
    let trimed = if len > 0 then (String.sub fstr 0 (len-1)) else fstr in
    "(" ^ trimed ^ ")"

(* generate fly wrapper, return string list *)
let handle_fd_fly fd refenv =
    match fd with
    | {tret=rt; tfname=name; tformals=fm; tbody=body ;_} ->
        let ret = ["void"] in
        let rtstr = type_to_code_string rt in
        let tlist = get_typelist_from_fm fm in
        let fname = List.fold_left
                    (fun ret type_ -> ret ^ "_" ^ (type_to_func_string type_))
                    name
                    (tlist @ [Signal(rt)]) in
        let sigvar = fname ^ "_sig" in
        let fmstr = handle_fm (fm @ [(sigvar, Signal(rt))]) refenv in
        let param = cat_string_list_with_comma (List.map (fun (n,_) -> n) fm) in
        let getvar = fname ^ "_var" in
        let body =
            match rt with
            | Void -> ["{"] @ [name ^ "(" ^ param ^ ");"] @ ["}"]
            | Array(_) | Class(_) ->
                ["{"] @ ["auto " ^ getvar ^ " = " ^ name ^ "(" ^ param ^ ");"] @
                [sigvar ^ "->notify(" ^ getvar ^ ");"] @
                ["}"]
            | _ ->
                ["{"] @
                [(type_to_code_string rt) ^ " " ^ getvar ^ " = " ^ name ^ "(" ^ param ^ ");"] @
                [sigvar ^ "->notify(shared_ptr<" ^ rtstr ^ ">(new " ^ rtstr ^ "(" ^ getvar ^ ")));"] @
                ["}"]
        in
        ret @ [fname] @ [fmstr] @ body

(* generate register wrapper, return string list *)
let handle_fd_register fd refenv =
    match fd with
    | {tret=rt; tfname=name; tformals=fm; tbody=body ;_} ->
        let (getvar, sigty) = match List.rev fm with
            | (var, ty)::tl -> (var, ty)
            | _ -> raise (Failure ("This register function doesn't have param")) in
        let sigvar = name ^ "_sig" in
        let nfm = match List.rev fm with
            | _::tl -> (List.rev tl) @ [(sigvar, Signal(sigty))]
            | _ -> [] in
        let rtstr = type_to_code_string rt in
        let tlist = get_typelist_from_fm nfm in
        let fname = List.fold_left
                    (fun ret type_ -> ret ^ "_" ^ (type_to_func_string type_))
                    name
                    tlist in
        let fmstr = handle_fm nfm refenv in
        let param = cat_string_list_with_comma (List.map (fun (n,_) -> n) fm) in
        let body =
            match sigty with
            | Class x ->
                ["{"] @
                [(type_to_code_string sigty) ^ " " ^ getvar ^ " = " ^ sigvar ^ "->wait();"] @
                [name ^ "(" ^ param ^ ");"] @
                ["}"]
            | _ ->
                ["{"] @
                [(type_to_code_string sigty) ^ " " ^ getvar ^ " = *" ^ sigvar ^ "->wait();"] @
                [name ^ "(" ^ param ^ ");"] @
                ["}"]
            in
        [rtstr] @ [fname] @ [fmstr] @ body

(* take signal name and fly call, return a string list *)
let rec handle_fly_expr signame expr refenv =
    let syncfunc = "detach()" in
    match expr with
    | TFly((fn, texpr_list), st) ->
            let expr_types_list = List.map get_expr_type_info texpr_list in
            let nfn = (List.fold_left (fun ret et -> ret ^ "_" ^ (type_to_func_string et)) fn expr_types_list)
                ^ "_" ^ (type_to_func_string st) in
            let param = [cat_string_list_with_comma (List.fold_left (fun ret ex -> ret@(handle_texpr ex refenv)) [] texpr_list)] in
            let param2 =
            (
                match param with
                | [""] -> []
                | _ -> param @ [","]
            ) in
            ["thread(";nfn;","] @ param2 @ [signame;")." ^ syncfunc]
    | _ -> raise (Failure ("Assigning something to Signal other than TFly"))

(* take one expr and return a string list *)
and handle_texpr expr refenv =
    match expr with
    | TLiteral(value) -> [string_of_int value]
    | TBoolLit(value) -> if value then ["true"] else ["false"]
    | TFloat(value) -> [string_of_float value]
    | TId(str, _) ->
        (* TODO if this is a func passing, we convert it into our self defined obj*)
        begin
        try
            let _ = Hashtbl.find func_binds str
            in let raw_clojure_class_name = gen_clojure_class_name str []
            in ["shared_ptr<" ^ raw_clojure_class_name ^ ">(new " ^ raw_clojure_class_name ^ "())"]
        with
        | _ -> [str]
        end
    | TSet(_) -> [] (* TODO *)
    | TMap(_) -> [] (* TODO *)
    | TArray(_) -> [] (* TODO *)
    | TString(str) -> [str]
    | TBinop((texpr1, op, texpr2), _) ->
        [cat_string_list_with_space
            (["("] @ (handle_texpr texpr1 refenv) @ [op_to_string op] @ (handle_texpr texpr2 refenv) @ [")"])
        ]
    | TUnop((uop, texpr), _) -> [cat_string_list_with_space (["("] @ [uop_to_string uop] @ (handle_texpr texpr refenv) @ [")"])]
    | TCall ((fn, texpr_list), t) ->
        (
        let expr_types = List.map get_expr_type_info texpr_list
        in let if_check_in = match_build_in fn expr_types
        in match if_check_in with
        | Some x ->
            [
            cat_string_list_with_space
            ([fn;"("]@
            [cat_string_list_with_comma (List.fold_left (fun ret ex -> ret@([merge_string_list (handle_texpr ex refenv)])) [] texpr_list)]@
            [")"])
            ]
        (* above are built-in functions *)
        | _ ->
            begin
            try
                let fdecl = Hashtbl.find func_binds fn
                in begin match fdecl with
                | {formals=binds;_} ->
                    let bind_len = List.length binds and expr_len = List.length texpr_list
                    in
                    if bind_len = expr_len then
                    [
                        cat_string_list_with_space
                        ([fn;"("]@
                        [cat_string_list_with_comma (List.fold_left (fun ret ex -> ret@(handle_texpr ex refenv)) [] texpr_list)]@
                        [")"])
                    ]
                    else
                    let raw_clojure_class_name = gen_clojure_class_name fn []
                    in let func_obj =
                        "(shared_ptr<" ^ raw_clojure_class_name ^ ">(new " ^ raw_clojure_class_name ^ "()))"
                    in
                    [
                    cat_string_list_with_space
                    ([func_obj;"->call("]@
                    [cat_string_list_with_comma (List.fold_left (fun ret ex -> ret@(handle_texpr ex refenv)) [] texpr_list)]@
                    [")"])
                    ]
                end
            with | _ ->
                [
                cat_string_list_with_space
                ([fn;"->call("]@
                [cat_string_list_with_comma (List.fold_left (fun ret ex -> ret@(handle_texpr ex refenv)) [] texpr_list)]@
                [")"])
                ]
            end
        )
    | TObjCall ((varname, mfname, texpr_list), ty) ->
        let res = search_key (!refenv) varname
        in begin match res with
        | Some x ->
            begin match (x:typ) with
            | Array _ ->
                let arr_code_gen varname mfname texpr_list =
                    let newfname =
                    begin match mfname with
                    | "get_at" ->
                        "operator[]"
                    | _ ->
                        mfname
                    end
                    in
                    let fn = varname ^ "->" ^ newfname
                    in
                    [
                        cat_string_list_with_space
                        ([fn;"("]@
                        [cat_string_list_with_comma (List.fold_left (fun ret ex -> ret@(handle_texpr ex refenv)) [] texpr_list)]@
                        [")"])
                    ]
                in
                    arr_code_gen varname mfname texpr_list
            | Map _ ->
                let map_code_gen varname mfname texpr_list =
                    let normal_gen fn =
                        [
                        cat_string_list_with_space
                        ([fn;"("]@
                        [cat_string_list_with_comma (List.fold_left (fun ret ex -> ret@(handle_texpr ex refenv)) [] texpr_list)]@
                        [")"])
                        ]
                    in
                    begin match mfname with
                    | "get" ->
                        normal_gen (varname ^ "->operator[]")
                    | "delete" ->
                        let epr = List.hd texpr_list
                        in
                        [varname ^ "->erase("
                            ^ (merge_string_list (handle_texpr epr refenv)) ^ ")"]
                    | "exist" ->
                        let epr = List.hd texpr_list
                        in
                        ["( " ^ varname ^ "->find("
                            ^ (merge_string_list (handle_texpr epr refenv)) ^ ") != " ^ varname ^ "->end() )"]
                    | "size" ->
                        (* change to int*)
                        ["int(" ^ (merge_string_list (normal_gen (varname ^"->size"))) ^ ")"]
                    | "insert" ->
                        begin match texpr_list with
                        | [x;y] ->
                            let key_code = merge_string_list (handle_texpr x refenv)
                            in let value_code = merge_string_list (handle_texpr y refenv)
                            in
                            [varname ^ "->insert(" ^ key_code ^ "," ^ value_code ^ ")"]
                        | _ -> failwith ("not support for insert map")
                        end
                    | _ -> failwith ("not support map function")
                    end
                in
                    map_code_gen varname mfname texpr_list
            | _ ->
                let fn = varname ^ "->" ^ mfname
                in
                [
                    cat_string_list_with_space
                    ([fn;"("]@
                    [cat_string_list_with_comma (List.fold_left (fun ret ex -> ret@(handle_texpr ex refenv)) [] texpr_list)]@
                    [")"])
                ]
            end
        | _ -> failwith ("inner error")
        end
    | TFunc(_) -> [] (* TODO *)
    | TAssign((str, expr), ty) ->
        let res = (search_key (!refenv) str) in
        (
            let type_code = type_to_code_string ty in
            let decl_type_code = if res = None then type_code else "" in
            ignore(update_env !refenv str ty);
            (
                match ty with
                (* deal with signal assignment from fly *)
                | Signal(x) ->
                    (* flying a no return function is not allowed *)
                    ignore(if x = Void then raise (Failure ("Function should return something to signal")));
                    (
                    let type_str =
                        match x with
                        | Class(class_type) -> class_type
                        | Array(sometype) -> "vector<" ^ type_to_code_string sometype ^ ">"
                        | _ -> type_to_code_string x
                    in
                    [decl_type_code ^ " " ^ str ^ " = " ^ type_code ^ "(new Signal<" ^ (type_str) ^ ">());";] @ handle_fly_expr str expr refenv
                    )
                (* normal *)
                | _ -> [decl_type_code ^ " " ^ str ^ " = "] @ handle_texpr expr refenv
            )
        )
    | TListComprehen(_) -> [] (* TODO *)
    | TExec(_) -> [] (* TODO *)
    | TDispatch(_) -> [] (* TODO *)
    | TChangen(containtype, x) ->
        let containname = new_type_to_code_string containtype
        in ["shared_ptr < Chan <" ^ containname ^ "> >(new Chan < " ^ containname ^ " >())"]
    | TChanbinop((x, y), containtype) ->
        (* according to different type wrap or unwrap shared_ptr*)
        begin match containtype with
        | Int | String | Float ->
            let res = search_key (!refenv) y
            in begin match res with
            | Some (Chan (a)) -> (* push*)
                let checkx = search_key (!refenv) x
                in if checkx = None then
                [(type_to_code_string containtype) ^ " " ^ x ^ "=*(" ^ y ^ "->wait_and_pop())"]
                else
                [x ^ "=*(" ^ y ^ "->wait_and_pop())"]
            | Some (a) ->
                [x ^ "->push(make_shared<" ^ (type_to_code_string a) ^ ">(" ^ y ^ "))"]
            | None -> failwith ("conflict binop")
            end
        | Class (c) ->
            let res = search_key (!refenv) y
            in begin match res with
            | Some (Chan (a)) -> (* push*)
                let checkx = search_key (!refenv) x
                in if checkx = None then
                [(type_to_code_string containtype) ^ " " ^ x ^ "=" ^ y ^ "->wait_and_pop()"]
                else
                [x ^ "=" ^ y ^ "->wait_and_pop()"]
            | Some (a) ->
                [x ^ "->push(" ^ y ^ ")"]
            | None -> failwith ("conflict binop")
            end
        end
    | TChanunop(x, containtype) ->
        [x ^ "->wait_and_pop()"] (* TODO *)
    | TFly((fn, texpr_list),st) ->
        let type_str =
        match st with
            | Signal(Class(tstr)) -> tstr
            | Signal(Array(sometype)) -> "vector<" ^ type_to_code_string sometype ^ ">"
            | Signal(t) -> type_to_code_string t
            | _ -> raise (Failure ("Fly type error"))
        in
        handle_fly_expr ("shared_ptr <Signal<" ^ type_str ^ ">> (new Signal<" ^ type_str ^">())")
            (TFly((fn, texpr_list),st)) refenv
    | TRegister ((sign, fn, texpr_list), t) ->
        (* must change the function name, appending type *)
        handle_fly_expr sign (TFly((fn, texpr_list), Signal(t))) refenv
    | TFlyo(_) -> [] (* TODO *)
    | TNull(_) -> [] (* TODO *)
    | TObjGen (typename, _) ->
        begin
        match typename with
        | Class x -> ["shared_ptr <" ^ x ^ ">(new " ^ x ^ "())"]
        | Array _ | Map _ -> [type_to_code_string typename ^ "(new " ^ (new_type_to_code_string typename) ^ "())"]
        | _ -> failwith ("not support for other TObjgen now")
        end
    | TObjid((objname, objid), _) -> [objname ^ "->" ^ objid]
    | TMAssign ((varname, mname, expr), ty)->
        begin
            match ty with
            (* deal with signal assignment *)
            | Signal(x) ->
                let tycode = type_to_code_string x
                in let str = varname ^ "->" ^ mname
                in  [str ^ "=shared_ptr <Signal<" ^ tycode ^ ">>(new Signal<" ^ tycode ^ ">());";] @
                handle_fly_expr str expr refenv
            (* normal *)
            | x ->
                let str = varname ^ "->" ^ mname
                in [str ^ " = "] @ handle_texpr expr refenv
        end


(* take one tstmt and return a string list *)
let rec handle_tstmt fkey tstmt_ refenv =
    let refnewenv = ref (append_new_level !refenv) in
    match tstmt_ with
    | TBlock(tstmtlist) ->
        ["{"] @
        (List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt fkey tstmt_ refnewenv)) [] tstmtlist)
        @ ["}"]
    | TExpr(expr) -> [cat_string_list_with_space ((handle_texpr expr refenv) @ [";"])]
    | TReturn(expr) -> [cat_string_list_with_space (["return"] @ (handle_texpr expr refenv) @ [";"])]
    | TIf(texp_, tstmtl1, tstmtl2) ->
        [cat_string_list_with_space (["if ("] @ (handle_texpr texp_ refnewenv) @ [")"])] @
        ["{"] @ ((List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt fkey tstmt_ refnewenv)) [] tstmtl1)) @ ["}"] @
        ["else"] @
        ["{"] @ ((List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt fkey tstmt_ refnewenv)) [] tstmtl2)) @ ["}"]
    | TFor(exp1, exp2, exp3, tstmtlist) ->
        let f1 = handle_texpr exp1 refnewenv in
        let f2 = handle_texpr exp2 refnewenv in
        let f3 = handle_texpr exp3 refnewenv in
        let tstmtstr = (List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt fkey tstmt_ refnewenv)) [] tstmtlist) in
        [cat_string_list_with_space (["for ("] @ f1 @ [";"] @ f2 @ [";"] @ f3 @ [")"])] @
        ["{"] @ tstmtstr @ ["}"]
    | TForeach (varname, t_base_expr, tstmt_list) ->
        (*array is changed to TFor, now TForeach is only for the need of map*)
        (*deal with foreach map which can not be done by the change of the for*)
        let base_expr_code = handle_texpr t_base_expr refenv
        in
        let for_code = "for (auto itr = (" ^ (merge_string_list base_expr_code) ^ ")->begin(); itr != ("
            ^ (merge_string_list base_expr_code) ^ ")->end(); ++itr){"
        in let var_type = begin match get_expr_type_info t_base_expr with
            | Map (x, y) -> x
            | _ -> failwith ("infer error for tforeach map")
            end
        in let assign_var_code = (type_to_code_string var_type) ^ " " ^ varname ^ "=itr->first;"
        in ignore(update_env !refnewenv varname var_type); (*do a env update*)
        let tstmtstr =  (List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt fkey tstmt_ refnewenv)) [] tstmt_list)
        in
        let lock = "std::unique_lock<std::recursive_mutex> lk(" ^ merge_string_list base_expr_code ^ "->m_mutex);"
        in ["{";lock;for_code;assign_var_code] @ tstmtstr @ ["}"] @ ["}"]
    | TWhile(expr_, tstmtlist) ->
        [cat_string_list_with_space (["while ("] @ (handle_texpr expr_ refnewenv) @ [")"])] @
        ["{"] @
        (List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt fkey tstmt_ refnewenv)) [] tstmtlist) @
        ["}"]

(* take tstmt list and return string list *)
(*对stmt list 产生code*)
let handle_body fkey body refenv =
    let refnewenv = ref (append_new_level !refenv) in
    let body_code = List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt fkey tstmt_ refnewenv)) [] body in
    ["{"] @ body_code @ ["}"]

(* return string list *)
(* take a function key, declaration and generate the string list *)
let handle_fdecl fkey fd refenv =
    let refnewenv = ref (append_new_level !refenv) in
    match fd with
    | {tret=rt; tfname=name; tformals=fm; tbody=body ;_} ->
        if name = ""
        then []
        else
            let fmstr = (handle_fm fm refnewenv) in
            let bodystr = (handle_body fkey body refnewenv) in
            [ cat_string_list_with_space [(type_to_code_string rt);name;fmstr]] @ bodystr

let code_header = ["
    #include <sstream>
    #include <iostream>
    #include <string>
    #include <string.h>     /* for memset() */
    #include <thread>
    #include <vector>
    #include <map>
    #include <mutex>
    #include <condition_variable>
    #include <queue>
    #include <sys/socket.h>
    #include <arpa/inet.h>  /* for sockaddr_in and inet_ntoa() */
    #include <stdlib.h>     /* for atoi() and exit() */
    #include <unistd.h>     /* for close() */
    #include <signal.h>     /* for signal() */

    using namespace std;
"]

(* take a texp and return function key list *)
let rec texp_helper texp_ =
    match texp_ with
    | TBinop ((texpr1, _, texpr2), _) -> (texp_helper texpr1) @ (texp_helper texpr2)
    | TUnop ((_, texpr_), _) -> texp_helper texpr_
    | TCall ((fn, texprlist), _) ->
        (
        match fn with
        | "print" -> []
        (* above are built-in functions *)
        | _ ->
            let expr_types_list = List.map get_expr_type_info texprlist in
            let hash_key = gen_hash_key fn expr_types_list in
            [hash_key] @ (List.fold_left (fun ret exp_ -> ret @ (texp_helper exp_)) [] texprlist)
        )
    | TFly ((fn, texprlist), Signal(t)) ->
        ignore(
            let expr_types_list = List.map get_expr_type_info texprlist in
            let hash_key = gen_hash_key fn expr_types_list in
            add_hash signal_funcs hash_key ""
        );
        texp_helper (TCall((fn, texprlist), t))
    | TRegister ((sign, fn, texpl), t) ->
        let expr_types_list = List.map get_expr_type_info texpl in
        (* register texpl will miss the last t, so append it*)
        let hash_key = gen_hash_key fn (expr_types_list @ [t]) in
        ignore(add_hash register_funcs hash_key "");
        [hash_key]
    (* TObjCall of (string * string * texpr list) * typ TODO*)
    | TObjCall (_) -> []
    (* TFunc of (string list * texpr) * typ *) (* lambda TODO*)
    | TFunc (_) -> []
    (* TAssign of (string * texpr) * typ *)
    | TAssign ((_, e_), _) -> texp_helper e_
    (* TListComprehen of (texpr * string * texpr) * typ (*can iterate a tuple?*) TODO*)
    | TListComprehen (_) -> []
    (* TExec of string * typ TODO*)
    | TExec (_) -> []
    (* TDispatch of (string * texpr list * string * string) * typ TODO *)
    | TDispatch (_) -> []
    (* TChan of texpr * typ TODO *)
    | TChangen(_) -> []
    (* TChanunop of string * typ TODO *)
    | TChanunop (_) -> []
    (* TChanbinop of (string * string) * typ TODO *)
    | TChanbinop (_) -> []
    (* | TFlyo of (string * string * texpr list) * typ TODO *)
    | TFlyo (_) -> []
    | _ -> []

(* take a tstmt and return function key list *)
let rec tstmt_helper tstmt_ =
    match tstmt_ with
    | TBlock(tstmtlist) -> List.fold_left (fun ret tstmt_ -> ret @ (tstmt_helper tstmt_)) [] tstmtlist
    | TExpr(texpr_) -> texp_helper texpr_
    | TReturn(texpr_) -> texp_helper texpr_
    | TIf(texpr_, tstmtlist_a, tstmtlist_b) ->
        (texp_helper texpr_) @
        (List.fold_left (fun ret tstmt_ -> ret @ (tstmt_helper tstmt_)) [] (tstmtlist_a @ tstmtlist_b))
    | TFor(ex1, ex2, ex3, tstmtlist) ->
        (texp_helper ex1) @ (texp_helper ex2) @ (texp_helper ex3) @
        (List.fold_left (fun ret tstmt_ -> ret @ (tstmt_helper tstmt_)) [] tstmtlist)
    | TForeach(_, texpr_, tstmtlist) ->
        (texp_helper texpr_) @ (List.fold_left (fun ret tstmt_ -> ret @ (tstmt_helper tstmt_)) [] tstmtlist)
    | TWhile(texpr_, tstmtlist) ->
        (texp_helper texpr_) @ (List.fold_left (fun ret tstmt_ -> ret @ (tstmt_helper tstmt_)) [] tstmtlist)


(* take a function key and return string list, which are the code *)
let rec dfs ht fkey refenv =
    let hash_value = find_hash fundone fkey in
    match hash_value with
    | None ->
        let sfd = find_hash ht fkey in
        (
            match sfd with
            | None -> []
            | Some (fd) ->
                ignore(Hashtbl.add fundone fkey "dummy");
                (
                match fd with
                | {tbody=body; _} ->
                    (*get all t_func_decl needed*)
                    let fklist = List.fold_left (fun ret tstmt_ -> ret @ (tstmt_helper tstmt_)) [] body in
                    (List.fold_left (fun ret key_ -> ret @ (dfs ht key_ refenv)) [] fklist) @ (handle_fdecl fkey fd refenv)
                )
        )
    | _ -> []

(*
let ht_left ht =
    Hashtbl.fold
    (fun k v ret ->
        let sfd = find_hash fundone k in
        match sfd with
        | None ->
            ignore(Hashtbl.add fundone k "dummy");
            (
                match v with
                | {tfname=name; _} -> print_string name
            );
            ret @ [v]
        | _ -> ret
    ) ht []
*)

let gen_rest ht refenv =
    let fcode = Hashtbl.fold
        (
            fun k v code ->
            match (find_hash fundone k) with
            | None ->
                ignore(add_hash fundone k "");
                code @ (handle_fdecl k v refenv)
            | _ ->
            code
        ) ht [] in
    fcode

(* generate signal wrappers *)
let gen_sig_wrapper ht =
    let g_env = init_level_env() in
    let sig_funcs = Hashtbl.fold
    (
        fun k v ret ->
        match find_hash fundone k with
        | None ->
            ignore(add_hash fundone k "");
            (
                match find_hash ht k with
                | Some(fd) -> ret @ (handle_fd_fly fd (ref g_env))
                | None -> raise (Failure (k ^ " not in fht"))
            )
        | _ -> ret
    ) signal_funcs [] in
    let regi_funcs = Hashtbl.fold
    (
        fun k v ret ->
        match find_hash fundone k with
        | None ->
            ignore(add_hash fundone k "");
            (
                match find_hash ht k with
                | Some(fd) -> ret @ handle_fd_register fd (ref g_env)
                | None -> raise (Failure (k ^ " not in fht"))
            )
        | _ -> ret
    ) register_funcs [] in
    sig_funcs @ regi_funcs

let handle_func_forward fd refenv =
    let refnewenv = ref (append_new_level !refenv) in
    match fd with
    | {tret=rt; tfname=name; tformals=fm;_} ->
        if name = ""
        then []
        else
            let fmstr = (handle_fm fm refnewenv) in
            [ cat_string_list_with_space [(type_to_code_string rt);name;fmstr;] ^ ";"]

let gen_forward ht refenv =
    Hashtbl.fold (fun k v code -> code @ (handle_func_forward v refenv)) ht []

let gen_sig_wrapper_forward ht refenv = []

(* take ht and return string list, which is code *)
let build_func_from_ht ht =
    let g_env = init_level_env() in
    let refenv = (ref g_env) in
    let res = dfs ht "main" refenv in
    let res2 = gen_rest ht refenv in
    let forward = gen_forward ht refenv in
    ignore(clean_up_hash fundone);
    let sig_wrapper_code = gen_sig_wrapper ht in
    let sig_wrapper_forward = gen_sig_wrapper_forward ht refenv in
    (forward @ sig_wrapper_forward, res2 @ sig_wrapper_code @ res)

(* take t_class_decl and return string list (code) of the class referrence *)
let handle_class_refer tcdecl = match tcdecl with
    | {tcname=cname;member_binds=binds;t_func_decls=tfdecls}->
        let class_header = "class " ^ cname ^ " {\npublic:\n" (*all public*)
        in let var_defs  =
            List.map (fun (varname, thistype) ->
                (type_to_code_string thistype) ^ " " ^ varname ^ ";"
            ) binds
        in let refer_map tfdecl = begin match tfdecl with
            | {tfname=fname;tformals=bind_list;tret=rtype;_} ->
            let var_refs = List.map (fun (varname, thistype) ->
                (type_to_code_string thistype) ^ " " ^ varname ^ ""
            ) bind_list
            in let fstr = list_join var_refs ","
            in
                (type_to_code_string rtype) ^ " " ^ fname ^ "(" ^ fstr ^ ");"
            end
        in let func_refers =
            List.map refer_map tfdecls
        in let tab_var_defs = tablize var_defs
        in let tab_func_refers = tablize func_refers
        in let end_lines = "};"
        in (*concate with \n*)
            let total = List.concat [[class_header];tab_var_defs;tab_func_refers;[end_lines]]
        in [List.fold_left (fun res item -> res ^ item ^ "\n") "" total]


(* take t_class_decl and return string list (code) of the class definition *)
let handle_class_def tcdecl = match tcdecl with
    | {tcname=cname;member_binds=binds;t_func_decls=tfdecls}->
        let def_map tfdecl = begin match tfdecl with
            | {tfname=fname;tformals=bind_list;tret=rtype;_} ->
            let var_refs = List.map (fun (varname, thistype) ->
                (type_to_code_string thistype) ^ " " ^ varname ^ ""
            ) bind_list
            in let fstr = list_join var_refs ","
            in
                (type_to_code_string rtype) ^ " " ^ cname ^ "::" ^fname ^ "(" ^ fstr ^ ")"
            end
        in
        let gen_body tfdecl = begin match tfdecl with
            | {tfname=fname;tformals=bind_list;tret=rtype;tbody=stmt_list;_} ->
                (*I don't know what is fkey*)
                (*create a new env with member variables and parameters*)
                let new_env = List.fold_left (
                    fun thisenv (varname, thistype) ->
                    update_env thisenv varname thistype
                    ) (init_level_env()) binds
                in let new_env = List.fold_left (fun thisenv (varname, thistype) ->
                    update_env thisenv varname thistype) new_env bind_list
                in let new_env_ref = ref(new_env)
                in handle_body "" stmt_list new_env_ref
                end
        in let gen_all tfdecl =
            let func_def = def_map tfdecl
            in let body = gen_body tfdecl
            in [func_def] @ body
        in List.concat (List.map gen_all tfdecls)


let handle_class_forward tcdecl = match tcdecl with
     | {tcname=cname;member_binds=binds;t_func_decls=tfdecls}->
        ["class " ^ cname ^ ";\n"]

(* take ht of string->class_decl and return string list *)
let build_class_from_ht cht =
    (*first generate forward decl*)
    let code_fw = Hashtbl.fold (fun k v code -> code @ (handle_class_forward v)) cht []
    in let code_v2 = Hashtbl.fold (fun k v code -> code @ (handle_class_refer v)) cht code_fw
    in (code_fw, Hashtbl.fold (fun k v code -> code @ (handle_class_def v)) cht code_v2)

let build_clojure_class clojure_classes =
    (*first generate forward decl*)
    let code_v1 = List.fold_left (fun code (k, v) -> code @ (handle_class_forward v)) [] clojure_classes
    in let code_v2 = List.fold_left (fun code (k, v) -> code @ (handle_class_refer v)) [] clojure_classes
    in let code_v3 = List.fold_left (fun code (k,v) -> code @ (handle_class_def v)) [] clojure_classes
    in (code_v1, code_v2, code_v3)

let codegen fht cht clojure_calls func_binds t_func_binds =
    let clojure_classes = gen_clojure_classes clojure_calls func_binds t_func_binds in
    let (clojure_class_forwards, clojure_class_refers, clojure_class_defs)= build_clojure_class clojure_classes in
    let (forward_codelist, func_codelist) = build_func_from_ht fht in
    let (class_fw, class_def) = build_class_from_ht cht in
    let buffer = code_header @ build_in_code @ build_in_class_code @ clojure_class_forwards @ class_fw @ forward_codelist @ clojure_class_refers @ clojure_class_defs @ class_def @ func_codelist in
    List.fold_left (fun ret ele -> ret ^ ele ^ "\n") "" buffer
