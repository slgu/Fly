open Ast
open Sast
open Env
open Util

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

let gen_clojure_class funcname type_list =
    funcname ^ (List.fold_left (fun res item -> res ^ "_" ^ (type_to_string item)) "" type_list)

(* mapping from function key to sigbind *)
let (signal_funcs : (string, sigbind) Hashtbl.t) = Hashtbl.create 16

(* mapping from function key to sigbind *)
let (register_funcs : (string, regibind) Hashtbl.t) = Hashtbl.create 16

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
    | _ -> raise (Failure ("type_to_func_string not yet support this type"))

let rec type_to_code_string = function
    | Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
    | String -> "string"
    | Float -> "float"
    | Signal(x) -> "shared_ptr <Signal<" ^ (type_to_code_string x) ^ ">>"
    | Class x -> "shared_ptr <" ^ x ^ ">"
    | Func (x, type_list) -> "shared_ptr <" ^ (gen_clojure_class x type_list) ^ ">"
    | _ -> raise (Failure ("type_to_code_string not yet support this type"))

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
            if rt = Void then
                ["{"] @
                [name ^ "(" ^ param ^ ");"] @
                ["}"]
            else
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
        let body = ["{"] @
            [(type_to_code_string sigty) ^ " " ^ getvar ^ " = *" ^ sigvar ^ "->wait();"] @
            [name ^ "(" ^ param ^ ");"] @
            ["}"] in
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
    | TId(str, _) -> [str]
    | TSet(_) -> [] (* TODO *)
    | TMap(_) -> [] (* TODO *)
    | TArray(_) -> [] (* TODO *)
    | TString(str) -> [str]
    | TBinop((texpr1, op, texpr2), _) ->
        ["("] @ (handle_texpr texpr1 refenv) @ [op_to_string op] @ (handle_texpr texpr2 refenv) @ [")"]
    | TUnop(_) -> [] (* TODO *)
    | TCall ((fn, texpr_list), t) ->
        (
        match fn with
        | "print" ->
            [
                cat_string_list_with_space
                (["cout"]@(List.fold_left (fun ret ex -> ret@["<<"]@(handle_texpr ex refenv)) [] texpr_list)@["<<endl"])
            ]
        (* above are built-in functions *)
        | _ ->
            [
                cat_string_list_with_space
                ([fn;"("]@
                [cat_string_list_with_comma (List.fold_left (fun ret ex -> ret@(handle_texpr ex refenv)) [] texpr_list)]@
                [")"])
            ]
        )
    | TObjCall ((varname, mfname, texpr_list), ty) ->
        let fn = varname ^ "->" ^ mfname
        in
        [
            cat_string_list_with_space
            ([fn;"("]@
            [cat_string_list_with_comma (List.fold_left (fun ret ex -> ret@(handle_texpr ex refenv)) [] texpr_list)]@
            [")"])
        ]

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
                    [decl_type_code ^ " " ^ str ^ " = " ^ type_code ^ "(new Signal<" ^ (type_to_code_string x) ^ ">());";] @ handle_fly_expr str expr refenv
                (* normal *)
                | _ -> [decl_type_code ^ " " ^ str ^ " = "] @ handle_texpr expr refenv
            )
        )
    | TListComprehen(_) -> [] (* TODO *)
    | TExec(_) -> [] (* TODO *)
    | TDispatch(_) -> [] (* TODO *)
    | TChan(_) -> [] (* TODO *)
    | TChanbinop(_) -> [] (* TODO *)
    | TChanunop(_) -> [] (* TODO *)
    | TFly((fn, texpr_list),st) ->
        let type_str =
        match st with
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
    | TObjGen (x, thistype) ->
        ["shared_ptr <" ^ x ^ ">(new " ^ x ^ "())"] (* TODO *)
    | TObjid(_) -> [] (* TODO *)
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
    | TReturn(expr) ->
        (
            match (find_hash signal_funcs fkey) with
            (* a normal function *)
            | None -> [cat_string_list_with_space (["return"] @ (handle_texpr expr refenv) @ [";"])]
            (* a fly function, add signal codes *)
            | Some({vn=_name; vt=_type}) ->
            (
                match _type with
                | Signal(_type2) ->
                    let tstr = type_to_code_string _type2 in
                    (
                        match expr with
                        | TId(str, _) ->
                            [_name ^ "->notify(shared_ptr<" ^ tstr ^ ">(new " ^ tstr^ "(" ^ str ^ ")));" ] @
                            [cat_string_list_with_space ["return"; str; ";"]]
                        | _ -> raise (Failure ("Should return Id"))
                    )
                | _ -> raise (Failure ("Should be Signal(t)"))
            )
        )
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
    | TForeach(_) -> [] (* TODO *)
    | TWhile(expr_, tstmtlist) ->
        [cat_string_list_with_space (["while ("] @ (handle_texpr expr_ refnewenv) @ [")"])] @
        ["{"] @
        (List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt fkey tstmt_ refnewenv)) [] tstmtlist) @
        ["}"]

(* take tstmt list and return string list *)
(*对stmt list 产生code*)
let handle_body fkey body refenv =
    let refnewenv = ref (append_new_level !refenv) in
    let pre_code =
    (
        match (find_hash register_funcs fkey) with
        | Some({vn=_name;vt=Signal(_t);rn=_rn}) -> (* register function: get value from signal first *)
            ignore(update_env (!refnewenv) _rn _t);
            [(type_to_code_string _t) ^ " " ^ _rn ^ " = *" ^ _name ^ "->wait();"]
        | _ -> [] (* normal function *)
    ) in
    let body_code = List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt fkey tstmt_ refnewenv)) [] body in
    ["{"] @ pre_code @ body_code @ ["}"]

(* return string list *)
(* take a function key, declaration and generate the string list *)
let handle_fdecl fkey fd refenv =
    let refnewenv = ref (append_new_level !refenv) in
    match fd with
    | {tret=rt; tfname=name; tformals=fm; tbody=body ;_} ->
        let nfm =
        (
            match (find_hash signal_funcs fkey) with
            | None ->
            (
                match (find_hash register_funcs fkey) with
                | None -> fm (* normal function *)
                (* a register function, make the last of fm = Signal(t) *)
                (* also update the variable name in register_funcs hash *)
                | Some({vn=_name;vt=_type}) ->
                    let tmpfm = List.rev fm in
                    (
                    match tmpfm with
                    | (_rn,_)::tl ->
                        ignore(add_hash register_funcs fkey {vn=_name; vt=_type; rn=_rn});
                        List.rev ([(_name, _type)] @ tl)
                    | _ -> raise (Failure ("Register function not accepting param? " ^ name))
                    )
            )
            (* a fly function, add signal to the end of fm *)
            | Some({vn=_name;vt=_type}) -> fm @ [(_name, _type)]
        ) in
        let nfname = (* must change name for recv/fly func, appending type *)
        (
            let type_list = (get_typelist_from_fm nfm) in
            match (find_hash register_funcs fkey) with
            (* a register function, append type to name *)
            | Some({vn=_;vt=r_type}) ->
                (   List.fold_left
                    (fun ret type_ -> ret ^ "_" ^ (type_to_func_string type_))
                    name
                    type_list
                )
            | None -> (
                match (find_hash signal_funcs fkey) with
                (* a fly function, append type to name *)
                | Some({vn=_;vt=s_type}) ->
                    (   List.fold_left
                        (fun ret type_ -> ret ^ "_" ^ (type_to_func_string type_))
                        name
                        type_list
                    )
                (* normal function *)
                | None -> name
            )
        ) in
        let fmstr = (handle_fm nfm refnewenv) in
        let bodystr = (handle_body fkey body refnewenv) in
        [ cat_string_list_with_space [(type_to_code_string rt);nfname;fmstr]] @ bodystr

(*class code generation*)
let class_code_gen cdecl =
    None

let code_header = [
    "#include <iostream>";
    "#include <string>";
    "#include <thread>";
    "#include <mutex>";
    "#include <condition_variable>";
    "#include <queue>";
    "using namespace std;"]

let code_predefined_class = [
    "template <typename T> class Signal {";
    "public:";
    "   condition_variable data_cond;";
    "   mutex data_mutex;";
    "   queue <std::shared_ptr <T>> data_queue;";
    "   shared_ptr <T> wait() {";
    "       std::unique_lock<std::mutex> lk(data_mutex);";
    "       data_cond.wait(lk, [this]{return !this->data_queue.empty();});";
    "       lk.unlock();";
    "       auto result = data_queue.front();";
    "       data_queue.pop();";
    "       return result;";
    "   }";
    "   void notify(std::shared_ptr <T> res) {";
    "       std::lock_guard<std::mutex> lk(data_mutex);";
    "       data_queue.push(res);";
    "       data_cond.notify_one();";
    "   }";
    "};"]

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
            add_hash signal_funcs hash_key {vn=fn ^ "_signal"; vt=Signal(t)}
        );
        texp_helper (TCall((fn, texprlist), t))
    | TRegister ((sign, fn, texpl), t) ->
        let expr_types_list = List.map get_expr_type_info texpl in
        (* register texpl will miss the last t, so append it*)
        let hash_key = gen_hash_key fn (expr_types_list @ [t]) in
        ignore(add_hash register_funcs hash_key {vn=fn ^ "_signal"; vt=Signal(t); rn="known"});
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
    | TChan (_) -> []
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
            | None -> raise (Failure ("Function not defined " ^ fkey))
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

(* from signal_funcs and register_funcs, get all the fk *)
let get_func_overload_klist ht =
    let kl1 = Hashtbl.fold
    (
        fun k v ret ->
        match find_hash ht k with
        | Some(fd_) -> ret @ [{fkey=k;fd=fd_}]
        | _ -> raise (Failure (""))
    )
    signal_funcs [] in
    let kl2 = Hashtbl.fold
    (
        fun k v ret ->
        match find_hash ht k with
        | Some(fd_) -> ret @ [{fkey=k;fd=fd_}]
        | _ -> raise (Failure (""))
    )
    register_funcs [] in
    kl1 @ kl2

(* take ht and return string list, which is code *)
let build_func_from_ht ht =
    let g_env = init_level_env() in
    let res = dfs ht "main" (ref g_env) in
    ignore(clean_up_hash fundone);
    let func_overload_klist = get_func_overload_klist ht in
    ignore(clean_up_hash signal_funcs);
    ignore(clean_up_hash register_funcs);
    let overload_code = List.fold_left
        (
            fun ret {fkey=fk_;fd=fd_} ->
            match find_hash fundone fk_ with
            | None ->
                ignore(add_hash fundone fk_ "");
                ret @ handle_fdecl fk_ fd_ (ref g_env)
            | _ -> ret
        ) [] func_overload_klist in
    (*print_endline (List.fold_left (fun ret ele -> ret ^ ele ^ "\n") "" res);*)
    overload_code @ res


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
    let code_v1 = Hashtbl.fold (fun k v code -> code @ (handle_class_forward v)) cht []
    in let code_v2 = Hashtbl.fold (fun k v code -> code @ (handle_class_refer v)) cht code_v1
    in Hashtbl.fold (fun k v code -> code @ (handle_class_def v)) cht code_v2

let codegen fht cht =
    let func_codelist = build_func_from_ht fht in
    let class_codelist = build_class_from_ht cht in
    let buffer = code_header @ code_predefined_class @ class_codelist @ func_codelist in
    List.fold_left (fun ret ele -> ret ^ ele ^ "\n") "" buffer
