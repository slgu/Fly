open Ast
open Sast
open Env

type sigbind = {
    vn : string;
    vt : typ;
}

type regibind = {
    vn : string; (* sig var name *)
    vt : typ;    (* Signal(t) *)
    rn : string; (* var name to wait for value *)
}

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

let rec type_to_code_string = function
    | Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
    | String -> "string"
    | Float -> "float"
    | Signal(x) -> "shared_ptr <Signal<" ^ (type_to_code_string x) ^ ">>"
    | _ -> raise (Failure ("type_to_code_string not yet support this type"))

(* take a string list and concatenate them with interleaving space into a single string *)
let rec cat_string_list_with_space sl =
    match sl with
    | [] -> ""
    | hd::tl -> hd ^ " " ^ (cat_string_list_with_space tl)

(* take a string list and concatenate them with interleaving comma into a single string *)
let rec cat_string_list_with_comma sl =
    match sl with
    | [] -> ""
    | hd::tl ->
        let tmp = hd ^ "," ^ (cat_string_list_with_space tl) in
        let len = (String.length tmp) in
        if len > 0 then (String.sub tmp 0 (len-1)) else tmp

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

(* take signal name and fly call, return a string list *)
let rec handle_fly_expr signame expr refenv =
    match expr with
    | TFly((fn, texpr_list),_) ->
            let param = [cat_string_list_with_comma (List.fold_left (fun ret ex -> ret@(handle_texpr ex refenv)) [] texpr_list)] in
            let param2 =
            (
                match param with
                | [""] -> []
                | _ -> param @ [","]
            ) in
            ["thread(";fn;","] @ param2 @ [signame;").detach()"]
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
    | TCall ((fn, texpr_list), _) ->
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
    | TObjCall(_) -> [] (* TODO *)
    | TFunc(_) -> [] (* TODO *)
    | TAssign((str, expr), ty) ->
        let res = search_key (!refenv) str in
        (
            match res with
            | None -> (* variable is first seen here *)
                ignore(update_env !refenv str ty);
                (
                    match ty with
                    (* deal with signal assignment *)
                    | Signal(x) ->
                        [(type_to_code_string ty) ^ " " ^ str ^ "(new Signal<" ^ (type_to_code_string x) ^ ">());";] @
                        handle_fly_expr str expr refenv
                    (* normal *)
                    | _ -> [(type_to_code_string ty) ^ " " ^ str ^ " = "] @ handle_texpr expr refenv
                )
            | _ -> (* variable has been declared before *)
                (
                    match ty with
                    (* deal with signal assignment *)
                    | Signal(_) -> raise (Failure ("Signal re-assigned? " ^ str))
                    (* normal *)
                    | _ -> [str ^ " = "] @ handle_texpr expr refenv
                )
        )
    | TListComprehen(_) -> [] (* TODO *)
    | TExec(_) -> [] (* TODO *)
    | TDispatch(_) -> [] (* TODO *)
    | TChan(_) -> [] (* TODO *)
    | TChanbinop(_) -> [] (* TODO *)
    | TChanunop(_) -> [] (* TODO *)
    | TFly((fn, texpr_list),t) ->
        handle_fly_expr "shared_ptr <Signal<string>> (new Signal<string>())" (TFly((fn, texpr_list),t)) refenv
    | TRegister ((sign, fn, texpr_list), t) -> 
        handle_fly_expr sign (TFly((fn, []),t)) refenv
    | TFlyo(_) -> [] (* TODO *)
    | TNull(_) -> [] (* TODO *)

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
        (List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt fkey tstmt_ refnewenv)) [] tstmtlist)

(* take tstmt list and return string list *)
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
                (* a register function, make the fm = [Signal(t)] *)
                (* also uprate the variable name in register_funcs hash *)
                | Some({vn=_name;vt=_type}) -> 
                    ignore (
                        match fm with
                        | [(_rn,_)] -> add_hash register_funcs fkey {vn=_name; vt=_type; rn=_rn}
                        | _ -> raise (Failure ("registered function can only have one param"))
                    );
                    [(_name, _type)]
            )
            (* a fly function, add signal to the end of fm *)
            | Some({vn=_name;vt=_type}) -> fm @ [(_name, _type)]
        ) in
        let fmstr = (handle_fm nfm refnewenv) in
        let bodystr = (handle_body fkey body refnewenv) in
        [ cat_string_list_with_space [(type_to_code_string rt);name;fmstr]] @ bodystr

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
            let hash_key = fn ^
            (List.fold_left (fun str item -> str ^ "@" ^ item) "" (List.map type_to_string expr_types_list)) in
            [hash_key] @ (List.fold_left (fun ret exp_ -> ret @ (texp_helper exp_)) [] texprlist)
        )
    | TFly ((fn, texpl), t) -> 
        ignore(
            let typel = List.map get_expr_type_info texpl in
            let hash_key = fn ^
                (List.fold_left 
                (fun str item -> str ^ "@" ^ item) "" (List.map type_to_string typel)) in
            add_hash signal_funcs hash_key {vn=fn ^ "_signal"; vt=t}
        );
        texp_helper (TCall((fn, texpl), t))
    | TRegister ((sign, fn, texpl), t) -> 
        (* here we let fname to be fn@t, but later we have to change it to fn@signal:t *)
        let hash_key = fn ^
            (List.fold_left 
            (fun str item -> str ^ "@" ^ item) "" (List.map type_to_string [t])) in
        ignore(add_hash register_funcs hash_key {vn=sign; vt=Signal(t); rn="known"});
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

(* take ht and return string list, which is code *)
let build_list_from_ht ht =
    let g_env = init_level_env() in
    dfs ht "main" (ref g_env)

let codegen ht =
    let codelist = build_list_from_ht ht in
    let buffer = code_header @ code_predefined_class @ codelist in
    List.fold_left (fun ret ele -> ret ^ ele ^ "\n") "" buffer
