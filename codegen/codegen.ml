open Ast
open Sast
open Env

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
            ret ^ " " ^ (type_to_string type_) ^ " " ^ str_ ^ ",") "" formals in
    let len = (String.length fstr) in
    let trimed = if len > 0 then (String.sub fstr 0 (len-1)) else fstr in
    "(" ^ trimed ^ ")"

(* take one expr and return a string list *)
let rec handle_texpr expr refenv =
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
        | None -> 
            ignore(update_env !refenv str ty);
            [(type_to_string ty) ^ " " ^ str ^ " = "] @ handle_texpr expr refenv
        | _ -> [str ^ " = "] @ handle_texpr expr refenv
        )
    | TListComprehen(_) -> [] (* TODO *)
    | TExec(_) -> [] (* TODO *)
    | TDispatch(_) -> [] (* TODO *)
    | TRegister(_) -> [] (* TODO *)
    | TChan(_) -> [] (* TODO *)
    | TChanbinop(_) -> [] (* TODO *)
    | TChanunop(_) -> [] (* TODO *)
    | TFly(_) -> [] (* TODO *)
    | TFlyo(_) -> [] (* TODO *)
    | TNull(_) -> [] (* TODO *)

(* take one tstmt and return a string list *)
let rec handle_tstmt tstmt_ refenv =
    let refnewenv = ref (append_new_level !refenv) in
    match tstmt_ with
    | TBlock(tstmtlist) ->
        ["{"] @
        (List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt tstmt_ refnewenv)) [] tstmtlist)
        @ ["}"]
    | TExpr(expr) -> [cat_string_list_with_space ((handle_texpr expr refenv) @ [";"])]
    | TReturn(expr) -> [cat_string_list_with_space (["return"] @ (handle_texpr expr refenv) @ [";"])]
    | TIf(texp_, tstmtl1, tstmtl2) ->
        [cat_string_list_with_space (["if ("] @ (handle_texpr texp_ refnewenv) @ [")"])] @
        ["{"] @ ((List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt tstmt_ refnewenv)) [] tstmtl1)) @ ["}"] @
        ["else"] @
        ["{"] @ ((List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt tstmt_ refnewenv)) [] tstmtl2)) @ ["}"]
    | TFor(exp1, exp2, exp3, tstmtlist) ->
        let f1 = handle_texpr exp1 refnewenv in
        let f2 = handle_texpr exp2 refnewenv in
        let f3 = handle_texpr exp3 refnewenv in
        let tstmtstr = (List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt tstmt_ refnewenv)) [] tstmtlist) in
        [cat_string_list_with_space (["for ("] @ f1 @ [";"] @ f2 @ [";"] @ f3 @ [")"])] @
        ["{"] @ tstmtstr @ ["}"]
    | TForeach(_) -> [] (* TODO *)
    | TWhile(expr_, tstmtlist) ->
        [cat_string_list_with_space (["while ("] @ (handle_texpr expr_ refnewenv))] @
        (List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt tstmt_ refnewenv)) [] tstmtlist)

(* take tstmt list and return string list *)
let handle_body body refenv =
    let refnewenv = ref (append_new_level !refenv) in
    let body_code = List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt tstmt_ refnewenv)) [] body in
    ["{"] @ body_code @ ["}"]

(* return string list *)
(* take a function declaration and generate the string list *)
let handle_fdecl fd g_env =
    let refnewenv = ref (append_new_level g_env) in
    match fd with
    | {tret=rt; tfname=name; tformals=fm; tbody=body ;_} ->
        let fmstr = (handle_fm fm refnewenv) in
        let bodystr = (handle_body body refnewenv) in
        [ cat_string_list_with_space [(type_to_string rt);name;fmstr]] @ bodystr

(* take a fdecl list and generate the string list *)
let handle_funlist funlist =
    let g_env = init_level_env() in
    List.fold_left (fun ret fdecl -> ret @ (handle_fdecl fdecl g_env)) [] funlist

let codegen_helper funlist =
    let header = ["#include<iostream>";"#include<string>";"using namespace std;"] in
    let buffer = header @ (handle_funlist funlist) in
    List.fold_left (fun ret ele -> ret ^ ele ^ "\n") "" buffer

let (fundone : (string, string) Hashtbl.t) = Hashtbl.create 16

let find_hash ht key =
    try
        Some (Hashtbl.find ht key)
    with
    | Not_found -> None

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
    | TFly ((fn, texpl), t) -> texp_helper (TCall((fn, texpl), t))
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
    (* TRegister of (string * string * texpr list) * typ TODO *)
    | TRegister (_) -> []
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

(* take a function key and return t_func_decl list,
which includes the function itself and all functions it calls *)
let rec dfs ht fkey =
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
                    [fd] @ (List.fold_left (fun ret key_ -> ret @ (dfs ht key_)) [] fklist)
                )
        )
    | _ -> []

let build_list_from_ht ht =
    List.rev (dfs ht "main")

let codegen ht =
    let funlist = build_list_from_ht ht in
    codegen_helper funlist
