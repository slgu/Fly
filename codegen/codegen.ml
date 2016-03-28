open Ast
open Sast

(* take a string list and concatenate them with interleaving space into a single string *)
let rec cat_string_list_with_space sl =
    match sl with
    | [] -> ""
    | hd::tl -> hd ^ " " ^ (cat_string_list_with_space tl)

(* take a formal and generate the string *)
let handle_fm formals =
    let fstr = 
        List.fold_left (fun ret (str_, type_) -> ret ^ " " ^ (type_to_string type_) ^ " " ^ str_ ^ ",") "" formals in
    let len = (String.length fstr) in
    let trimed = if len > 0 then (String.sub fstr 0 (len-1)) else fstr in
    "(" ^ trimed ^ ")"

(* take one expr and return a string list *)
let rec handle_texpr expr =
    match expr with
    | TCall ((fn, texpr_list), _) -> 
        if fn = "print" then
            [
                cat_string_list_with_space 
                (["cout"]@(List.fold_left (fun ret ex -> ret@["<<"]@(handle_texpr ex)) [] texpr_list)@["<<endl"])
            ]
        else
            [
                cat_string_list_with_space 
                ([fn;"("]@(List.fold_left (fun ret ex -> ret@(handle_texpr ex)) [] texpr_list)@[")"])
            ]
    | TString(str) -> ["\"" ^ str ^ "\""]
    | TId(str, _) -> [str]
    | TLiteral(value) -> [string_of_int value]
    | TAssign((str, expr), ty) -> [(type_to_string ty) ^ " " ^ str ^ " = "] @ handle_texpr expr
    | _ -> [] (* TODO *)

(* take one tstmt and return a string list *)
let handle_tstmt tstmt_ =
    match tstmt_ with
    | TBlock(_) -> [] (* TODO *)
    | TExpr(expr) -> [cat_string_list_with_space ((handle_texpr expr) @ [";"])]
    | TReturn(expr) -> [cat_string_list_with_space (["return"] @ (handle_texpr expr) @ [";"])]
    | TIf(_) -> [] (* TODO *)
    | TFor(_) -> [] (* TODO *)
    | TForeach(_) -> [] (* TODO *)
    | TWhile(_) -> [] (* TODO *)

(* take tstmt list and return string list *)
let handle_body body = 
    let body_code = List.fold_left (fun ret tstmt_ -> ret @ (handle_tstmt tstmt_)) [] body in
    ["{"] @ body_code @ ["}"]

(* return string list *)
(* take a function declaration and generate the string list *)
let handle_fdecl fd =
    match fd with
    | {tret=rt; tfname=name; tformals=fm; tbody=body ;_} -> 
        [ cat_string_list_with_space [(type_to_string rt);name;(handle_fm fm)]] @ (handle_body body)

(* take a fdecl list and generate the string list *)
let handle_funlist funlist = 
    List.fold_left (fun ret fdecl -> ret @ (handle_fdecl fdecl)) [] funlist

let codegen_helper funlist = 
    let header = ["#include<iostream>";"#include<string>";"using namespace std;"] in
    let buffer = header @ (handle_funlist funlist) in
    List.fold_left (fun ret ele -> ret ^ ele ^ "\n") "" buffer

let (fundone : (string, t_func_decl) Hashtbl.t) = Hashtbl.create 16

let find_hash ht key =
    try
        Some (Hashtbl.find ht key)
    with
    | Not_found -> None

let rec dfs ht fkey = 
    (*
    1. get t_func_decl and push to list
    2. go over the body, when meet a funcall, recur
    *)
    let hash_value = find_hash fundone fkey in
    match hash_value with 
    | None ->
        let ret = [] in
        let sfd = find_hash ht fkey in
        (
            match sfd with 
            | None -> raise (Failure ("Function not defined " ^ fkey))
            | Some (fd) -> 
                ignore(Hashtbl.add fundone fkey fd); (* TODO *)
                [fd]
        )
    | _ -> []

let build_list_from_ht ht =
    List.rev (dfs ht "main_void")

let codegen ht = 
    let funlist = build_list_from_ht ht in
    codegen_helper funlist
