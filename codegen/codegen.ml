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
        if fn = "print" then (* built-in function *)
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

let (fundone : (string, string) Hashtbl.t) = Hashtbl.create 16

let find_hash ht key =
    try
        Some (Hashtbl.find ht key)
    with
    | Not_found -> None

(*
type texpr =
  | TCall of (string * texpr list) * typ
  | TObjCall of (string * string * texpr list) * typ(*invoke a method of an object*)
  | TFunc of (string list * texpr) * typ (*lambda expr*)
  | TAssign of (string * texpr) * typ
  | TListComprehen of (texpr * string * texpr) * typ (*can iterate a tuple?*)
  (*below are network specified exprs*)
  | TExec of string * typ
  | TDispatch of (string * texpr list * string * string) * typ
  | TRegister of (string * string * texpr list) * typ
  | TChan of texpr * typ
  | TChanunop of string * typ
  | TChanbinop of (string * string) * typ
  | TFly of (string * texpr list) * typ
  | TFlyo of (string * string * texpr list) * typ
*)
(* take a texp and return function key list *)
let rec texp_helper texp_ =
    match texp_ with
    | TBinop ((texpr1, _, texpr2), _) -> (texp_helper texpr1) @ (texp_helper texpr2)
    | TUnop ((_, texpr_), _) -> texp_helper texpr_
    | TCall ((fn, texprlist), _) ->
        (
        match fn with
        | "print" -> []
        (* built-in function *)
        | _ ->
            let expr_types_list = List.map get_expr_type_info texprlist in
            let hash_key = fn ^ 
            (List.fold_left (fun str item -> str ^ "@" ^ item) "" (List.map type_to_string expr_types_list)) in
            [hash_key] @ (List.fold_left (fun ret exp_ -> ret @ (texp_helper exp_)) [] texprlist)
        )
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
                    [fd] @ (List.fold_left (fun ret key_ -> dfs ht key_) [] fklist)
                )
        )
    | _ -> []

let build_list_from_ht ht =
    List.rev (dfs ht "main")

let codegen ht = 
    let funlist = build_list_from_ht ht in
    codegen_helper funlist
