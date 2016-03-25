open Ast
open Sast

(* return string *)
(* take a string list and concatenate them with interleaving space into a single string *)
let rec cat_string_list_with_space sl =
    match sl with
    | [] -> ""
    | hd::tl -> hd ^ " " ^ (cat_string_list_with_space tl)

(* return formals in string *)
(* take a formal and generate the string *)
let handle_fm formals =
    let fstr = 
        List.fold_left (fun ret (str_, type_) -> ret ^ " " ^ (type_to_string type_) ^ " " ^ str_ ^ ",") "" formals in
    let len = (String.length fstr) in
    let trimed = if len > 0 then (String.sub fstr 0 (len-1)) else fstr in
    "(" ^ trimed ^ ")"

(* return string list *)
(* take a function declaration and generate the string list *)
let handle_fdecl fd =
    match fd with
    | {tret=rt; fname=name; tformals=fm; _} -> 
        [ 
            cat_string_list_with_space [(type_to_string rt);name;(handle_fm fm)] 
        ]

(* take a fdecl list and generate the string list *)
let handle_sast sast = 
    List.fold_left (fun ret fdecl -> ret @ (handle_fdecl fdecl)) [] sast

let codegen sast = 
    let header = ["#include<iostream>"] in
    let buffer = header @ (handle_sast sast) in
    List.fold_left (fun ret ele -> ret ^ ele ^ "\n") "" buffer
