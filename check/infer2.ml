(*infer type and do a static syntax checking*)
open Ast
open Sast

let (func_binds : (string, t_func_decl) Hashtbl.t) = Hashtbl.create 16

(*
type t_func_decl = {
        ttkey: string; (* for matching*)
        tfname: string;
        tformals: (string * typ) list;
        tbody: tstmt list;
        tret: typ (*the return value type*)
    }
*)

let gen_example_ht =
    let body = [TExpr(TCall(("coolfunc",[TString("Hello World")]), Void));TReturn(TLiteral(0))] in
    let mainfunc = {tret=Int;tbody=body;tformals=[];tfname="main";ttkey=""} in
    let anobody = [TExpr(TAssign(("ret", TLiteral(0)),Int));TExpr(TCall(("print",[TId("b",String)]), Void));TReturn(TId("ret",Int))] in
    let anotherfunc = {tret=Int;tbody=anobody;tformals=[("b",String)];tfname="coolfunc";ttkey=""} in
    let flist = [mainfunc;anotherfunc] in
    List.iter 
        (fun fdecl -> match fdecl with | {tfname = name ; _} -> Hashtbl.add func_binds name fdecl) flist

let infer_check (ast : program) = (* return function list *)
    let body = [TExpr(TCall(("coolfunc",[TString("Hello World")]), Void));TReturn(TLiteral(0))] in
    let mainfunc = {tret=Int;tbody=body;tformals=[];tfname="main";ttkey=""} in
    let anobody = [TExpr(TAssign(("ret", TLiteral(0)),Int));TExpr(TCall(("print",[TId("b",String)]), Void));TReturn(TId("ret",Int))] in
    let anotherfunc = {tret=Int;tbody=anobody;tformals=[("b",String)];tfname="coolfunc";ttkey=""} in
    [anotherfunc;mainfunc]
