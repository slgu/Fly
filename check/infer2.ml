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

let infer_check (ast : program) = (* build the hash *)
    let body = [TExpr(TCall(("coolfunc",[TString("Hello World"); TLiteral(4)]), Void));TReturn(TLiteral(0))] in
    let mainfunc = {tret=Int;tbody=body;tformals=[];tfname="main";ttkey="main"} in
    let anobody = [
        TExpr (
            TAssign(
                ("ret", TBinop(
                            (
                                TBinop(
                                    (TLiteral(2), Add, TLiteral(3)), Int
                                ),
                                Mult,
                                TId("v",Int)
                            ),
                            Int
                        )),
                Int
            )
        );
        TBlock (
            [
                TExpr (
                    TCall(
                        ("print",[TId("b",String)]), Void
                    )
                );
                TExpr (
                    TCall(
                        ("print",[TId("ret",String)]), Void
                    )
                )
            ]
        );
        TReturn(
            TId("ret",Int)
        )
    ] in
    let anotherfunc = {tret=Int;tbody=anobody;tformals=[("b",String);("v",Int)];tfname="coolfunc";ttkey="coolfunc@string@int"} in
    let flist = [mainfunc;anotherfunc] in
    ignore(List.iter
        (fun fdecl -> match fdecl with | {ttkey = key ; _} -> Hashtbl.add func_binds key fdecl) flist);
    func_binds

(*
let infer_check (ast : program) = (* return function list *)
    let body = [TExpr(TCall(("coolfunc",[TString("Hello World")]), Void));TReturn(TLiteral(0))] in
    let mainfunc = {tret=Int;tbody=body;tformals=[];tfname="main";ttkey=""} in
    let anobody = [TExpr(TAssign(("ret", TLiteral(0)),Int));TExpr(TCall(("print",[TId("b",String)]), Void));TReturn(TId("ret",Int))] in
    let anotherfunc = {tret=Int;tbody=anobody;tformals=[("b",String)];tfname="coolfunc";ttkey=""} in
    [anotherfunc;mainfunc]
*)
