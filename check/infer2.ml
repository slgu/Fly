(*infer type and do a static syntax checking*)
open Ast
open Sast

let infer_check (ast : program) = (* return function list *)
    let body = [TExpr(TCall(("coolfunc",[TString("Hello World")]), Void));TReturn(TLiteral(0))] in
    let mainfunc = {tret=Int;tbody=body;tformals=[];tfname="main";ttkey=""} in
    let anobody = [TExpr(TAssign(("ret", TLiteral(0)),Int));TExpr(TCall(("print",[TId("b",String)]), Void));TReturn(TId("ret",Int))] in
    let anotherfunc = {tret=Int;tbody=anobody;tformals=[("b",String)];tfname="coolfunc";ttkey=""} in
    [anotherfunc;mainfunc]
