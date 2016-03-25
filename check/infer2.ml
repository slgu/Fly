(*infer type and do a static syntax checking*)
open Ast
open Sast

let infer_check (ast : program) = (* return function list *)
    let body = [TExpr(TCall(("print",[TString("Hello World")]), Void));TReturn(TLiteral(0))] in
    let mainfunc = {tret=Int;tbody=body;tformals=[];tfname="main";ttkey=""} in
    let anobody = [TExpr(TAssign(("val", TLiteral(0)),Int));TReturn(TId("val",Int))] in
    let anotherfunc = {tret=Int;tbody=anobody;tformals=[("a",Int);("b",String)];tfname="cool";ttkey=""} in
    [anotherfunc;mainfunc]
