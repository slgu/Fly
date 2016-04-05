open Infer
open Codegen

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Scanner.token lexbuf in
    let (fht,cht) = infer_check ast in
    let code = codegen fht cht in
    let oc = open_out "tmp.cc" in
    ignore(print_string code);
    ignore(output oc code 0 (String.length code));
    close_out oc
