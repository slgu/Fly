open Infer2
open Codegen

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Scanner.token lexbuf in
    let ht = infer_check ast in
    let code = codegen ht in
    print_string code
