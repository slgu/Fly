open Infer2
open Codegen

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Scanner.token lexbuf in
    let ret = infer_check ast in
    let code = codegen ret in
    print_string code
