open Ast
let rec br_con_check ast =
    let rec check_stmt stmt pstmt = match stmt with
        | Block stmt_list -> List.iter (fun item -> check_stmt item pstmt) stmt_list
        | Expr expr -> ()
        | Return expr -> ()
        | If (a, b, c) -> List.iter (fun item -> check_stmt item pstmt) b;List.iter (fun item -> check_stmt item pstmt) c
        | For (a, b, c, d) -> List.iter (fun item -> check_stmt item true) d
        | Foreach (a, b, c) -> List.iter (fun item -> check_stmt item true) c
        | While (a, b) -> List.iter (fun item -> check_stmt item true) b
        | Break -> if pstmt then () else failwith("break must be inside for/while")
        | Continue -> if pstmt then () else failwith("continue must be inside for/while")
    in
    let rec check_fdecl fdecl = match fdecl with
        | {body=stmt_list;}->
            List.iter (fun item -> check_stmt item false) stmt_list
    in
    match ast with
    | Program (cdecl_list, fdecl_list) ->
        List.iter check_fdecl fdecl_list
