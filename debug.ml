(*below are some debuging function to show some sub-tree of ast
    TODO modified when writing our codes*)
open Ast


let string_of_op = function
    | Add -> "add"
    | Sub -> "sub"
    | Mult -> "mul"
    | Div -> "div"
    | Neq -> "neq"
    | Less -> "less"
    | Greater -> "greater"
    | Geq -> "geq"
    | And -> "and"
    | Or -> "or"
    | RArrow -> "->"
    | LArrow -> "<-"
    | _ -> "nothing"

let string_of_uop = function
    | Neg -> "neg"
    | Not -> "not"

let rec debug_expr = function
    | Literal a -> print_endline ("a integer:" ^ (string_of_int a))
    | BoolLit a -> if a = true then print_endline "a bool:true" else print_endline "a bool:false"
    | Id a -> print_endline ("an id:" ^ a)
    | Set a -> print_endline "a set:";List.iter debug_expr a
    | Map a -> print_endline "a set:";List.iter (fun (x,y) -> print_endline "kv";debug_expr x;debug_expr y) a
    | Array a -> print_endline "an array:";List.iter debug_expr a
    | Binop (a, op, b) -> print_endline ("binop:" ^ (string_of_op op));
        print_endline "left:";debug_expr a;debug_expr b
    | Unop (uop, a) -> print_endline ("unop:" ^ (string_of_uop uop));
        debug_expr a
    | Assign (a, b) -> print_endline ("assign: " ^ a ^" by:");debug_expr b
    | Call (id, exprs) -> print_endline ("call: " ^ id);List.iter debug_expr exprs;
    | ObjCall (id1, id2, exprs) -> print_endline ("call by" ^ id1 ^ "." ^ id2);List.iter debug_expr exprs;
    | Func (a, b) -> print_endline "lambda:";List.iter print_endline a;print_endline "lambda expr:";debug_expr b
    | _ -> print_endline "nothing"

let debug_stmt = function
    | Expr a -> debug_expr a
    | _ -> print_endline "nothing"

let debug_fdecl = function
    | {body = stmts;_} -> List.iter debug_stmt stmts

let debug_ast = function
    | Program (stmts, cdecls, fdecls) -> List.iter debug_fdecl fdecls

(* debug debug.ml
let _ =
    let a = BoolLit true and b = Set ([BoolLit true;BoolLit false;BoolLit true])
    and c  = Map ([Id "a", BoolLit true;Id "b", BoolLit false;
        Id "c", BoolLit true])
    in debug_expr c
    *)
