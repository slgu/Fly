(*below are some debuging function to show some sub-tree of ast
    TODO modified when writing our codes*)
open Ast


let string_of_op = function
    | Add -> "add"
    | Sub -> "sub"
    | Mult -> "mul"
    | Div -> "div"
    | Equal -> "equal"
    | Neq -> "neq"
    | Less -> "less"
    | Leq -> "less or equal"
    | Greater -> "greater"
    | Geq -> "greater or equal"
    | And -> "and"
    | Or -> "or"
    | RArrow -> "->"
    | LArrow -> "<-"
    | SAdd -> "add string"

let string_of_uop = function
    | Neg -> "neg"
    | Not -> "not"

let rec debug_expr = function
    | Literal a -> "a integer: " ^ (string_of_int a)
    | BoolLit a -> if a = true then "a bool: true" else "a bool: false"
    | Float a -> "a float: " ^ (string_of_float a)
    | Id a -> "an id: " ^ a
    | Set a -> "a set: " ^ List.fold_left (func str item -> str ^ ", " ^ item) "" (List.map debug_expr a)
    | Map a -> "a map: " ^ List.fold_left (func str item -> str ^ ", " ^ item) "" (List.map (fun (x,y) -> "(" ^ debug_expr x ^ "," ^ debug_expr y ^ ")") a)
    | Array a -> "an array: " ^ List.fold_left (func str item -> str ^ ", " ^ item) "" (List.map debug_expr a)
    | String a -> "a string: " ^ a
    | Binop (a, op, b) -> "binop:" ^ (string_of_op op) ^ ", left:"^ debug_expr a^ ", right:" ^ debug_expr b
    | Unop (uop, a) -> "unop: " ^ (string_of_uop uop) ^ debug_expr a
    | Call (id, exprs) -> "call: " ^ id ^ "; expressions: " List.fold_left (func str item -> str ^ ", " ^ item) "" (List.map debug_expr exprs);
    | ObjCall (id1, id2, exprs) -> "call by" ^ id1 ^ "." ^ id2 ^ "; expressions: " ^ List.fold_left (func str item -> str ^ ", " ^ item) "" (List.map debug_expr exprs)
    | Func (a, b) -> "lambda: " ^ List.fold_left (func str item -> str ^ ", " ^ item) "" (List.map debug_expr a) ^ "; lambda expr: " ^ debug_expr b
    | Assign (a, b) -> "assign: " ^ a ^" by: " ^ debug_expr b
    | ListComprehen (a, b, c) -> "list comprehension: " ^ debug_expr a ^ b ^ debug_expr c;
    | _ -> print_endline "nothing"
    | Exec(a) -> "exec: " ^ a
    | Dispatch(a, exprs, b, c) -> "dispatch: " ^ a ^ ( List.fold_left (func str item -> str ^ "," ^ item) "" (List.map debug_expr exprs) )
    | Register (a, b, exprs) -> "register: " ^ a ^ " " ^ b  ^ " " ^ ( List.fold_left (func str item -> str ^ "," ^ item) "" (List.map debug_expr exprs) )
    | Chan (a) -> "chan: " ^ debug_expr a
    | Chanunop (a) -> "chaunop: " ^ a
    | Chanbinop (a, b) -> "chanbinop: " ^ a ^ " " ^ b
    | Fly (a, exprs) -> "fly: " ^ a ^ " " ^ ( List.fold_left (func str item -> str ^ "," ^ item) "" (List.map debug_expr exprs)  )
    | Flyo (a, b, exprs) -> "flyo: " ^ a ^ " " ^ b ^ " " ^ ( List.fold_left (func str item -> str ^ "," ^ item) "" (List.map debug_expr exprs) )
  
    
    




let debug_stmt = function
    | Expr a -> debug_expr a
    | _ -> print_endline "nothing"

let debug_fdecl (fdecl : func_decl) = match fdecl with
    | {body = stmts;_} -> List.iter debug_stmt stmts

let debug_ast = function
    | Program (cdecls, fdecls) -> List.iter debug_fdecl fdecls

let _ =
    let a = BoolLit true and b = Set ([BoolLit true;BoolLit false;BoolLit true])
    and c  = Map ([Id "a", BoolLit true;Id "b", BoolLit false;
        Id "c", BoolLit true])
    in debug_expr c
