(*below are some debuging function to show some sub-tree of ast
    TODO modified when writing our codes*)
open Ast
open Sast

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
    | Literal a ->"a integer:" ^ (string_of_int a)
    | BoolLit a -> if a = true then "a bool:true" else "a bool:false"
    | Id a -> "an id:" ^ a
    | Set a -> "a set:" ^ (List.fold_left (fun res item -> res ^ "," ^ debug_expr item) "" a)
    | Map a -> "a map:" ^ (List.fold_left (fun res (item1, item2) -> res ^ ",k:" ^(debug_expr item1)^ "_v:" ^(debug_expr item2)) "" a)
    | Array a -> "an array:" ^ (List.fold_left (fun res item -> res ^ "," ^ debug_expr item) "" a)
    | Binop (a, op, b) -> "binop:" ^ (string_of_op op) ^ "_left:" ^ (debug_expr a) ^ "_right:" ^ (debug_expr b)
    | Unop (uop, a) -> "unop:" ^ (string_of_uop uop) ^ "expr:" ^ (debug_expr a)
    | Assign (a, b) -> "assign: " ^ a ^ " by:" ^ (debug_expr b)
    | Call (id, exprs) -> "call: " ^ id ^ "_" ^ (List.fold_left (fun res item -> res ^ "," ^ (debug_expr item)) "" exprs);
    | ObjCall (id1, id2, exprs) -> "call by" ^ id1 ^ "." ^ id2 ^ (List.fold_left (fun res item -> res ^ "," ^ (debug_expr item)) "" exprs)
    | Func (a, b) -> "lambda:" ^ (List.fold_left (fun res item -> res ^ "," ^ item) "" a) ^ "lambda expr:" ^ (debug_expr b)
    | Exec(a) -> "exec: " ^ a
    | Dispatch(a, exprs, b, c) -> "dispatch: " ^ a ^ (List.fold_left (fun str item -> str ^ "," ^ (debug_expr item)) "" exprs)
    | Register (a, b, exprs) -> "register: " ^ a ^ " " ^ b  ^ " " ^ ( List.fold_left (fun str item -> str ^ "," ^ item) "" (List.map debug_expr exprs))
    | Chan (a) -> "chan: " ^ debug_expr a
    | Chanunop (a) -> "chaunop: " ^ a
    | Chanbinop (a, b) -> "chanbinop: " ^ a ^ " " ^ b
    | Fly (a, exprs) -> "fly: " ^ a ^ " " ^ ( List.fold_left (fun str item -> str ^ "," ^ item) "" (List.map debug_expr exprs)  )
    | Flyo (a, b, exprs) -> "flyo: " ^ a ^ " " ^ b ^ " " ^ ( List.fold_left (fun str item -> str ^ "," ^ item) "" (List.map debug_expr exprs) )
    | _ -> "nothing"





let debug_stmt = function
    | Expr a -> "expr:" ^ (debug_expr a)
    | _ -> "" (*TODO fill out other stmt debug*)

let debug_fdecl (fdecl : func_decl) = match fdecl with
    | {fname=name;body = stmts;formals=param_list} ->
        "function name: " ^ name ^ "," ^ "params:" ^ (List.fold_left (fun res item -> res ^ "," ^ item) "" param_list)
        ^ "body:" ^ (List.fold_left (fun res item -> res ^ "," ^ (debug_stmt item)) "" stmts)


(*debug for typed things*)
let rec debug_texpr = function
    | TId (name, this_type) -> "id:" ^ name ^ "_withtype_" ^ type_to_string this_type
    | _ -> "" (*TODO fill out the other texprs debug*)

(*debug for typed stmts*)
let rec debug_tstmt = function
    | _ -> "" (*TODO fill out tstmt like stmt*)

(*debug for a typed function call*)
let debug_t_fdecl tfdecl = match tfdecl with
    | _ -> "" (*TODO fill out t_fecl debug like fdecl*)


(* debug debug.ml
let _ =
    let a = BoolLit true and b = Set ([BoolLit true;BoolLit false;BoolLit true])
    and c  = Map ([Id "a", BoolLit true;Id "b", BoolLit false;
        Id "c", BoolLit true])
    in debug_expr c
    *)
