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
    | Float a -> "a float:" ^ (string_of_float a)
    | Id a -> "an id:" ^ a
    | Set a -> "a set:" ^ (List.fold_left (fun res item -> res ^ "," ^ debug_expr item) "" a)
    | Map a -> "a map:" ^ (List.fold_left (fun res (item1, item2) -> res ^ ",k:" ^(debug_expr item1)^ "_v:" ^(debug_expr item2)) "" a)
    | Array a -> "an array:" ^ (List.fold_left (fun res item -> res ^ "," ^ debug_expr item) "" a)
    | String a -> "a string:" ^ a
    | Binop (a, op, b) -> "binop:" ^ (string_of_op op) ^ "_left:" ^ (debug_expr a) ^ "_right:" ^ (debug_expr b)
    | Unop (uop, a) -> "unop:" ^ (string_of_uop uop) ^ "expr:" ^ (debug_expr a)
    | Call (id, exprs) -> "call: " ^ id ^ "_" ^ (List.fold_left (fun res item -> res ^ "," ^ (debug_expr item)) "" exprs);
    | ObjCall (id1, id2, exprs) -> "call by" ^ id1 ^ "." ^ id2 ^ (List.fold_left (fun res item -> res ^ "," ^ (debug_expr item)) "" exprs)
    | Func (a, b) -> "lambda:" ^ (List.fold_left (fun res item -> res ^ "," ^ item) "" a) ^ "lambda expr:" ^ (debug_expr b)
    | Assign (a, b) -> "assign: " ^ a ^ " by:" ^ (debug_expr b)
    | ListComprehen (a, b, c) -> "list comprehension:" ^ (debug_expr a) ^ b ^ (debug_expr c)
    | Noexpr -> "no expression"
    (*network specified exprs*)
    | Exec(a) -> "exec: " ^ a
    | Dispatch(a, exprs, b, c) -> "dispatch: " ^ a ^ " " ^ (List.fold_left (fun str item -> str ^ "," ^ (debug_expr item)) "" exprs) ^ " " ^ b ^ " " ^ c
    | Register (a, b, exprs) -> "register: " ^ a ^ " " ^ b  ^ " " ^ ( List.fold_left (fun str item -> str ^ "," ^ item) "" (List.map debug_expr exprs))
    | Chan (a) -> "chan: " ^ debug_expr a
    | Chanunop (a) -> "chaunop: " ^ a
    | Chanbinop (a, b) -> "chanbinop: " ^ a ^ " " ^ b
    | Fly (a, exprs) -> "fly: " ^ a ^ " " ^ ( List.fold_left (fun str item -> str ^ "," ^ item) "" (List.map debug_expr exprs)  )
    | Flyo (a, b, exprs) -> "flyo: " ^ a ^ " " ^ b ^ " " ^ ( List.fold_left (fun str item -> str ^ "," ^ item) "" (List.map debug_expr exprs) )

let rec debug_stmt = function
      Block stmts -> "block:" ^ (List.fold_left (fun acc item -> acc ^ "," ^ (debug_stmt item)) "" stmts)
    | Expr a -> "expr:" ^ (debug_expr a)
    | Return a -> "return: " ^ (debug_expr a)
    | If (a, stmts1, stmts2) -> "if:" ^ (debug_expr a) ^ " " ^ (List.fold_left (fun acc item -> acc ^ "," ^ (debug_stmt item)) "" stmts1) ^ " " ^ (List.fold_left (fun acc item -> acc ^ "," ^ (debug_stmt item)) "" stmts2)
    | For (a, b, c, stmts) -> "for:" ^ (debug_expr a) ^ " " ^ (debug_expr b) ^ " " ^ (debug_expr c) ^ " " ^ ( List.fold_left (fun acc item -> acc ^ "," ^ item) "" (List.map debug_stmt stmts) )
    | Foreach (a, expr, stmts) -> "for each:" ^ a ^ " " ^ (debug_expr expr) ^ " " ^ ( List.fold_left (fun acc item -> acc ^ "," ^ item) "" (List.map debug_stmt stmts) )
    | While (expr, stmts) -> "while:" ^ (debug_expr expr) ^ " " ^ ( List.fold_left (fun acc item -> acc ^ "," ^ item) "" (List.map debug_stmt stmts) )

let debug_fdecl (fdecl : func_decl) = match fdecl with
    | {fname=name; body=stmts; formals=param_list} ->
        "function name:" ^ name ^ ", " ^ "params:" ^ (List.fold_left (fun res item -> res ^ "," ^ item) "" param_list)
        ^ ", body:" ^ (List.fold_left (fun res item -> res ^ "," ^ (debug_stmt item)) "" stmts)

(*debug for typed things*)
let rec debug_texpr = function
      TLiteral a -> "literal: " ^ (string_of_int a)
    | TBoolLit a -> if a = true then "bool: true" else "bool: false"
    | TFloat a -> "float: " ^ (string_of_float a)
    | TId (name, this_type) -> "id:" ^ name ^ "_withtype_" ^ type_to_string this_type
	| TSet (a, this_type) -> "set: " ^ (List.fold_left (fun res item -> res ^ "," ^ debug_texpr item) "" a) ^ "_withtype_" ^ type_to_string this_type
	| TMap (a, this_type) -> "map: " ^ (List.fold_left (fun res (item1, item2) -> res ^ ",k:" ^(debug_texpr item1)^ "_v:" ^(debug_texpr item2)) "" a) ^ "_withtype_" ^ type_to_string this_type
	| TArray(a, this_type) -> "array: " ^ (List.fold_left (fun res item -> res ^ "," ^ debug_texpr item) "" a) ^ "_withtype_" ^ type_to_string this_type
	| TString a -> "string:" ^ a
    |TBinop (binop, this_type) -> (fun (a, op, b) -> "binop: " ^ (string_of_op op) ^ "_left:" ^ (debug_texpr a) ^ "_right:" ^ (debug_texpr b)) binop ^ "_withtype_" ^ type_to_string this_type
    |TUnop (unop, this_type) -> (fun (uop, a) -> "unop: " ^ (string_of_uop uop) ^ "_expr: " ^ (debug_texpr a)) unop ^ "_withtype_" ^ type_to_string this_type
    | TCall(a, this_type) -> (fun (id, texprs) -> "call: " ^ id ^ "_" ^ (List.fold_left (fun res item -> res ^ "," ^ (debug_texpr item)) "" texprs) ) a ^ "_withtype_" ^ type_to_string this_type
    | TObjCall (a, this_type) -> (fun (id1, id2, texprs) -> "call by: " ^ id1 ^ "." ^ id2 ^ (List.fold_left (fun res item -> res ^ "," ^ (debug_texpr item)) "" texprs) ) a ^ "_withtype_" ^ type_to_string this_type
    | TFunc (args, this_type) -> (fun (a, b) -> "lambda:" ^ (List.fold_left (fun res item -> res ^ "," ^ item) "" a) ^ "lambda expr:" ^ (debug_texpr b) ) args ^ "_withtype_" ^ type_to_string this_type
    | TAssign (args, this_type) -> (fun (a, b) -> "assign: " ^ a ^ " by:" ^ (debug_texpr b) ) args ^ "_withtype_" ^ type_to_string this_type
    | TListComprehen(args, this_type) -> (fun (a, b, c) -> "list comprehension: " ^ (debug_texpr a) ^ b ^ (debug_texpr c) ) args ^ "_withtype_" ^ type_to_string this_type
    | TExec (a, this_type) -> "exec: " ^ a ^ "_withtype_" ^ type_to_string this_type
    | TDispatch (args, this_type) -> (fun (a, exprs, b, c) -> "dispatch: " ^ a ^ (List.fold_left (fun str item -> str ^ "," ^ (debug_texpr item)) "" exprs) ) args ^ "_withtype_" ^ type_to_string this_type
    | TRegister (args, this_type) -> (fun (a, b, exprs) -> "register: " ^ a ^ " " ^ b  ^ " " ^ ( List.fold_left (fun str item -> str ^ "," ^ item) "" (List.map debug_texpr exprs)) ) args ^ "_withtype_" ^ type_to_string this_type
    | TChan (a, this_type) -> "chan: " ^ debug_texpr a ^ "_withtype_" ^ type_to_string this_type
    | TChanunop (a, this_type) -> "chaunop: " ^ a ^ "_withtype_" ^ type_to_string this_type
    | TChanbinop (args, this_type) -> (fun (a, b) -> "chanbinop: " ^ a ^ " " ^ b ) args ^ "_withtype_" ^ type_to_string this_type
    | TFly (args, this_type) -> (fun (a, exprs) -> "fly: " ^ a ^ " " ^ ( List.fold_left (fun str item -> str ^ "," ^ item)  "" (List.map debug_texpr exprs)) ) args ^ "_withtype_" ^ type_to_string this_type
    | TFlyo (args, this_type) -> (fun (a, b, exprs) -> "flyo: " ^ a ^ " " ^ b ^ " " ^ ( List.fold_left (fun str item -> str ^ "," ^ item) "" (List.map debug_texpr exprs)) ) args^ "_withtype_" ^ type_to_string this_type


(*debug for typed stmts*)
let rec debug_tstmt = function
       TBlock tstmts -> "typed block:" ^ ( List.fold_left (fun acc item -> acc ^ "," ^ item) "" (List.map debug_tstmt tstmts) )
    |  TExpr a -> "typed expr:" ^ (debug_texpr a)
    |  TReturn a -> "typed return:" ^ (debug_texpr a)
    |  TIf (a, tstmts1, tstmts2) -> "typed if:" ^ (debug_texpr a) ^ " " ^ (List.fold_left (fun acc item -> acc ^ "," ^ (debug_tstmt item)) "" tstmts1) ^ " " ^ (List.fold_left (fun acc item -> acc ^ "," ^ (debug_tstmt item)) "" tstmts2)
    |  TFor (a, b, c, tstmts) -> "typed for:" ^ (debug_texpr a) ^ " " ^ (debug_texpr b) ^ " " ^ (debug_texpr c) ^ " " ^ ( List.fold_left (fun acc item -> acc ^ "," ^ item) "" (List.map debug_tstmt tstmts) )
    |  TForeach (a, texpr, tstmts) -> "typed for each:" ^ a ^ " " ^ (debug_texpr texpr) ^ " " ^ ( List.fold_left (fun acc item -> acc ^ "," ^ item) "" (List.map debug_tstmt tstmts) )
    |  TWhile (texpr, tstmts) -> "typed while:" ^ (debug_texpr texpr) ^ " " ^ ( List.fold_left (fun acc item -> acc ^ "," ^ item) "" (List.map debug_tstmt tstmts) )

(*debug for a typed function call*)
let debug_t_fdecl (tfdecl: t_func_decl) = match tfdecl with
    | {ttkey=key; tfname=name; tformals=param_list; tbody=tstmts; tret=return} ->
        "key:" ^ key ^ ", " ^ "function name:" ^ name ^ "\n"
        ^ "params:\n" ^ ( List.fold_left (fun acc (str, typ) -> acc ^ "str:" ^ str ^ "_type:" ^ (type_to_string typ) ^ "\n" ) "" param_list )
        ^"body:\n" ^ (List.fold_left (fun acc item -> acc ^ ",\n" ^ (debug_tstmt item)) "" tstmts)
        ^ "return type:" ^ (type_to_string return)

let debug_t_lambda_decl (tldecl: t_lambda_decl) = match tldecl with
    | {ltkey=key; ltfname=name; ltbinds=bind_list; ltformals=param_list; ltbody=tstmts; ltret=return} ->
        "key:" ^ key ^ ", " ^ "function name:" ^ name ^ "\n"
        ^ "binds:\n" ^ ( List.fold_left (fun acc (str, typ) -> acc ^ "str:" ^ str ^ "_type:" ^ (type_to_string typ) ^ "\n") "" bind_list )
        ^ "params:\n" ^ ( List.fold_left (fun acc (str, typ) -> acc ^ "str:" ^ str ^ "_type:" ^ (type_to_string typ) ^ "\n") "" param_list )
        ^ "body:\n" ^ (List.fold_left (fun acc item -> acc ^ ",\n" ^ (debug_tstmt item)) "" tstmts)
        ^ "return type:" ^ (type_to_string return)
