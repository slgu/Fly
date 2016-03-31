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
	| TLiteral(a, this_type) -> "integer: " ^ (string_of_int a) ^ "_withtype_" ^ type_to_string this_type
	| TBoolLit (a, this_type) -> (if a = true then "bool:true: " else " bool:false: ") ^ "_withtype_" ^ type_to_string this_type
	| TFloat(a, this_type) -> "float: " ^ (string_of_float a) ^ "_withtype_" ^ type_to_string this_type
	| TSet (a, this_type) -> "set: " ^ (List.fold_left (fun res item -> res ^ "," ^ debug_texpr item) "" a) ^ "_withtype_" ^ type_to_string this_type
	| TMap (a, this_type) -> "map: " ^ (List.fold_left (fun res (item1, item2) -> res ^ ",k:" ^(debug_texpr item1)^ "_v:" ^(debug_texpr item2)) "" a) ^ "_withtype_" ^ type_to_string this_type
	| TArray(a, this_type) -> "array: " ^ (List.fold_left (fun res item -> res ^ "," ^ debug_texpr item) "" a) ^ "_withtype_" ^ type_to_string this_type
	| TString (a, this_type) -> "string:" ^ a ^ "_withtype_" ^ type_to_string this_type
	| TBinop (a, op, b, this_type) -> "binop: " ^ (string_of_op op) ^ "_left:" ^ (debug_texpr a) ^ "_right:" ^ (debug_texpr b) ^ "_withtype_" ^ type_to_string this_type
	| TUnop (uop, a, this_type) -> "unop:" ^ (string_of_uop uop) ^ "texpr:" ^ (debug_texpr a) ^ "_withtype_" ^ type_to_string this_type
	| TCall(id, exprs, this_type) -> "call: " ^ id ^ "_" ^ (List.fold_left (fun res item -> res ^ "," ^ (debug_texpr item)) "" exprs) ^ "_withtype_" ^ type_to_string this_type;
	| TObjCall (id1, id2, exprs, this_type) -> "call by" ^ id1 ^ "." ^ id2 ^ (List.fold_left (fun res item -> res ^ "," ^ (debug_texpr item)) "" exprs) ^ "_withtype_" ^ type_to_string this_type;
	| TFunc (a, b, this_type) -> "lambda:" ^ (List.fold_left (fun res item -> res ^ "," ^ item) "" a) ^ "lambda expr:" ^ (debug_texpr b) ^ "_withtype_" ^ type_to_string this_type
    | TAssign (a, b, this_type) -> "assign: " ^ a ^ " by:" ^ (debug_texpr b) ^ "_withtype_" ^ type_to_string this_type
	| TListComprehen(a, b, c, this_type) -> "list comprehension: " ^ (debug_texpr a) ^ b ^ (debug_texpr c) ^ "_withtype_" ^ type_to_string this_type
	| TExec (a, this_type) -> "exec: " ^ a ^ "_withtype_" ^ type_to_string this_type
	| TDispatch (a, exprs, b, c, this_type) -> "dispatch: " ^ a ^ (List.fold_left (fun str item -> str ^ "," ^ (debug_texpr item)) "" exprs) ^ "_withtype_" ^ type_to_string this_type
	| TRegister (a, b, exprs. this_type) -> "register: " ^ a ^ " " ^ b  ^ " " ^ ( List.fold_left (fun str item -> str ^ "," ^ item) "" (List.map debug_texpr exprs)) ^ "_withtype_" ^ type_to_string this_type
	| TChan (a, this_type) -> "chan: " ^ debug_texpr a ^ "_withtype_" ^ type_to_string this_type
  	| TChanunop (a, this_type) -> "chaunop: " ^ a ^ "_withtype_" ^ type_to_string this_type
 	| TChanbinop (a, b, this_type) -> "chanbinop: " ^ a ^ " " ^ b ^ "_withtype_" ^ type_to_string this_type
	| TFly (a, exprs, this_type) -> "fly: " ^ a ^ " " ^ ( List.fold_left (fun str item -> str ^ "," ^ item)  "" (List.map debug_texpr exprs)  ) ^ "_withtype_" ^ type_to_string this_type
    | TFlyo (a, b, exprs, this_type) -> "flyo: " ^ a ^ " " ^ b ^ " " ^ ( List.fold_left (fun str item -> str ^ "," ^ item) "" (List.map debug_texpr exprs) ) ^ "_withtype_" ^ type_to_string this_type
    | _ -> "nothing"


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
