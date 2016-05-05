type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | RArrow | LArrow | SAdd | Mod

type uop = Neg | Not

(* define for data type *)
type typ =
    Int | Bool | Void | String | Float (*basic type*)
    | Array of typ (*array*)
    | Set of typ (*set*)
    | Map of typ * typ (*map*)
    | Class of string (* a class variable *)
    | Chan of typ (* a chan that contains which type *)
    | Signal of typ (*signal is like a future obj with typ*)
    | Undef (*which means this is a nulptr*)
    | Func of string * typ list (* function name along witH
        some type clojure*)
    | Cfunc of string * string (*class member function cname * fname*)
    | Lfunc of string * typ list (*lambda function along with some type clojure*)
    (*for built-in defned type*)

let uop_to_string = function
    | Neg -> "-"
    | Not -> "!"

let op_to_string = function
    | Add -> "+"
    | Mod -> "%"
    | Sub -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Equal -> "=="
    | Neq -> "!="
    | Less -> "<"
    | Leq -> "<="
    | Greater -> ">"
    | Geq -> ">="
    | And -> "&&"
    | Or -> "||"
    | RArrow -> "->"
    | LArrow -> "<-"
    | SAdd -> "+"

(* type to string function used for hash
    _ to concat type
    @ to concat different type
*)
let rec type_to_string = function
    | Int -> "int"
    | Bool -> "Bool"
    | Void -> "void"
    | String -> "string"
    | Float -> "float"
    | Array x -> "array_" ^ (type_to_string x)
    | Set x -> "set_" ^ (type_to_string x)
    | Map (x, y) -> "map_" ^ (type_to_string x) ^ "_" ^ (type_to_string y)
    | Class x -> x
    | Chan x -> "chan_" ^ (type_to_string x)
    | Signal x -> "signal:" ^ (type_to_string x)
    | Undef -> "undef"
    | Func (x, type_list) -> "func_" ^ x ^ (List.fold_left
            (fun str item -> str ^ "_" ^ item) "" (List.map type_to_string type_list))
    | Lfunc (x, type_list) -> "lfunc_" ^ x ^ (List.fold_left
            (fun str item -> str ^ "_" ^ item) "" (List.map type_to_string type_list))
    | _ -> raise (Failure ("ast.ml: type_to_string not yet support this type"))


type expr =
    Literal of int
  | BoolLit of bool
  | Null of string (*nullpointer belong to class s*)
  | Float of float
  | Id of string (* id token *)
  | Objid of string * string
  | Set of expr list
  | Map of (expr * expr) list
  | Array of expr list
  | String of string (*represent const string*)
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Call of string * expr list
  | ObjGen of typ
  | ObjCall of string * string * expr list (*invoke a method of an object*)
  | Func of string list * expr (*lambda expr*)
  | Assign of string * expr
  | MAssign of string * string * expr
  | ListComprehen of expr * string * expr (*can iterate a tuple?*)
  | Noexpr
  (*below are network specified exprs*)
  | Exec of string
  | Dispatch of string * expr list * string * string
  | Register of string * string * expr list
  | Changen of typ(*chan of a type*)
  | Chanunop of string
  | Chanbinop of string * string
  | Fly of string * expr list
  | Flyo of string * string * expr list


type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt list * stmt list
  | For of expr * expr * expr * stmt list
  | Foreach of string * expr * stmt list (*for each*)
  | While of expr * stmt list
  (*if for while just with list of stmt*)
  (*need to append lambda stmt, lots of built-in keyword stmt,
  like map func list*)


type func_decl = {
        fname : string; (*function name*)
        formals : string list; (*function paramters*)
        body : stmt list;
    }


type class_decl = {
        cname : string; (* class name *)
        member_binds:  (string * typ) list; (* member variables*)
        func_decls : func_decl list; (* member functions *)
    }

let get_class_name cdecl = match cdecl with
    | {cname=name;_} -> name

let get_class_member_type cdecl var = match cdecl with
    | {member_binds=binds;_} -> List.assoc var binds

type program = Program of class_decl list * func_decl list
