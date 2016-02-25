type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Arrow

type uop = Neg | Not

(* define for data type *)
type typ =
    Int | Bool | Void | String | Float (*basic type*)
    | Array of typ (*array*)
    | Set of typ (*set*)
    | Map of typ * typ (*map*)
    | Func of typ list * typ (*function type*)
    | Undef (*which means the type is undefined for this node*)
     (*TODO maybe need support for record*)

type bind = typ * string

type expr =
    Literal of int
  | BoolLit of bool
  | Id of string (* id token *)
  | Set of expr list
  | Map of (expr * expr) list
  | String of string (*represent const string*)
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | ObjCall of string * string * expr list (*invoke a method of an object*)
  | Func of string list * expr (*lambda expr*)
  | Noexpr

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
        typ : typ; (*return type*)
        fname : string; (*function name*)
        formals : bind list; (*function paramters*)
        locals : bind list; (*local variables*)
        body : stmt list;
        guards : expr list; (*all guards*)
    }

type program = stmt list * func_decl list


(*below are some debuging function to show some sub-tree of ast
    TODO modified when writing our codes*)
