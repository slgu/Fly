open Ast
type texpr =
    Literal of int * typ
  | BoolLit of bool * typ
  | Float of float * typ
  | Id of string * typ(* id token *)
  | Set of texpr list * typ
  | Map of (texpr * texpr) list * typ
  | Array of expr list * typ
  | String of string * typ(*represent const string*)
  | Binop of texpr * op * texpr * typ
  | Unop of uop * texpr * typ
  | Call of string * texpr list * typ
  | ObjCall of string * string * texpr list * typ(*invoke a method of an object*)
  | Func of string list * texpr * typ (*lambda expr*)
  | Assign of string * texpr * typ
  | ListComprehen of texpr * string * texpr * typ (*can iterate a tuple?*)
  | Noexpr
  (*below are network specified exprs*)
  | Exec of string * typ
  | Dispatch of string * expr list * string * string * typ
  | Register of string * string * texpr list * typ
  | Chan of texpr * typ
  | Chanunop of string
  | Chanbinop of string * string
  | Fly of string * texpr list * typ
  | Flyo of string * string * texpr list * typ

type tstmt =
    Block of tstmt list
  | Expr of texpr
  | Return of texpr
  | If of texpr * tstmt list * tstmt list
  | For of texpr * texpr * texpr * tstmt list
  | Foreach of string * texpr * tstmt list (*for each*)
  | While of texpr * tstmt list



(* this is for lambda decl, with type information*)
type t_lambda_decl = {
        key: string; (*for matching*)
        fname: string; (* random hash *)
        binds: (string * typ) list;
        formals : (string * typ) list;
        body: tstmt list;
    }

type t_func_decl = {
        key: string; (* for matching*)
        fname: string;
        formals: (string * typ) list;
        body: tstmt list;
    }
