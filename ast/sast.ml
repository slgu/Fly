open Ast
type texpr =
    TLiteral of int
  | TBoolLit of bool
  | TFloat of float
  | TId of string * typ(* id token *)
  | TSet of texpr list * typ
  | TMap of (texpr * texpr) list * (typ * typ)
  | TArray of expr list * typ
  | TString of string * typ(*represent const string*)
  | TBinop of (texpr * op * texpr) * typ
  | TUnop of (uop * texpr) * typ
  | TCall of (string * texpr list) * typ
  | TObjCall of (string * string * texpr list) * typ(*invoke a method of an object*)
  | TFunc of (string list * texpr) * typ (*lambda expr*)
  | TAssign of (string * texpr) * typ
  | TListComprehen of (texpr * string * texpr) * typ (*can iterate a tuple?*)
  | TNoexpr
  (*below are network specified exprs*)
  | TExec of string * typ
  | TDispatch of (string * expr list * string * string) * typ
  | TRegister of (string * string * texpr list) * typ
  | TChan of texpr * typ
  | TChanunop of string
  | TChanbinop of string * string
  | TFly of (string * texpr list) * typ
  | TFlyo of (string * string * texpr list) * typ

type tstmt =
    TBlock of tstmt list
  | TExpr of texpr
  | TReturn of texpr
  | TIf of texpr * tstmt list * tstmt list
  | TFor of texpr * texpr * texpr * tstmt list
  | TForeach of string * texpr * tstmt list (*for each*)
  | TWhile of texpr * tstmt list



(* this is for lambda decl, with type information*)
type t_lambda_decl = {
        ltkey: string; (*for matching*)
        ltfname: string; (* random hash *)
        ltbinds: (string * typ) list;
        ltformals : (string * typ) list;
        ltbody: tstmt list;
        ltret: typ (* the return value*)
    }

type t_func_decl = {
        ttkey: string; (* for matching*)
        fname: string;
        tformals: (string * typ) list;
        tbody: tstmt list;
        tret: typ (*the return value type*)
    }
