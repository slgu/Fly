open Ast
open Sast
(* create a new env*)
let get_new_env() =
    let (env : (string, typ) Hashtbl.t) = Hashtbl.create 16
    in env

(* a multi-layer env operation *)
let init_level_env () =
    [get_new_env()]




(* append a new level env to level_env*)
let append_new_level level_env =
    get_new_env() :: level_env

let update_env level_env k v = match level_env with
    | (this_level :: arr) -> Hashtbl.add this_level k v;this_level :: arr
    | _ -> failwith ("no env internal error")

let rec search_id level_env k = match level_env with
    | [] -> failwith ("variable refered without defined" ^ k)
    | (this_level :: arr) ->
        try
            Hashtbl.find this_level k
        with
        | Not_found -> search_id arr k

let back_level level_env = match level_env with
    | [] -> failwith ("no level to be back")
    | (this_level :: arr) -> arr

(*debug a level env, just print out to the screeen*)
let debug_level_env level_env =
    let rec inner_debug level_env cnt = match level_env with
        | [] -> ()
        | (this_level :: arr) -> print_endline ("this level: " ^ (string_of_int cnt));
    in
    inner_debug level_env 0
