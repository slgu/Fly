(* these are all functional implement without side-effect*)
let get_new_env() =
    let (env : (string, string) Hashtbl.t) = Hashtbl.create 16
    in env

let append_new_level level_env =
    get_new_env() :: level_env

let update_env level_env k v = match level_env with
    | (this_level :: arr) -> Hashtbl.add this_level k v; this_level::arr
    | _ -> failwith ("no env internal error")

let rec search_id level_env k = match level_env with
    | [] -> failwith ("variable refered without defined " ^ k)
    | (this_level :: arr) ->
        try
            Hashtbl.find this_level k
        with
        | Not_found -> search_id arr k

let () =
    let level_env = ref ([get_new_env()])
    in
        level_env := append_new_level (!level_env);
        level_env := update_env (!level_env) "123" "456";
    let a = search_id (!level_env) "123"
    in print_endline a;print_endline  "fuck"
