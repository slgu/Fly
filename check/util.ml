(*code for generate random unique string*)
open Ast
let global_cnt = ref(0)

let rec generate_str num =
    if num < 26 then String.make 1 (Char.chr (num + 97))
    else
        let extra = (num mod 26)
        in let this_pos = String.make 1 (Char.chr (extra + 97))
        in this_pos ^ (generate_str (num / 26))

let next_random_string () =
    let nxt = (!global_cnt) + 1
    in global_cnt := nxt;
    generate_str nxt


let gen_hash_key fname type_list =
    fname ^ (List.fold_left
        (fun str item -> str ^ "@" ^ item) "" (List.map type_to_string type_list))

let rec explode = function
    "" -> []
    | s  -> (String.get s 0) :: explode (String.sub s 1 ((String.length s) - 1));;

let tablize arr =
    List.map (fun item -> "\t" ^ item) arr

let list_join string_arr join_string = match string_arr with
    | (x::y) ->
        x ^ (List.fold_left (fun res item -> res ^ "," ^ item) "" y)
    | [] -> ""

let rec zip arr1 arr2 = match arr1, arr2 with
    | [], _ -> []
    | _, [] -> []
    | (x1::y1), (x2::y2) -> (x1, x2) :: zip y1 y2

let rec drop_first arr n = match arr, n with
    | _, 0 -> arr
    | (x::y), _ ->  drop_first y (n - 1)
    | _, _ -> failwith ("drop_first error not enought arr elements")
