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
