open Printf
open Types 

type gamma_t = (string * t) list 

let init = [] 

let lookup_type_in_context gamma n = match List.nth gamma n with 
| (_, t) -> t

let lookup_name_in_context gamma n = match List.nth gamma n with 
| (x, _) -> x

let extend gamma x t = (x, t)::gamma 

let gamma_to_str s = 
  let items = (List.fold_left (fun accum c -> 
    (match c with (x, t) -> (sprintf "%s: %s" x (Types.type_to_str t)):: accum)) [] s) in 
  let rec separated_items = (function 
    | [] -> []
    | [i] -> [i]
    | i::is -> i::", "::(separated_items is)) in 
  (List.fold_left (^) "{" (separated_items items)) ^ "}"