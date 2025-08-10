open Printf

module IntMap = Map.Make(Int)

type loc_t = int 

type t = int IntMap.t 

type sigma_t = Types.t IntMap.t 

let get = IntMap.find

let get_type = IntMap.find

let create xs =
  let creator acc x = (match acc with 
  | (i, s) -> (i+1, IntMap.add i x s)) in
  match List.fold_left creator (0, IntMap.empty) xs with 
  | (_, s) -> s

let create_types = create 

let update k v s = IntMap.update k (fun _ -> Some(v)) s

let loc_to_str (l) = sprintf "l%i" l 

let store_to_str s = 
  let items = (IntMap.fold (fun k v accum ->  (sprintf "%s: %i" (loc_to_str k) v):: accum) s []) in 
  let rec separated_items = (function 
    | [] -> []
    | [i] -> [i]
    | i::is -> i::", "::(separated_items is)) in 
  (List.fold_left (^) "{" (separated_items items)) ^ "}"


let sigma_to_str s = 
  let items = (IntMap.fold (fun k v accum ->  (sprintf "%s: %s" (loc_to_str k) (Types.type_to_str v)):: accum) s []) in 
  let rec separated_items = (function 
    | [] -> []
    | [i] -> [i]
    | i::is -> i::", "::(separated_items is)) in 
  (List.fold_left (^) "{" (separated_items items)) ^ "}"