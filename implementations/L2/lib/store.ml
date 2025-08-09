open Printf
module IntMap = Map.Make(Int)

type loc_t = int 

type t = int IntMap.t 

let get k s = IntMap.find k s 

let create xs =
  let creator acc x = (match acc with 
  | (i, s) -> (i+1, IntMap.add i x s)) in
  match List.fold_left creator (0, IntMap.empty) xs with 
  | (_, s) -> s

let update k v s = IntMap.update k (fun _ -> Some(v)) s

let loc_to_str (l) = sprintf "l%i" l 

let store_to_str s = 
  (IntMap.fold (fun k v accum -> accum ^ (sprintf "%s:%i, " (loc_to_str (k)) v)) s "{") ^ "}"