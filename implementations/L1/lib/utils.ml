open Grammar
open Printf
open Types 
open Store

exception BadStoreException
exception BadTypeException

module IntMap = Map.Make(Int)

let config_to_str ((e, s): config) = 
  sprintf "<%s, %s>" (expr_to_str e) (Store.store_to_str s)

let indent n = match n with 
  | 0 -> ""
  | n -> let rec indent_i = (function 
            | 0 -> "  - "
            | m -> "  " ^ indent_i (m-1)) 
         in indent_i (n-1)

let judgement_to_str sigma e t = sprintf "%s ⊦ %s : %s" (sigma_to_str sigma) (expr_to_str e) (type_to_str t) 

let sigma_lookup_to_str sigma l t = sprintf "%s: %s ∊ %s "  (loc_to_str l) (type_to_str t) (sigma_to_str sigma) 

let (~>) (c: config) = print_endline (sprintf "%s ->" (config_to_str c))
let (~.) (c: config) = print_endline (config_to_str c); c 

let (|-) n (sigma, e, t) = "\n" ^ indent n ^ (judgement_to_str sigma e t)
let (|=) n (sigma, Loc(l), t) = match t with 
   | Ref(t) -> "\n" ^ indent n ^ (sigma_lookup_to_str sigma l (Ref t))
   | _ -> raise BadTypeException

