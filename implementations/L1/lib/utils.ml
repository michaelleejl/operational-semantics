open Grammar
open Printf
open Types 
open Store
open Format

exception BadStoreException

module IntMap = Map.Make(Int)

let config_to_str ((e, s): config) = 
  sprintf "<%s, %s>" (expr_to_str e) (Store.store_to_str s)

let judgement_to_str sigma e t = sprintf "%s |- %s : %s" (sigma_to_str sigma) (expr_to_str e) (type_to_str t) 

let (~>) (c: config) = print_endline (sprintf "%s ->" (config_to_str c))
let (~.) (c: config) = print_endline (config_to_str c); c 


