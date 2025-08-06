open Grammar
open Printf

exception BadStoreException

let loc_to_str (Loc l) = sprintf "l%i" l 
let rec expr_to_str = function
 | Bool(b)    -> sprintf "%B" b 
 | Int(n)      -> sprintf "%i" n
 | Plus(e1, e2) -> 
    sprintf "(%s)+(%s)" (expr_to_str e1) (expr_to_str e2)
 | Gteq(e1, e2) -> 
    sprintf "(%s)≥(%s)" (expr_to_str e1) (expr_to_str e2)
 | If(e1, e2, e3) -> 
    sprintf "if (%s) then (%s) else (%s)" 
      (expr_to_str e1) 
      (expr_to_str e2)
      (expr_to_str e3)
 | Assign(l, e) -> sprintf "%s := %s" (loc_to_str l) (expr_to_str e)
 | Deref(l) -> sprintf "!%s" (loc_to_str l) 
 | Skip  -> "skip"
 | Seq(e1, e2) -> 
    sprintf "%s;%s" (expr_to_str e1) (expr_to_str e2)
 | While(e1, e2) ->
    sprintf "while %s do %s" (expr_to_str e1) (expr_to_str e2)

let store_to_str s = 
  (Store.fold (fun k v accum -> accum ^ (sprintf "%s:%i, " (loc_to_str (Loc k)) v)) s "{") ^ "}"

let config_to_str ((e, s): config) = 
  sprintf "<%s, %s>" (expr_to_str e) (store_to_str s)

let (~>) (c: config) = print_endline (sprintf "%s ->" (config_to_str c))
let (~.) (c: config) = print_endline (config_to_str c); c 



