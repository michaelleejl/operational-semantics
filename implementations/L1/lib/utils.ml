open Grammar
open Printf

exception BadStoreException

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
 | Assign(Loc(l), e) -> sprintf "%s := %s" (Store.loc_to_str l) (expr_to_str e)
 | Deref(Loc(l)) -> sprintf "!%s" (Store.loc_to_str l) 
 | Skip  -> "skip"
 | Seq(e1, e2) -> 
    sprintf "%s;%s" (expr_to_str e1) (expr_to_str e2)
 | While(e1, e2) ->
    sprintf "while %s do %s" (expr_to_str e1) (expr_to_str e2)

let config_to_str ((e, s): config) = 
  sprintf "<%s, %s>" (expr_to_str e) (Store.store_to_str s)

let (~>) (c: config) = print_endline (sprintf "%s ->" (config_to_str c))
let (~.) (c: config) = print_endline (config_to_str c); c 



