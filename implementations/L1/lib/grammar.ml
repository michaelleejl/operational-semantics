open Printf

type store = Store.t

type loc = Loc of int 

type expr = Bool of bool | Int of int 
          | Plus of expr * expr | Gteq of expr * expr 
          | If of expr * expr * expr 
          | Assign of loc * expr 
          | Deref of loc 
          | Skip 
          | Seq of expr * expr 
          | While of expr * expr

type config = expr * store

let is_val = function
  | Bool(_)
  | Int(_) 
  | Skip   -> true
  | _      -> false


  let rec expr_to_str (e: expr)= match e with 
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