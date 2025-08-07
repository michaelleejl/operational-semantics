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

