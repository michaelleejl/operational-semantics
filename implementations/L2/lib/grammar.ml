open Types 

type store = Store.t

type loc = Loc of int 
and param = string 
and lambda = param * t * expr 
and expr = Bool of bool | Int of int 
          | Plus of expr * expr | Gteq of expr * expr 
          | If of expr * expr * expr 
          | Assign of loc * expr 
          | Deref of loc 
          | Skip 
          | Seq of expr * expr 
          | While of expr * expr
          | Fun of lambda
          | App of expr * expr 
          | Var of int
          | Let of param * t * expr * expr 
          | LetRec of param * t * t * lambda * expr

type config = expr * store

let is_val = function
  | Bool(_)
  | Int(_) 
  | Skip   
  | Fun(_, _, _) -> true
  | _      -> false

