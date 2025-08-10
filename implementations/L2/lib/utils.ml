open Grammar
open Context
open Types 
open Store
open Printf

exception BadStoreException
exception BadTypeException 

let resolve scope n = List.nth scope n 

let indent n = match n with 
  | 0 -> ""
  | n -> let rec indent_i = (function 
            | 0 -> "  - "
            | m -> "  " ^ indent_i (m-1)) 
         in indent_i (n-1)

let rec expr_to_str_in_scope (e: expr) scope = match e with 
   | Bool(b)    -> sprintf "%B" b 
   | Int(n)      -> sprintf "%i" n
   | Plus(e1, e2) -> 
      sprintf "(%s)+(%s)" (expr_to_str_in_scope e1 scope) (expr_to_str_in_scope e2 scope)
   | Gteq(e1, e2) -> 
      sprintf "(%s)≥(%s)" (expr_to_str_in_scope e1 scope) (expr_to_str_in_scope e2 scope)
   | If(e1, e2, e3) -> 
      sprintf "if (%s) then (%s) else (%s)" 
         (expr_to_str_in_scope e1 scope) 
         (expr_to_str_in_scope e2 scope)
         (expr_to_str_in_scope e3 scope)
   | Assign(Loc(l), e) -> sprintf "%s := %s" (Store.loc_to_str l) (expr_to_str_in_scope e scope)
   | Deref(Loc(l)) -> sprintf "!%s" (Store.loc_to_str l) 
   | Skip  -> "skip"
   | Seq(e1, e2) -> 
      sprintf "%s;%s" (expr_to_str_in_scope e1 scope) (expr_to_str_in_scope e2 scope)
   | While(e1, e2) ->
      sprintf "while %s do %s" (expr_to_str_in_scope e1 scope) (expr_to_str_in_scope e2 scope)
   | Fun (x, t, e) -> sprintf "fun %s: %s => %s" x (type_to_str t) (expr_to_str_in_scope e (x::scope))
   | App(e1, e2) -> sprintf "(%s) (%s)" (expr_to_str_in_scope e1 scope) (expr_to_str_in_scope e2 scope)
   | Let(x, t, e1, e2) -> sprintf "let val %s:%s = %s in %s" x (type_to_str t) (expr_to_str_in_scope e1 scope) (expr_to_str_in_scope e2 (x::scope))
   | LetRec(x, t1, t2, (y, t3, e1), e2) -> sprintf "let val rec %s:%s = %s in %s" x (type_to_str (Arrow (t1, t2))) (expr_to_str_in_scope (Fun (y, t3, e1)) (x::scope)) (expr_to_str_in_scope e2 (x::scope))
   | Var(n) -> sprintf "%s" (resolve scope n) 

let expr_to_str e = expr_to_str_in_scope e []

 let config_to_str ((e, s): config) = 
  sprintf "<%s, %s>" (expr_to_str e) (Store.store_to_str s)

let judgement_to_str sigma gamma e t = sprintf "%s ; %s ⊦ %s : %s" (sigma_to_str sigma) (gamma_to_str gamma) (expr_to_str_in_scope e (List.map (fun (x, _) -> x) gamma)) (type_to_str t) 

let sigma_lookup_to_str sigma l t = sprintf "%s: %s ∊ %s "  (loc_to_str l) (type_to_str t) (sigma_to_str sigma) 

let (~>) (c: config) = print_endline (sprintf "%s ->" (config_to_str c))
let (~.) (c: config) = print_endline (config_to_str c); c 

let (|-) n (sigma, gamma, e, t) = "\n" ^ indent n ^ (judgement_to_str sigma gamma e t)
let (|=) n (sigma, Loc(l), t) = match t with 
   | Ref(t) -> "\n" ^ indent n ^ (sigma_lookup_to_str sigma l (Ref t))
   | _ -> raise BadTypeException

let (|+) n (gamma, x, t) =  "\n" ^ indent n ^ (sprintf "%s: %s ∊ %s "  x (type_to_str t) (gamma_to_str gamma))

