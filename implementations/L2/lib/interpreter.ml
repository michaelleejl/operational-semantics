open Grammar 
open Store
open Utils
open Substitution

exception Stuck 

let rec step config = match config with
  | Int (n)                , s -> Int(n) , s
  | Bool (b)               , s -> Bool(b), s
  | Plus (Int(n), Int(m))  , s -> Int(n+m), s
  | Plus (Int(n), e2)      , s -> (match step (e2, s) with 
                                | e2', s' -> Plus(Int(n), e2'), s')
  | Plus (e1    , e2)      , s -> (match step (e1, s) with 
                                | e1', s' -> Plus(e1', e2)    , s')
  | Gteq (Int(n), Int(m))  , s -> Bool(n>=m), s
  | Gteq (Int(n), e2)      , s -> (match step (e2, s) with 
                                | e2', s' -> Gteq(Int(n), e2'), s')
  | Gteq (e1    , e2)      , s -> (match step (e1, s) with 
                                | e1', s' -> Gteq(e1', e2)    , s')
  | If (Bool(b), e2, e3)   , s -> if b then e2, s else e3 , s
  | If (e1     , e2, e3)   , s -> (match step (e1, s) with
                                | e1', s' -> If(e1', e2, e3)   , s')
  | Assign (Loc(l), Int(n)), s -> Skip, update l n s
  | Assign (Loc(l), e)     , s -> (match step (e, s) with
                                | e', s' -> Assign(Loc(l), e'), s')
  | Deref (Loc(l))         , s -> Int(get l s), s
  | Skip                  , s -> Skip               , s
  | Seq (Skip, e2)         , s -> e2                 , s
  | Seq (e1  , e2)         , s -> (match step (e1, s) with
                                | e1', s' -> Seq(e1', e2), s')
  | While (e1, e2)         , s -> If(e1, Seq(e2, While(e1, e2)), Skip), s
  | Fun (x, t, e)          , s -> Fun(x, t, e), s
  | App (Fun(_, _, e), e2) , s 
    when is_val e2 -> (subst e [e2], s)
  | App (Fun(x, t, e), e2) , s 
    when not (is_val e2) -> (match step (e2, s) with
                          | e2', s' -> App(Fun(x, t, e), e2'), s')
  | App (e1, e2)           , s -> (match step (e1, s) with
                                | e1', s' -> App(e1', e2), s')
  | Let (_, _, e1, e2)     , s
    when is_val e1 -> subst e2 [e1], s
  | Let (x, t, e1, e2)     , s -> (match step (e1, s) with
                            | e1', s' -> Let(x, t, e1', e2), s')
  | LetRec(x, t1, t2, (y, t3, e1), e2), s ->
    let f = Fun(y, t3, LetRec(x, t1, t2, (y, t3, shift_expr 2 e1), (swap_expr 0 e1))) in 
    subst e2 [f], s
  | Var (_)                , _ -> raise Stuck 

 let verbose_interpret (e, s) = 
  let rec verbose_interpret_i (e, s) = 
    if is_val e then (~. (e, s)) else ((~> (e, s)) ; verbose_interpret_i (step (e, s)))
  in print_endline("");  verbose_interpret_i(e, s)

let debug_interpret (e, s) n = 
  let rec debug_interpret_i (e, s) m = 
    if m == 0 then (e, s) else 
      if is_val e then (~. (e, s)) else ((~> (e, s)) ; debug_interpret_i (step (e, s)) (m-1))
  in print_endline("");  debug_interpret_i(e, s) n 

 let interpret (e, s) =
  let rec interpret_i (e, s) = 
    if is_val e then (~. (e, s)) else interpret_i (step (e, s))
  in print_endline("");  interpret_i(e, s)
