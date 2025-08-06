open Grammar 
open Utils

let rec step config = match config with
 | Int(n)                , s -> Int(n) , s
 | Bool(b)               , s -> Bool(b), s
 | Plus(Int(n), Int(m))  , s -> Int(n+m), s
 | Plus(Int(n), e2)      , s -> (match step (e2, s) with 
                                | e2', s' -> Plus(Int(n), e2'), s')
 | Plus(e1    , e2)      , s -> (match step (e1, s) with 
                                | e1', s' -> Plus(e1', e2)    , s')
 | Gteq(Int(n), Int(m))  , s -> Bool(n>=m), s
 | Gteq(Int(n), e2)      , s -> (match step (e2, s) with 
                                | e2', s' -> Gteq(Int(n), e2'), s')
 | Gteq(e1    , e2)      , s -> (match step (e1, s) with 
                                | e1', s' -> Gteq(e1', e2)    , s')
 | If(Bool(b), e2, e3)   , s -> if b then e2, s else e3 , s
 | If(e1     , e2, e3)   , s -> (match step (e1, s) with
                                | e1', s' -> If(e1', e2, e3)   , s')
 | Assign(Loc(l), Int(n)), s -> Skip, Store.update l (fun _ -> Some(n)) s
 | Assign(Loc(l), e)     , s -> (match step (e, s) with
                                | e', s' -> Assign(Loc(l), e'), s')
 | Deref(Loc(l))         , s -> Int(Store.find l s), s
 | Skip                  , s -> Skip               , s
 | Seq(Skip, e2)         , s -> e2                 , s
 | Seq(e1  , e2)         , s -> (match step (e1, s) with
                                | e1', s' -> Seq(e1', e2), s')
 | While(e1, e2)         , s -> If(e1, Seq(e2, While(e1, e2)), Skip), s

 let rec verbose_interpret (e, s) = 
  if is_val e then (~. (e, s)) else ((~> (e, s)) ; verbose_interpret (step (e, s)))

 let rec interpret (e, s) =
  if is_val e then (~. (e, s)) else interpret (step (e, s))
