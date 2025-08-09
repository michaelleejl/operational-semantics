open Grammar
open List 

type substitution = expr list 
let rec shift_expr m (e: expr) = match e with
| Bool _
| Int _
| Deref _ 
| Skip -> e
| Plus (e1, e2) -> Plus (shift_expr m e1, shift_expr m e2)
| Gteq (e1, e2) -> Gteq (shift_expr m e1, shift_expr m e2)
| If (e1, e2, e3) -> If (shift_expr m e1, shift_expr m e2, shift_expr m e3)
| Assign (l, e) -> Assign(l, shift_expr m e)
| Seq (e1, e2) -> Seq(shift_expr m e1, shift_expr m e2)
| While (e1, e2) -> While(shift_expr m e1, shift_expr m e2)
| Fun (x, t, e) -> Fun(x, t, shift_expr (m+1) e)
| App (e1, e2) -> App(shift_expr m e1, shift_expr m e2)
| Var (n) -> if n >= m then Var(n+1) else Var(n)
| Let (x, t, e1, e2) -> Let(x, t, shift_expr (m+1) e1, shift_expr m e2)
| LetRec (x, t1, t2, (y, t3, e1), e2) -> 
  LetRec (x, t1, t2, (y, t3, shift_expr (m+2) e1), shift_expr (m+1) e2)

let rec swap_expr m (e: expr) = match e with
| Bool _
| Int _
| Deref _
| Skip -> e
| Plus (e1, e2) -> Plus (swap_expr m e1, swap_expr m e2)
| Gteq (e1, e2) -> Gteq (swap_expr m e1, swap_expr m e2)
| If (e1, e2, e3) -> If (swap_expr m e1, swap_expr m e2, swap_expr m e3)
| Assign (l, e) -> Assign (l, swap_expr m e)
| Seq (e1, e2) -> Seq (swap_expr m e1, swap_expr m e2)
| While (e1, e2) -> While (swap_expr m e1, swap_expr m e2)
| Fun (x, t, e) -> Fun(x, t, swap_expr (m+1) e)
| App (e1, e2) -> App(swap_expr m e1, swap_expr m e2)
| Var (n) when n == m -> Var(n+1)
| Var (n) when n == m+1 -> Var(m)
| Var (n) -> Var(n)
| Let (x, t, e1, e2) -> Let (x, t, (swap_expr m e1), (swap_expr (m+1) e2))
| LetRec (x, t1, t2, (y, t3, e1), e2) -> 
  LetRec (x, t1, t2, (y, t3, swap_expr (m+2) e1), swap_expr (m+1) e2)

let shift s = Var(0) :: (map (shift_expr 0) s)

let lookup (n: int) (s: substitution) = List.nth s n

let rec subst (e: expr) (s: substitution) = match e with
  | Bool _
  | Int _
  | Deref _
  | Skip  -> e
  | Plus (e1, e2) -> Plus(subst e1 s, subst e2 s)
  | Gteq (e1, e2) -> Gteq(subst e1 s, subst e2 s)
  | If (e1, e2, e3) -> If(subst e1 s, subst e2 s, subst e3 s)
  | Assign (l, e) -> Assign(l, subst e s)
  | Seq (e1, e2) -> Seq(subst e1 s, subst e2 s)
  | While (e1, e2) -> While(subst e1 s, subst e2 s)
  | Fun (x, t, e) -> Fun(x, t, subst e (shift s))
  | App (e1, e2) -> App(subst e1 s, subst e2 s)
  | Var (p) -> lookup p s
  | Let (x, t, e1, e2) -> Let(x, t, subst e1 s, subst e2 (shift s))
  | LetRec (x, t1, t2, (y, t3, e1), e2) -> 
      LetRec (x, t1, t2, (y, t3, subst e1 (shift (shift s))), subst e2 (shift s))