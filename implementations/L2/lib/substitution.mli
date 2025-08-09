open Grammar

type substitution = expr list

val subst: expr -> substitution -> expr 

val swap_expr: int -> expr -> expr 

val shift_expr : int -> expr -> expr 