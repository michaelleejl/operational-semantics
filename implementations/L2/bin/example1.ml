open L2
open Grammar
open Interpreter
open Store

let s = create [];;

let e = Let("x", Int, Int(3), Var(0));;

verbose_interpret (e, s)