open L2
open Grammar
open Interpreter
open Store

let s = create [];;

let e = Let("x", Int, Int(1), Let("y", Int, Int(2), Plus(Var(1), Var(0))));;

verbose_interpret (e, s)