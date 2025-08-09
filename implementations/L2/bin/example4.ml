open L2
open Grammar
open Interpreter
open Store
(* let val rec x:int->int = (fn y:int => if y >=1 then y + (x (y+ -1)) else 0) in x 3 end
*)

let s = create [];;

let e = LetRec("triangle", Int, Int, 
                ("x", Int, If(Gteq(Var(0), Int(1)), 
                              Plus(Var(0), App(Var(1), Plus(Var(0), Int(-1)))),
                              Int(0))),
                App(Var(0), Int(3)));;

verbose_interpret (e, s);;