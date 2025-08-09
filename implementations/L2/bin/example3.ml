open L2
open Grammar
open Interpreter
open Store

let s = create [];;

(* let val rec x:int->int = fn y:int => if true then y else x y in x 3 end *)

let e = LetRec("x", Int, Int, ("y", Int, If(Bool(true), Var(0), App(Var(1), Var(0)))), App(Var(0), Int(3)));;

verbose_interpret (e, s)