open L2
open Grammar
open Interpreter
open Store
open Types
open Context
open Inference

let s = create [];;

let e = Let("x", Int, Int(3), Var(0));;

let gamma = init;;

let sigma = create_types [Ref(Int)];; 

verbose_interpret (e, s);;

verbose_infer e gamma sigma;;

