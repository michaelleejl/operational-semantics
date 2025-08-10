open L1
open Grammar
open Interpreter
open Inference
open Types
open Store

let e = (Seq(Assign(Loc(0), Int(3)),
              Deref(Loc(0))))
let s = create [0];;

let sigma = create_types [Ref(Int)];;

verbose_interpret (e, s);;

verbose_infer e sigma;;
