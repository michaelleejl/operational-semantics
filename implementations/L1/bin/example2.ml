open L1
open Grammar
open Interpreter
open Inference
open Store

let e = (Plus(
            Seq(Assign(Loc(0), Int(1)),
            Int(0)),
            Seq(Assign(Loc(0), Int(2)),
            Int(0))
          ))
let s = create [0];;

let sigma = create_types [Ref(Int)];;

verbose_interpret (e, s);;

verbose_infer e sigma;;
