open L1
open Grammar
open Interpreter
open Store

let e = (Plus(
            Seq(Assign(Loc(0), Int(1)),
            Int(0)),
            Seq(Assign(Loc(0), Int(2)),
            Int(0))
          ))
let s = create [0];;

verbose_interpret (e, s)