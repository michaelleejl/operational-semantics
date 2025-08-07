open L1
open Grammar
open Interpreter
open Store

let e = (Seq(Assign(Loc(0), Int(3)),
              Deref(Loc(0))))
let s = create [0];;

verbose_interpret (e, s)