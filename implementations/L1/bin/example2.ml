open L1
open Grammar
open Interpreter
let e = (Plus(
            Seq(Assign(Loc(0), Int(1)),
            Int(0)),
            Seq(Assign(Loc(0), Int(2)),
            Int(0))
          ))
let s = Store.(empty |> add 0 0);;

verbose_interpret (e, s)