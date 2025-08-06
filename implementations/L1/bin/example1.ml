open L1
open Grammar
open Interpreter
let e = (Seq(Assign(Loc(0), Int(3)),
              Deref(Loc(0))))
let s = Store.(empty |> add 0 0);;

verbose_interpret (e, s)