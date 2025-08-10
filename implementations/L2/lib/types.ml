open Printf

type t = Unit | Int | Bool | Ref of t | Arrow of t * t 

let rec type_to_str typ = match typ with
 | Unit -> "unit"
 | Bool -> "bool"
 | Int -> "int"
 | Ref(t) -> sprintf "%s ref" (type_to_str t)
 | Arrow(t1, t2) -> sprintf "%s -> %s" (type_to_str t1) (type_to_str t2)