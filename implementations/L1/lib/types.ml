open Printf

type t = Unit | Bool | Int | Ref of t 

let rec type_to_str typ = match typ with
 | Unit -> "unit"
 | Bool -> "bool"
 | Int -> "int"
 | Ref(t) -> sprintf "%s ref" (type_to_str t)