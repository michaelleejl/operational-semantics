type loc_t = int

type t

val get: loc_t -> t -> int 

val create: int list -> t 

val update: int -> int -> t -> t

val store_to_str: t -> string

val loc_to_str: loc_t -> string