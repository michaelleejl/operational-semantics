type loc_t = int

type sigma_t 

type t

val get: loc_t -> t -> int 

val create: int list -> t 

val create_types: Types.t list -> sigma_t

val get_type: loc_t -> sigma_t -> Types.t 

val update: int -> int -> t -> t

val store_to_str: t -> string

val loc_to_str: loc_t -> string

val sigma_to_str: sigma_t -> string