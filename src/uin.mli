type t 

val init : int -> unit

val generate : unit -> t

val generate_well_known : int -> t 

val dummy : t

val to_string : t -> string

val to_html : t -> string

val to_name : t -> string
