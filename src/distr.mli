

val init_server : int -> unit

val resolve_host : string -> Unix.inet_addr 

val send : (Unix.inet_addr * int) -> 'a list -> bool

val get : unit -> 'a list 
