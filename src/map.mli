val hashtblqueueiter :
  ('a, 'b Queue.t) Hashtbl.t -> ('a -> 'b -> unit) -> unit

val itertree : 'tree -> ('tree -> 'tree list) -> ('tree -> 'a) -> ('a list)

val add_safe : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit

(* push elements of the second queue in the first *)
val concat : 'a Queue.t -> 'a Queue.t -> unit
