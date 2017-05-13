(**
   
*)
type ('msg,'location) manager


type result =
    MatchProgressing
      (** The substitution has been taken into account but matching
	  is not complete.  If new messages are made available, they
	  should be queued until this message is actually consumed.
      *)
  | MatchMismatch
      (** The substitution has been rejected (usually because of an
	  unsatisfied [when] statement) by at least one of the
	  components of this [handler].
	  
	  Further messages should be tried for matching, as they may
	  fulfill the condition.
      *)
  | MatchComplete
      (** The substitution is complete, the [handler] has already
	  instructed the [reactor] to remove consumed messages and
	  possibly the trigger and to unleash continuations.*)
      
type ('msg,'location) consumer

type ('msg,'location) message

val create_manager: 'location -> ('msg,'location) manager

val create_consumer: oracle:('msg -> 'b) ->
  pusher:('b -> 'location -> result) -> popper:('msg -> unit) -> 
  ('msg,'location) consumer

val create_message: content:'msg -> callback:(from:'location->unit) -> ('msg,'location) message


(**
   Add a consumer to this channel manager.

   This may cause a reaction. If so, all the results of the reaction should be handled
   by the [popper] of the consumer(s) and the [callback] of the message(s).
*)
val push_consumer: ('msg,'location) manager -> ('msg,'location) consumer -> unit


val pop_consumer: ('msg,'location) manager -> ('msg,'location) consumer -> unit

(**
   Add a message to this channel manager.

   This may cause a reaction. If so, all the results of the reaction should be handled
   by the [popper] of the consumer(s) and the [callback] of the message(s).
*)
val push_message: ('msg,'location) manager -> ('msg,'location) message -> unit

(**
   Consume a message.

   The message is removed from the list, it's [callback] is invoked and if any [consumer]
   had accepted this message, other messages are offered to that [consumer].
*)
val consume_message: ('msg, 'location) manager -> 'msg -> unit

(*val consume_message: ('msg, 'location) consumer -> 'msg -> unit*)

(**
   Remove a message.

   The message is removed from the list, it's [callback] is {b not} invoked and if any [consumer]
   had accepted this message, other messages are offered to that [consumer].
*)
val forget_message: ('msg, 'location) manager -> 'msg -> unit

val merge_with: kept:('msg,'location) manager -> discarded:('msg, 'location) manager -> unit

val fold_messages: ('a -> ('b,'c) message -> 'b -> 'a) -> 'a -> ('b,'c) manager -> 'a
