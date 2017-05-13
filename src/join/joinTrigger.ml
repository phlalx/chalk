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

type ('msg,'location) consumer =
    {
      match_with: 'msg -> 'location -> result;
      unmatch:    'msg -> unit;
      mutable satisfied: 'msg option;
      (**
	 If [None], this [consumer] is waiting for an appropriate message.
	 If [Some a], this [consumer] is satisfied by message [a]
      *)
    }

type ('msg,'location) message =
    {
      content: 'msg;
      callback: from:'location -> unit
    }

type ('msg,'location) manager =
    {
      mutable consumers : ('msg,'location) consumer list;
      mutable messages  : ('msg,'location) message list;
      location      : 'location;
    }

let create_manager l =
  {
    consumers = [];
    messages  = [];
    location  = l
  }

let create_consumer ~oracle:o ~pusher:p ~popper:q =
  let matcher   x = p (o x)
  and unmatcher x = q x
  in
  {
    match_with = matcher;
    unmatch    = unmatcher;
    satisfied  = None
  }

let create_message ~content:c ~callback: cb =
  {
    content = c;
    callback= cb
  }

let pop_consumer m c =
  m.consumers <- List.filter (fun x -> x!=c) m.consumers

let rec find_message_for_consumer m c =
  let rec aux l =
    match l with
	msg::t ->
	  let message = msg.content in
	  let result = c.match_with message m.location
	  in
	    (
	      match result with
		| MatchComplete ->
		    (*The consumer is already aware that he is a winner, nothing to do there*)
		    (*The winner is supposed to have 
		      = unregistered itself (if necessary)
		      = instructed every single channel under his authority to 
		        consume the corresponding messages so we should be done on this front.*)
		    MatchComplete

		| MatchProgressing ->
		    (*The consumer is satisfied*)
		    c.satisfied <- Some message;
		    MatchProgressing

		| _   ->
		    aux t
	    )
      | _ -> MatchMismatch  
  in
    aux m.messages

and give_message_to_consumers m msg =
  let message = msg.content in
  let rec aux mismatch l =
    match l with
	c::t ->
	  if c.satisfied == None then
	    let result = c.match_with message m.location
	    in
	      (
		match result with
		  | MatchComplete ->
		      (*The consumer is already aware that he is a winner, nothing to do there*)
		      (*The winner is supposed to have 
			= unregistered itself (if necessary)
			= instructed every single channel under his authority to 
		        pop the corresponding messages
			so we should be done
			on this front.*)
		      MatchComplete
		  | MatchProgressing -> 
		      c.satisfied <- Some message;
		      aux mismatch l
		  | _   ->    
		      (*
			We have a mismatch. Currently, we do not really care
		      *)
		      aux true l
	      )
	  else
	    aux mismatch l
      | _ -> 
	  if mismatch then
	    MatchMismatch
	  else
	    MatchProgressing
  in
    aux false m.consumers

and remove_message_from_consumers m message =
  let aux acc x =
    match x.satisfied with 
	Some msg when msg==message -> 
	  x.unmatch message;
	  x.satisfied <- None;
	  x::acc
      | _ -> acc
  in
    List.fold_left aux [] m.consumers

(**
   Adds the consumer and brings it up-to-date wrt already-tried messages
*)
and push_consumer m c =
  (*
    First, add the consumer.
    Then, find consumers (and, possibly, consume it along the way)
  *)
  m.consumers <- c::m.consumers;
  ignore (find_message_for_consumer m c)


and push_message m msg =
  (**
     First, add the message to the list.
     Then, distribute it (and, possibly, remove it along the way)
  *)
  m.messages <- msg::m.messages;
  ignore (give_message_to_consumers m msg)
  (*Chances are that they will also be interested by other messages*)

and forget_message m msg =
  let must_update = remove_message_from_consumers m msg
  and update c = 
    let result = find_message_for_consumer m c 
    in 
      match result with 
	  MatchComplete -> () (*Everything has been handled somewhere upstream*)
	| _ -> ()
  in
    List.iter update must_update    

and consume_message m msg =
  let rec aux l =
    match l with
	message::t ->
	  if message.content == msg then
	    message.callback ~from:m.location
	  else
	    aux t
      | _ -> ()
  in
    aux m.messages;
    forget_message m msg

let merge_with ~kept:m ~discarded:z =
  List.iter (push_consumer m) z.consumers;
  List.iter (push_message z) z.messages

let fold_messages f accu m =
  List.fold_left (fun a x -> f a x x.content) accu m.messages
