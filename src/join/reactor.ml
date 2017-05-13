(***************************************************************************
 *   Copyright (C) 2005                                                    *
 *     Philippe Bidinger (Philippe.Bidinger@inrialpes.fr)                  *
 *     David Teller (D.O.Teller@sussex.ac.uk)                              *
 *                                                                         *
 *                                                                         *
 *                                                                         *
 *   This file is part of CHALK.                                           *
 *                                                                         *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Library General Public License as       *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU Library General Public     *
 *   License along with this program; if not, write to the                 *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************)

 
(**
 * File : Reactor.mli
 *
 * Unit : Reactions
 *
 * Description : 
 *
 * Author :
 *
 * $Log: reactor.ml,v $
 * Revision 1.1.1.1  2005/10/03 14:54:37  formel
 * creation of chalk2
 *
 * Revision 1.1  2005/07/29 15:33:55  formel
 *
 * Both styles of trigger-handling now compile.
 *
 * Revision 1.1  2005/07/29 13:30:06  formel
 *
 * We now have two modules for reactions: Reactor and JoinReactor.
 *
 * Revision 1.1  2005/07/26 20:12:45  formel
 *
 * Now starting to work on the reactor itself.
 *
 * Work on non-blocking system calls continues.
 *
 *)

(**
   A content which may be used for matching.
*)
type ('location, 'value) content =
    Message of 'value
      (**A message.*)
  | Kell    of 'location 
      (**A kell. In the current implementation, it must be child of the 
	 location owning this [Reactor].*)

(**
   A container for either a message or a kell.
*)
type ('location, 'name, 'value) message_container =
    {
      value  : ('location, 'value) content;
      (**The content of the message.*)
      origin : 'location;
      (**The origin of the message.*)
    }


(**
   The type of a matcher oracle.

   A matcher oracle.

   Returns [None] in case of mismatch, usually if there is a side condition.
*)
type ('location, 'name, 'value) matcher =
    value:('location, 'value) content -> 
      ('name * 'value) list option


(**
   A handler for the continuation of exactly one message.

   The role of that handler is to push the continuation on the
   appropriate [runqueue].

   [origin] is the current location of the [Reactor] containing the
   message.  Note that one cannot suppose that [origin] is the
   original location of the [Reactor], as that location may have been
   passivated and reactivated as a process of a different location.
*)


(**
   A handler for the continuation of exactly one trigger.

   The role of that trigger is to push the coninuation on the
   appropriate [runqueue].

   [from] is the current location of the [Reactor] containing the
   trigger.  Note that one cannot suppose that [from] is the
   original location of the [Reactor], as that location may have been
   passivated and reactivated as a process of a different location.
*)

type ('location, 'name, 'value) trigger_continuation_handler =
    from: 'location ->
    substitution: ('name*'value) list ->
    unit



type ('location, 'name, 'value) channel_queue = (('location, 'name, 'value) message_container, 'location)
    JoinTrigger.manager




type ('location, 'key, 'name, 'value) t = 
    {
      table : ('key, ('location, 'name, 'value)channel_queue) Hashtbl.t;
      owner : 'location
    }

type ('location, 'name, 'value) reference = ('location, 'name, 'value) channel_queue

    
let create ~size:s ~owner:o =
  {
    table = Hashtbl.create s;
    owner = o
  }

(**
   Get the reference for one channel_queue. If the reference doesn't exist yet, create it.
*)
let get_reference t k =
  try
    Hashtbl.find t.table k
  with
      _ -> 
	let reference =  JoinTrigger.create_manager t.owner;
	in
	  Hashtbl.add t.table k reference;
	  reference


let push_content ~reference:r ~origin:o ~content:c ~handler:h =
  let message = JoinTrigger.create_message 
    ~content:{value = c;origin = o}
    ~callback:h 
  in
    JoinTrigger.push_message r message


let push_trigger (b:(('a, 'b, 'c) reference * ('a, 'b, 'c) matcher) list)
    ~replicated:(repl:bool) ~callback:(c:('a, 'b, 'c) trigger_continuation_handler) 
    =
  let length = List.length b
  in
  let results       = Array.create length None        (**The list of substitutions per channel.*)
  and consumers     = Array.create length None        (**The consumers themselves*)
  and unmatched     = ref length                      (**Number of unmatched channels.*)
  and index         = ref 0                           (**Number of the current consumer.*)
  in
  let match_complete owner = 
    let (subs:('b*'c) list) =		    (*Gather all the substitutions*)
      Array.fold_left 
	(fun a x -> match x with None -> assert(false)|Some (s,_) ->(List.append s a) )
	[] results
    and msgs = 	                            (*Gather all the messages*)
      Array.fold_left 
	(fun a x -> match x with None -> assert(false)|Some (_,m) -> m::a) 
	[] results
    in 		                            (*Consume all the messages*)
      List.iter2 (fun (owner,_) message -> JoinTrigger.consume_message owner message) b msgs;
      if repl then                           (*Reinitialize the trigger*)
	begin
	  unmatched := length;
	  for i = 0 to length-1 do
	    results.(i) <- None
	  done
	end
      else                                     (*Remove all consumers*)
	begin
	  Array.iter (fun x -> match x with None -> assert(false)| Some (m,c) -> JoinTrigger.pop_consumer m c) consumers;
	end;
      (*c ~from:owner ~substitution:(List.flatten subs); (*Callback with the result*)    *)
      JoinTrigger.MatchComplete
  in
  let aux (reference,matcher) =
    let create_consumer i =
      let pusher (v:(('b* 'c) list * ('a, 'b, 'c) message_container) option) l = match v with
	  None  -> JoinTrigger.MatchMismatch
	| Some _ as z ->                          (*s : substitution, m : original message, l : current location*)
	    assert(results.(i)=None);        (*Check that we are not locally satisfied already*)
	    assert(!unmatched>0);                  (*Check that we are not fully satisfied already*)
	    results.(i) <- z;
	    decr unmatched;
	    if !unmatched>0 then
	      JoinTrigger.MatchProgressing
	    else                                   (*We are now fully satisfied*)
	      match_complete l
      and popper (m:('a, 'b, 'c) message_container) =
	let test_sanity () = match results.(i) with
	    None      -> assert(false)
	  | Some(_,x) when not (x==m) -> assert(false)
	  | _ -> assert(!unmatched>0)
	in
	  test_sanity ();
	  results.(i) <- None;
	  incr unmatched
      and oracle (msg:('a, 'b, 'c) message_container) = 
	match matcher ~value:msg.value with
	    Some (subst:('b * 'c) list) -> Some (subst,msg)
	  | _ -> None
      in
      let consumer = JoinTrigger.create_consumer ~oracle:oracle ~pusher:pusher ~popper:popper
      in
	consumers.(i) <- Some(reference,consumer);
	JoinTrigger.push_consumer reference consumer
    in
      create_consumer !index;
      incr index
  in
    List.iter aux b



	
let merge_with ~kept:a ~discarded:b =
  Hashtbl.iter (fun k c -> JoinTrigger.merge_with ~kept:(get_reference a k) ~discarded:c) b.table

let remove_vertical_messages_from t origin =
  let filter k l m c=
    if c.origin ==origin then
      (k,c.value)::l
    else
      l
  in
    Hashtbl.fold (fun k manager l -> JoinTrigger.fold_messages (filter k) l manager) t.table []

let to_html  
  (table : ('location, 'key, 'name, 'value) t )
  (location_to_html : ('location -> string)) 
  (key_to_html : ('key -> string))
  (uin_to_html : ('name -> string))
  (value_to_html : ('value -> string))
  : string = "<Not implemented yet>"
