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
   * Description : The [Reactor] is a structure
   * which performs reactions between messages 
   * (or kells) and triggers, possibly join.
   *
   * Each [location] has its own [Reactor].
   *
   * Author : Philippe Bidinger, David Teller
   *
   * $Log: reactor.mli,v $
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
   * Revision 1.4  2005/07/27 14:39:35  formel
   *
   * Typo fixed
   *
   * Revision 1.3  2005/07/27 14:36:11  formel
   *
   * Interface updated.
   *
   * Revision 1.2  2005/07/27 11:22:15  formel
   * Runtime uses now module Reactor
   *
   * Revision 1.1  2005/07/26 20:12:45  formel
   *
   * Now starting to work on the reactor itself.
   *
   * Work on non-blocking system calls continues.
   *
*)

(**
   The type of a [Reactor].

   Use [create] to create a [Reactor].

   [('location, 'key, 'name, 'value) t]
*)
type ('location, 'key, 'name, 'value) t

(**
   A reference for quick access to the data structure associated to one 
   message/one binder.

   [('location, 'name, 'value) reference]
*)
type ('location, 'name, 'value) reference


(**
   A content which may be used for matching.
*)
type ('location, 'value) content =
    Message of 'value
      (**A message.*)
  | Kell    of 'location 
      (**A kell. In the current implementation, it must be child of the current location.*)

(**
   The type of a matcher oracle.

   A matcher oracle.

   Returns [None] in case of mismatch, usually if there is a side condition.
*)
type ('location, 'name, 'value) matcher =
    value:('location, 'value) content -> 
    ('name * 'value) list option


(**
   A handler for the continuation of exactly one trigger.

   The role of that trigger is to push the coninuation on the
   appropriate [runqueue].

   [origin] is the current location of the [Reactor] containing the
   trigger.  Note that one cannot suppose that [origin] is the
   original location of the [Reactor], as that location may have been
   passivated and reactivated as a process of a different location.
*)
type ('location, 'name, 'value) trigger_continuation_handler =
    from: 'location ->
    substitution: ('name*'value) list ->
    unit

(**
   Create a [Reactor] for use by one location.

   [create size:s owner:l] initializes the [Reactor] with a size of
   [s] for a location [l].  Note that the location may change with
   time, as [Reactor]s may be merged or duplicated as a consequence of
   passivations and reactivations.
*)
val create : size:int -> owner:'location -> ('location, 'key, 'name, 'value) t


(**
   Get a reference to the individual structure used to perform matching on a
   specific key.
*)
val get_reference : ('location, 'key, 'name, 'value) t -> 'key -> 
  ('location, 'name, 'value) reference


(**
   Pushes a trigger on the reactor, possibly causing a reaction.

   A upwards trigger accepts {b ascending} messages (i.e. messages sent
   from a sublocation).
*)
val push_trigger : 
  (('location, 'name, 'value) reference * ('location, 'name, 'value) matcher) list ->
  replicated:bool ->
  callback:('location, 'name, 'value) trigger_continuation_handler -> unit

(**
   Pushes a message or a kell on the reactor, possibly causing a reaction.

   Function [callback] is called when the reaction takes place. Its
   role is to put the continuation of the message onto the appropriate
   [runqueue].
*)
val push_content :  
  reference:('location, 'name, 'value) reference -> 
  origin:'location -> 
  content:('location,'value) content ->
  handler:(from:'location -> unit) 
  -> unit


(**
   Removes all the messages waiting for reaction originated from a specific 
  location.

   Returns the list of keys/contents for these messages.
*)
val remove_vertical_messages_from :('location, 'key, 'name, 'value) t ->
  'location -> ('key * ('location, 'value) content) list

(**
   Adds the content of one [Reactor] to another.

   The owner of [receiver] is kept as the new owner. Everything else
   is merged without losses. If this fusion causes any reaction, they
   are undertaken immediately and the appropriate callbacks are called.
*)
val merge_with : kept:('location, 'key, 'name, 'value)t ->
  discarded:('location, 'key, 'name, 'value)t -> unit

(**
   Currently not implemented
*)
val to_html : ('location, 'key, 'name, 'value) t 
  -> ('location -> string) 
  -> ('key -> string)
  -> ('name -> string)
  -> ('value -> string)
  -> string
