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
 * Revision 1.5  2005/07/29 13:31:11  formel
 * *** empty log message ***
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

type keydir = KUp | KDn | KLc | KPs

and key = Uin.t * keydir 

type ('msg, 'location, 'value) t

(**
   A reference to a reaction queue
*)

type ('msg, 'location, 'value) reference

(**
   A reaction queue element
   Message : 'location is the location that emitted the msg. 'value
    is its content.
   Kell : 'location is the location corresponding to a kell
   Trig : 'value is the abstraction corresponding to a trigger, bool
          = true if the trigger is replicated
*)

type ('msg, 'location, 'value) reactant =
    Message of 'location * 'value
  | Kell    of 'location   
  | Kell'   of 'msg list 
  | Trig    of 'value * bool 

(**
  Possible returned value of an insertion in a reactor
  NoReac : no reaction
  ReactC : communication.
  'value(1) is the abstraction, 'value(2) is the value of the message 
  ReactP : passivation. 'location is the location to be passivated,
    'value is the abstraction.
*)
type ('msg, 'location, 'value) reaction =
    NoReac
  | Msglist of 'location * 'msg list
  | ReactC of 'value * 'value
  | ReactP of 'location * 'value  * 'msg list

(**
   Create a [Reactor] for use by one location.
   [create size:s] initializes the [Reactor] with a size of [s].
*)
val create : int -> ('msg, 'location, 'value) t

(**
   Get a reference to a reaction queue associated to a specific key
*)
val get_reference :
  ('msg,'location, 'value) t -> key -> ('msg, 'location, 'value) reference


(* extract messages from a child reactor r of a child location of 
   name q                                                          *) 
(* result is to be put in the parent location *)
val from_parent : 
  ('msg, 'location, 'value) t -> (* r *)
  Uin.t -> (* q *) 
  (Uin.t -> Uin.t -> 'value -> 'closure)  ->
  'closure list 

(* extract messages from a parent reactor r, emitted from a sublocation l *)
(* result is to be put in the passivated location *)
val from_child :
  ('msg, 'location, 'value) t -> (* r *)
  'location -> (* l *)
  (Uin.t -> 'value -> 'closure) ->
  'closure list


val iter : 
  ('msg, 'location, 'value) t -> 
  (key -> ('msg, 'location, 'value) reactant -> unit )  ->
  unit

(**
   Pushes a reactant on the reactor
*)
val push_reactant : 
  ('msg, 'location, 'value) reference ->
  ('msg, 'location, 'value) reactant ->
  ('msg, 'location, 'value) reaction
  
val to_html : ('msg, 'location,  'value) t 
  -> ('location -> string) 
  -> (Uin.t -> string)
  -> ('value -> string)
  -> string
 



