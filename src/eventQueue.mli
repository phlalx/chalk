(***************************************************************************
 *   Copyright (C) 2005                                                    *
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

 
(*
 * File : events.mli
 *
 * Unit : Threads
 *
 * Description : Interface of a event handler. In order to avoid costly synchronizations
 * before each operation on one of the reactors, we use a thread event system. Whenever
 * a thread wishes to perform an (asynchronous) operation which requires being in the
 * critical section, it should post this operation as an event. At some point, the operation
 * will be executed by the thread owning the event queue.
 *
 * Author : David Teller (D.O.Teller@sussex.ac.uk)
 *
 * $Log: eventQueue.mli,v $
 * Revision 1.1.1.1  2005/10/03 14:54:36  formel
 * creation of chalk2
 *
 * Revision 1.2  2005/07/26 13:35:13  formel
 *
 * Working on the runtime.
 *
 * Revision 1.1  2005/07/25 16:27:40  formel
 *
 * Updated event queue mechanism.
 *
 * Revision 1.1  2005/07/25 11:20:53  formel
 *
 * Work on non-blocking system calls continues.
 *
 *
 *)

(**
 * The type of an event queue for events of type 'a.
 **) 
type 'a t

(**
 * Possible outcomes of an attempt at getting the next
 * available event.
 *)
type 'a event =
	QueueEmpty
|	QueueLocked
|	NextEvent of 'a

(**
 * Create a new event queue.
 *
 * The executing thread becomes the owner thread for that queue.
 *)
val create : unit -> 'a t

(**
 * Add an event to the queue.
 *
 * This function may be called from any thread.
 *)
val push : 'a t -> 'a ->  unit
(**
   Add several events to the queue.

   This is faster than calling [push] several times in a row.
*)
val push_many : 'a t -> 'a list -> unit

(**
  Get the next element of the queue.
 
  This function may only be called by the owner thread.
 *)
val pop  : 'a t -> 'a event

(**
  Get all waiting elements from the queue.
  
  This function may only be called by the owner thread.
 *)
val pop_all : 'a t -> 'a Queue.t option

