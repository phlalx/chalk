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
 * File : threadpool.ml
 *
 * Unit : Threads
 *
 * Description :
 *  Implementation of a [ThreadPool]
 *
 * Author : David Teller (D.O.Teller@sussex.ac.uk)
 *
 * $Log: threadPool.ml,v $
 * Revision 1.1.1.1  2005/10/03 14:54:36  formel
 * creation of chalk2
 *
 * Revision 1.3  2005/07/27 11:22:15  formel
 * Runtime uses now module Reactor
 *
 * Revision 1.2  2005/07/26 20:12:45  formel
 *
 * Now starting to work on the reactor itself.
 *
 * Revision 1.1  2005/07/26 13:35:13  formel
 *
 * Working on the runtime.
 *
 * Revision 1.1  2005/07/25 11:20:53  formel
 *
 * Work on non-blocking system calls continues.
 *
 * Revision 1.2  2005/07/22 19:21:16  formel
 * Starting to work on asynchronous event handling.
 *
 *
 *)

(**
 A reserve of threads for system calls.
*)
  (**The type of a thread pool.*)
  type t = 
      {
	mutex         : Mutex.t;
	task_queue    : (unit -> unit) Queue.t;
	thread_created   : Thread.t Queue.t;
	thread_queue  : (unit -> unit) Event.channel Queue.t;
	mutable empty : int
      }
  
  (**
     Create a thread pool above [size] OCaml/native threads.
     The threads themselves are created lazily.
  *)
  let create ~size:s =
    {
      mutex        = Mutex.create ();
      task_queue   = Queue.create ();
      thread_created = Queue.create();
      thread_queue = Queue.create ();
      empty        = s
    }
    

  let task t channel =
    let get_task_from_queue () =
      if Queue.is_empty t.task_queue then      (*If there is no task waiting, prepare to wait for one.*)
	let _ = Queue.push channel t.thread_queue 
	in
	  None
      else                                      (*If there is a task waiting, take it.*)
	Some(Queue.pop t.task_queue)
    in
    let get_next_task () =
      Mutex.lock t.mutex;
      let from_queue = get_task_from_queue ()
      in
	Mutex.unlock t.mutex;
	match from_queue with
	  None   -> Event.sync (Event.receive channel)
	| Some x -> x
    in
      (*Error.log "Starting thread";*)
      while true do
	let next_task = get_next_task() 
	in
	  try
	    next_task();
	  with
	      _ -> ()
      done
	    
  (**Create a new task and run it as soon as a thread is available.*)
  let enqueue t ~runnable:r =
    (*Error.log "Entering enqueue";*)
    Mutex.lock t.mutex;
    (
      try
	if Queue.is_empty t.thread_queue=false then   (*If a thread is currently marked as waiting, use it*)	  
	  let signal = Queue.pop t.thread_queue
	  in
	    ignore (Event.sync (Event.send signal r))
	else 
	  (
	    Queue.push r t.task_queue;                  (*Otherwise, put the task on hold*)
	    if t.empty>0 then                         (*If possible, create a channel to run it*)
	      let channel = Event.new_channel ()
	      in
	      let f () = task t channel
	      in
		t.empty <- t.empty - 1;
		Queue.push (Thread.create f ()) t.thread_created;
	    else
	      (
	      )
	  )
      with 
	|	_ -> ()
    );
    Mutex.unlock t.mutex
    (*;
    Error.log "Leaving enqueue"*)
