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
 * File : threadpool.mli
 *
 * Unit : Threads
 *
 * Description :
 *  A thread pool is a reserve of threads. Tasks may be enqueued
 *  and will be executed as soon as a thread is available.
 *
 *  Thread pooling is necessary on many systems as these systems
 *  ofen do not allow many OCaml/native threads to be executed
 *  concurrently (typically, 300 OCaml threads is the maximum
 *  under Linux). In addition, thread creation/destruction is
 *  typically a slow task, which is avoided by thread pooling.
 *
 *  Note that the threads should not attempt to communicate, as
 *  they might not be executed concurrently. These threads are
 *  meant to be used for system calls. If communication is really
 *  needed, the best choice might be to uses [EventQueue]
 *
 * Author : David Teller (D.O.Teller@sussex.ac.uk)
 *
 * $Log: threadPool.mli,v $
 * Revision 1.1.1.1  2005/10/03 14:54:36  formel
 * creation of chalk2
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
  type t
  
  (**
     Create a thread pool to manage [size] OCaml/native threads.

     The actual threads are created lazily, prioritizing thread reuse.
  *)
  val create : size:int -> t

  (**
     Create a new task and run it as soon as a thread is available.

     The task should not throw exceptions.
  *)
  val enqueue : t -> runnable:(unit -> unit) -> unit
