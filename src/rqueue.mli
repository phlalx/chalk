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
 * File :  rqueue.mli
 *
 * Unit :  Main
 *
 * Description : FIFO queue
 *
 * Author : Philippe Bidinger
 *
 *)


type 'a t

val push : 'a -> 'a t ->  unit 

val pushlast : 'a -> 'a t ->  unit 
			 
val pop : 'a t -> 'a

val peek : 'a t -> 'a 

val create : unit -> 'a t 

val is_empty : 'a t -> bool 

val iter : ('a -> unit) -> 'a t -> unit 

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t  -> 'a   


exception Filter 

(* iter : 'a -> 'b is an iterator that may raise a filter exception.    *)
(* when iter does not raise a filter exception, the corresponding 
   elements of the queue ('a t) are suppressed from the queue and are
  not mapped. *)
val mapandfilter :
  ('a -> 'b) ->
  'a t ->
  'b list 




