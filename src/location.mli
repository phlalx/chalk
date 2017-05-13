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
 * File :  location.mli
 *
 * Unit :  Main
 *
 * Description :
 *
 * Author : Philippe Bidinger
 *
 *)

type t 

(* create the root location, initialized with a closure  *)
val create_root : t Values.closure -> t 

(* connect the root location to the Lib location *)
(* only the root location can be connected       *)
(* this function must be called before the first *)
(* call to "reduction"                           *)
val connect_to_lib : t -> t Values.loc_interface -> unit

(* provide an access point to the root location to be *)
(* used by the Lib location                           *)
val get_interface : t -> t Values.loc_interface 

(* returns the sublocation of a location *)
val sublocations : t -> t list

(* schedule a location : returns true iff the runqueue is    *)
(* empty                                                     *)
(* sublocations of a location t may change after a reduction *)
(* beware of the order of invocations of "sublocations" and 
   "reduction" *)
val reduction : t -> bool 

val to_html : t -> string
