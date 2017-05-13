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
 * File :  lib.mli
 *
 * Unit :  Main
 *
 * Description :
 *
 * Author : Philippe Bidinger
 *
 *)

type 'location t

(* creates a Lib location initialized with a port number               *)
(* >= 1024 : "server mode" *)
(* \ 1024  means that the library does not provide communication primitives  *)
val create : int -> 'location t 	

(* connects the Lib location to the root location                      *)
(* this function must be called before the first call to "reduction"  *)
val connect_to_root :
  'location t ->
  'location Values.loc_interface ->
  unit

(* provides an access point to the Lib location to be   *)
(* used by the root location                            *)
val get_interface : 'location t -> 'location Values.loc_interface

(* schedules the Lib location : returns true iff       *)
(* runqueue is empty AND NOT server mode               *)
val reduction : 'location t -> bool

(* defines initial names                               *)
val initenv :  int -> (string * 'location Values.t) list

(* defines types of the names in initenv               *)
val typingenv : (string * Ktype.t) list 

  
