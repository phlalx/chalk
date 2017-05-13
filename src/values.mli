
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
 * File :  values.mli
 *
 * Unit :  Main
 *
 * Description :
 *
 * Author : Philippe Bidinger
 *
 *)

type 'location t = 
    VNil
  | VTrue
  | VFalse
  | VVmid   of (string * int)
  | VId     of Uin.t
  | VInt    of int
  | VString of string
  | VTuple  of ('location t list)
  | VClos   of (string list * 'location environment * Basesynt.t) 
  | VPass   of bool ref * 'location    
  | VList   of ('location t list)

and 'location msg = (Uin.t * 'location t * direction_msg)

and 'location loc_interface = Uin.t -> 'location t -> unit 

and 'location environment = (string * 'location t) list 
  
and 'location closure = 'location environment * Basesynt.t

and direction_msg = MUp | MIn | MDown of Uin.t

val lookupid : string -> 'location environment -> Uin.t 

val evaluate : 'location environment -> Basesynt.value -> 'location t

val to_string : 'location t -> string

val to_html : 'location t -> string

val clos_to_html : 'location closure -> string

val addloc : 'location t  -> Uin.t -> direction_msg -> 'location t

val removeloc : 'location t -> 'location t

val gen_substitution : string list -> 'location t -> 'location environment
