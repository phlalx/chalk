(***************************************************************************
 *   Copyright (C) 2005                                                    *
 *     Philippe Bidinger (Philippe.Bidinger@inrialpes.fr)                  *
 *      modified by                                                        *
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
 * File : Basesynt.mli
 *
 * Unit : Syntax
 *
 * Description : The abstract syntax of Chalk.
 *
 * Author :
 *
 * $Log: basesynt.mli,v $
 * Revision 1.1.1.1  2005/10/03 14:54:36  formel
 * creation of chalk2
 *
 * Revision 1.5  2005/09/22 21:44:41  formel
 * Major change !
 *
 * Revision 1.4  2005/07/30 15:21:21  formel
 * replication added, and more lib functions
 *
 * Revision 1.3  2005/07/29 15:33:55  formel
 *
 * Both styles of trigger-handling now compile.
 *
 * Revision 1.3  2005/07/25 11:20:53  formel
 *
 * Work on non-blocking system calls continues.
 *
 *)

open Error

(**
   The type of a Chalk process.
*)
type t =
      Nil
	(**Terminated process*)
    | New     of string * t 
	(**Instanciation of a new name*)
    | Let     of string * value *  t 
	(**Creation of a named value*)
    | ITE     of value * t * t 
	(**If...then...else...*)
    | Par     of t * t 
	(**Parallel composition*)
    | Kell    of string * t
	(**Creation of a named sub-location*)
    | Trig    of pattern * t 
	(**A trigger for message/cell reception/consumption*)
    | Value   of value
	(**A value*)
    | Msg     of string * value * Syntax.direction_msg
	(**Emission of a message along a channel*)

(**
   A trigger pattern.

   [def a<b,c,d> | e<f,g> {...}] is represented by
   [["a",["b";"c";"d"],In];["e",["f";"g"],In]]
   
*)
and pattern = 
    (string * (string list) * Syntax.direction) list * bool

and value = 
  | VNil        
  | VTrue       
  | VFalse      
  | VVmid       of  value
  | VId         of  string
  | VInt        of  int
  | VString     of  string
  | VProc       of  (string) list * t
  | VTuple      of  (value list)
  | VEq         of  value * value 
  | VMult       of  value * value
  | VPlus       of  value * value
  | VDiv        of  value * value
  | VMinus      of  value * value
  | VUMinus     of  value 
  | VNot        of  value
  | VAppli      of  value * value
  | VAnd        of  value * value
  | VOr         of  value * value
  | VApp        of  value * value
  | VMarshall   of  value
  | VUnmarshall of  value
  | VEmptyList 
  | VIsNil      of  value
  | VCons       of  value * value
  | VHead       of  value
  | VTail       of  value
  | VList       of  (value list)

val to_html : t -> string

val to_html2 : t -> (string -> string) -> string

