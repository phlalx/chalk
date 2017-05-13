
(***************************************************************************
 *   Copyright (C) 2005                                                    *
 *     Philippe Bidinger (Philippe.Bidinger@inrialpes.fr)                  *
 *      modified by                                                        *
 *     David Teller (D.O.Teller@sussex.ac.uk)                              *
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
 * File : ktype.mli
 *
 * Unit : Type checker
 *
 * Description : Definition of object-level types.
 *
 * Author : Philippe Bidinger (Philippe.Bidinger@inrialpes.fr)
 *
 * Documentation : David Teller (D.O.Teller@sussex.ac.uk)
 *
 * $Log: ktype.mli,v $
 * Revision 1.1.1.1  2005/10/03 14:54:36  formel
 * creation of chalk2
 *
 * Revision 1.2  2005/07/22 19:21:16  formel
 * Starting to work on asynchronous event handling.
 *
 *
 **)
 
(*Object-level types*)
type t =
    TVar of string      (* Type variable. TVar s is a type variable with *type* name s.*)
  | TUnit               (**)
  | TBool
  | TVmid
  | TString
  | TInt
  | TKell 
  | TProc
  | TTuple of t list
(**
 *  Channel type. 
 * In a channel of type TChan(s,r,d),
 ** s is the type of messages carried by channel s
 ** r is the result type : for an asynchroneous channel, this is always TProc. For a functional channel, this is the result type of
    the function, which may also be TProc
 ** d is the nature of the channel : TAsync for an asnychroneous channel, TSync for a functional channel.
 *)
  | TChan of t * t * t
  | TList of t 
  | TArrow of t * t

(* not really types *)
  | TAsync
  | TSync
  | TReply of string * t

val ischanneltype : t -> bool

(* generate a fresh Var type *)
val generate : unit -> t

val to_html : t -> string
   
val to_string : t -> string
   

