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
 * File :  Runtime.ml
 *
 * Unit :  Main
 *
 * Description : All runtime and initialization functions.
 *
 * Author : Philippe Bidinger
 *
 * $Log: runtime.ml,v $
 * Revision 1.1.1.1  2005/10/03 14:54:36  formel
 * creation of chalk2
 *
 * Revision 1.19  2005/09/22 21:44:41  formel
 * Major change !
 *
 * Revision 1.18  2005/07/30 18:20:11  formel
 * *** empty log message ***
 *
 * Revision 1.17  2005/07/30 15:21:21  formel
 * replication added, and more lib functions
 *
 * Revision 1.16  2005/07/30 11:38:31  formel
 *
 * Removed un-consumption bug in the handling of join triggers.
 *
 * Revision 1.15  2005/07/29 18:14:43  formel
 * Fixing CVS error.
 *
 * Revision 1.14  2005/07/29 16:11:43  formel
 * *** empty log message ***
 *
 * Revision 1.13  2005/07/29 15:33:55  formel
 *
 * Both styles of trigger-handling now compile.
 *
 * Revision 1.12  2005/07/29 15:04:30  formel
 * *** empty log message ***
 *
 * Revision 1.10  2005/07/27 11:22:15  formel
 * Runtime uses now module Reactor
 *
 * Revision 1.9  2005/07/26 20:12:45  formel
 *
 * Now starting to work on the reactor itself.
 *
 * Revision 1.8  2005/07/26 15:14:17  formel
 *
 * Currently merging modifications.
 *
 * Revision 1.7  2005/07/26 15:09:37  formel
 * passivation, reactivation, bug fixed in red msg
 *
 * Revision 1.6  2005/07/26 14:18:18  formel
 * *** empty log message ***
 *
 * Revision 1.5  2005/07/26 13:48:49  formel
 * *** empty log message ***
 *
 * Revision 1.4  2005/07/26 13:35:13  formel
 *
 * Working on the runtime.
 *
 * Revision 1.3  2005/07/25 17:35:35  formel
 * addition of "execute" system call...
 *
 * Revision 1.1  2005/07/25 16:25:39  formel
 *
 * Merging modifications wrt system events.
 *
 *)

type  runtime =  { 
  mutable statenum  : int ;
  mutable lib : Location.t Lib.t option ;
  mutable rootloc : Location.t option;
}

let some x = match x with None -> assert false | Some x -> x 

(* a runtime is defined by a lib location and a root location *)
(* statenum is used for html trace only *)
let runtime =   
  { 
    statenum = 0 ;
    lib = None ; 
    rootloc = None ;
  }
    
(* creates a library, a root location, and connects 
   them together                                   *)
let init_runtime process port  =  
  Uin.init port ;
  let lib = Lib.create port in
  let initenv = Lib.initenv port in
  let root = Location.create_root (initenv, process) in
    runtime.lib <- Some lib  ;
    runtime.rootloc <- Some root ;
    Lib.connect_to_root lib (Location.get_interface root) ;
    Location.connect_to_lib root (Lib.get_interface lib) 
    
(* in depth traversal of a tree *)
let itertree leaf subleafs fct = 
  let rec parcours_aux lleaf =
    match lleaf with
	[] -> []
      | h :: t -> 
         (fct h) ::  parcours_aux (t @ subleafs h)
  in parcours_aux [ leaf ] 

(* reduction of the library, and reduction of the tree of location
   using itertree. If reduction of the library and of every location
   returns true, then the execution is over *)
let schedule () = 
  let res = ref false in
  let iter x = let res' = Location.reduction x in res := res' && !res in
  let schedule_aux x = let _ = itertree x Location.sublocations iter
  in !res  
  in
    runtime.statenum <- runtime.statenum + 1 ;
    Action.clear () ;
    res := Lib.reduction (some runtime.lib) ;
    schedule_aux (some runtime.rootloc)

let to_html () =
  let ostate s = Html.h1 ("State " ^ (Html.int s)) in
  let oloc a i = Html.h3 ("Location "^" "^Html.id a^" "^ Uin.to_html i) in 
    
  let rec loctree_to_html location =
    let id x = x in
    let lloc = itertree location Location.sublocations (fun x -> x) in
      Html.list_to_html Location.to_html lloc in
    
  let col1 = loctree_to_html (some runtime.rootloc) in
  let col2 = Action.to_html ()	     
  in 
    ostate runtime.statenum ^ Html.table12 col1 col2 ^ Html.hr
