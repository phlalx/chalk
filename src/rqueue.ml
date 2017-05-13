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
 * File :  rqueue.ml
 *
 * Unit :  Main
 *
 * Description :
 *
 * Author : Philippe Bidinger
 *
 *)

type 'a t = 'a list ref 

let push a q = q := a :: !q
			 
let pushlast a q = q :=  !q @ [a] 

let pop q =
  match !q with
      [] -> raise Not_found
    | a :: q' -> (q := q') ; a


let peek q =
  match !q with
      [] -> raise Not_found
    | a :: q' -> a

let create () = ref [] 

let is_empty q = !q = [] 

let iter iter q = List.iter iter !q

(* map and filter *)
exception Filter 

let mapandfilter iterandfilter q =
  let filter = ref [] in 
  let map = ref [] in
  let iter x =  
    try 
      map := (iterandfilter x)  :: !map 
    with 
      Filter -> filter := x :: !filter 
  in
    List.iter iter !q ; 
    q := !filter ;
    !map

let fold a b q = List.fold_left a b !q 

