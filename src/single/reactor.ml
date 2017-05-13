
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
 * File : Reactor.ml
 *
 * Unit : Reactions
 *
 * Description : 
 *
 * Author : Philippe Bidinger
 *
*) 

type keydir = KUp | KDn | KLc | KPs

and key = Uin.t * keydir 

let key_to_html (uin,d) = 
  let s x = "(" ^ Uin.to_html uin ^ "," ^ x ^ ")" in
    match d with
	KUp -> s "up"
      | KDn -> s "dn"
      | KLc -> s "lc"
      | KPs -> s "ps"

type ('msg, 'location, 'value) reactant =
    Message of 'location * 'value
  | Kell    of 'location
  | Kell'   of 'msg list
  | Trig    of 'value * bool 
      
let reactant_to_html reac =  
  match reac with
      Message (l,v) -> "message"
    | Kell l ->  "location" 
    | Kell' _ -> "msglist" 
    | Trig (l,_) -> "trigger"
	
type ('msg, 'location, 'value) reaction =
    NoReac
  | Msglist of 'location * 'msg list 
  | ReactC of 'value * 'value
  | ReactP of 'location * 'value * 'msg list 

let reaction_to_html reac =  
  match reac with
      NoReac -> "NoReac" 
    | Msglist _ -> "Msglist" 
    | ReactC _ -> "Reaction" 
    | ReactP _ -> "Passivation" 


type ('msg, 'location, 'value) reference = ('msg, 'location, 'value) reactant Rqueue.t 
      
type ('msg, 'location, 'value) t = (key, ('msg, 'location,'value) reference) Hashtbl.t 

let create s = Hashtbl.create s 

let get_reference t key  =  
    try
      Action.gen (Action.GetRef (key_to_html key)) ;
      Hashtbl.find t key
    with
      _ -> let queue = Rqueue.create () in Hashtbl.add t key queue  ; queue

let push_reactant 
  (q :  ('msg, 'location, 'value) reference) 
  (reactant : ('msg, 'location, 'value) reactant )
  =
  Action.gen (Action.PushReactant (reactant_to_html reactant)) ; 
  let rec push_reactant_bis 
    (q :    ('msg, 'location, 'value) reference) 
    (reactant : ('msg, 'location, 'value) reactant )
    (loca : 'location option)
    (msglist : 'msg list) 
    =
    let pop_aux q repl = 
      let a = Rqueue.pop q in (if repl then Rqueue.pushlast a q)  
    in
      if Rqueue.is_empty q then  (
	match msglist, loca with
	    [], None -> Rqueue.push reactant q ; NoReac 
	  | (_::_), Some l -> Msglist (l,msglist) 
	  |_ -> assert false
      )
      else
	let firstelt = Rqueue.peek q in
	  match (firstelt, reactant) with
	      (Trig (abs,repl), Message (_,conc)) -> 
		pop_aux q repl ;
		ReactC (abs,conc)
	    | (Trig _, Trig _)        ->
		Rqueue.push reactant q ;
		NoReac
	    | (Message _, Message _)  ->
		Rqueue.push reactant q ;
		NoReac
	    | (Message (_,conc), Trig (abs,repl))  -> 
		pop_aux q repl ;
		ReactC (abs,conc)
	    | (Kell _, Kell _) -> Error.err "Two sibling kells with the same name" 
	    | (Kell' ml, Kell l ) ->
		ignore(Rqueue.pop q) ;
		assert (ml != []) ;
		push_reactant_bis q reactant (Some l) ml ;
	    | (Kell' lm, Kell' m) ->
		ignore(Rqueue.pop q) ;
		Rqueue.push (Kell' (m @ lm)) q ;
		NoReac  
	    | (Kell' _, Trig _) ->
		ignore(Rqueue.pop q) ;  
		Rqueue.push reactant q ;  
		Rqueue.push firstelt q  ;
		NoReac
	    | (Kell l, Kell' lm) ->
		Msglist (l,lm)
	    | (Trig _, Kell' _) ->
		Rqueue.push reactant q ;
		NoReac 
	    | (Kell l, Trig (abs,repl)) -> 
		pop_aux q repl ; ReactP (l,abs,msglist) 
	    | (Trig (abs,repl), Kell l) ->  
		pop_aux q repl ; ReactP (l,abs,msglist)
	    | (_,_) -> assert false 
    in
    let res = push_reactant_bis q reactant None [] in
     Action.gen (Action.Reaction (reaction_to_html res)) ;
     res

let to_html  
  (table : ('msg, 'location, 'value) t )
  (location_to_html : ('location -> string)) 
  (uin_to_html : (Uin.t -> string))
  (value_to_html : ('value -> string))
  : string = 
  
  let reactant_to_html reac =  
    match reac with
	Message (l,v) -> (location_to_html l) ^ "-" ^ value_to_html v 
      | Kell l -> location_to_html l 
      | Kell' _ -> "msglist" 
      | Trig (l,_) -> value_to_html l 
  in
  let keyval_to_html key values =
      key_to_html key ^ ":" ^ 
      Rqueue.fold (fun x y -> (reactant_to_html y) ^ " " ^ x) "" values  
  and hashtbl_to_list table = Hashtbl.fold  (fun x y z -> (x,y) :: z) table []
  in let hashtbl_to_list2 table = 
      Hashtbl.fold  
	(fun x y z -> ((keyval_to_html x y) ^ Html.br) :: z) 
	table 
	[] 
  in 
  let table_to_html table = 
    Html.list_to_html (fun x -> x) (hashtbl_to_list2 table) 
  in  
    table_to_html table  


 (* messages sur le canal (p) dans subloc (q) qu'on va remettre dans parloc *) 
let from_parent 
  (r: ('msg, 'location, 'value) t)
  (q:Uin.t)
  (create_msg: Uin.t -> Uin.t -> 'value -> 'closure) :
  'closure list =

  let extract
    (p:Uin.t)
    (q:Uin.t)
    (rq: ('msg, 'location, 'value) reactant Rqueue.t) :
    'closure list =
    let iter x = 
      match x with 
	  Message (l,v) -> create_msg p q v 
	| _ -> raise Rqueue.Filter 
    in
      Rqueue.mapandfilter iter rq 
  in 
  let res = ref [] in
  let iterator
    (k:key)
    (rq: ('msg, 'location, 'value) reference) :
    unit = 
    match k with
	(p,KUp) -> res := (extract p q rq) @ !res  
      | _ -> ()
  in
    Hashtbl.iter iterator r ;
    !res 
			
(* messages sur le canal p dans parloc qu'on va remettre ds
   la subloc (l) sur *)   
let from_child 
  (r:('msg, 'location, 'value) t)
  (l:'location)
  (create_msg : Uin.t -> 'value -> 'closure) :
  'closure list = 
  let rec extract2 
    (p:Uin.t)
    (l:'location)
    (rq : ('msg, 'location, 'value) reactant Rqueue.t) =
    let iter x = 
      match x with
	  Message (l',v) ->
	    if l' == l then ( 
	      create_msg p v  )
	    else
	      raise Rqueue.Filter
	| _ -> raise Rqueue.Filter
    in
      Rqueue.mapandfilter iter rq
  in
  let res = ref [] in
  let iterator k rq  =
    match k with 
	(p,KDn) ->
	  let msg = extract2 p l rq in
	    res := msg @ !res ;
      | _ -> ()
  in
    Hashtbl.iter iterator r ;
    !res 
      
let iter r iter =
  let iterator key rq = Rqueue.iter (iter key) rq 
  in  
  Hashtbl.iter iterator r
