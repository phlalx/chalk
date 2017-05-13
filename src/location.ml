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
 * File :  location.ml
 *
 * Unit :  Main
 *
 * Description :
 *
 * Author : Philippe Bidinger
 *
 *)

type t = 
    { 
      mutable parentlocation :  t option ;
      mutable sublocations :  t list ;
      mutable uin : Uin.t ;
      mutable runqueue :  t Values.closure Queue.t ;
      mutable reactor :
	(((Uin.t * t Values.t), t, t Values.t) Reactor.t) option; 
      mutable name : string ;               (* for trace info *)
      (*  the function to_lib put messages
                 in the "reactor" of the lib location                   *)
	     mutable to_lib : t Values.loc_interface option
    }

let reactor_size = 10

let create 
  (parentlocation :  t option)
  (name : string) 
  (uin : Uin.t) 
  (to_lib : (t Values.loc_interface) option)
  (process :  t Values.closure) =
  let l = 
    { 
      uin            = uin ;
      name           = name ;
      runqueue       = Queue.create () ; 
      parentlocation = parentlocation ;
      sublocations   = [] ; 
      reactor        = None ;
      to_lib         = to_lib 
    }
  in
    l.reactor <- Some (Reactor.create reactor_size) ;
    Queue.push process l.runqueue ; l

let some x = match x with None -> assert false | Some x -> x 

let vm_library_location =
  create
    None
    "VM"
    Uin.dummy
    None
    ([],Basesynt.Nil)

let create_root (initenv, process) =
  let thislocId = Uin.generate () in
  create
    (Some vm_library_location)
    "Top"
    thislocId
    None 
    (initenv, process)

let sublocations l = l.sublocations

let connect_to_lib l itf =
  assert (some(l.parentlocation) == vm_library_location) ;
  l.to_lib <- Some itf 

let push_closure 
  (c:t Values.closure)
  (location: t) :
  unit = 
  Action.gen  ( Action.Push location.uin ) ; 
  Queue.push c location.runqueue 
    
let push_closures
  (c:t Values.closure list)
  (location: t) :
  unit = 
  List.iter (fun x -> push_closure x location) c 
  
let kell_to_key subkelluin : Reactor.key = (subkelluin, Reactor.KPs) 

let msg_to_key uin loc dir : ( Reactor.key * t) option =  
  let findsubloc (id:Uin.t) (loc: t) :  t option = 
    let rec aux l id =  
      match l with
	  [] -> None 
	| h :: t -> if h.uin = id then (Some h) else aux t id  
    in
      aux loc.sublocations id
  in
    match dir with
	Values.MUp -> 
	  Some ((uin, Reactor.KDn), (some loc.parentlocation))
      | Values.MDown duin -> (
	  match findsubloc duin loc with
	      Some l -> Some ((uin, Reactor.KUp), l)
	    | None -> Some ((duin, Reactor.KPs), loc)
		(* returned value loc is not used in this case *)
	)
      | Values.MIn -> Some ((uin, Reactor.KLc), loc)
	  
let trig_to_key uin dir vl =
  match dir with
      Syntax.Up ->  (uin, Reactor.KUp), vl
    | Syntax.Down d -> (uin, Reactor.KDn), d :: vl
    | Syntax.In ->  (uin, Reactor.KLc), vl
    | Syntax.Pass -> (uin, Reactor.KPs), vl 

(* remove location sl from list of sublocation l  *)
let remove_subloc sl l =
  let rec remove a la =
    match la with
	[] -> []
      | u :: v ->
	  if u == a then v else u :: remove a v 
  in
    l.sublocations <- remove sl l.sublocations 

let eval_msg 
  (e:t Values.environment) 
  (id:string) 
  (v:Basesynt.value)
  (dir:Syntax.direction_msg) :
   t Values.msg =
  let dir' = match dir with
      Syntax.MUp -> Values.MUp
    | Syntax.MIn -> Values.MIn
    | Syntax.MDown id' -> Values.MDown (Values.lookupid id' e) in
  let i = Values.lookupid id e in
  let value = Values.evaluate e v in
    (i, value, dir')

	  

(* dispatch_reaction computes the reaction of two reactants    *)
(* and push the result in the runqueue of curloc               *)
(* r is a result of an insertion  in a reactor                 *)
(* SIDE EFFECT : passivation of sublocations                   *)
let rec dispatch_reaction
  (curloc : t)
  r (* reaction *) :
  unit
 =
  let reactC destloc abstraction value = 
    match abstraction with
	Values.VClos (vars,env,p) -> 
	  let subst = Values.gen_substitution vars value in 
	    push_closure ((subst @ env),p) destloc 
      | _ -> assert false 
  in
  let push_waiting_messages 
    (curloc : t) 
    (passloc : t)  
    (ml : (Uin.t * t Values.t) list ) :
    unit 
    =
    if ml != [] then Action.gen (Action.Waiting   ) ;
    let iter (curloc:t) (passloc:t) (id, v) = 
      let key = (id,Reactor.KUp) in
      let refer = Reactor.get_reference (some passloc.reactor) key in
      let res =
	Reactor.push_reactant refer (Reactor.Message (curloc,v)) 
      in 
	dispatch_reaction passloc res
    in
      List.iter (iter curloc passloc) ml 
  in
      
  let reactP curloc passloc abstraction ml =
    Action.gen  (Action.Passivate (passloc.uin, curloc.uin)) ;
    let from_parent =
      let create_msg_down (p:Uin.t) (q:Uin.t) (v:'location Values.t)  = 
	let res =
	  [("x",Values.VId p);("y",Values.VId q);("z",v)],
	  Basesynt.Msg ("x", (Basesynt.VId "z"), Syntax.MDown "y") 
	in
	  Action.gen (Action.CreateMsg (Values.clos_to_html res)) ;
	  res 
      in  
	Reactor.from_parent  (some passloc.reactor) passloc.uin create_msg_down 
    in
    let from_child =
      let create_msg_up p v = 
	let res =
	  [("x",Values.VId p);("z",(Values.removeloc v))],
	  Basesynt.Msg ("x", (Basesynt.VId "z"), Syntax.MUp) 
	in
	  Action.gen (Action.CreateMsg (Values.clos_to_html res)) ; res
      in
	Reactor.from_child (some curloc.reactor) passloc create_msg_up 
    in
      match abstraction with
	  Values.VClos ([var],env,p) ->
	    push_waiting_messages curloc passloc ml ;
	    sred passloc ;   
	    push_closures from_child passloc ;
	    remove_subloc passloc curloc ;
	    passloc.parentlocation  <-  None ;
	    let reify = Values.VPass (ref false, passloc) in
	      push_closure ((var,reify) :: env,p) curloc ;
	      push_closures from_parent curloc  
	| _ -> assert false 
  in
    match r with
	Reactor.NoReac -> ()
      | Reactor.ReactC (abstraction , value) ->
	  reactC curloc abstraction value
      | Reactor.ReactP (passloc,  abstraction, ml) ->
	  reactP curloc passloc abstraction ml
      | Reactor.Msglist (passloc,ml) ->
	  push_waiting_messages (curloc:t) passloc ml
 
and get_interface root_loc id v  =  
  assert (some(root_loc.parentlocation) == vm_library_location) ;
  let key = (id,Reactor.KUp) in
  let refer = Reactor.get_reference (some root_loc.reactor) key in
  let res =
    Reactor.push_reactant 
      refer 
      (Reactor.Message (vm_library_location,v)) 
  in 
    dispatch_reaction root_loc res

and activate 
  ( ldest  : t )
  ( lpass  : t )            (* the passivated location *) 
  ( tobecopied : bool ref ) (* flag to avoid the recreation of pointers in 
			       a location at the first reactivation      *) 
  =
  let concat inqueue =
    Queue.iter (fun x -> Queue.push x inqueue)
  in
  let active_aux ldest lpass = 
    let iter key reactant =
      let reference = Reactor.get_reference (some ldest.reactor) key in
      let reaction = Reactor.push_reactant reference reactant in
        (* check that ldest is always correct *)
	dispatch_reaction ldest reaction
    in
      ldest.sublocations <- ldest.sublocations @ lpass.sublocations ;
      Reactor.iter (some lpass.reactor) iter  ;
      concat ldest.runqueue lpass.runqueue ;
  in
    if !tobecopied then   
      (* doesn't work  *)
      let copy = Marshal.from_string (Marshal.to_string lpass []) 0 in
	active_aux ldest copy  
    else (
      tobecopied := true ; 
      active_aux ldest lpass
    ) 
    
	
(****************************************************)
(*  REDUCTION                                       *)
(****************************************************)

  
and red_new location env name process =
  let uin = Uin.generate () in
  let id =  Values.VId uin in
  let env' = (name, id) :: env in  
    Action.gen  (Action.New (name, uin)) ;
    push_closure (env',process) location 
      
and red_val location env value =
  Action.gen  Action.Val ;
  match Values.evaluate env value with
      Values.VClos (vars, u, v) ->
	assert (vars = []) ;
	push_closure (u,v) location 
    | Values.VPass (u,l) -> activate location l u 
    | _ -> assert false
	
and red_ite location e v p p' = 
  match (Values.evaluate e v) with
      Values.VTrue -> 
	Action.gen  (Action.If true) ;  
	push_closure (e,p) location
    | Values.VFalse -> 
	Action.gen  (Action.If false) ;  
	push_closure (e,p') location 
    | _ -> assert false
	
and red_par location env p1 p2 = 
  Action.gen  Action.Par ;
  push_closure (env,p1) location ; 
  push_closure (env,p2) location

and red_let location e x v p' =
  Action.gen  (Action.Let x) ;
  push_closure (((x, (Values.evaluate e v) ) :: e ), p') location
    
and red_trig location e t p =
  let (pattern, repl) = t in
  let deconstruct_pattern p =
    match p with
	[l] -> l
      | _ -> Error.err "Join patterns not implemented" 
  in
  let channel_name, variables, direction = deconstruct_pattern pattern in
  let channel_id  = Values.lookupid channel_name e in
    Action.gen (Action.Trig channel_id) ;
  let (key,vlist) = trig_to_key channel_id direction variables in
    (* vlist is the new variables list for a trigger listening down *)
  let reference = Reactor.get_reference (some location.reactor) key  in
  let res =
    Reactor.push_reactant 
      reference
      (Reactor.Trig ((Values.VClos (vlist, e, p)),repl) )
  in	
      dispatch_reaction location res
	
  and red_msg location e id v dir =
  let (id', v', dir') = eval_msg e id v dir in 
    Action.gen  (Action.Msg id') ;
    let v'' = Values.addloc v' location.uin dir' in
    let keydestopt = msg_to_key id' location dir' in 
      match keydestopt with
	  None -> assert(false)
	| Some(key,dest) when dest == vm_library_location ->
	    (*  the function location.to_lib put messages on name id'
                in the reactor of the lib location                   *)
	    (some location.to_lib) id' v'
	| Some (key,dest) ->
	    let refer = Reactor.get_reference (some dest.reactor) key in
	      match key with
		  (_,Reactor.KPs) -> 
		    ignore(
		      Reactor.push_reactant refer (Reactor.Kell' [id',v'']))
		| _ -> 
		    let res =
		      Reactor.push_reactant 
			refer 
			(Reactor.Message (location,v'')) 
		    in 
		      dispatch_reaction dest res
      
  and red_kell location e n p1 =
    let i = Values.lookupid n e in
    let newloc = create (Some location) n i None (e,p1) 
    in
      Action.gen  (Action.Kell i) ;
      location.sublocations <- newloc :: location.sublocations ;
      let key = kell_to_key newloc.uin in
      let refer = Reactor.get_reference (some location.reactor) key in
      let res = 
	Reactor.push_reactant refer (Reactor.Kell newloc)
      in
	dispatch_reaction location res
	   
  and reduction location =
  Action.gen (Action.Reduce location.uin) ;
    if Queue.is_empty location.runqueue then ( 
      Action.gen  Action.Empty_rq ; 
      true
    ) else (
      Action.gen  Action.Pop_rq ; 
      let (e,p) = Queue.pop location.runqueue in
	(
	  match p with
	      Basesynt.Nil          -> Action.gen  Action.Nil 
	    | Basesynt.Value v      -> red_val location e v
	    | Basesynt.New (x, p)   -> red_new location e x p
	    | Basesynt.Let (x,v,p)  -> red_let location e x v p
	    | Basesynt.ITE (v,p,q)  -> red_ite location e v p q
	    | Basesynt.Par (p,q)    -> red_par location e p q
	    | Basesynt.Trig (t,p)   -> red_trig location e t p
	    | Basesynt.Msg  (x,v,d) -> red_msg location e x v d
	    | Basesynt.Kell (x,p)   -> red_kell location e x p  
	);
	false
    )
      
  and sred location =
  (* in depth traversal of a tree *)
  let itertree leaf subleafs fct = 
    let rec parcours_aux lleaf =
      match lleaf with
	  [] -> []
	| h :: t -> 
            (fct h) ::  parcours_aux (t @ subleafs h)
    in parcours_aux [ leaf ] 
  in
    
  (* one subreduction of a location *)
  (* msg and triggers are pushed in a fresh runqueue *)
  let one_sred location newrq = 
    if Queue.is_empty location.runqueue then ( 
      Action.gen  Action.Empty_rq ; 
      true
    ) else (
      Action.gen  Action.Pop_rq ;
      let (e,p) = Queue.pop location.runqueue in
	(
	  match p with
	      Basesynt.Nil          -> Action.gen Action.Nil 
	    | Basesynt.Value v      -> red_val location e v 
	    | Basesynt.New (x, p)   -> red_new location e x p
	    | Basesynt.Let (x,v,p)  -> red_let location e x v p 
	    | Basesynt.ITE (v,p,q)  -> red_ite location e v p q 
	    | Basesynt.Par (p,q)    -> red_par location e p q 
	    | Basesynt.Trig _       -> Queue.push (e,p) newrq
	    | Basesynt.Msg  _       -> Queue.push (e,p) newrq 
	    | Basesynt.Kell (x,p)   -> red_kell location e x p 
	) ;
	false
    )
  in

  (* apply sred until a normal form is reached   *)
  (* ie. only messages and triggers remain *)
  (* TODO : define formally a normal form  *)
  (*  prove the terminate (if not replicated passivating trigs) *)
  let normal_sred location =
    Action.gen (Action.SReduce location.uin) ;
    let newrq = Queue.create () in
      while not (one_sred location newrq) do () done  ;
      location.runqueue <- newrq
  in

  (* normalize every sublocations of location  *) 
    itertree location sublocations normal_sred
      
    

(*****************************************)
(* to html                               *)
(*****************************************)

let oproc = Html.h4 "Runqueue" 
let otable = Html.h4 "Tables" 

let oloc a i = 
  Html.h3 ("Location " ^ " " ^ Html.id a ^ " " ^ Uin.to_html i ) 
    
let queue_to_list = Queue.fold (fun x y -> y :: x) [] 
		      
let locuin_to_html l =
  Uin.to_html l.uin
    
let to_html l = 
  (oloc l.name l.uin)   ^ 
  let col1 =
    oproc ^ (Html.list_to_html_list Values.clos_to_html 
	       (queue_to_list l.runqueue)) 
  and col2 = otable   ^
	     (Reactor.to_html
		(some l.reactor)
		locuin_to_html
		Uin.to_html
		Values.to_html ) 
	     ^ Html.br 
  in Html.table12 col1 col2 




