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
 * File :  values.ml
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

let lookup id e = 
  try 
    List.assoc id e 
  with 
    Not_found -> assert false

let lookupid id e =
  match (lookup id e) with
     VId i ->  i
   | _ -> assert false

let booltovbool b =
  match b with
      true -> VTrue
    | false -> VFalse
	
let rec evaluate_int e value =
  match (evaluate e value) with
      VInt s -> s
    | _ -> assert false

and evaluate_abstraction e v =
  match (evaluate e v) with
      VClos (vars, env, p) -> (
	match vars with
	    h :: t -> h, t, env, p 
	  | _ -> assert false )
    | _ -> assert false
	
and evaluate_closure e v =
  match (evaluate e v) with
    VClos (vars, env, p) -> (
	match vars with
	    h :: t -> assert false
	  | [] -> env, p  )
  | _ -> assert false

and evaluate_vmid e value =
  match (evaluate e value) with
      VTuple [ VString s; VInt i ] -> s, i
    | _ -> assert false
	
and evaluate_string e value =
  match (evaluate e value) with
      VString s -> s
    | _ -> assert false
	
and evaluate_bool e value =
  match (evaluate e value) with
      VTrue -> true
    | VFalse -> false
    | _ -> assert false

and evaluate_list e value =
  match (evaluate e value) with
    | VList l -> l
    | _ -> assert false

and evaluate e value = 
  let eval_binary_int v1 v2 op  =
    VInt ( op (evaluate_int e v1) (evaluate_int e v2))
  and eval_binary_bool v1 v2 op =
    booltovbool (op (evaluate_bool e v1) (evaluate_bool e v2))
  in
  match value with
      Basesynt.VNil          -> VNil
    | Basesynt.VTrue         -> VTrue
    | Basesynt.VFalse        -> VFalse
    | Basesynt.VEmptyList    -> VList []          
    | Basesynt.VVmid (v)      -> VVmid (evaluate_vmid e v) 
    | Basesynt.VId (i)      -> lookup i e 
    | Basesynt.VInt (i)     -> VInt i
    | Basesynt.VString (s)  -> VString s
    | Basesynt.VProc (vars,p)  -> VClos (vars,e,p)
    | Basesynt.VAppli (v1,v2) -> 
	(
	  let (v, nextvars,  e', p) = evaluate_abstraction e v1 in 
	  let e'' = (v, evaluate e v2) :: e' in
            VClos (nextvars, e'', p) )
    | Basesynt.VTuple (u)   -> VTuple (List.map (evaluate e) u)
    | Basesynt.VEq (v1,v2)  -> booltovbool ((evaluate e v1) = (evaluate e v2))
    | Basesynt.VApp (v1,v2) ->  
        VString ((evaluate_string e v1) ^ (evaluate_string e v2))
    | Basesynt.VMult  (v1,v2) ->  eval_binary_int v1 v2 ( * ) 
    | Basesynt.VPlus  (v1,v2) ->  eval_binary_int v1 v2 (+)
    | Basesynt.VDiv   (v1,v2) ->  eval_binary_int v1 v2 (/)
    | Basesynt.VMinus (v1,v2) ->  eval_binary_int v1 v2 (-)
    | Basesynt.VAnd (v1,v2)   ->  eval_binary_bool v1 v2 (&&)
    | Basesynt.VOr (v1,v2)    ->  eval_binary_bool v1 v2 (||)
    | Basesynt.VUMinus (v1)     ->  VInt ( - (evaluate_int e v1))
    | Basesynt.VNot (v1)        ->  booltovbool (not (evaluate_bool e v1))
    | Basesynt.VMarshall (v1) ->
	VString (Marshal.to_string (evaluate e v1) [])
    | Basesynt.VUnmarshall (v1) -> (
        try  
	  Marshal.from_string (evaluate_string e v1) 0 
	with
	    _ -> assert false )
    |  Basesynt.VHead (v1) -> ( 
         match (evaluate_list e v1) with
	     [] -> Error.err "Empty list exception" 
           | p :: q -> p )
    |  Basesynt.VTail (v1) -> (
         match (evaluate_list e v1) with
	     [] -> VList [] 
           | p :: q -> VList q )
    |  Basesynt.VCons (v1,v2) -> VList ((evaluate e v1)::(evaluate_list e v2))
    |  Basesynt.VList (l) -> VList (List.map (evaluate e) l) 
    |  Basesynt.VIsNil (v1) -> booltovbool ( (evaluate_list e v1) = [] )
	
let addloc v u d =
  let vuin = VId u in
    match v, d with
	VTuple l, MUp -> VTuple ( vuin :: l )
      | _, MUp -> VTuple ( vuin :: v :: [] )
      | _ -> v

let removeloc v =
  match v with
      VTuple l -> (
	match l with
	    [] -> assert false
	  | [v] -> assert false
	  | [v;v'] ->  v' 
	  | v :: lv -> VTuple lv  )
    | _ -> assert false
	      
let gen_substitution lid value
  =
  match (lid, value) with
      ( [], VNil ) -> []
    | ( [id], _) -> [ (id, value) ] 
    | ( lid ,VTuple lvalue ) ->
	(List.map2 (fun x y -> (x, y)) lid lvalue)
    | _ -> assert false  

(*****************************************)
(* to html                               *)
(*****************************************)

let vm v i = Html.id ("vm(" ^ v ^ "," ^ (Html.int i) ^ ")") 
let vm_to_string v i = "(" ^ v ^ "," ^ (Html.int i) ^ ")" 
let lpar = Html.kwd "("
let rpar = Html.kwd ")"
let vnil = Html.kwd "()"
let com = Html.kwd ","
let scol = Html.kwd ";"
let htrue = Html.kwd "true"
let hfalse = Html.kwd "false"
let closure = Html.kwd "closure"
let pass = Html.kwd "pass"	     

let direction_to_html d =
  match d with 
      MUp     -> Html.kwd "up"
    | MDown i -> Html.kwd "down " ^ Uin.to_html i
    | MIn     -> Html.kwd "in"

let rec to_html v = 
  match v with
      VNil -> vnil
    | VTrue -> htrue 
    | VFalse -> hfalse 
    | VVmid (v,i) -> vm v i   
    | VInt i -> Html.int i
    | VString s -> Html.string ("\"" ^ s ^ "\"")
    | VTuple t -> tuple_to_html t com 
    | VList l -> tuple_to_html l scol
    | VClos (lid,enew,p) ->
	(Html.kwd "(") ^ lid_to_html lid ^ (Html.kwd ")") ^
	(Html.kwd "{ code }")
    | VPass (_,_) -> pass 
    | VId uin -> Uin.to_html uin
	
and lid_to_html l = 
  String.concat com (List.map Html.id l)

and tuple_to_html t del =
  match t with 
      [] -> ""
    | [h] -> to_html_bis h  
    | h::t -> (to_html_bis h) ^ del ^ (tuple_to_html t del)
	
and to_html_bis v =
  match v with
      VTuple t -> lpar ^ to_html v ^ rpar 
    |_ -> to_html v   

and to_string v = 
  match v with
      VNil -> vnil
    | VTrue -> htrue 
    | VFalse -> hfalse 
    | VVmid (v,i) -> vm_to_string v i   
    | VInt i -> Int32.to_string (Int32.of_int i)
    | VString s -> s  (* attention aux eventuels caracteres html dans s *)
    | VTuple t -> tuple_to_string t com
    | VList l -> tuple_to_string l scol
    | VClos (_,enew,p) -> "closure"
    | VPass (_,_) -> "pass" 
    | VId uin -> Uin.to_string uin
	
and clos_to_html (e,p) =
  let value x =
    try
      to_string (lookup x e) 
    with
	_ -> ""
  in
    Html.code (Basesynt.to_html2 p value)

and tuple_to_string t del =
  match t with 
      [] -> ""
    | [h] -> to_string_bis h  
    | h::t -> (to_string_bis h) ^ del ^ (tuple_to_string t del)
	
and to_string_bis v =
  match v with
      VTuple t -> lpar ^ to_string v ^ rpar 
    |_ -> to_string v   

let env_to_html e = 
  let aux = List.map (fun (x,y) -> (Html.id x), (to_html y)) e in
    Html.tablen2 aux
