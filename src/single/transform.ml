open Basesynt

let rec addcomp v newv =
  match v with
    |  VNil -> newv
    |  VTuple l -> VTuple (newv :: l)
    |_ -> VTuple ( newv :: v :: [] )

and swapdir d =
  match d with
      Syntax.MUp -> Syntax.Up
    | Syntax.MIn -> Syntax.In
    | Syntax.MDown _ -> Syntax.Down (Syntax.fresh_var ()) 
	  
and build_context var  proc =
  let rec build_context_aux var =
    match var with
	[] -> (fun x -> x), (fun x -> x), proc 
      | (u, id, value, dir) :: t ->
	  let cont = Syntax.fresh_name () in
	  let news,msgs,trigs = build_context_aux t in
	  let news' = fun x -> New (cont, (news x)) in
	  let msgs' = 
	    fun x -> Par (
	      (msgs x),
	      Msg (id, (addcomp value (VId cont)), dir)) 
	  in 
	  let trigs' =
	    Trig (
	      ( [( cont, [ u ], (swapdir dir))], false),
	      trigs )
	  in
	    news', msgs', trigs' 
  in
    if var = [] then proc else
      let (news, msgs, trigs) =
	build_context_aux var
      in
	news (msgs trigs)

and transform_pat pat (s : string option) rep=
  let iter (id,vars,dir) =
    if (Some id) = s then
      (id, "reply" :: vars, dir)
    else
      (id,vars,dir)
  in
    (List.map iter pat), rep
      
let rec transform p =
  match p with
      Syntax.Nil _ -> Nil
    | Syntax.New (_, a, s, r) -> New (a,(transform r))
    | Syntax.Let (_, a, v, r) ->
	let newvalue, vars = transform_value v in
	let r' = transform r in
	  build_context vars (Let (a,newvalue,r'))
    | Syntax.ITE (_, v, p, q) ->
	let newvalue, vars = transform_value v in
	let p' = transform p in
	let q' = transform q in
	  build_context vars (ITE (newvalue, p', q'))
    | Syntax.Par (_, p, q) -> 
	Par ((transform p), (transform q))
    | Syntax.Kell (_, s, p) -> 
	Kell (s, (transform p))
    | Syntax.Value (_,v) -> (
	match v with 
	    Syntax.VMsg (_, id, v, dir) -> 
	      let newvalue, vars = transform_value v in
		build_context vars (Msg (id, newvalue, dir))
	  | _ ->    
	    let newvalue, vars = transform_value v in
	      build_context vars (Value newvalue )	      
      )
    | Syntax.Trig (_, (s,rep), u, p) -> 
	Trig ((transform_pat s !u rep), (transform p))
    | Syntax.Reply (_, v, _, d) -> (
	let newvalue, vars = transform_value v in
	let proc =
	  match !d with
	      None -> assert false
	    | Some u -> (
		match u with
		    Syntax.Up -> Msg ("reply", newvalue, Syntax.MUp)
		  | Syntax.In -> Msg ("reply", newvalue, Syntax.MIn)
		  | Syntax.Down s -> Msg ( "reply", newvalue, Syntax.MDown s)
		| Syntax.Pass -> assert false
	      )
	in
	  build_context vars proc
      )
    | Syntax.Com (_, v, q) -> (
	let newvalue, vars = transform_value v in
	let q' = transform q in
	  build_context vars q'
      )



and transform_value_list l =
  match l with
      [] -> [], []
    | h :: t -> 
	let v, newvars = transform_value h in
	let lv, newvars' = transform_value_list t in
	  v :: lv, newvars @ newvars' 

and transform_value v  =
  match v with
      Syntax.VMsg  (_, id, value, dir) -> 
	let u = Syntax.fresh_var () in 
	let v, newvars' = transform_value value in
	  VId u, (u, id, v, dir) :: newvars'
    | Syntax.VNil          _ ->  VNil, []
    | Syntax.VTrue         _ ->  VTrue, []
    | Syntax.VFalse        _ ->  VFalse, []
    | Syntax.VEmptyList    _ ->  VEmptyList, []
    | Syntax.VVmid   (_,v) -> 
	let v, newvars = transform_value v  in
	  VVmid v, newvars
    | Syntax.VInt    (_,i) ->   (VInt i), []
    | Syntax.VString (_,s) ->   (VString s), []
    | Syntax.VProc   (_,vars,p) ->
	let vars' = List.map (fun (x,y) -> x) vars in 
	  (VProc (vars', (transform p))), []
    | Syntax.VTuple  (_,t) ->   
	let t', newvars = transform_value_list t in
	  VTuple t', newvars
    | Syntax.VList   (_,l) ->  
	let l', newvars = transform_value_list l in
	  VList l', newvars
    | Syntax.VId     (_,i) ->  (VId i), []
    | Syntax.VEq     (_,x,y) ->  
	let v1, newvars1 = transform_value x  in
	let v2, newvars2 = transform_value y  in
	VEq (v1, v2), newvars1 @ newvars2 
    | Syntax.VMult   (_,x,y) ->
	let v1, newvars1 = transform_value x  in
	let v2, newvars2 = transform_value y  in
	VMult (v1, v2), newvars1 @ newvars2 
    | Syntax.VPlus   (_,x,y) ->
	let v1, newvars1 = transform_value x  in
	let v2, newvars2 = transform_value y  in
	VPlus (v1, v2), newvars1 @ newvars2 
    | Syntax.VDiv    (_,x,y) ->
	let v1, newvars1 = transform_value x  in
	let v2, newvars2 = transform_value y in
	VDiv (v1, v2), newvars1 @ newvars2  
    | Syntax.VMinus  (_,x,y) ->
	let v1, newvars1 = transform_value x  in
	let v2, newvars2 = transform_value y in
	VMinus (v1, v2), newvars1 @ newvars2 
    | Syntax.VAppli  (_,x,y) ->
	let v1, newvars1 = transform_value x  in
	let v2, newvars2 = transform_value y in
	VAppli (v1, v2), newvars1 @ newvars2 
    | Syntax.VAnd    (_,x,y) ->
	let v1, newvars1 = transform_value x  in
	let v2, newvars2 = transform_value y in
	VAnd (v1, v2), newvars1 @ newvars2 
    | Syntax.VOr     (_,x,y) ->
	let v1, newvars1 = transform_value x  in
	let v2, newvars2 = transform_value y in
	VOr (v1, v2), newvars1 @ newvars2 
    | Syntax.VApp    (_,x,y) ->
	let v1, newvars1 = transform_value x  in
	let v2, newvars2 = transform_value y in
	VApp (v1, v2), newvars1 @ newvars2   
    | Syntax.VCons   (_,x,y) ->
	let v1, newvars1 = transform_value x  in
	let v2, newvars2 = transform_value y in
	VCons (v1, v2), newvars1 @ newvars2 
    | Syntax.VUMinus (_,x) ->
	let v1, newvars1 = transform_value x  in
          VUMinus v1, newvars1
    | Syntax.VNot    (_,x) -> 
	let v1, newvars1 = transform_value x  in
          VNot (v1), newvars1
    | Syntax.VHead   (_,x) -> 
	let v1, newvars1 = transform_value x  in
          VHead (v1), newvars1
    | Syntax.VTail   (_,x) -> 
	let v1, newvars1 = transform_value x  in
          VTail (v1), newvars1
    | Syntax.VIsNil  (_,x) -> 
	let v1, newvars1 = transform_value x  in
          VIsNil (v1), newvars1
    | Syntax.VMarshall   (_,x,_) ->
	let v1, newvars1 = transform_value x  in
          VMarshall (v1), newvars1
    | Syntax.VUnmarshall (_,x,_) ->
	let v1, newvars1 = transform_value x  in
          VUnmarshall (v1), newvars1
