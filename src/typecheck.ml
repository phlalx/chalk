open Syntax
open Ktype
open Error

type sort = 
    Name of (direction option) ref
  | Variable

let html_constraints = ref ""

let html_solved = ref "Unsolved"

type constr = (info * Ktype.t * Ktype.t) list

type environment = (string * ( Ktype.t * sort ) ) list
                                                                              
let emptyconstr = []

let gettype x inf env =
  try 
    List.assoc x env 
  with
      Not_found ->  error_no_exit inf "identifier not bound"
    	
let type_abstraction vars =
  let iter y (x,t) = TArrow (y, t) in List.fold_left iter TProc vars 

(* to rewrite *)
let tuple l dir  = 
  let rec aux l =
    match l with
	[] -> [], []
      | x :: t  -> 
	  let env, lt = aux t in 
	  let tv = generate () in
	    ((x, (tv, Variable))  :: env), (tv :: lt)
  in
  let env, lt =
    match l with
	[] -> [], TUnit
      | [x] -> let tv = generate () in [ x, (tv, Variable) ], tv
      | l  -> let env, lt = aux l in env, TTuple lt
  in
    match dir with
	Down x -> if x != "_" then 
	  (x, (TKell, Variable)) :: env, lt 
	else env, lt
      | _ ->  env, lt

let check_variable inf s dir =
  match s with
      Variable -> error_no_exit inf "Identifier in trigger cannot be variable"
    | Name t -> t := Some dir

let rec split_pattern pat =
  match pat with
      [] -> [], []
    | (u, v, Syntax.Pass) :: tail ->
	let a,b = split_pattern tail in a, ( (u,v, Syntax.Pass) :: b )
    |  p :: tail ->  
	 let a,b = split_pattern tail in (p :: a), b

(* to rewrite *)
let check_pat pat =  
  let rec get_vars pat =
    match pat with
	[] -> []
      | (x, l, Down u) :: q ->
	  let vars = get_vars q in u :: l @ vars 
      | (x, l, _ ) :: q ->
	  let vars = get_vars q in l @ vars
  in
  let rec check_vars vars =
    match vars with
	[] -> true
      | t :: q -> 
	  if List.mem t q then
	    false
	  else
	    check_vars q 
  in
    check_vars (get_vars pat)

let rec receiv_const 
  inf
  (env : environment) 
  pat 
=  
  match pat with
      [] -> [],[]
    | (_, _, Syntax.Pass) :: tail -> assert false
    | (x,l,dir) :: tail -> 
	let (t,s) = gettype x inf env in
	  check_variable inf s dir ;
	  let (env', t') = tuple l dir in
	  let y = generate () in
	  let y' = generate () in
	  let (a,b) = receiv_const inf env tail in
	    ((inf, t, (TChan (t',y,y'))) :: a), (env' @ b)

let rec receiv_const2 
  inf
  (env : environment) 
  pat  
  =  
  match pat with
      [] -> [],[]
    | (x, [y], Syntax.Pass) :: tail -> 
	let (t,s) = gettype x inf env in
	  let (a,b) = receiv_const2 inf env tail in
	    ((inf, t, TKell) :: a), ((y,(TProc,Variable)) :: b) 	
    | _ -> assert false

let rec typecheckvalue v env =
  match v with
      VNil inf ->  (inf, TUnit, TSync, [])
    | VTrue inf -> (inf, TBool, TSync, [])
    | VFalse inf -> (inf, TBool, TSync, [])
    | VVmid (inf, s) ->  
	let inf', res, sync, const = typecheckvalue s env in
	  inf, TVmid, TSync, (inf', res, (TTuple [TString; TInt])) :: const
    | VId (inf,x) -> 
	let y = generate () in
	let (t,_) = gettype x inf env in inf,t,y,[] 
    | VInt  (inf,x) -> inf, TInt,TSync, [] 
    | VString (inf,x) -> inf, TString, TSync,[]
    | VProc  (inf,vars,x) -> ( 
	match vars with
	    [] -> let _,_,const = typecheck x env in inf, TProc, TSync, const
	  | (v1, t1) :: vn -> 
	      let newval = VProc (inf, vn, x) 
	      and newenv = (v1, (t1, Variable)) :: env in
	      let inf, res, sync, const = typecheckvalue newval newenv  in
		inf, TArrow (t1, res), TSync, const ) 
    | VAppli (inf, v1, v2) ->
	let inf1, res1, sync, const1 = typecheckvalue v1 env in
	let inf2, res2, sync, const2 = typecheckvalue v2 env in
	let x = generate () in
	let y = generate () in
	let z = generate () in
	  inf, y, z,
	  ((inf1, res1, TArrow (x,y)) :: (inf2, res2, x) :: const1 @ const2)
    | VTuple (inf,l) -> 
        let lvar, const = typecheckvaluelist l env in
	  inf, (TTuple lvar), TSync, const
    | VEq (inf,v1,v2) -> 
	let inf1, res1, sync, const1 = typecheckvalue v1 env in
	let inf2, res2, sync, const2 = typecheckvalue v2 env in
	  inf, TBool,TSync, ((inf1, res1, res2) :: const1 @ const2)
    | VMult (inf,v1,v2) -> 
   	let inf1, res1, sync, const1 = typecheckvalue v1 env in
	let inf2, res2, sync, const2 = typecheckvalue v2 env in
	  inf, TInt,TSync, ((inf1, res1, TInt) :: (inf2,res2, TInt) :: 
			const1 @ const2)
    | VApp (inf,v1,v2) -> 
 	let inf1, res1, sync, const1 = typecheckvalue v1 env in
	let inf2, res2, sync, const2 = typecheckvalue v2 env in
	  inf, TString,TSync,((inf1, res1, TString)::(inf2,res2, TString)::
			  const1@const2)
    | VPlus (inf,v1,v2) ->  
 	let inf1, res1, sync, const1 = typecheckvalue v1 env in
	let inf2, res2, sync, const2 = typecheckvalue v2 env in
	  inf, TInt,TSync, ((inf1, res1, TInt) :: (inf2,res2, TInt) :: 
			const1 @ const2)
    | VDiv (inf,v1,v2) -> 
 	let inf1, res1, sync, const1 = typecheckvalue v1 env in
	let inf2, res2, sync, const2 = typecheckvalue v2 env in
	  inf, TInt,TSync, ((inf1, res1, TInt) :: (inf2,res2, TInt) :: 
			const1 @ const2)
    | VMinus (inf,v1,v2) -> 
 	let inf1, res1, sync, const1 = typecheckvalue v1 env in
	let inf2, res2, sync, const2 = typecheckvalue v2 env in
	  inf, TInt,TSync, ((inf1, res1, TInt) :: (inf2,res2, TInt) :: 
			const1 @ const2)
    | VUMinus (inf,v1) -> 
     	let (inf1, res1, sync, const1) = typecheckvalue v1 env in
	  inf, TInt,TSync, ((inf1, res1, TInt) ::  const1)
    | VAnd (inf,v1,v2) -> 
  	let (inf1,res1, sync, const1) = typecheckvalue v1 env in
	let (inf2,res2, sync, const2) = typecheckvalue v2 env in
	  inf, TBool,TSync, ((inf1,res1, TBool) :: (inf2,res2, TBool) ::  
			 const1 @ const2)
    | VOr (inf,v1,v2) -> 
 	let (inf1,res1, sync, const1) = typecheckvalue v1 env in
	let (inf2,res2, sync, const2) = typecheckvalue v2 env in
	  inf, TBool, TSync,((inf1,res1, TBool) :: (inf2,res2, TBool) ::  
			 const1 @ const2)
    | VNot (inf,v1) ->         
   	let (inf1,res1, sync, const1) = typecheckvalue v1 env in
	  inf, TBool,TSync, ((inf1, res1, TBool) ::  const1)
    | VMarshall (inf,v1,t) ->         
   	let (inf1,res1, sync, const1) = typecheckvalue v1 env in
	  inf, TString,TSync, ((inf1, res1, t) ::  const1)
    | VUnmarshall (inf,v1,t) ->         
   	let (inf1,res1, sync, const1) = typecheckvalue v1 env in
	  inf, t,TSync, ((inf1, res1, TString) ::  const1)
    | VEmptyList inf ->         
	inf, (TList (generate ())) ,TSync, []
    | VHead (inf,v1) ->         
	let (inf1,res1, sync, const1) = typecheckvalue v1 env in
	let u = generate () in 
	  inf, u, TSync,((inf1, res1, TList u) ::  const1)
    | VTail (inf,v1) ->         	
	let (inf1,res1,sync, const1) = typecheckvalue v1 env in
	let u = generate () in 
	  inf, (TList u), TSync,((inf1, res1, TList u) ::  const1)
    | VCons (inf,v1,v2) ->     	
	let (inf1,res1, sync, const1) = typecheckvalue v1 env in
	let (inf2,res2, sync, const2) = typecheckvalue v2 env in
	let u = generate () in 
	  inf, (TList u), TSync,((inf1, res1, u) :: (inf2,res2, (TList u)) :: 
			     const1 @ const2 )	  
    | VList (inf,l) ->
	let u = generate () in
	let const = typecheckvaluelistlist u l env in
	  inf, (TList u),TSync, const
    | VIsNil (inf,v1) ->        
	let (inf1,res1,sync,const1) = typecheckvalue v1 env in
	let u = generate () in
	  inf, TBool,TSync, ((inf1, res1, TList u) ::  const1)
    | VMsg (inf, x, v, dir) -> 
	let (t,_) = gettype x inf env in
	let (inf', res, sync, const) =  typecheckvalue v env in
	let y = generate () in
	let y' = generate () in
	let y'' = generate () in
	let const' = (inf, t, TChan (y,y',y'') ) :: 
		       (inf', res, y) :: const  in
	let const'' =
	  (
	    match dir with
		MDown u -> let (t',_) = gettype u inf env 
		in (inf, t', TKell) :: const'
	      | _ -> const' 
	  ) in
	  inf, y', y'', const''

and typecheck p env  =  
  match p with
      Nil inf -> inf, TProc, []
    | Value (inf, v) -> 
	let inf', res, sync, const = typecheckvalue v env in
	  inf, TProc, 
	  ((inf', sync, TAsync) :: (inf', res, TProc) :: const) 
    | New (inf, x, t, q) -> 
	typecheck q ( (x, (t, Name (ref None)) ) :: env )
    | Let (inf, x, v, q) -> 
	let (inf, res, sync, const) = typecheckvalue v env in
	let inf', res', const' = 
	  typecheck q (( x, (res, Variable) ) :: env ) in
	  inf, res', ( const @ const' )
    | ITE (_, v, p, q) ->
	let (inf, res, sync, const) = typecheckvalue v env in
	let inf1, res1, const1 = typecheck p env in
	let inf2, res2, const2 = typecheck q env in
	let t = generate () in
	  inf, res1, ( (inf1, res1, t) :: (inf2, res2, t) :: 
		   (inf, res, TBool) :: (const @ const1 @ const2) )
    | Par (inf, p, q) ->
	let inf1, t1, const1 = typecheck p env in
	let inf2, t2, const2 = typecheck q env in
	  inf, TProc,  
	  ( (inf1, t1, TProc) :: (inf2, t2, TProc) ::  const1 @ const2 )
    | Kell (inf, x, p) -> 
	let (t,_)  = gettype x inf env in 
	let (inf',t',const) = typecheck p env in
	  inf, TProc, ( ( inf, t, TKell ) :: ( inf', t', TProc) :: const ) 
    | Trig (inf, (pat,rep), reply , p) ->
	if not (check_pat pat) then
	    error_no_exit inf "pattern not linear" ;   
	let pat1, pat2 = split_pattern pat in
	let (const, env1) = receiv_const inf env pat1 in 
	let (const', env2) = receiv_const2 inf (env1 @ env) pat2 in
	let _, res, const'' = typecheck p (env1 @ env2 @ env) in (
	  match res with
	      TProc -> () 
	    | TReply (s,_) -> reply := Some s
	    | _ -> assert false
	  ) ;
	  inf, TProc, ( const @ const' @ const'' )
    | Reply (inf, v, id, u) -> 
	let inf1, t1, sync, const1 = typecheckvalue v env in
	  ( match id with
		None -> error_no_exit inf "qsdf" 
	      | Some id -> (
		  let (t,u') = gettype id inf env in (
		      match u' with
			  Name u'' -> (
			    (* update of the trigger continuation info *)
			    match !u'' with
				Some u''' -> u := Some u'''
			      | _ ->  error_no_exit inf
				  "identifier cannot be used for reply"
			  )
			|_ -> assert false ) ;
		    let y = generate () in
		      inf, TReply (id,t1) ,
		  ( (inf1, t, (TChan (y,t1,TSync))) :: const1 ) ) )
    | Com  (inf, p1, p2) -> 
	let inf1, t1, sync, const1 = typecheckvalue p1 env in
	let inf2, t2, const2 = typecheck p2 env in
	  inf, t2, ( (inf1, sync, TSync) :: 
		       (inf1, t1, TUnit)  ::  const1 @ const2 )
	    
			
and typecheckvaluelist lv env =
	  match lv with
	      [] -> [], []
	    | v :: t -> 
		let (inf, res, sync,  const) = typecheckvalue v env in
		let (lvar, const') = typecheckvaluelist t env in
		let x = generate () in
		  x :: lvar, (inf, x, res) :: ( const @ const' )

and typecheckvaluelistlist u lv env =
	  match lv with
	      [] -> []
	    | v :: t -> 
		let (inf, res, sync, const) = typecheckvalue v env in
		let const' = typecheckvaluelistlist u  t env in
		  (inf, u, res) :: ( const @ const' )

let occursin tyX tyT =
  let rec o tyT = 
    match tyT with
      | TVar s -> (s = tyX)
      | TChan (tyT1,tyT2,tyT3)  -> (o tyT1) || (o tyT2)|| (o tyT3)
      | TTuple l -> List.exists  o l
      | _ -> false
  in o tyT
       
let substinty tyX tyT tyS =
  let rec f tyS = match tyS with
      TChan (tyS,tyS',tyS'')  -> TChan ((f tyS),(f tyS'),(f tyS''))
    | TVar s -> if s = tyX then tyT else TVar s
    | TTuple l -> TTuple (List.map f l) 
    | x -> x
  in f tyS

let substinconstr tyX tyT constr =
  List.map
    (fun (inf ,tyS1,tyS2) ->
       (inf, substinty tyX tyT tyS1, substinty tyX tyT tyS2))
    constr

let unify fi msg constr =
  let rec u constr = 
    match constr with
	[] -> []
      | ( inf, tyS, TVar tyX ) :: rest ->
          if tyS = TVar tyX then 
	    u rest
          else if occursin tyX tyS then
            error_no_exit inf (msg ^ ": circular constraints")
          else
            (u (substinconstr tyX tyS rest)) @ [inf, (TVar tyX) ,tyS ]
      | ( inf, TVar tyX, tyT ) :: rest ->
          if tyT = TVar tyX then 
	    u rest
          else if occursin tyX tyT then
            error_no_exit inf (msg ^ ": circular constraints")
          else
            (u (substinconstr tyX tyT rest)) @ [inf, (TVar tyX),tyT]
      | ( _, TString, TString) :: rest -> u rest
      | ( _, TProc, TProc) :: rest -> u rest
      | ( _, TSync, TSync) :: rest -> u rest
      | ( _, TAsync, TAsync) :: rest -> u rest
      | ( _, TUnit, TUnit) :: rest -> u rest
      | ( _, TKell, TKell) :: rest -> u rest
      | ( _, TVmid, TVmid) :: rest -> u rest
      | ( _, TInt, TInt) :: rest -> u rest
      | ( _, TBool, TBool) :: rest -> u rest
      | ( inf, TChan (tyS1, tyS2, tyS3), 
	  TChan (tyT1, tyT2, tyT3) ) :: rest -> 
	  u (((inf, tyS3, tyT3) :: (inf, tyS2, tyT2) :: 
		(inf, tyS1, tyT1) :: rest))
      | ( inf, TReply (id1, tyS1), 
	  TReply (id2, tyT1) ) :: rest ->
	  if id1 = id2 then
	    u ( (inf, tyS1, tyT1) :: rest)
	  else
	    error_no_exit inf "Unsolvable constraints"
      | ( inf, TArrow (tyS1, tyS2), TArrow (tyT1, tyT2) ) :: rest -> 
	  u (  (inf, tyS1, tyT1) :: (inf, tyS2, tyT2) :: rest) 
      | ( inf, TList tyS1, TList tyT1) :: rest -> u ((inf, tyS1, tyT1) :: rest) 
      | ( inf, TTuple tyS1, TTuple tyT1) :: rest -> (
	    try 
	      let lc = (List.map2 (fun x y -> inf, x,y) tyS1 tyT1) @ rest in
		u lc   
	    with
		Invalid_argument _ ->  
		  error_no_exit inf "Unsolvable constraints" )
      | ( inf, _, _)::rest -> error_no_exit inf "Unsolvable constraints" 
  in
    u constr
      
let constraints_to_html cts =
  let aux = List.map (fun (_,x,y) -> (Ktype.to_html x), (Ktype.to_html y)) cts 
  in
    (Html.tablen2 aux)

let type_check p =  
  let typingenv = List.map (fun (x,y) -> (x, (y, (Name (ref None)))))
		     Lib.typingenv in
  try 
    let _, _, c = (typecheck p typingenv) in
      html_constraints := constraints_to_html c ; 
      html_solved := constraints_to_html (unify dummyinfo "" c) ; true
  with
(* CHANGE THIS *)	_ -> false
