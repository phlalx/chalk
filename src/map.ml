let concat inqueue  =
  Queue.iter (fun x -> Queue.push x inqueue)

let hashtblqueueiter t iter = 
  let iter' key valuequeue = 
    Queue.iter (fun value -> iter key value) valuequeue 
  in Hashtbl.iter iter' t
 
let add_safe t a b =
  try 
    let _ = Hashtbl.find t a in assert false
  with
      Not_found -> Hashtbl.add t a b

  
let rec get k table = 
  match table with 
      [] -> raise Not_found
    | ( k',  lval ) :: table' ->
        if (k = k') then (
          match lval with 
              [] -> assert false
            |  [ value ] -> value, table'
            |  value :: t -> value, ((k', t) :: table') )
        else
          let value, table'' = (get k table') in
            value, (k', lval) :: table''  
	      
	      
let rec add key value table =   
  match table with 
      [] -> [key, [ value ] ]
    | ( key', l )  :: q  -> 
        if (key' = key) then 
          ( key,  (value :: l) ) :: q
        else 
          ( key', l ) :: (add key value q) 
	    
let rec flat table =
  match table with 
      [] -> []
    | ( key, [] )  :: q  -> flat q
    | ( key, t :: q) :: q' -> (key,t) :: (flat ((key,q) :: q')) 
	
let rec union t1 t2 = 
  let rec aux k l1 t2 =
    match t2 with 
        [] -> [k, l1]
      | ( k', l )  :: q  -> 
          if (k' = k) then 
            ( k,  (l1 @ l) ) :: q
          else 
            ( k', l ) :: (aux k l1  q) 
  in
    match t1 with
	[] -> t2
      | (x, l) :: t1' -> union t1' (aux x l t2)  
	 

let itertree leaf subleafs fct = 
  let rec parcours_aux lleaf =
    match lleaf with
	[] -> []
      | h :: t -> 
         (fct h) ::  parcours_aux (t @ subleafs h)
  in parcours_aux [ leaf ] 




