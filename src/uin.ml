type t = int

let count = ref 1
  
let wknames = ref 1
  
let port = ref 0 
  
let init p = 
  if (p > 65535) then 
    assert false ;
  port := p ; 
  count := !count + 65536 * p + 1024 ;;

let generate () = incr count ; !count
  
let generate_well_known n  = assert (n < 1024) ; n 

let to_string x =  Int32.to_string (Int32.of_int x)
  
let to_html x = Html.integer (to_string x)

let to_name u = "_" ^ (to_string u)

let dummy = 0 
