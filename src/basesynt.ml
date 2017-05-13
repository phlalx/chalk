open Error

type t =
      Nil 
    | New     of  string * t 
    | Let     of  string * value *  t 
    | ITE     of  value * t * t 
    | Par     of  t * t 
    | Kell    of  string * t 
    | Trig    of  pattern * t 
    | Value   of  value
    | Msg     of  string * value * Syntax.direction_msg

and pattern = (string * (string list) * Syntax.direction) list * bool

and value = 
  | VNil        
  | VTrue       
  | VFalse      
  | VVmid       of  value
  | VId         of  string
  | VInt        of  int
  | VString     of  string
  | VProc       of  string list * t
  | VTuple      of  (value list)
  | VEq         of  value * value 
  | VMult       of  value * value
  | VPlus       of  value * value
  | VDiv        of  value * value
  | VMinus      of  value * value
  | VUMinus     of  value 
  | VNot        of  value
  | VAppli      of  value * value
  | VAnd        of  value * value
  | VOr         of  value * value
  | VApp        of  value * value
  | VMarshall   of  value
  | VUnmarshall of  value
  | VEmptyList 
  | VIsNil      of  value
  | VCons       of  value * value
  | VHead       of  value
  | VTail       of  value
  | VList       of  value list

let up = Html.kwd "up"
let dn = Html.kwd "dn"
let nil = Html.kwd "nil"
let hnew = Html.kwd "new"
let lpar = Html.kwd "("
let rpar = Html.kwd ")"
let vnil = Html.kwd "()"
let lkell = Html.kwd "["
let rkell = Html.kwd "]"
let lmsg = Html.kwd "&lt"
let rmsg = Html.kwd "&gt"
let col = Html.kwd ":"
let hin = Html.kwd "in"
let def = Html.kwd "once"
let rdef = Html.kwd "on"
let hlet = Html.kwd "let"
let hif = Html.kwd "if"
let helse = Html.kwd "else"
let hthen = Html.kwd "then"
let heq = Html.kwd "="
let com = Html.kwd ","
let htrue = Html.kwd "true"
let hfalse = Html.kwd "false"
let par = Html.kwd "|"
let eqs = Html.kwd "=="
let plus = Html.kwd "+"
let minus = Html.kwd "-"
let mult = Html.kwd "*"
let div = Html.kwd "/"
let hand = Html.kwd "and"
let hor = Html.kwd "or"
let hnot = Html.kwd "not"
let app = Html.kwd "^"
let br = Html.br 
let kto = Html.kwd "to"
let reply = Html.kwd "reply"
let dot = Html.kwd "."
let lbr = Html.kwd "{" 
let rbr = Html.kwd "}"
let head =   Html.kwd "head"
let tail =  Html.kwd "tail"
let isnil =  Html.kwd "isnil"
let scol =  Html.kwd ";"
let emptylist =  Html.kwd "[]"
let cons =  Html.kwd "::"
let marshall =  Html.kwd "marshall"
let unmarshall =  Html.kwd "unmarshall"
let appl =  Html.kwd "@"

let rec direction_to_html d =
  let optname n =
    if n = "" then "" else n ^ " "
  in
  match d with
      Syntax.In -> ""
    | Syntax.Up -> " " ^ up ^ " " 
    | Syntax.Down n -> " " ^ dn ^ " " ^ (optname n)
    | Syntax.Pass -> ""
	
and pattern_to_html pat html_id = 
  let patt pat =
    let (s, l, d) = pat in 
    let var = lid_to_html l  html_id in
    let p = 
      if d = Syntax.Pass then 
	lkell ^ var ^ rkell
      else
	lmsg ^ var ^ rmsg
    in
      (html_id s) ^ (direction_to_html d)  ^ p
  in
  let (lpat, repl) = pat in
    String.concat par (List.map patt lpat)

	
and lid_to_html l  html_id = 
  String.concat com (List.map Html.id l)

and vars_to_html vars =
  let var_to_html (x) = lpar ^ (Html.id x) ^ rpar   
  in  
  String.concat "" (List.map var_to_html vars)

and value_to_html v html_id = 
  let binary x y op =
    lpar ^ (value_to_html x html_id) 
    ^ op ^ (value_to_html y  html_id) ^ rpar  
  and unary x op =
    op ^ lpar ^ (value_to_html x html_id) ^ rpar  
  and binary2 x y op =
    op ^ lpar ^ (value_to_html x html_id) ^ rpar     
  in
  match v with
      VNil           ->  vnil
    | VTrue          ->  htrue 
    | VFalse         ->  hfalse 
    | VEmptyList     ->  emptylist
        
    | VVmid     (v) ->   value_to_html v html_id  
    | VInt    (i) ->   Html.int i
    | VString (s) ->   Html.string ("\"" ^ s ^ "\"")
    | VProc   (vars,p) -> 
	(vars_to_html vars) ^ lbr 
        ^  to_html_bis p  html_id ^ rbr
    | VTuple  (t) ->   tuple_to_html t com  html_id
    | VList (l) ->   lkell ^ (tuple_to_html l scol html_id) ^ rkell
    | VId     (i) ->   html_id i 

    | VEq     (x,y) ->  binary x y heq
    | VMult   (x,y) ->  binary x y mult
    | VPlus   (x,y) ->  binary x y plus
    | VDiv    (x,y) ->  binary x y div
    | VMinus  (x,y) ->  binary x y minus
    | VAppli  (x,y) ->  binary x y appl
    | VAnd    (x,y) ->  binary x y hand
    | VOr     (x,y) ->  binary x y hor
    | VApp    (x,y) ->  binary x y app
    | VCons   (x,y) ->  binary x y cons 

    | VUMinus (x) -> unary x minus 
    | VNot    (x) -> unary x hnot
    | VHead   (x) -> unary x head
    | VTail   (x) -> unary x tail
    | VIsNil  (x) -> unary x isnil 

    | VMarshall   (x) -> unary x marshall  
    | VUnmarshall (x) -> unary x unmarshall

and tuple_to_html t del html_id =
  match t with 
      [] -> ""
    | [h] -> value_to_html_bis h  html_id 
    | h::t -> (value_to_html_bis h  html_id) ^ del ^ 
	(tuple_to_html t del  html_id)
	
and value_to_html_bis v  html_id =
  match v with
      VTuple (t) -> lpar ^ value_to_html v  html_id ^ rpar 
    |_ -> value_to_html v  html_id
           
(* utilisé pour l'affichage d'une valeur d'ordre sup *)
and to_html_bis p  html_id =
  match p with
      Nil  -> nil 
    | New (a, r) -> 
	hnew  ^  " " ^ Html.id a ^  " " ^ to_html_bis r html_id
    | Let (a, v, r) -> 
        hlet ^  " " ^(Html.id a) ^ " " 
	^ heq ^  " " ^ (value_to_html v html_id) ^  " " ^ hin  ^  " " ^
	(to_html_bis r html_id)    
    | ITE (v, p, q) -> 
        hif ^  " " ^ (value_to_html v html_id) ^ " " ^ hthen ^  " " ^ 
	(to_html_bis p html_id) ^  " " ^ helse ^ " " ^ 
	(to_html_bis q html_id)    
    | Value (v) -> value_to_html v html_id
    | Par ( p, q) -> to_html_bis p  html_id ^  " " ^ par ^ " " ^ 
	to_html_bis q html_id 
    | Kell ( s, p) -> html_id s ^ lkell ^ (to_html_bis p html_id) ^ rkell 
    | Trig ( s, p) -> def ^  " " ^ (pattern_to_html s html_id) ^ " " ^ lbr ^ 
	" " ^ (to_html_bis p html_id)  ^ rbr
    | Msg ( s, v, dir) ->  
	(msg_to_html s v dir  html_id)

and msg_to_html s v dir  html_id = 
  let m =  (html_id s) ^ lmsg  ^ (value_to_html v  html_id) ^ rmsg in
  match dir with
    Syntax.MUp ->  Html.kwd "env." ^ m
  | Syntax.MIn -> m
  | Syntax.MDown n -> (html_id n) ^ dot ^ m
	
let to_html0 p  html_id=  
  let indent = Html.space ^ Html.space ^ Html.space in
  let rec to_html_ind p spc =
    match p with
        Nil -> spc ^ nil ^ br 
      | New ( a, r) -> 
          spc ^ hnew ^ " " ^(Html.id a) ^ " " ^ hin ^ br 
          ^ (to_html_ind r spc)  
      | Let ( a, v, r) -> 
          spc ^ hlet ^ " " ^ (Html.id a) ^ " " ^ heq 
               ^ " " ^ (value_to_html v  html_id) ^ " " ^  hin ^ br ^ 
	  (to_html_ind r (spc ^ indent))  
      | ITE ( v, p, q) -> 
          spc ^ hif ^ " " ^ (value_to_html v  html_id) ^" " ^ hthen ^ br ^ 
             (to_html_ind p (spc ^ indent)) ^ 
	  spc ^  helse ^ br ^ (to_html_ind q (spc ^ indent))     
      | Par ( p, q) -> (to_html_ind p spc) ^ (to_html_ind q spc)  
      | Kell ( s, p) -> 
          spc ^ (html_id s) ^ lkell ^ br 
          ^ (to_html_ind p (spc ^ indent )) ^ 
	  spc ^ rkell ^ br 
      | Value (v) -> spc ^ (value_to_html v html_id) ^ br
      | Msg ( s, v, dir) ->  
	  spc ^ (msg_to_html s v dir  html_id)  ^ br 
      | Trig ( s, p) -> 
            spc ^ def  ^" " ^ (pattern_to_html s  html_id) ^" " ^ lbr ^ br ^ 
	    (to_html_ind p (spc ^ indent) ) ^ spc ^ rbr ^ br

   in to_html_ind p ""    

let to_html p = to_html0 p Html.id

let to_html2 p param = to_html0 p (fun x -> Html.id_span x (param x))  



	             
