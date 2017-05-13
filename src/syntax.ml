open Error

type direction = Up | Down of string | In | Pass

type direction_msg = MUp | MIn | MDown of string

type t =
      Nil     of info
    | New     of info * string * Ktype.t * t 
    | Let     of info * string * value *  t 
    | ITE     of info * value * t * t 
    | Par     of info * t * t 
    | Kell    of info * string * t 
    | Trig    of info * pattern * string option ref * t 
    | Value   of info * value
    | Reply   of info * value * string option * direction option ref
    | Com     of info * value * t

and pattern = (string * string list * direction) list * bool

and value = 
  | VMsg        of info * string * value * direction_msg
  | VNil        of info
  | VTrue       of info
  | VFalse      of info
  | VVmid       of info * value
  | VId         of info * string
  | VInt        of info * int
  | VString     of info * string
  | VProc       of info * (string * Ktype.t) list * t
  | VTuple      of info * (value list)
  | VEq         of info * value * value 
  | VMult       of info * value * value
  | VPlus       of info * value * value
  | VDiv        of info * value * value
  | VMinus      of info * value * value
  | VUMinus     of info * value 
  | VNot        of info * value
  | VAppli      of info * value * value
  | VAnd        of info * value * value
  | VOr         of info * value * value
  | VApp        of info * value * value
  | VMarshall   of info * value * Ktype.t
  | VUnmarshall of info * value * Ktype.t
  | VEmptyList  of info
  | VIsNil      of info * value
  | VCons       of info * value * value
  | VHead       of info * value
  | VTail       of info * value
  | VList       of info * value list
  
let count = ref 0

let fresh_var () = 
  incr count ; 
   ("?var" ^ (Int32.to_string (Int32.of_int !count)) )

let fresh_name () = 
  incr count ; 
   ("?name" ^ (Int32.to_string (Int32.of_int !count)) )

let up =  "up"
let dn =  "dn"
let nil =  "nil"
let hnew =  "new"
let lpar =  "("
let rpar =  ")"
let vnil =  "()"
let lkell =  "["
let rkell =  "]"
let lmsg =  "&lt"
let rmsg =  "&gt"
let col =  ":"
let hin =  "in"
let def =  "once"
let rdef =  "on"
let hlet =  "let"
let rdef =  "on"
let hif =  "if"
let helse =  "else"
let hthen =  "then"
let heq =  "="
let com =  ","
let htrue =  "true"
let hfalse =  "false"
let par =  "|"
let eqs =  "=="
let plus =  "+"
let minus =  "-"
let mult =  "*"
let div =  "/"
let hand =  "and"
let hor =  "or"
let hnot =  "not"
let app =  "^"
let br = Html.br 
let kto =  "to"
let reply =  "reply"
let dot =  "."
let lbr =  "{" 
let rbr =  "}"
let head =    "head"
let tail =   "tail"
let isnil =   "isnil"
let scol =   ";"
let emptylist =   "[]"
let cons =   "::"
let marshall =   "marshall"
let unmarshall =   "unmarshall"
let appl =   "@"
let env =   "env"

let rec direction_to_gen kwd d =
  match d with
      In -> ""
    | Up -> " " ^ (kwd up) ^ " " 
    | Down n -> " " ^ (kwd dn) ^ " " ^ n (* modif *)
    | Pass -> ""
	
and pattern_to_gen
  (hid : string -> string )
  (kwd : string -> string )
  pat = 
  let patt pat =
    let (s, l, d) = pat in 
    let var = lid_to_gen hid kwd l  in
    let p = 
      if d = Pass then 
	(kwd lkell) ^ var ^ (kwd rkell)
      else
	(kwd lmsg) ^ var ^ (kwd rmsg)
    in
      (hid s) ^ (direction_to_gen kwd d) ^ p
  in
  let (lpat,repl) = pat in
    String.concat (kwd par) (List.map patt lpat)

and lid_to_gen hid kwd l = 
  String.concat (kwd com) (List.map Html.id l)

and vars_to_gen hid kwd htype vars =
  let var_to_gen (x,t) = 
    (kwd lpar) ^ (hid x) ^ (htype t) ^ (kwd rpar)  
  in  
  String.concat "" (List.map var_to_gen vars)

and value_to_gen hid kwd hint hstring hspace hbr htype v = 
  let tuple_to_gen = tuple_to_gen hid kwd hint hstring hspace hbr htype in
  let vars_to_gen = vars_to_gen hid kwd htype in
  let to_gen =
    to_gen hid kwd hint hstring hspace hbr htype   
  and value_to_gen = 
    value_to_gen hid kwd hint hstring hspace hbr htype
  and msg_to_gen =
    msg_to_gen hid kwd hint hstring hspace hbr htype  in
  let binary x y op =
    (kwd lpar) ^ (value_to_gen x) 
    ^ (kwd op) ^ (value_to_gen y) ^ (kwd rpar)  
  and unary x op =
    (kwd op) ^ (kwd lpar) ^ (value_to_gen x) ^ (kwd rpar)  
  and binary2 x y op =
    (kwd op) ^ (kwd lpar) ^
    (value_to_gen x) ^ (htype y)
    ^ (kwd rpar)     
  in
  match v with
      VNil          _ ->  kwd vnil
    | VTrue         _ ->  kwd htrue 
    | VFalse        _ ->  kwd hfalse 
    | VEmptyList    _ ->  kwd emptylist
    | VMsg (_, s, v, dir) -> msg_to_gen s v dir       
    | VVmid     (_,v) ->   value_to_gen v 
    | VInt    (_,i) ->   hint i
    | VString (_,s) ->   hstring ("\"" ^ s ^ "\"")
    | VProc   (_,vars,p) -> 
	(vars_to_gen vars) ^
	(kwd lbr) ^ (to_gen p) ^ (kwd rbr)
    | VTuple  (_,t) -> tuple_to_gen t (kwd com)
    | VList (inf,l) ->  
	(kwd lkell) ^ (tuple_to_gen l (kwd scol)) ^ (kwd rkell)
    | VId     (_,i) ->   hid i 
    | VEq     (_,x,y) ->  binary x y heq
    | VMult   (_,x,y) ->  binary x y mult
    | VPlus   (_,x,y) ->  binary x y plus
    | VDiv    (_,x,y) ->  binary x y div
    | VMinus  (_,x,y) ->  binary x y minus
    | VAppli  (_,x,y) ->  binary x y appl
    | VAnd    (_,x,y) ->  binary x y hand
    | VOr     (_,x,y) ->  binary x y hor
    | VApp    (_,x,y) ->  binary x y app
    | VCons   (_,x,y) ->  binary x y cons 
    | VUMinus (_,x) -> unary x minus 
    | VNot    (_,x) -> unary x hnot
    | VHead   (_,x) -> unary x head
    | VTail   (_,x) -> unary x tail
    | VIsNil  (_,x) -> unary x isnil 
    | VMarshall   (_,x,y) -> binary2 x y marshall  
    | VUnmarshall (_,x,y) -> binary2 x y unmarshall

and tuple_to_gen hid kwd hint hstring hspace hbr htype t del  =
  let value_to_gen_bis =
      value_to_gen_bis hid kwd hint hstring hspace hbr htype
  in
  let tuple_to_gen = tuple_to_gen hid kwd hint hstring hspace hbr htype
  in (* A CHANGER *)
    match t with 
      [] -> ""
    | [h] -> value_to_gen_bis h 
    | h::t -> (value_to_gen_bis h) ^ del ^ (tuple_to_gen t del)
	
and value_to_gen_bis hid kwd hint hstring hspace hbr htype v =
    let value_to_gen = 
      value_to_gen hid kwd hint hstring hspace hbr htype
    in
      match v with
      VTuple (_,t) -> (kwd lpar) ^ value_to_gen v ^ (kwd rpar) 
    |_ -> value_to_gen v 

and msg_to_gen hid kwd hint hstring hspace hbr htype s v dir = 
  let value_to_gen = value_to_gen hid kwd hint hstring hspace hbr htype in
  let m =  (hid s) ^ (kwd lmsg)  ^ (value_to_gen v) ^ (kwd rmsg) in
    match dir with
	MUp ->  (kwd env) ^ (kwd dot)  ^ m
      | MIn -> m
      | MDown n -> (hid n) ^ (kwd dot) ^ m
	  
and to_gen  hid kwd hint hstring hspace hbr htype p =  
  let value_to_gen =
    value_to_gen hid kwd hint hstring hspace hbr htype in
  let pattern_to_gen = pattern_to_gen hid kwd in
  let indent = hspace ^ hspace ^ hspace in
  let rec to_gen_ind p spc =
    match p with
        Nil _ -> spc ^ (kwd nil) ^ hbr 
      | New (_, a, s, r) -> 
          spc ^ (kwd hnew) ^ " " ^ (hid a) ^ " "  ^  
	  (htype s) ^ " " ^ (kwd hin) ^ hbr 
          ^ (to_gen_ind r spc)  
      | Let (_, a, v, r) -> 
          spc ^ (kwd hlet) ^ " " ^ (hid a) ^ " " ^ (kwd heq) 
               ^ " " ^ (value_to_gen v) ^ " " ^ (kwd hin) ^ hbr ^ 
	  (to_gen_ind r (spc ^ indent))  
      | ITE (_, v, p, q) -> 
          spc ^ (kwd hif) ^ " " ^ (value_to_gen v) ^ " " ^
	  (kwd hthen) ^ hbr ^ 
             (to_gen_ind p (spc ^ indent)) ^ 
	  spc ^  (kwd helse) ^ hbr ^ (to_gen_ind q (spc ^ indent))     
      | Par (_, p, q) -> (to_gen_ind p spc) ^ spc ^ (kwd par) ^ hbr ^
	  (to_gen_ind q spc)  
      | Kell (_, s, p) -> 
          spc ^ (hid s) ^ (kwd lkell) ^ hbr 
          ^ (to_gen_ind p (spc ^ indent )) ^ 
	  spc ^ (kwd rkell) ^ hbr 
      | Value (_,v) -> spc ^ (value_to_gen v) ^ hbr
      | Trig (_, s,_, p) -> 
            spc ^ (kwd def)  ^ " " ^ (pattern_to_gen s) ^ " " ^
	  (kwd lbr) ^ hbr ^ 
	    (to_gen_ind p (spc ^ indent) )  ^ spc ^ (kwd rbr) ^ hbr
      | Reply (_, v, x,_) ->
	  let to_id x = 
	    match x with 
		None -> "" 
	      | Some x -> (kwd kto) ^ " " ^ (hid x)
	  in
	    spc ^ (kwd reply) ^  " " ^  
	      (value_to_gen v) ^ " " ^  (to_id x) ^ hbr
      | Com (_, v, q) -> spc ^ (value_to_gen v)  ^  (kwd scol) ^
	  (to_gen_ind q spc)   
  in to_gen_ind p ""    
 
let to_html p = to_gen
		  Html.id
		  Html.kwd
		  Html.int
		  Html.string
		  Html.space
		  Html.br
		  (fun x -> (Html.kwd col) ^ Ktype.to_html x)
		  p 
 
let to_latex p = 
 Latex.env (to_gen
		  Latex.id
		  Latex.kwd
		  Latex.int
		  Latex.string
		  Latex.space
		  Latex.br
	          (fun x -> "")
		  p 
           )

