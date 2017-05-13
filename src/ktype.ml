
type t =
    TVar of string
  | TUnit
  | TBool
  | TVmid
  | TString
  | TInt
  | TKell 
  | TProc
  | TTuple of (t list)
  | TChan of t * t * t
  | TList of t 
  | TArrow of t * t
  | TAsync
  | TSync
  | TReply of string * t

let count = ref 0

let generate () = 
  incr count ; 
  TVar ("?X" ^ (Int32.to_string (Int32.of_int !count)) )

let ischanneltype t =
  match t with
    | TChan _ -> true
    | _ -> false

let com =     Html.kwd ","
let hsync =   Html.kwd "sync"
let hasync =  Html.kwd "async"
let hreply =  Html.kwd "reply"
let lpar =    Html.kwd "("
let rpar =    Html.kwd ")"
let lmsg =    Html.kwd "&lt"
let rmsg =    Html.kwd "&gt"
let hunit =   Html.kwd "unit"
let hbool =   Html.kwd "bool"
let hlist =   Html.kwd "list"
let kell =    Html.kwd "kell"
let proc =    Html.kwd "proc"
let hint =    Html.kwd "int"
let hstring = Html.kwd "string"
let vmid =    Html.kwd "vmid"
let arrow =   Html.kwd "->"

let rec to_html t =
  match t with
      TUnit    -> hunit
    | TBool    -> hbool
    | TString  -> hstring
    | TInt     -> hint
    | TVmid    -> vmid
    | TKell    -> kell
    | TProc    -> proc
    | TTuple l -> lpar ^ String.concat com (List.map to_html l) ^ rpar
    | TList t  -> (to_html t) ^ hlist
    | TChan (t,t',t'') 
               -> lmsg ^ (to_html t) ^ rmsg ^ arrow ^ (to_html t') ^ lpar ^ 
	(to_html t'') ^ rpar
    | TVar s   -> Html.id s
    | TArrow (t,t')   -> lpar ^ (to_html t) ^ arrow ^ (to_html t') ^ rpar
    | TAsync -> hasync
    | TSync -> hsync 
    | TReply (_,_) -> hreply

let rec to_string t  =
  match t with
      TUnit    -> "unit"
    | TBool    -> "bool"
    | TString  -> "string"
    | TInt     -> "int"
    | TVmid    -> "vmid"
    | TKell    -> "kell"
    | TProc    -> "proc"
    | TTuple l -> "(" ^ String.concat "," (List.map to_string l) ^ ")" 
    | TChan (t,t',_)  
               -> "<" ^ (to_string t)  ^ ">"  
    | TVar s   -> s
    | TList t  -> (to_string t) ^ " list"
    | TArrow (t,t') 
               -> "(" ^ (to_string t) ^ "->" ^  (to_string t') ^ ")"
    | TAsync   -> "async"
    | TSync    -> "sync"
    | TReply (_,_)  
               -> "reply"
