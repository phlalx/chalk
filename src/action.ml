type action =
    Reduce     of Uin.t
  | SReduce    of Uin.t
  | Info of   string 
  | Passivate    of Uin.t * Uin.t
  | Empty_rq 
  | GetRef        of string
  | Reaction      of string 
  | PushReactant  of string
  | Push          of Uin.t 
  | Pop_rq
  | Nil 
  | Val
  | New           of string * Uin.t
  | Let           of string
  | If            of bool 
  | Msg           of Uin.t (* direction_msg  *)
  | Kell          of Uin.t 
  | Par 
  | Syscall       of string
  | Waiting
  | CreateMsg     of string 
  | Trig          of Uin.t 

type t = action Queue.t
    
let aqueue = Queue.create ()
	       
let clear () = Queue.clear aqueue
		 
let gen a = Queue.push a aqueue 
	      
let info s = Queue.push (Info s) aqueue

let action_to_html action =
  let ind =  Html.space ^ Html.space ^ Html.space in
  let ind2 = ind ^ ind in
  let ind3 = ind ^ ind ^ ind in (* 
  let uindir u d = 
    Uin.to_html u ^ ", " ^ direction_to_html d in *)
  match action with
    | Info s-> "Info:" ^ s 
    | Reduce i -> "Reduce location" ^ Uin.to_html i ^ "." 
    | SReduce i -> "SubReduce location " ^ Uin.to_html i ^ "." 

    | Pop_rq ->  ind ^ "Pop runqueue."
    | Push l -> 
    ind ^  "Push closure in runqueue of location " ^ Uin.to_html l ^  "."

    (* possible runqueue element  *)
    | Empty_rq -> ind ^ "Runqueue is empty."
    | Nil -> ind ^ "Nil process: discard it."
    | Val -> ind ^ "Value : evaluate it."
    | New (x, u) -> 
	ind ^ "New : Bind name " ^ Html.id x ^ 
	" to new identifier " ^ Uin.to_html u ^ "."
    | Let x -> 
	ind ^ "Let : evaluate value and bind it to variable "
	^ Html.id x ^ "."
    | If b -> 
	ind ^ "If-Then-Else : evaluate if condition to " ^ 
	(if b then Html.kwd "true" else Html.kwd "false") 
	  ^ "."
    | Msg u -> ind ^ "Message on channel: " ^ Uin.to_html u ^ "." 
    | Trig u -> ind ^ "Trigger on channel: " ^ Uin.to_html u ^ "." 
    | Kell u -> ind ^ "Kell : Create new location " ^ Uin.to_html u 
	^ "."
    | Par -> ind ^ "Parallel composition."
 
    (*  interaction with module reactor *)
    | GetRef s-> ind ^ "Get reaction queue for key: " ^ s 
    | PushReactant s-> ind2 ^ "Push reactant:" ^ s 
    | Reaction s-> ind2 ^ "Reaction to be performed: " ^ s 

    (* Possible reactions  *)
    | Waiting -> ind3 ^ "Existing messages for this location." 

    | CreateMsg s  -> ind3 ^ "Recreate Msg: " ^ s
    | Passivate (u,v) ->  ind3 ^ "Passivate location " ^ Uin.to_html u ^
	 " from location " ^ Uin.to_html v ^ "."

    | Syscall s -> ind ^ "System call " ^ Html.kwd s ^ "."
   	
let to_html () = Queue.fold (fun x y  -> x ^ (action_to_html y) ^ Html.br) 
		 "" aqueue    
