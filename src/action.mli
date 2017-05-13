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

val clear : unit -> unit

val gen : action -> unit 

val info :  string -> unit

val to_html : unit -> string
