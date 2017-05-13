
(*
open Error


type closure = (environment * Syntax.t)

and rqelements = Closure of closure | Tag of Uin.t * direction_msg
    
and environment = (string * value) list 

and value = 
    VNil
  | VTrue
  | VFalse
  | VIp     of (string * int)
  | VId     of Uin.t
  | VInt    of int
  | VString of string
  | VTuple  of (value list)
  | VClos   of closure 
  | VPass   of bool ref * location
  | VList   of (value list)
  
and location = 
    { 
      mutable passivable : trigger option ;
      mutable uin : Uin.t ;
      mutable name : string ;               (* needed for html output only *)
      runqueue : rqelements Queue.t ;
      mutable parentlocation : location option ;
      mutable sublocations : location list ;
      mutable trignb : int ;
      mutable triggers : trigger  list ;
      table : table ;
      waiting_msgs : value table2 ;
    }
      
and table = { 
  up : (Uin.t, (trigger * int) ) Hashtbl.t ; 
  loc : (Uin.t, (trigger * int) ) Hashtbl.t ; 
  down : ( Uin.t, (trigger * int) ) Hashtbl.t ; 
  sublocs : ( Uin.t, locorpass ref )  Hashtbl.t ; 
}
   
and 'a table2 = { 
  up2 : (Uin.t, 'a Queue.t ) Hashtbl.t ; 
  loc2 : (Uin.t, 'a Queue.t ) Hashtbl.t ; 
  down2 : ( (Uin.t * Uin.t), 'a Queue.t ) Hashtbl.t ; 
}

and locorpass = Location of location | Passivable of trigger

and trigger = {
  mutable loca : location ;
  passivvar : string option ;
  passivuin : Uin.t option ;
  variables : string list array ;
  queues : value Queue.t array ;
  closure : closure ;
  final : int array ;
  trigger : closure ;                  (* for output information *)
  mutable num : int ;                  (* for output information *)
} 

and direction_msg = MUp | MIn | MDown of Uin.t


(**** evaluation *)

val lookup : string -> environment -> value 

val lookupid : string -> environment -> Uin.t

val evaluate : environment -> Syntax.value -> value

val value_to_string : value -> string

val value_to_int : value -> int

val value_to_ip : value ->  (string * int) * value



*)
(**************************************************************)
(* Runtime                                                    *)
(**************************************************************)


val init_runtime : Basesynt.t -> int -> unit 

val schedule : unit -> bool

val to_html : unit -> string  




(*






(**** trigger creation and message routing *)

val addtrigger : location -> environment -> Syntax.pattern -> Syntax.t -> unit

val routemsg : location  -> environment -> string -> Syntax.value -> Syntax.direction_msg -> bool

val addcell : location -> location -> unit


(**** locations *)

 val new_top :
    Uin.t ->
    closure -> (environment * Syntax.pattern * Syntax.t) list -> location

  
val new_location : location -> string -> Uin.t -> closure -> location

val location_to_html : location -> string

val passivate : trigger -> location -> unit




*)
