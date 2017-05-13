open Error

type direction = Up | Down of string | In | Pass

type direction_msg = MUp | MIn | MDown of string

(* Trig  : "string option ref" is the receiver carrying the reply channel   *
 *         Ref None after parsing, updated in typecheck                     *
 * Reply : "direction option ref" is the direction of the receiver carrying *
 *          the reply channel                                               *)

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

(* bool = true if replicated *)
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
  | VTuple      of info * value list
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
  
val fresh_var : unit -> string

val fresh_name : unit -> string

val to_html : t -> string

val to_latex : t -> string
