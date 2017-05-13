
val br : string

val kwd : string -> string

val space : string

val id : string -> string
val id_span : string -> string -> string
val action : string -> string

val string : string -> string
val integer : string -> string 
val int : int -> string
val h6 : string -> string
val h5 : string -> string
val h4 : string -> string
val h3 : string -> string
val h2 : string -> string
val h1 : string -> string
val small : string -> string
val table22 : string -> string -> string -> string -> string
val table12 : string -> string -> string 
val table13 : string -> string -> string -> string 
val tablen2 : ((string * string) list) -> string

val hr : string

val code : string -> string

val open_html : string -> string 
val close_html : string

val list_to_html : ('a -> string) -> ('a list) -> string
val list_to_html_list : ('a -> string) -> ('a list) -> string
