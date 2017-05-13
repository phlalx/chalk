let br = "<br>\n"

let space =  "&nbsp;"

let kwd s = "<font color=\"#7f0000\"><tt>" ^ s ^ "</tt></font>"

let id s = "<tt>" ^ s ^ "</tt>"

let id_span s v =  
  if v = "" then 
    id s 
  else 
    "<tt><span title= \" " ^ v ^ " \" >" ^ s ^ "</span></tt>"

let action s  = "<font color=\"#007f00\"><tt>" ^ s ^ "</tt></font>"

let string s = "<font color=\"#3f3f3f\"><tt>" ^ s ^ "</tt></font>"
let integer s = "<font color=\"#00007f\"><tt>" ^ s ^ "</tt></font>"
let int i = integer (Int32.to_string (Int32.of_int i))

let h6 s = "<h6> " ^ s ^ "</h6>\n" 
let h5 s = "<h5> " ^ s ^ "</h5>\n" 
let h4 s = "<h4> " ^ s ^ "</h4>\n" 
let h3 s = "<h3> " ^ s ^ "</h3>\n" 
let h2 s = "<h2> " ^ s ^ "</h2>\n" 
let h1 s = "<h1> " ^ s ^ "</h1>\n" 

let hr = "<hr>"

let code s = "<code> " ^ s ^ "</code>\n" 

let small s = "<small>" ^ s ^ "</small>\n"

let table12 c1 c2  = 
   "<table  border = \"0\" ><tr VALIGN=top><td>\n" ^ c1 ^  
   "</td><td>" ^ c2 ^
   "</td></tr></table>"

let table13 c1 c2 c3  = 
   "<table  border = \"1\" ><tr VALIGN=top><td>\n" ^ c1 ^  
   "</td><td>" ^ c2 ^
   "</td><td>" ^ c3 ^
   "</td></tr></table>"

let table22 n1 n2 c1 c2  = 
   "<table  border = \"1\" ><tr VALIGN=top><td>\n" ^ n1 ^  
   "</td><td>" ^ n2 ^
   "</td></tr><tr><td valign = \"top\">" ^ c1 ^  
   "</td><td valign = \"top\">" ^ c2 ^ 
   "</td></tr></table>"

let rec list_to_html f l =
  match l with
      [] -> ""
    | h :: t -> (f h) ^ list_to_html f t


let tablen2 l  =
  let aux (x,y) = "<tr><td><small>" ^ x ^ "</small></td><td><small>" ^ y ^ "</small></td><tr>\n" in
    "<table>\n" ^  list_to_html aux l    ^ "</table>\n"


let list_to_html_list f l =
  let rec aux f l =
    match l with
	[] -> ""
      | h :: t -> "<li>\n" ^ (f h) ^  "</li>\n" ^ (aux f t)
  in 
    if (aux f l) = "" 
    then 
      "" 
    else
      "<ul>\n" ^ (aux f l) ^  "</ul>\n"

let open_html title = "<html>\n <head>\n <title>" ^ title ^ "  </title>\n </head>\n<body>\n" 

let close_html = "</body>\n </html>\n"
