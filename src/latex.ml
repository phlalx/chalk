
let acc kwd s = "\\" ^ kwd ^ "{" ^ s ^ "}"

let id s = acc "texttt" s
let string s = acc "texttt" s
let br = "\\\\\n" 
let space = " "
let kwd s = acc "texttt" s
let int i = acc "texttt" (Int32.to_string (Int32.of_int i))

let env s = "\\begin{texttt}\n" ^ s ^ "\\end{texttt}" 
