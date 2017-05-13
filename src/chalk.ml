(***************************************************************************
 *   Copyright (C) 2005                                                    *
 *     Philippe Bidinger (Philippe.Bidinger@inrialpes.fr)                  *
 *     David Teller (D.O.Teller@sussex.ac.uk)                              *
 *                                                                         *
 *                                                                         *
 *                                                                         *
 *   This file is part of CHALK.                                           *
 *                                                                         *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Library General Public License as       *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU Library General Public     *
 *   License along with this program; if not, write to the                 *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************)

 
(**
 * File :  chalk.ml
 *
 * Unit :  Main
 *
 * Description :
 *
 * Author : Philippe Bidinger
 *
 *)


open Error

let output_size = ref 30 

let port = ref 0

let output = ref ""

let in_file = ref "" 
let html_out_file = ref "" 
let latex_out_file = ref "" 

let in_filed = ref stdin 
let html_out_filed = ref stdout
let latex_out_filed = ref stdout

let res_typecheck = ref false
    
let version = "Chalk\n" 

let syntax_tree = ref (Syntax.Nil dummyinfo)

let args =    
  [("-v", Arg.Unit (fun _ -> print_string version ; exit 1) ,"version") ;
   ("-p", Arg.Int (fun x -> port := x ) ,"port") ;
   ("-o", Arg.String (fun x -> html_out_file := x ) ,"output name") ; 
(*   ("-l", Arg.String (fun x -> latex_out_file := x ) ,"latex output name") ;  *)
   ("-s", Arg.Int (fun x -> output_size := x ), "output size : default = 30 cycles")]
   
let parse_args () =
  let inFile = ref (None : string option) in
  Arg.parse args
     (fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "usage: chalk file";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> in_file := s
 
let main =
  parse_args () ;

  let open_file f =
    try 
      open_in f 
    with
	_ -> err ("Could not find " ^ !in_file)
  in
    in_filed := open_file !in_file ; 
    let lexbuf = Lexer.create !in_file !in_filed in
    let result =
      try 
	Parser.main Lexer.token lexbuf 
      with 
	  Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error"
    in
      Parsing.clear_parser () ; 
      close_in !in_filed ; 
      syntax_tree := result.v ;

      res_typecheck := Typecheck.type_check (!syntax_tree) ;
       
      if (String.length (!latex_out_file) != 0) then (
	latex_out_filed := open_out !latex_out_file ;  
	let output s = output_string !latex_out_filed  s in
	  output (Syntax.to_latex !syntax_tree)  ;
	  close_out !latex_out_filed 
      ) ;
	
      if (String.length (!html_out_file) != 0) then (
	html_out_filed := open_out !html_out_file ;  
	let output s = output_string !html_out_filed  s in
	  output ( Html.h1 ("Typechecking")) ; 
	  output (Html.open_html (!html_out_file)) ;
	  output (    Html.table13 (Syntax.to_html !syntax_tree) 
			!(Typecheck.html_constraints)   
			!(Typecheck.html_solved) ) ;
	  output (Html.hr) ;
	  
	  if (not !res_typecheck) then (
	    output Html.close_html ;
	    close_out !html_out_filed 
	  )
      ) ;
      
      if (not !res_typecheck) then
	exit 0 ;
      
      Runtime.init_runtime (Transform.transform !syntax_tree) !port ;
    
      if (String.length (!html_out_file) != 0) then (
	let output s = output_string !html_out_filed  s in
	let i = ref 1 in
	
	output ( Runtime.to_html ()) ; 
        while (!i < !output_size) && (not (Runtime.schedule ())) do
	  output (Runtime.to_html ()) ;
          flush !html_out_filed ;
          i := !i + 1  
        done ;
	
	output Html.close_html ;
	close_out !html_out_filed 
      ) ;
      
      while not (Runtime.schedule ()) do () done
      
	
      
