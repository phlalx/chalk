%{
open Error
open Syntax
%}


/* keywords */

%token <Error.info> ENV 
%token <Error.info> REPLY 
%token <Error.info> TO
%token <Error.info> NEW 
%token <Error.info> LET 
%token <Error.info> ONCE  
%token <Error.info> ON  
%token <Error.info> IN 
%token <Error.info> NIL 
%token <Error.info> TKELL 
%token <Error.info> TINT 
%token <Error.info> TSTRING 
%token <Error.info> TPROC 
%token <Error.info> TUNIT 
%token <Error.info> TBOOL 
%token <Error.info> TLIST 
%token <Error.info> TVMID
%token <Error.info> IF 
%token <Error.info> THEN 
%token <Error.info> ELSE
%token <Error.info> TRUE 
%token <Error.info> FALSE 
%token <Error.info> UP 
%token <Error.info> DOWN 
%token <Error.info> VM
%token <Error.info> NOT 
%token <Error.info> AND 
%token <Error.info> OR
%token <Error.info> MARSHALL
%token <Error.info> UNMARSHALL
%token <Error.info> AS
%token <Error.info> EMPTYLIST
%token <Error.info> CONS
%token <Error.info> HEAD
%token <Error.info> TAIL
%token <Error.info> ISNIL

/* symbolic tokens */

%token <Error.info> DOT
%token <Error.info> COL 
%token <Error.info> SCOL 
%token <Error.info> PAR 
%token <Error.info> COM 
%token <Error.info> VNIL
%token <Error.info> PLUS 
%token <Error.info> MINUS
%token <Error.info> MULT 
%token <Error.info> DIV  
%token <Error.info> EQ 
%token <Error.info> CONC
%token <Error.info> LPAREN 
%token <Error.info> RPAREN 
%token <Error.info> LKELL 
%token <Error.info> RKELL 
%token <Error.info> LMSG 
%token <Error.info> RMSG
%token <Error.info> LBR 
%token <Error.info> RBR
%token  <Error.info> APPLY
%token <Error.info> EOF


/* identifiers and constant values */ 
%token <int Error.withinfo> INT
%token <string Error.withinfo> STRING
%token <string Error.withinfo> NAME 
%token <string Error.withinfo> UNAME

%left IN
%left PAR
%left ELSE
%left CONS
%right SCOL

%right EQ
%left  OR
%left  AND
%left  PLUS MINUS
%left  MULT DIV
%left  CONC 
%left HEAD TAIL ISNIL VM
%left  APPLY

%start main
%type <Ktype.t> ktype
%type <string Error.withinfo> name
%type <Syntax.value Error.withinfo> value
%type <Syntax.t Error.withinfo> main
%% 

main: 
     proc EOF { $1 }
;

proc:
     NIL	                 { { i = $1;    v = Nil $1 }  }
|    proc PAR proc               { { i = $1.i;  v = Par ($1.i, $1.v, $3.v) }}  
|    NEW name optype IN proc     { { i = $1;    v = New ($1, $2.v, $3 ,$5.v)}}
|    name LKELL opproc  RKELL    { { i = $1.i;  v = Kell ($1.i, $1.v, $3.v)} }  
|    LPAREN proc RPAREN         { { i = $1; v = $2.v   } }  
|    ONCE lpattern LBR proc RBR  
	 { { i = $1; v = Trig ($1, ($2, false) , (ref None),  $4.v)} } 
|    ON lpattern LBR proc RBR  
	 { { i = $1; v = Trig ($1, ($2, true), (ref None),  $4.v)} } 
|    IF value THEN proc ELSE proc   
	 { { i = $1;    v = ITE ($1, $2.v, $4.v, $6.v) } }
|    LET name EQ value IN proc  { { i = $1; v = Let ($1, $2.v, $4.v, $6.v) } } 
|    value1                     { { i = $1.i ; v = Value ($1.i, $1.v) }  }
|    REPLY value opttoname        
	 { { i = $1 ; v = Reply ($1, $2.v, $3.v, ref None ) }} 
|    value1 SCOL proc             { { i = $1.i ; v = Com ($1.i, $1.v, $3.v)}}
;

opttoname:
                                           { { i = dummyinfo;  v = None} }
|  TO name                                 { { i = $1; v = Some $2.v } }  

directmsg:
   name                                   { { i = $1.i; v = ( MIn, $1.v )  }		}			
|  name DOT name                          { { i = $1.i; v = ((MDown $1.v), $3.v )  } }
|  ENV DOT name                           { { i = $1; v = ( MUp, $3.v ) } }
  
  
optype:
                 { Ktype.generate () } 		  
|   COL ktype    { $2 } 

opproc:
     	                         { { i = dummyinfo; v = Nil dummyinfo } }
|    proc                        { $1 }

pattern:
|       name LKELL name RKELL      {  $1.v, [ $3.v ], Pass  }
|       name LMSG lname RMSG       {  $1.v, $3.v, In }
|	name UP LMSG lname RMSG    {  $1.v, $4.v, Up }
|	name DOWN opname LMSG lname RMSG  {  $1.v, $5.v, Down $3.v  }


opname:
     	                           { { i = dummyinfo; v = Syntax.fresh_var () } }
|    name                          { $1 }

lpattern:
     pattern                         { [ $1 ] }  
|    pattern PAR lpattern            { $1 :: $3 }

ktype:
     TUNIT                 { Ktype.TUnit }
|    TBOOL                 { Ktype.TBool }
|    TVMID                 { Ktype.TVmid }
|    TPROC	           { Ktype.TProc }
|    TINT	           { Ktype.TInt }
|    TSTRING	           { Ktype.TString }
|    TKELL	           { Ktype.TKell }
|    LPAREN lktype3 RPAREN { Ktype.TTuple $2 }
|    LMSG lktype RMSG      
	 { Ktype.TChan ($2, Ktype.generate (), Ktype.generate ()) }
|    ktype TLIST           { Ktype.TList $1 }
;

lktype: 
     	                  { Ktype.TUnit }
|    ktype COM lktype2    { Ktype.TTuple ($1 :: $3) }
|    ktype                { $1 }
;

lktype3:
|    ktype COM lktype2    { $1 :: $3  } 

lktype2: 
|    ktype COM lktype2    { $1 :: $3 }
|    ktype                { [ $1 ] }
;

value:
  value1                   {  $1 }
| value2                   {  $1 }

value1:
|     name                 {  {i = $1.i ; v = VId ($1.i, $1.v)} } 
|     value APPLY value    {  {i = $1.i ; v = VAppli ($1.i, $1.v, $3.v)} }
|    directmsg LMSG optlvalue RMSG 
	 {  let (a,b) = $1.v in { i = $1.i; v = VMsg ($1.i, b, $3.v, a)} }  
value2:  
|     VNIL                 {  {i = $1; v = VNil $1 } }
|     TRUE                 {  {i = $1; v = VTrue $1 } }
|     FALSE                {  {i = $1; v = VFalse $1 } } 
|     INT	           {  {i = $1.i; v =  VInt ($1.i, $1.v) } }
|     VM value             {  {i = $1; v = VVmid ($1, $2.v)  } }
|     EMPTYLIST            {  {i = $1; v = VEmptyList ($1)  } }
|     ISNIL value          {  {i = $1; v = VIsNil ($1, $2.v)  } }
|     HEAD value  {  {i = $1; v = VHead ($1, $2.v)  } }
|     TAIL value  {  {i = $1; v = VTail ($1, $2.v)  } }
|     MARSHALL value COL ktype       
                              {  {i = $1; v = VMarshall ($1, $2.v, $4)  } }
|     UNMARSHALL value AS ktype     
                              {  {i = $1; v = VUnmarshall ($1, $2.v, $4)  } }
|     LPAREN lvalue RPAREN    {  {i = $1; v = $2.v } }
|     STRING                  {  {i = $1.i ; v = VString ($1.i,$1.v) } }
|     MINUS value             {  {i = $1; v = VUMinus ($1,$2.v) } } %prec MULT 
|     NOT value               {  {i = $1; v = VNot ($1, $2.v) } } %prec AND  
|     value CONS value     {  {i = $1.i;  v = VCons ($1.i, $1.v, $3.v) ;  } }
|     LKELL listvalues RKELL  {  {i = $1; v = VList ($2.i, $2.v) } }
|     value PLUS value        {  {i = $1.i ; v = VPlus ($1.i , $1.v, $3.v) } }
|     value MINUS value       {  {i = $1.i ; v = VMinus ($1.i , $1.v, $3.v) } }
|     value MULT value        {  {i = $1.i ; v = VMult ($1.i , $1.v, $3.v) } }
|     value DIV value         {  {i = $1.i ; v = VDiv  ($1.i , $1.v, $3.v) } }
|     value EQ value          {  {i = $1.i ; v = VEq  ($1.i , $1.v, $3.v) } }
|     value AND value         {  {i = $1.i ; v = VAnd ($1.i , $1.v, $3.v) } }
|     value OR value          {  {i = $1.i ; v = VOr ($1.i ,$1.v ,$3.v ) } }
|     value CONC value        {  {i = $1.i ; v = VApp  ($1.i, $1.v, $3.v)} }
|     params LBR proc RBR      { {i = $1.i ; v = VProc ($1.i, $1.v, $3.v)}  }
;

param:
      LPAREN name optype RPAREN   {  {i = $1;   v = ($2.v, $3) } }

params:
                              {  {i = dummyinfo ; v = []  }  }
|     param                   {  {i = $1.i ; v = [ $1.v ] }  }
|     param params            {  {i = $1.i; v = $1.v :: $2.v } }                           
;

lvalue: 
|    value       	   { $1 }
|    value COM tvalue      { { i = $1.i; v = VTuple ($1.i, ($1.v :: $3.v)) } }
;

tvalue:
|    value                 { { i = $1.i ; v = [ $1.v ] } }
|    value COM tvalue      { { i = $1.i; v = $1.v :: $3.v } } 

optlvalue: 
                           { { i = dummyinfo; v = VNil dummyinfo } }
|    lvalue       	   { $1 }  
;

listvalues: 
|    value       	        { { i = $1.i; v = [ $1.v ] } }
|    value SCOL listvalues     { { i = $1.i; v = $1.v :: $3.v } }
;

name:
	NAME		  { $1 }
|       UNAME 	          { $1 }
;

lname:
	                   { { i = dummyinfo; v = [] } }
|       name COM lname     {  { i = $1.i; v = $1.v :: $3.v }  }     
|       name               { { i = $1.i; v = [ $1.v ] } }
;

