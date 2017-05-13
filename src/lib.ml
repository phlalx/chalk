open Ktype

let syscall_size = 10 
  
type lib_process = unit -> unit 

type 'location syscall = 'location Values.t -> unit 

type 'location t = {
  port      : int ;
  mutable waitmsg   : Thread.t ;
  events    : (unit -> unit) EventQueue.t ;
  threads   : ThreadPool.t ;
  syscalls  : (Uin.t, ('location syscall)) Hashtbl.t ;
  runqueue  : lib_process Queue.t ;
  mutable to_top_root : 'location Values.loc_interface option
  (* to_top_root is a function that put messages in the reactor
     of the root location *)
}

let server_mode lib = lib.port >= 1024
 
let some x = match x with None -> assert false | Some x -> x 

let echoId       = Uin.generate_well_known 1 
and echointId    = Uin.generate_well_known 2 
and sendId       = Uin.generate_well_known 3 
and receiveId    = Uin.generate_well_known 4 
and printId      = Uin.generate_well_known 5 
and executeId    = Uin.generate_well_known 6 

let initenv port = 
  [  ("echo",        Values.VId echoId); 
     ("echo_int",    Values.VId echointId);
     ("print",       Values.VId printId);
     ("send",        Values.VId sendId ) ;  
     ("receive",     Values.VId receiveId);
     ("thisloc",     Values.VVmid ("localhost", port)  );
     ("execute",     Values.VId executeId)]

let echoTy = TChan (TString, TProc, TAsync)
and echointTy = TChan (TInt, TProc, TAsync)
and printTy = TChan (TString, TUnit, TSync)
and sendTy = TChan ((TTuple [TVmid ; TString]), TProc, TAsync)
and receiveTy = TChan (TString, TProc, TAsync) 
and thislocTy =  TVmid 
		   
let typingenv  =
  [("echo", echoTy ); 
   ("echo_int", echointTy )  ;
   ("print", printTy ) ;
   ("send", sendTy  ); 
   ("receive", receiveTy ); 
   ("thisloc", thislocTy ) 
]
   
let reduction lib = (
  try 
    (Queue.pop lib.runqueue ())  
  with
      Queue.Empty -> ()) ; not (server_mode lib)
	  
let envcall_echo lib (value: 'location Values.t) : unit =
  Action.gen (Action.Syscall "echo") ; 
  match value with 
      Values.VString s -> print_string s ; flush stdout
    | _ -> assert false

let envcall_echoint lib value =
  Action.gen (Action.Syscall "echoint") ; 
  match value with 
      Values.VInt i -> print_int i ; flush stdout
    | _ -> assert false

let envcall_print lib (value:'location Values.t) : unit =
  match value with 
      Values.VTuple [ Values.VId id ; Values.VString s ] -> 
	begin
	  print_string s;
	  flush stdout;
	  (some lib.to_top_root) id Values.VNil 
	end
    | _ -> assert false

(*
let envcall_execute value cont =  fun () -> () 
  (*  Action.gen runtime.actions (Action.Syscall "execute") ; *)
  match value with 
						 Values.VTuple [ Values.VId callback_channel ;
						 Values.VList(program) ] -> 
	let buffer = Buffer.create 80
	in
	let unix_command = 
	  List.iter 
	    (fun x -> Buffer.add_string buffer (Values.t_to_string x); 
	       Buffer.add_char buffer ' ') program;
	  Buffer.contents buffer
	and cb_channel   = ()
	in
	let runnable () =
	  ignore (Unix.system unix_command);
	  EventQueue.push runtime.events (fun () -> ()) (*insert callback here*)
	    (*Execute the command out-of-thread, then push the continuation back 
	      on the *)
	in
	  ThreadPool.enqueue runtime.threadsrunnable
    | _ -> assert false
*)
	
 
let envcall_send lib value =
  Action.gen (Action.Syscall "send") ; 
  match value with
      Values.VTuple [ Values.VVmid (ip,port); Values.VString s ] -> 
	ignore (Distr.send ((Distr.resolve_host ip), port) [s] ) 
    | _ -> assert false

let syscalls = 
  [ (echoId, envcall_echo) ; 
    (echointId, envcall_echoint) ; 
    (sendId, envcall_send) ;
    (printId, envcall_print)
  ]

let max_number_of_threads = 50

(**
 * Non-blocking execution of an external command.
 *
 * Note : this execution uses OCaml threads and is therefore not robust for massive concurrency.
 * As much as possible, system calls should use primitive non-blocking system calls, which are
 * much more robust.
 *)

let handle_system_events lib =
  match EventQueue.pop_all lib.events with
      None     -> false
    | Some l -> 
	Queue.iter (fun x -> x()) l; 
	true 

let net2 lib =
  let send_down x =  
    (some lib.to_top_root) receiveId (Values.VString x) 
  in
  let getmsgs () =
    let msgs = Distr.get () in
      EventQueue.push_many 
	lib.events
	(List.map (fun x -> 
		     send_down x ;
		     (fun () -> ())) msgs) 
  in
    while true do
      getmsgs ()
    done 

let create port = 
  let lib  = { 
    port = port ;
    waitmsg = Thread.create (fun x -> ()) ()  ;
    threads   = ThreadPool.create max_number_of_threads;
    events    = EventQueue.create () ;
    syscalls = Hashtbl.create syscall_size  ;
    runqueue = Queue.create () ;
    to_top_root = None
  } in 
    if (port >= 1024) then 
      ( 
	Distr.init_server port ;
	lib.waitmsg  <- Thread.create net2 lib 
      ) ; 
    List.iter (fun (x, y) -> Hashtbl.add lib.syscalls x (y lib)) syscalls ;
    lib

let connect_to_root lib loc_itf =
  lib.to_top_root <- Some loc_itf 
 
let get_interface  
  (lib:'location t)
  (uin:Uin.t) 
  (value:'location Values.t)  =
  try
    let libcall = fun _ -> Hashtbl.find lib.syscalls uin value in
      Queue.push libcall lib.runqueue 
  with
      Not_found -> assert false (* to be replaced by a warning *) 
