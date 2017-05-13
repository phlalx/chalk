let pending_requests = 10

let message_length = 2048

let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 

let resolve_host server =
    let server_addr =
        try
            Unix.inet_addr_of_string server
        with
        | Failure ("inet_addr_of_string") ->
              (Unix.gethostbyname server).Unix.h_addr_list.(0)
    in server_addr

let server_sock = ref Unix.stdin

let init_server port =  
   let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
   Unix.bind sock (Unix.ADDR_INET ((resolve_host "localhost"), port )) ;   
   server_sock := sock

let get () =
   let buf = ref (String.create message_length) in
   let (len, addr) = Unix.recvfrom !server_sock !buf 0 message_length [] in
   let answer = String.sub !buf 0 len in
   let x = Marshal.from_string answer 0 in x
    
let send i t =   
   let (ip,port) = i in 
   let sockaddr = Unix.ADDR_INET (resolve_host "localhost", port) in
   let ser = Marshal.to_string t [] in 
   let res = Unix.sendto sock ser 0 (String.length ser) [] sockaddr in
     res > 0
