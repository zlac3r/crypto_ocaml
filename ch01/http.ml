let parse_url url =
  let split_url = String.split_on_char '/' url in
  match split_url with
  | protocol :: "" :: host :: [] -> (host, "/")
  | protocol :: "" :: host :: path -> (host, String.concat "/" path)
  | _ -> ("", "")

let http_get socket path host =
  let get_command = Printf.sprintf "GET /%s HTTP/1.1\r\nHost: %s\r\n" path host in
  let _ = Unix.send_substring socket get_command 0 (String.length get_command) [] in
  let get_command = Printf.sprintf "Connection: close\r\n\r\n" in
  Unix.send_substring socket get_command 0 (String.length get_command) []


let display_result socket =
  let recv_buf = Bytes.create 256 in
  let rec display received =
    if received > 0 then (
      Bytes.set recv_buf received '\x00'; 
      Printf.printf "%s" (Bytes.to_string recv_buf);
      display (Unix.recv socket recv_buf 0 255 [])
    )
    else (
      ()
    )
  in
  display (Unix.recv socket recv_buf 0 255 []);
  Printf.printf "\n"
    
let () =
  let url = Sys.argv.(1) in
  let (host, path) = parse_url url in
  let () = Printf.printf "Connecting to host '%s'\n" host in
  let client_connection = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let host_name = Unix.gethostbyname host in 
  let host_address = Unix.ADDR_INET (host_name.h_addr_list.(0), 80) in
  let () = Unix.connect client_connection host_address in
  let () = Printf.printf "Retrieving document: %s\n" path in
  let status = http_get client_connection path host in
  let () = Printf.printf "%d" status in
  let () = display_result client_connection in
  Unix.close client_connection
