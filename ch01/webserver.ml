let build_success_response connection =
  let response = "HTTP/1.1 200 Success\r\nConnection: Close\r\nContent-Type:text/html\r\n\r\n\r\n<html><head><title>Test Page</title></head><body>Nothing here</body></html>\r\n" in
  Unix.send_substring connection response 0 (String.length response) []
  
let build_error_response connection =
  let response = "HTTP/1.1 501 Error occurred\r\n" in
  Unix.send_substring connection response 0 (String.length response) []

let process_http_request connection =
  let inchan = Unix.in_channel_of_descr connection in
  try
    let line = input_line inchan in
    let is_GET = String.equal "GET" (String.sub line 0 3) in
    if is_GET then
      let _ = build_success_response connection in
      Unix.close connection
    else
      let _ = build_error_response connection in
      Unix.close connection
  with
    End_of_file -> ()

let () =
  let listen_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt listen_sock Unix.SO_REUSEADDR true;
  let local_addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 8089) in
  Unix.bind listen_sock local_addr;
  Unix.listen listen_sock 5;
  while true do
    let (connect_sock, client_addr) = Unix.accept listen_sock in
    process_http_request connect_sock
  done
