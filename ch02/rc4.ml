module Byte = struct
  include Bytes
  let g_ b i = Char.code (get b i)
  let s_ b i v = set b i (Char.chr v)
  let print_byte b =
    let char_seq = Bytes.to_seq b in
    Printf.printf "0x";
    Seq.iter (fun c -> Printf.printf "%.2x" (Char.code c)) char_seq;
    Printf.printf "\n"
end

let rc4_operate (plaintext: bytes) (plain_len: int) (ciphertext: bytes) (key: bytes) (key_len: int) : unit =
  let s = Array.make 256 0 in
  for i = 0 to 255 do
    s.(i) <- i
  done;
  let j = ref 0 in
  for i = 0 to 255 do
    j := (!j + s.(i) + (Byte.g_ key (i mod key_len))) mod 256;
    let tmp = s.(i) in
    s.(i) <- s.(!j);
    s.(!j) <- tmp
  done;

  let (i, j) = (ref 0, ref 0) in
  for loop = plain_len downto 1 do
    i := (!i + 1) mod 256;
    j := (!j + s.(!i)) mod 256;
    let tmp = s.(!i) in
    s.(!i) <- s.(!j);
    s.(!j) <- tmp;
    Byte.s_ ciphertext (plain_len - loop) (s.((s.(!i) + s.(!j)) mod 256) lxor (Byte.g_ plaintext (plain_len - loop)))
  done

let () =
  let e_or_d = Sys.argv.(1) in
  let key = Sys.argv.(2) in
  let input = Sys.argv.(3) in
  let len = String.length input in
  let key_len = String.length key in
  let m_input =
    if input.[0] = '0' && input.[1] = 'x' then
      Hex.to_string (Hex.(`Hex (String.sub input 2 (len - 2))))
    else
      input
  in  
  let input_len = String.length m_input in
  let output = Bytes.create input_len in
  rc4_operate (Bytes.of_string m_input) input_len output (Bytes.of_string key) key_len;
  Byte.print_byte output;
  Printf.printf "%s\n" (Byte.to_string output)
