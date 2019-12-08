module Byte = struct
  include Bytes
  let g_ b i = Char.code (get b i)
  let s_ b i v black_magic = set b i (Char.chr v)
  let print_byte b = 
    let char_seq = Bytes.to_seq b in
    Printf.printf "0x";
    Seq.iter (fun c -> Printf.printf "%.2x" (Char.code c)) char_seq;
    Printf.printf "\n"
end

let test_mod = 
  let test_mod_list = [
    0xC4; 0xF8; 0xE9; 0xE1; 0x5D; 0xCA; 0xDF; 0x2B;
    0x96; 0xC7; 0x63; 0xD9; 0x81; 0x00; 0x6A; 0x64;
    0x4F; 0xFB; 0x44; 0x15; 0x03; 0x0A; 0x16; 0xED;
    0x12; 0x83; 0x88; 0x33; 0x40; 0xF2; 0xAA; 0x0E;
    0x2B; 0xE2; 0xBE; 0x8F; 0xA6; 0x01; 0x50; 0xB9;
    0x04; 0x69; 0x65; 0x83; 0x7C; 0x3E; 0x7D; 0x15;
    0x1B; 0x7D; 0xE2; 0x37; 0xEB; 0xB9; 0x57; 0xC2;
    0x06; 0x63; 0x89; 0x82; 0x50; 0x70; 0x3B; 0x3F
  ] in
  let test_mod_byte = Byte.create 64 in
  let rev_test_mod = List.rev test_mod_list in
  List.iteri (fun i ch -> Byte.set test_mod_byte i (Char.chr ch)) rev_test_mod;
  Z.of_bits (Byte.to_string test_mod_byte)

let test_pub_key = 
  let pub_byte = Byte.create 3 in
  Byte.set pub_byte 0 (Char.chr 0x01);
  Byte.set pub_byte 1 (Char.chr 0x00);
  Byte.set pub_byte 2 (Char.chr 0x01);
  Z.of_bits (Byte.to_string pub_byte)

let test_priv_key =
  let test_priv_key_list = [
    0x8a; 0x7e; 0x79; 0xf3; 0xfb; 0xfe; 0xa8; 0xeb;
    0xfd; 0x18; 0x35; 0x1c; 0xb9; 0x97; 0x91; 0x36;
    0xf7; 0x05; 0xb4; 0xd9; 0x11; 0x4a; 0x06; 0xd4;
    0xaa; 0x2f; 0xd1; 0x94; 0x38; 0x16; 0x67; 0x7a;
    0x53; 0x74; 0x66; 0x18; 0x46; 0xa3; 0x0c; 0x45;
    0xb3; 0x0a; 0x02; 0x4b; 0x4d; 0x22; 0xb1; 0x5a;
    0xb3; 0x23; 0x62; 0x2b; 0x2d; 0xe4; 0x7b; 0xa2;
    0x91; 0x15; 0xf0; 0x6e; 0xe4; 0x2c; 0x41
  ] in
  let test_priv_byte = Byte.create 63 in
  let rev_test_priv_key = List.rev test_priv_key_list in
  List.iteri (fun i ch -> Byte.set test_priv_byte i (Char.chr ch)) rev_test_priv_key;
  Z.of_bits (Byte.to_string test_priv_byte)


(* compute c = m ^ e mod n *)
let machine_word_size = 8

let rsa_compute (m: Z.t) (e: Z.t) (n: Z.t) : Z.t = Z.powm m e n

type rsa_key = {
  mod_n: Z.t;
  exp: Z.t;
}

let pub_key = {
  mod_n = test_mod;
  exp = test_pub_key;
}

let priv_key = {
  mod_n = test_mod;
  exp = test_priv_key;
}

let load_huge (message: bytes) : Z.t = Z.of_bits (Bytes.to_string message)
  

let rsa_encrypt (input: bytes) (len: int ref) (out: bytes ref) (pub_key: rsa_key) =
  let modulus_len = (Z.size pub_key.mod_n) * machine_word_size in
  let padded_block = Bytes.create modulus_len in
  let encrypted_size = ref 0 in
  let in_off = ref 0 in
  while (!len != 0) do
    encrypted_size := !encrypted_size + modulus_len;
    let block_size = if !len < (modulus_len - 11) then !len else modulus_len - 11 in
    Bytes.fill padded_block 0 modulus_len (Char.chr 0x00);
    Bytes.blit input !in_off padded_block (modulus_len - block_size) block_size;
    Bytes.set padded_block 1 (Char.chr 0x02);
    for i = 2 to modulus_len - block_size - 2 do
      (* TODO: make these random *)
      Bytes.set padded_block i (Char.chr i)
    done;
    let m = load_huge padded_block in
    let c = Bytes.of_string (Z.to_bits (rsa_compute m pub_key.exp pub_key.mod_n)) in
    out := Bytes.extend !out 0 !encrypted_size;
    Bytes.blit c 0 !out (!encrypted_size - modulus_len) modulus_len;
    len := !len - block_size;
    in_off := !in_off + block_size
  done

let rsa_decrypt (input: bytes) (len: int ref) (out: bytes ref) (priv_key: rsa_key) =
  let modulus_len = (Z.size priv_key.mod_n) * machine_word_size in
  let padded_block = Bytes.create modulus_len in
  let in_off = ref 0 in
  let out_len = ref 0 in
  while (!len != 0) do
    let c = load_huge (Byte.sub input !in_off modulus_len) in
    let m = Bytes.of_string (Z.to_bits (rsa_compute c priv_key.exp priv_key.mod_n)) in
    Bytes.blit m 0 padded_block 0 modulus_len;
    Printf.printf "padded_block[1] = %.2x\n" (Byte.g_ padded_block 1);
    let i = ref 2 in
    while ((Byte.g_ padded_block !i) != 0) do
      i := !i + 1
    done;
    out_len := !out_len + modulus_len - !i;
    out := Bytes.extend !out 0 !out_len;
    Bytes.blit padded_block !i !out (!out_len - (modulus_len - !i)) (modulus_len - !i);
    len := !len - modulus_len;
    in_off := !in_off + modulus_len
  done

let () =
  let e_or_d = Sys.argv.(1) in
  let input = Sys.argv.(2) in
  let len = String.length input in
  let m_input =
    if input.[0] = '0' && input.[1] = 'x' then
      Hex.to_string (Hex.(`Hex (String.sub input 2 (len - 2))))
    else
      input
  in  
  let input_len = ref (String.length m_input) in
  let output = ref Bytes.empty in
  let () =
    if e_or_d = "-e" then
      rsa_encrypt (Bytes.of_string m_input) input_len output pub_key
    else
      rsa_decrypt (Bytes.of_string m_input) input_len output priv_key
  in  
  Byte.print_byte !output;
  Printf.printf "%s\n" (Byte.to_string !output)

(*
 * TESTING RSA:
 *
 * ▶ ./rsa -e abc
0x75cc7464195921ac07567afeb7c9e3548e707788ccffff8ee44a7d42b2899a607953996911f497b6ca1b92c6e9763d28d50be9ddf561c7d0b5e98c5a9f111a5e
u?tdY!?Vz????T?pw??????J}B???`yS?i??????v=(?
                                            ???a?е?Z?^

Documents/ssl_implementation/ch03  master ✗                                                                      3d ◒
▶ ./rsa -d 0x75cc7464195921ac07567afeb7c9e3548e707788ccffff8ee44a7d42b2899a607953996911f497b6ca1b92c6e9763d28d50be9ddf561c7d0b5e98c5a9f111a5e
padded_block[1] = 02
0x00616263
abc
*)
