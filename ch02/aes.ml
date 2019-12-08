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

let (&) x y = (Char.code x) land y
let (^) x y = (Char.code x) lxor y
let (<<) x y = (x lsl y) land 0xFF

let sbox = [|
  [| 0x63; 0x7c; 0x77; 0x7b; 0xf2; 0x6b; 0x6f; 0xc5;
    0x30; 0x01; 0x67; 0x2b; 0xfe; 0xd7; 0xab; 0x76 |];
  [| 0xca; 0x82; 0xc9; 0x7d; 0xfa; 0x59; 0x47; 0xf0;
    0xad; 0xd4; 0xa2; 0xaf; 0x9c; 0xa4; 0x72; 0xc0 |];
  [| 0xb7; 0xfd; 0x93; 0x26; 0x36; 0x3f; 0xf7; 0xcc;
    0x34; 0xa5; 0xe5; 0xf1; 0x71; 0xd8; 0x31; 0x15 |];
  [| 0x04; 0xc7; 0x23; 0xc3; 0x18; 0x96; 0x05; 0x9a;
    0x07; 0x12; 0x80; 0xe2; 0xeb; 0x27; 0xb2; 0x75 |];
  [| 0x09; 0x83; 0x2c; 0x1a; 0x1b; 0x6e; 0x5a; 0xa0;
    0x52; 0x3b; 0xd6; 0xb3; 0x29; 0xe3; 0x2f; 0x84 |];
  [| 0x53; 0xd1; 0x00; 0xed; 0x20; 0xfc; 0xb1; 0x5b;
    0x6a; 0xcb; 0xbe; 0x39; 0x4a; 0x4c; 0x58; 0xcf |];
  [| 0xd0; 0xef; 0xaa; 0xfb; 0x43; 0x4d; 0x33; 0x85;
    0x45; 0xf9; 0x02; 0x7f; 0x50; 0x3c; 0x9f; 0xa8 |];
  [| 0x51; 0xa3; 0x40; 0x8f; 0x92; 0x9d; 0x38; 0xf5;
    0xbc; 0xb6; 0xda; 0x21; 0x10; 0xff; 0xf3; 0xd2 |];
  [| 0xcd; 0x0c; 0x13; 0xec; 0x5f; 0x97; 0x44; 0x17;
    0xc4; 0xa7; 0x7e; 0x3d; 0x64; 0x5d; 0x19; 0x73 |];
  [| 0x60; 0x81; 0x4f; 0xdc; 0x22; 0x2a; 0x90; 0x88;
    0x46; 0xee; 0xb8; 0x14; 0xde; 0x5e; 0x0b; 0xdb |];
  [| 0xe0; 0x32; 0x3a; 0x0a; 0x49; 0x06; 0x24; 0x5c;
    0xc2; 0xd3; 0xac; 0x62; 0x91; 0x95; 0xe4; 0x79 |];
  [| 0xe7; 0xc8; 0x37; 0x6d; 0x8d; 0xd5; 0x4e; 0xa9;
    0x6c; 0x56; 0xf4; 0xea; 0x65; 0x7a; 0xae; 0x08 |];
  [| 0xba; 0x78; 0x25; 0x2e; 0x1c; 0xa6; 0xb4; 0xc6;
    0xe8; 0xdd; 0x74; 0x1f; 0x4b; 0xbd; 0x8b; 0x8a |];
  [| 0x70; 0x3e; 0xb5; 0x66; 0x48; 0x03; 0xf6; 0x0e;
    0x61; 0x35; 0x57; 0xb9; 0x86; 0xc1; 0x1d; 0x9e |];
  [| 0xe1; 0xf8; 0x98; 0x11; 0x69; 0xd9; 0x8e; 0x94;
    0x9b; 0x1e; 0x87; 0xe9; 0xce; 0x55; 0x28; 0xdf |];
  [| 0x8c; 0xa1; 0x89; 0x0d; 0xbf; 0xe6; 0x42; 0x68;
    0x41; 0x99; 0x2d; 0x0f; 0xb0; 0x54; 0xbb; 0x16 |]
|]

let print_word_arr (w: char array array) : unit =
  for r = 0 to 3 do
    for c = 0 to 3 do
      Printf.printf "%.2x " (Char.code w.(r).(c))
    done;
    Printf.printf "\n"
  done;
  print_string "\n"

let xor (target : bytes) (src : bytes) (len : int) : unit =
  let rec iter i =
    if i < len then begin
      Byte.s_ target i ((Byte.g_ target i) lxor (Byte.g_ src i)) "xor iter";
      iter (i + 1)
    end
  in
  iter 0

let rot_word (w: char array) : unit =
  let tmp = w.(0) in
  w.(0) <- w.(1);
  w.(1) <- w.(2);
  w.(2) <- w.(3);
  w.(3) <- tmp

let sub_word (w: char array) : unit =
  Array.iteri
    (fun i ch ->
      w.(i) <- Char.chr (sbox.((ch & 0xF0) lsr 4).(ch & 0x0F)))
    w

let compute_key_schedule (key: bytes) (key_len: int) (w: char array array) : unit =
  let key_words = key_len lsr 2 in
  let rcon = ref 0x01 in
  let key_2_word_array key w =
    for i = 0 to key_len - 1 do
      w.(i / 4).(i mod 4) <- Bytes.get key i
    done
  in
  key_2_word_array key w;

  for i = key_words to 4 * (key_words + 7) - 1 do
    w.(i) <- Array.copy w.(i - 1);
    if (i mod key_words = 0) then (
      rot_word w.(i);
      sub_word w.(i);
      if (i mod 36 = 0) then rcon := 0x1b;
      w.(i).(0) <- Char.chr (w.(i).(0) ^ !rcon);
      rcon := !rcon << 1
    ) else if ((key_words > 6) && (i mod key_words = 4)) then sub_word w.(i);
    w.(i).(0) <- Char.chr (w.(i).(0) ^ (Char.code w.(i - key_words).(0)));
    w.(i).(1) <- Char.chr (w.(i).(1) ^ (Char.code w.(i - key_words).(1)));
    w.(i).(2) <- Char.chr (w.(i).(2) ^ (Char.code w.(i - key_words).(2)));
    w.(i).(3) <- Char.chr (w.(i).(3) ^ (Char.code w.(i - key_words).(3)))
  done

let add_round_key (state: char array array) (w: char array array) (off: int) : unit =
  for c = 0 to 3 do
    for r = 0 to 3 do
      state.(r).(c) <- Char.chr (state.(r).(c) ^ (Char.code w.(c + off).(r)))
    done
  done

let sub_bytes (state: char array array) : unit =
  for r = 0 to 3 do
    for c = 0 to 3 do
      let ch = Char.code state.(r).(c) in
      state.(r).(c) <- Char.chr sbox.((ch land 0xF0) lsr 4).(ch land 0x0F)
    done
  done


let shift_rows (state: char array array) : unit =

  let tmp = state.(1).(0) in
  state.(1).(0) <- state.(1).(1);
  state.(1).(1) <- state.(1).(2);
  state.(1).(2) <- state.(1).(3);
  state.(1).(3) <- tmp;

  let tmp = state.(2).(0) in
  state.(2).(0) <- state.(2).(2);
  state.(2).(2) <- tmp;
  let tmp = state.(2).(1) in
  state.(2).(1) <- state.(2).(3);
  state.(2).(3) <- tmp;

  let tmp = state.(3).(3) in
  state.(3).(3) <- state.(3).(2);
  state.(3).(2) <- state.(3).(1);
  state.(3).(1) <- state.(3).(0);
  state.(3).(0) <- tmp

let xtime (x: char) : char =
  let code_x = Char.code x in 
  Char.chr ((code_x << 1) lxor (if (code_x land 0x80) = 0 then 0x00 else 0x1b))

let dot (x: char) (y: char) : char =
  let mask = ref 0x01 in
  let product = ref 0 in
  let m_x = ref '\000' in
  m_x := x;
  while !mask != 0 do
    if (y & !mask) != 0 then (
      product := !product lxor (Char.code !m_x)
    );
    m_x := xtime !m_x;
    mask := !mask << 1
  done;
  Char.chr !product

let mix_columns (s : char array array) : unit =
  for c = 0 to 3 do
    let t0 = 
      let t00 = Char.code (dot (Char.chr 2) s.(0).(c)) in
      let t01 = Char.code (dot (Char.chr 3) s.(1).(c)) in
      let t02 = Char.code s.(2).(c) in
      let t03 = Char.code s.(3).(c) in
      t00 lxor t01 lxor t02 lxor t03
    in
    let t1 = Char.code s.(0).(c) lxor Char.code (dot (Char.chr 2) s.(1).(c)) lxor
             Char.code (dot (Char.chr 3) s.(2).(c)) lxor Char.code s.(3).(c)
    in
    let t2 = Char.code s.(0).(c) lxor Char.code s.(1).(c) lxor
             Char.code (dot (Char.chr 2) s.(2).(c)) lxor Char.code (dot (Char.chr 3) s.(3).(c))
    in
    let t3 = Char.code (dot (Char.chr 3) s.(0).(c)) lxor Char.code s.(1).(c) lxor Char.code s.(2).(c) lxor
             Char.code (dot (Char.chr 2) s.(3).(c))
    in
    s.(0).(c) <- Char.chr t0;
    s.(1).(c) <- Char.chr t1;
    s.(2).(c) <- Char.chr t2;
    s.(3).(c) <- Char.chr t3
  done

let aes_block_encrypt (input_block: bytes) (output_block: bytes) (key: bytes) (key_size: int) : unit =
  let state = Array.make_matrix 4 4 '\000' in
  let w = Array.make_matrix 60 4 '\000' in
  for r = 0 to 3 do
    for c = 0 to 3 do
      state.(r).(c) <- Bytes.get input_block (r + 4 * c);
    done;
  done;
  let nr = (key_size lsr 2) + 6 in

  compute_key_schedule key key_size w;
  add_round_key state w 0;

  for round = 0 to nr - 1 do
    sub_bytes state;
    shift_rows state;
    if round < (nr - 1) then mix_columns state;
    add_round_key state w ((round + 1) * 4)
  done;

  for r = 0 to 3 do
    for c = 0 to 3 do
      Bytes.set output_block (r + 4 * c) state.(r).(c)
    done
  done

let inv_shift_rows (state: char array array) : unit =
  let tmp = state.(1).(2) in
  state.(1).(2) <- state.(1).(1);
  state.(1).(1) <- state.(1).(0);
  state.(1).(0) <- state.(1).(3);
  state.(1).(3) <- tmp;

  let tmp = state.(2).(0) in
  state.(2).(0) <- state.(2).(2);
  state.(2).(2) <- tmp;
  let tmp = state.(2).(1) in
  state.(2).(1) <- state.(2).(3);
  state.(2).(3) <- tmp;

  let tmp = state.(3).(0) in
  state.(3).(0) <- state.(3).(1);
  state.(3).(1) <- state.(3).(2);
  state.(3).(2) <- state.(3).(3);
  state.(3).(3) <- tmp

let inv_sbox = [|
  [| 0x52; 0x09; 0x6a; 0xd5; 0x30; 0x36; 0xa5; 0x38; 0xbf; 0x40; 0xa3; 0x9e; 0x81; 0xf3; 0xd7; 0xfb |];
  [| 0x7c; 0xe3; 0x39; 0x82; 0x9b; 0x2f; 0xff; 0x87; 0x34; 0x8e; 0x43; 0x44; 0xc4; 0xde; 0xe9; 0xcb |];
  [| 0x54; 0x7b; 0x94; 0x32; 0xa6; 0xc2; 0x23; 0x3d; 0xee; 0x4c; 0x95; 0x0b; 0x42; 0xfa; 0xc3; 0x4e |];
  [| 0x08; 0x2e; 0xa1; 0x66; 0x28; 0xd9; 0x24; 0xb2; 0x76; 0x5b; 0xa2; 0x49; 0x6d; 0x8b; 0xd1; 0x25 |];
  [| 0x72; 0xf8; 0xf6; 0x64; 0x86; 0x68; 0x98; 0x16; 0xd4; 0xa4; 0x5c; 0xcc; 0x5d; 0x65; 0xb6; 0x92 |];
  [| 0x6c; 0x70; 0x48; 0x50; 0xfd; 0xed; 0xb9; 0xda; 0x5e; 0x15; 0x46; 0x57; 0xa7; 0x8d; 0x9d; 0x84 |];
  [| 0x90; 0xd8; 0xab; 0x00; 0x8c; 0xbc; 0xd3; 0x0a; 0xf7; 0xe4; 0x58; 0x05; 0xb8; 0xb3; 0x45; 0x06 |];
  [| 0xd0; 0x2c; 0x1e; 0x8f; 0xca; 0x3f; 0x0f; 0x02; 0xc1; 0xaf; 0xbd; 0x03; 0x01; 0x13; 0x8a; 0x6b |];
  [| 0x3a; 0x91; 0x11; 0x41; 0x4f; 0x67; 0xdc; 0xea; 0x97; 0xf2; 0xcf; 0xce; 0xf0; 0xb4; 0xe6; 0x73 |];
  [| 0x96; 0xac; 0x74; 0x22; 0xe7; 0xad; 0x35; 0x85; 0xe2; 0xf9; 0x37; 0xe8; 0x1c; 0x75; 0xdf; 0x6e |];
  [| 0x47; 0xf1; 0x1a; 0x71; 0x1d; 0x29; 0xc5; 0x89; 0x6f; 0xb7; 0x62; 0x0e; 0xaa; 0x18; 0xbe; 0x1b |];
  [| 0xfc; 0x56; 0x3e; 0x4b; 0xc6; 0xd2; 0x79; 0x20; 0x9a; 0xdb; 0xc0; 0xfe; 0x78; 0xcd; 0x5a; 0xf4 |];
  [| 0x1f; 0xdd; 0xa8; 0x33; 0x88; 0x07; 0xc7; 0x31; 0xb1; 0x12; 0x10; 0x59; 0x27; 0x80; 0xec; 0x5f |];
  [| 0x60; 0x51; 0x7f; 0xa9; 0x19; 0xb5; 0x4a; 0x0d; 0x2d; 0xe5; 0x7a; 0x9f; 0x93; 0xc9; 0x9c; 0xef |];
  [| 0xa0; 0xe0; 0x3b; 0x4d; 0xae; 0x2a; 0xf5; 0xb0; 0xc8; 0xeb; 0xbb; 0x3c; 0x83; 0x53; 0x99; 0x61 |];
  [| 0x17; 0x2b; 0x04; 0x7e; 0xba; 0x77; 0xd6; 0x26; 0xe1; 0x69; 0x14; 0x63; 0x55; 0x21; 0x0c; 0x7d |]
|]

let inv_sub_bytes (state: char array array) : unit =
  for r = 0 to 3 do
    for c = 0 to 3 do
      state.(r).(c) <- Char.chr inv_sbox.((state.(r).(c) & 0xF0) lsr 4).(state.(r).(c) & 0x0F)
    done
  done

let inv_mix_columns (s: char array array) : unit =
  for c = 0 to 3 do
    let t0 = Char.code (dot (Char.chr 0x0e) s.(0).(c)) lxor
             Char.code (dot (Char.chr 0x0b) s.(1).(c)) lxor
             Char.code (dot (Char.chr 0x0d) s.(2).(c)) lxor
             Char.code (dot (Char.chr 0x09) s.(3).(c))
    in
    let t1 = Char.code (dot (Char.chr 0x09) s.(0).(c)) lxor
             Char.code (dot (Char.chr 0x0e) s.(1).(c)) lxor
             Char.code (dot (Char.chr 0x0b) s.(2).(c)) lxor
             Char.code (dot (Char.chr 0x0d) s.(3).(c))
    in
    let t2 = Char.code (dot (Char.chr 0x0d) s.(0).(c)) lxor
             Char.code (dot (Char.chr 0x09) s.(1).(c)) lxor
             Char.code (dot (Char.chr 0x0e) s.(2).(c)) lxor
             Char.code (dot (Char.chr 0x0b) s.(3).(c))
    in
    let t3 = Char.code (dot (Char.chr 0x0b) s.(0).(c)) lxor
             Char.code (dot (Char.chr 0x0d) s.(1).(c)) lxor
             Char.code (dot (Char.chr 0x09) s.(2).(c)) lxor
             Char.code (dot (Char.chr 0x0e) s.(3).(c))
    in
    s.(0).(c) <- Char.chr t0;
    s.(1).(c) <- Char.chr t1;
    s.(2).(c) <- Char.chr t2;
    s.(3).(c) <- Char.chr t3
  done

let aes_block_decrypt (input_block: bytes) (output_block: bytes) (key: bytes) (key_size: int) : unit =
  let state = Array.make_matrix 4 4 '\000' in
  let w = Array.make_matrix 60 4 '\000' in

  for r = 0 to 3 do
    for c = 0 to 3 do
      state.(r).(c) <- Bytes.get input_block (r + 4 * c)
    done
  done;

  let nr = (key_size lsr 2) + 6 in
  compute_key_schedule key key_size w;
  add_round_key state w (nr * 4);

  for round = nr downto 1 do
    inv_shift_rows state;
    inv_sub_bytes state;
    add_round_key state w ((round - 1) * 4);
    if round > 1 then inv_mix_columns state
  done;

  for r = 0 to 3 do
    for c = 0 to 3 do
      Bytes.set output_block (r + 4 * c) state.(r).(c)
    done
  done


let aes_BLOCK_SIZE = 16

let aes_encrypt (input: bytes) (input_len: int) (output: bytes) (iv: bytes) (key: bytes) (key_len: int) : unit =
  let input_block = Bytes.create aes_BLOCK_SIZE in
  let output_block = Bytes.create aes_BLOCK_SIZE in
  let off = ref 0 in
  let in_len = ref input_len in
  while (!in_len >= aes_BLOCK_SIZE) do
    Bytes.blit input !off input_block 0 aes_BLOCK_SIZE;
    xor input_block iv aes_BLOCK_SIZE;
    aes_block_encrypt input_block output_block key key_len;
    Bytes.blit output_block 0 iv 0 aes_BLOCK_SIZE;
    Bytes.blit output_block 0 output !off aes_BLOCK_SIZE;
    off := !off + aes_BLOCK_SIZE;
    in_len := !in_len - aes_BLOCK_SIZE
  done

let aes_decrypt (input: bytes) (input_len: int) (output: bytes) (iv: bytes) (key: bytes) (key_len: int) : unit =
  let input_block = Bytes.create aes_BLOCK_SIZE in
  let output_block = Bytes.create aes_BLOCK_SIZE in
  let off = ref 0 in
  let in_len = ref input_len in
  while (!in_len >= aes_BLOCK_SIZE) do
    Bytes.blit input !off input_block 0 aes_BLOCK_SIZE;
    aes_block_decrypt input_block output_block key key_len;
    xor output_block iv aes_BLOCK_SIZE;
    Bytes.blit input_block 0 iv 0 aes_BLOCK_SIZE;
    Bytes.blit output_block 0 output !off aes_BLOCK_SIZE;
    off := !off + aes_BLOCK_SIZE;
    in_len := !in_len - aes_BLOCK_SIZE
  done

let aes_128_encrypt (plaintext: bytes) (plain_len: int) (ciphertext: bytes) (iv: bytes) (key: bytes) : unit =
  aes_encrypt plaintext plain_len ciphertext iv key 16

let aes_128_decrypt (ciphertext: bytes) (cipher_len: int) (plaintext: bytes) (iv: bytes) (key: bytes) : unit =
  aes_decrypt ciphertext cipher_len plaintext iv key 16

let aes_256_encrypt (plaintext: bytes) (plain_len: int) (ciphertext: bytes) (iv: bytes) (key: bytes) : unit =
  aes_encrypt plaintext plain_len ciphertext iv key 32

let aes_256_decrypt (ciphertext: bytes) (cipher_len: int) (plaintext: bytes) (iv: bytes) (key: bytes) : unit =
  aes_decrypt ciphertext cipher_len plaintext iv key 32


(* testing AES *)
let () =
  let e_or_d = Sys.argv.(1) in
  let key = Sys.argv.(2) in
  let iv = Sys.argv.(3) in
  let input = Sys.argv.(4) in
  let len = String.length input in
  let m_input =
    if input.[0] = '0' && input.[1] = 'x' then
      Hex.to_string (Hex.(`Hex (String.sub input 2 (len - 2))))
    else
      input
  in
  let input_len = String.length m_input in
  let output = Bytes.create input_len in
  let () =
    if e_or_d = "-e" then
      aes_128_encrypt (Bytes.of_string m_input) input_len output (Bytes.of_string iv) (Bytes.of_string key)
    else
      aes_128_decrypt (Bytes.of_string m_input) input_len output (Bytes.of_string iv) (Bytes.of_string key)
  in
  Byte.print_byte output;
  Printf.printf "%s\n" (Byte.to_string output)
