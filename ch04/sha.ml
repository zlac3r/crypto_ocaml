open Unsigned.UInt32

module Byte = struct
  include Bytes
  let g_ b i = of_int32 (Int32.of_int (Char.code (get b i)))
  let s_ b i v black_magic = set b i (Char.chr v)
  let print_byte b =
    let char_seq = Bytes.to_seq b in
    Printf.printf "0x";
    Seq.iter (fun c -> Printf.printf "%.2x" (Char.code c)) char_seq;
    Printf.printf "\n"
end

let k = [|
  of_int32 (Int32.of_int 0x5a827999); (* 0 <= t <= 19 *)
  of_int32 (Int32.of_int 0x6ed9eba1); (* 20 <= t <= 39 *)
  of_int32 (Int32.of_int 0x8f1bbcdc); (* 40 <= t <= 59 *)
  of_int32 (Int32.of_int 0xca62c1d6) (* 60 <= t <= 79 *)
|]

let ch x y z = logxor (logand x y) (logand (lognot x) z)
let parity x y z = logxor x (logxor y z)
let maj x y z = logxor (logand x y) (logxor (logand x z) (logand y z))
let rotr x n = logor (shift_right x n) (shift_left x (32 - n))
let shr x n = shift_right x n
let sigma_rot x i = 
  match i with
  | 0 -> logxor (rotr x 2) (logxor (rotr x 13) (rotr x 22))
  | 1 -> logxor (rotr x 6) (logxor (rotr x 11) (rotr x 25))
let sigma_shr x i =
  match i with
  | 0 -> logxor (rotr x 7) (logxor (rotr x 18) (shr x 3))
  | 1 -> logxor (rotr x 17) (logxor (rotr x 19) (shr x 10))

let int32_to_bytes n =
  let byte_1 = shift_right (logand n (of_int 0xff000000)) 24 in
  let byte_2 = shift_right (logand n (of_int 0x00ff0000)) 16 in
  let byte_3 = shift_right (logand n (of_int 0x0000ff00)) 8 in
  let byte_4 = logand n (of_int 0x000000ff) in
  let byte = Byte.create 4 in
  Byte.set byte 0 (Char.chr (to_int byte_1));
  Byte.set byte 1 (Char.chr (to_int byte_2));
  Byte.set byte 2 (Char.chr (to_int byte_3));
  Byte.set byte 3 (Char.chr (to_int byte_4));
  byte

let display_hash hash hash_len =
  let output = Byte.create (4 * hash_len) in
  for i = 0 to (hash_len - 1) do
    let byte = int32_to_bytes hash.(i) in
    Byte.blit byte 0 output (i * 4) 4
  done;
  Byte.print_byte output

let sha1_result_size = 5
let sha256_result_size = 8

let sha1_block_operate block hash =
  let w = Array.make 80 zero in
  let (a, b, c, d, e, tT) = (ref zero, ref zero, ref zero, ref zero, ref zero, ref zero) in
  for t = 0 to 79 do
    if (t < 16) then begin
      w.(t) <- logor (shift_left (Byte.g_ block (t * 4)) 24)
                     (logor (shift_left (Byte.g_ block (t * 4 + 1)) 16)
                            (logor (shift_left (Byte.g_ block (t * 4 + 2)) 8)
                                   (Byte.g_ block (t * 4 + 3))));
    end else begin
      w.(t) <- logxor w.(t - 3) (logxor w.(t - 8) (logxor w.(t - 14) w.(t - 16)));
      w.(t) <- logor (shift_left w.(t) 1)  (shift_right (logand w.(t) (of_int32 (Int32.of_int 0x80000000))) 31);
    end
  done;
  a := hash.(0);
  b := hash.(1);
  c := hash.(2);
  d := hash.(3);
  e := hash.(4);
  for t = 0 to 79 do
    tT := add (logor (shift_left !a 5) (shift_right !a 27)) (add !e (add k.(t / 20) w.(t)));
    if (t <= 19) then tT := add !tT (ch !b !c !d)
    else if (t <= 39) then tT := add !tT (parity !b !c !d)
    else if (t <= 59) then tT := add !tT (maj !b !c !d)
    else tT := add !tT (parity !b !c !d);
    e := !d;
    d := !c;
    c := logor (shift_left !b 30) (shift_right !b 2);
    b := !a;
    a := !tT
  done;
  hash.(0) <- add hash.(0) !a;
  hash.(1) <- add hash.(1) !b;
  hash.(2) <- add hash.(2) !c;
  hash.(3) <- add hash.(3) !d;
  hash.(4) <- add hash.(4) !e

let k256 = [|
    of_int 0x428a2f98; of_int 0x71374491; of_int 0xb5c0fbcf; of_int 0xe9b5dba5; of_int 0x3956c25b; of_int 0x59f111f1;
    of_int 0x923f82a4; of_int 0xab1c5ed5; of_int 0xd807aa98; of_int 0x12835b01; of_int 0x243185be; of_int 0x550c7dc3;
    of_int 0x72be5d74; of_int 0x80deb1fe; of_int 0x9bdc06a7; of_int 0xc19bf174; of_int 0xe49b69c1; of_int 0xefbe4786;
    of_int 0x0fc19dc6; of_int 0x240ca1cc; of_int 0x2de92c6f; of_int 0x4a7484aa; of_int 0x5cb0a9dc; of_int 0x76f988da;
    of_int 0x983e5152; of_int 0xa831c66d; of_int 0xb00327c8; of_int 0xbf597fc7; of_int 0xc6e00bf3; of_int 0xd5a79147;
    of_int 0x06ca6351; of_int 0x14292967; of_int 0x27b70a85; of_int 0x2e1b2138; of_int 0x4d2c6dfc; of_int 0x53380d13;
    of_int 0x650a7354; of_int 0x766a0abb; of_int 0x81c2c92e; of_int 0x92722c85; of_int 0xa2bfe8a1; of_int 0xa81a664b;
    of_int 0xc24b8b70; of_int 0xc76c51a3; of_int 0xd192e819; of_int 0xd6990624; of_int 0xf40e3585; of_int 0x106aa070;
    of_int 0x19a4c116; of_int 0x1e376c08; of_int 0x2748774c; of_int 0x34b0bcb5; of_int 0x391c0cb3; of_int 0x4ed8aa4a;
    of_int 0x5b9cca4f; of_int 0x682e6ff3; of_int 0x748f82ee; of_int 0x78a5636f; of_int 0x84c87814; of_int 0x8cc70208;
    of_int 0x90befffa; of_int 0xa4506ceb; of_int 0xbef9a3f7; of_int 0xc67178f2
|]

let sha256_block_operate block hash =
  let w = Array.make 64 zero in
  let (a, b, c, d, e, f, g, h) = (ref zero, ref zero, ref zero, ref zero, ref zero, ref zero, ref zero, ref zero) in
  let (t1, t2) = (ref zero, ref zero) in
  for t = 0 to 63 do
    if (t <= 15) then begin
      w.(t) <- logor (shift_left (Byte.g_ block (t * 4)) 24)
                     (logor (shift_left (Byte.g_ block (t * 4 + 1)) 16)
                            (logor (shift_left (Byte.g_ block (t * 4 + 2)) 8)
                                   (Byte.g_ block (t * 4 + 3))));
    end else begin
      w.(t) <- add (sigma_shr w.(t-2) 1) (add w.(t-7) (add (sigma_shr w.(t-15) 0) w.(t-16)));
    end
  done;
  a := hash.(0);
  b := hash.(1);
  c := hash.(2);
  d := hash.(3);
  e := hash.(4);
  f := hash.(5);
  g := hash.(6);
  h := hash.(7);
  for t = 0 to 63 do
    t1 := add !h (add (sigma_rot !e 1) (add (ch !e !f !g) (add k256.(t) w.(t))));
    t2 := add (sigma_rot !a 0) (maj !a !b !c);
    h := !g;
    g := !f;
    f := !e;
    e := add !d !t1;
    d := !c;
    c := !b;
    b := !a;
    a := add !t1 !t2
  done;
  hash.(0) <- add !a hash.(0);
  hash.(1) <- add !b hash.(1);
  hash.(2) <- add !c hash.(2);
  hash.(3) <- add !d hash.(3);
  hash.(4) <- add !e hash.(4);
  hash.(5) <- add !f hash.(5);
  hash.(6) <- add !g hash.(6);
  hash.(7) <- add !h hash.(7)


let sha_input_block_size = 56
let sha_block_size = 64

let sha1_initial_hash = [
  of_int32 (Int32.of_int 0x67452301);
  of_int32 (Int32.of_int 0xefcdab89);
  of_int32 (Int32.of_int 0x98badcfe);
  of_int32 (Int32.of_int 0x10325476);
  of_int32 (Int32.of_int 0xc3d2e1f0)
]

let sha256_initial_hash = [
  of_int32 (Int32.of_int 0x6a09e667);
  of_int32 (Int32.of_int 0xbb67ae85);
  of_int32 (Int32.of_int 0x3c6ef372);
  of_int32 (Int32.of_int 0xa54ff53a);
  of_int32 (Int32.of_int 0x510e527f);
  of_int32 (Int32.of_int 0x9b05688c);
  of_int32 (Int32.of_int 0x1f83d9ab);
  of_int32 (Int32.of_int 0x5be0cd19)
]

let sha1_hash input len hash =
  let padded_block = Byte.create sha_block_size in
  let length_in_bits = !len * 8 in
  hash.(0) <- List.nth sha1_initial_hash 0;
  hash.(1) <- List.nth sha1_initial_hash 1;
  hash.(2) <- List.nth sha1_initial_hash 2;
  hash.(3) <- List.nth sha1_initial_hash 3;
  hash.(4) <- List.nth sha1_initial_hash 4;
  let in_off = ref 0 in
  while (!len >= sha_input_block_size) do
    if (!len < sha_block_size) then begin
      Byte.fill padded_block 0 sha_block_size (Char.chr 0x00);
      Byte.blit input !in_off padded_block 0 !len;
      Byte.set padded_block !len (Char.chr 0x80);
      sha1_block_operate padded_block hash;
      in_off := !in_off + !len;
      len := -1
    end
    else begin
      sha1_block_operate (Byte.sub input !in_off sha_block_size) hash;
      in_off := !in_off + sha_block_size;
      len := !len - sha_block_size
    end
  done;
  Byte.fill padded_block 0 sha_block_size (Char.chr 0x00);
  if (!len >= 0) then begin
    Byte.blit input !in_off padded_block 0 !len;
    Byte.set padded_block !len (Char.chr 0x80)
  end;
  Byte.set padded_block (sha_block_size - 4) (Char.chr ((length_in_bits land 0xff000000 ) lsr 24));
  Byte.set padded_block (sha_block_size - 3) (Char.chr ((length_in_bits land 0x00ff0000 ) lsr 16));
  Byte.set padded_block (sha_block_size - 2) (Char.chr ((length_in_bits land 0x0000ff00 ) lsr 8));
  Byte.set padded_block (sha_block_size - 1) (Char.chr (length_in_bits land 0x000000ff ));
  sha1_block_operate padded_block hash

let sha256_hash input len hash =
  let padded_block = Byte.create sha_block_size in
  let length_in_bits = !len * 8 in
  hash.(0) <- List.nth sha256_initial_hash 0;
  hash.(1) <- List.nth sha256_initial_hash 1;
  hash.(2) <- List.nth sha256_initial_hash 2;
  hash.(3) <- List.nth sha256_initial_hash 3;
  hash.(4) <- List.nth sha256_initial_hash 4;
  hash.(5) <- List.nth sha256_initial_hash 5;
  hash.(6) <- List.nth sha256_initial_hash 6;
  hash.(7) <- List.nth sha256_initial_hash 7;
  let in_off = ref 0 in
  while (!len >= sha_input_block_size) do
    if (!len < sha_block_size) then begin
      Byte.fill padded_block 0 sha_block_size (Char.chr 0x00);
      Byte.blit input !in_off padded_block 0 !len;
      Byte.set padded_block !len (Char.chr 0x80);
      sha256_block_operate padded_block hash;
      in_off := !in_off + !len;
      len := -1
    end
    else begin
      sha256_block_operate (Byte.sub input !in_off sha_block_size) hash;
      in_off := !in_off + sha_block_size;
      len := !len - sha_block_size
    end
  done;
  Byte.fill padded_block 0 sha_block_size (Char.chr 0x00);
  if (!len >= 0) then begin
    Byte.blit input !in_off padded_block 0 !len;
    Byte.set padded_block !len (Char.chr 0x80);
  end;
  Byte.set padded_block (sha_block_size - 4) (Char.chr ((length_in_bits land 0xff000000) lsr 24));
  Byte.set padded_block (sha_block_size - 3) (Char.chr ((length_in_bits land 0x00ff0000) lsr 16));
  Byte.set padded_block (sha_block_size - 2) (Char.chr ((length_in_bits land 0x0000ff00) lsr 8));
  Byte.set padded_block (sha_block_size - 1) (Char.chr (length_in_bits land 0x000000ff));
  sha256_block_operate padded_block hash

let sha_finalize padded_block length_in_bits =
  Byte.set padded_block (sha_block_size - 4) (Char.chr ((length_in_bits land 0xff000000) lsr 24));
  Byte.set padded_block (sha_block_size - 3) (Char.chr ((length_in_bits land 0x00ff0000) lsr 16));
  Byte.set padded_block (sha_block_size - 2) (Char.chr ((length_in_bits land 0x0000ff00) lsr 8));
  Byte.set padded_block (sha_block_size - 1) (Char.chr (length_in_bits land 0x000000ff))

(*
let () =
  let algo = Sys.argv.(1) in
  let input = Sys.argv.(2) in
  let len = String.length input in
  let m_input =
    if input.[0] = '0' && input.[1] = 'x' then
      Hex.to_string (Hex.(`Hex (String.sub input 2 (len - 2))))
    else
      input
  in
  let input_len = ref (String.length m_input) in
  if algo = "-sha1" then begin
    let hash = Array.make 5 zero in
    sha1_hash (Byte.of_string m_input) input_len hash;
    print_string "sha1 hash: \n";
    display_hash hash 5
  end else begin
    let hash256 = Array.make 8 zero in
    sha256_hash (Byte.of_string m_input) input_len hash256;
    print_string "sha256 hash: \n";
    display_hash hash256 8
  end
*)
