open Unsigned.UInt32

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

let int32_to_bytes n =
  let byte_1 = shift_right (logand n (of_int 0xff000000)) 24 in
  let byte_2 = shift_right (logand n (of_int 0x00ff0000)) 16 in
  let byte_3 = shift_right (logand n (of_int 0x0000ff00)) 8 in
  let byte_4 = logand n (of_int 0x000000ff) in
  let byte = Byte.create 4 in
  Byte.set byte 0 (Char.chr (to_int byte_4));
  Byte.set byte 1 (Char.chr (to_int byte_3));
  Byte.set byte 2 (Char.chr (to_int byte_2));
  Byte.set byte 3 (Char.chr (to_int byte_1));
  byte

let display_hash hash =
  let output = Byte.create (4 * 4) in
  for i = 0 to 3 do
    let byte = int32_to_bytes hash.(i) in
    Byte.blit byte 0 output (i * 4) 4
  done;
  Byte.print_byte output

let f x y z = logor (logand x y) (logand (lognot x) z)
let g x y z = logor (logand x z) (logand y (lognot z))
let h x y z = logxor x (logxor y z)
let i x y z = logxor y (logor x (lognot z))

let base_t = 4294967296.0

let round f a b c d k s i x =
  a := add !a (add (f b c d) (add x.(k) (of_int64 (Int64.of_float (base_t *. Float.abs (sin i))))));
  a := logor (shift_left !a s) (shift_right !a (32 - s));
  a := add !a b

let md5_initial_hash = [
  of_int 0x67452301;
  of_int 0xefcdab89;
  of_int 0x98badcfe;
  of_int 0x10325476
]

let print_abcd a b c d =
  let a_byte = int32_to_bytes a in
  let b_byte = int32_to_bytes b in
  let c_byte = int32_to_bytes c in
  let d_byte = int32_to_bytes d in
  print_string "a :: \n";
  Byte.print_byte a_byte;
  print_string "b :: \n";
  Byte.print_byte b_byte;
  print_string "c :: \n";
  Byte.print_byte c_byte;
  print_string "d :: \n";
  Byte.print_byte d_byte


let md5_block_operate input hash =
  let (a, b, c, d) = (ref zero, ref zero, ref zero, ref zero) in
  let x = Array.make 16 (of_int 0x00) in
  a := hash.(0);
  b := hash.(1);
  c := hash.(2);
  d := hash.(3);
  for j = 0 to 15 do
    let input_3 = shift_left (of_int (Byte.g_ input (j * 4 + 3))) 24 in
    let input_2 = shift_left (of_int (Byte.g_ input (j * 4 + 2))) 16 in
    let input_1 = shift_left (of_int (Byte.g_ input (j * 4 + 1))) 8 in
    let input_0 = of_int (Byte.g_ input (j * 4 + 0)) in
    x.(j) <- logor input_3 (logor input_2 (logor input_1 input_0))
  done;

  (* round 1 *)
  round f a !b !c !d 0 7 1.0 x ;
  round f d !a !b !c 1 12 2.0 x ;
  round f c !d !a !b 2 17 3.0 x ;
  round f b !c !d !a 3 22 4.0 x ;
  round f a !b !c !d 4 7 5.0 x ;
  round f d !a !b !c 5 12 6.0 x ;
  round f c !d !a !b 6 17 7.0 x ;
  round f b !c !d !a 7 22 8.0 x ;
  round f a !b !c !d 8 7 9.0 x ;
  round f d !a !b !c 9 12 10.0 x ;
  round f c !d !a !b 10 17 11.0 x ;
  round f b !c !d !a 11 22 12.0 x ;
  round f a !b !c !d 12 7 13.0 x ;
  round f d !a !b !c 13 12 14.0 x ;
  round f c !d !a !b 14 17 15.0 x ;
  round f b !c !d !a 15 22 16.0 x ;

  (* round 2 *)
  round g a !b !c !d 1 5 17.0 x;
  round g d !a !b !c 6 9 18.0 x;
  round g c !d !a !b 11 14 19.0 x;
  round g b !c !d !a 0 20 20.0 x;
  round g a !b !c !d 5 5 21.0 x;
  round g d !a !b !c 10 9 22.0 x;
  round g c !d !a !b 15 14 23.0 x;
  round g b !c !d !a 4 20 24.0 x;
  round g a !b !c !d 9 5 25.0 x;
  round g d !a !b !c 14 9 26.0 x;
  round g c !d !a !b 3 14 27.0 x;
  round g b !c !d !a 8 20 28.0 x;
  round g a !b !c !d 13 5 29.0 x;
  round g d !a !b !c 2 9 30.0 x;
  round g c !d !a !b 7 14 31.0 x;
  round g b !c !d !a 12 20 32.0 x;

  (* round 3 *)
  round h a !b !c !d 5 4 33.0 x ;
  round h d !a !b !c 8 11 34.0 x ;
  round h c !d !a !b 11 16 35.0 x ;
  round h b !c !d !a 14 23 36.0 x ;
  round h a !b !c !d 1 4 37.0 x ;
  round h d !a !b !c 4 11 38.0 x ;
  round h c !d !a !b 7 16 39.0 x ;
  round h b !c !d !a 10 23 40.0 x ;
  round h a !b !c !d 13 4 41.0 x ;
  round h d !a !b !c 0 11 42.0 x ;
  round h c !d !a !b 3 16 43.0 x ;
  round h b !c !d !a 6 23 44.0 x ;
  round h a !b !c !d 9 4 45.0 x ;
  round h d !a !b !c 12 11 46.0 x ;
  round h c !d !a !b 15 16 47.0 x ;
  round h b !c !d !a 2 23 48.0 x ;

  (* round 4 *)
  round i a !b !c !d 0 6 49.0 x;
  round i d !a !b !c 7 10 50.0 x;
  round i c !d !a !b 14 15 51.0 x;
  round i b !c !d !a 5 21 52.0 x;
  round i a !b !c !d 12 6 53.0 x;
  round i d !a !b !c 3 10 54.0 x;
  round i c !d !a !b 10 15 55.0 x;
  round i b !c !d !a 1 21 56.0 x;
  round i a !b !c !d 8 6 57.0 x;
  round i d !a !b !c 15 10 58.0 x;
  round i c !d !a !b 6 15 59.0 x;
  round i b !c !d !a 13 21 60.0 x;
  round i a !b !c !d 4 6 61.0 x;
  round i d !a !b !c 11 10 62.0 x;
  round i c !d !a !b 2 15 63.0 x;
  round i b !c !d !a 9 21 64.0 x;

  hash.(0) <- add hash.(0) !a;
  hash.(1) <- add hash.(1) !b;
  hash.(2) <- add hash.(2) !c;
  hash.(3) <- add hash.(3) !d


let md5_block_size = 64
let md5_input_block_size = 56
let md5_result_size = 4

let md5_hash input len hash =
  let padded_block = Byte.create md5_block_size in
  let length_in_bits = !len * 8 in
  hash.(0) <- List.nth md5_initial_hash 0;
  hash.(1) <- List.nth md5_initial_hash 1;
  hash.(2) <- List.nth md5_initial_hash 2;
  hash.(3) <- List.nth md5_initial_hash 3;
  let in_off = ref 0 in
  while (!len >= md5_input_block_size) do
    if (!len < md5_block_size) then begin
      Byte.fill padded_block 0 md5_block_size (Char.chr 0x00);
      Byte.blit input !in_off padded_block 0 !len;
      Byte.set padded_block !len (Char.chr 0x80);
      md5_block_operate padded_block hash;
      in_off := !in_off + !len;
      len := -1
    end 
    else begin
      md5_block_operate (Byte.sub input !in_off md5_block_size) hash;
      in_off := !in_off + md5_block_size;
      len := !len - md5_block_size
    end
  done;
  Byte.fill padded_block 0 md5_block_size (Char.chr 0x00);
  if (!len >= 0) then begin
    Byte.blit input !in_off padded_block 0 !len;
    Byte.set padded_block !len (Char.chr 0x80)
  end;
  Byte.set padded_block (md5_block_size - 5) (Char.chr ((length_in_bits land 0xff000000 ) lsr 24));
  Byte.set padded_block (md5_block_size - 6) (Char.chr ((length_in_bits land 0x00ff0000 ) lsr 16));
  Byte.set padded_block (md5_block_size - 7) (Char.chr ((length_in_bits land 0x0000ff00 ) lsr 8));
  Byte.set padded_block (md5_block_size - 8) (Char.chr (length_in_bits land 0x000000ff ));
  md5_block_operate padded_block hash

let md5_finalize padded_block length_in_bits =
  Byte.set padded_block (md5_block_size - 5) (Char.chr ((length_in_bits land 0xff000000 ) lsr 24));
  Byte.set padded_block (md5_block_size - 6) (Char.chr ((length_in_bits land 0x00ff0000 ) lsr 16));
  Byte.set padded_block (md5_block_size - 7) (Char.chr ((length_in_bits land 0x0000ff00 ) lsr 8));
  Byte.set padded_block (md5_block_size - 8) (Char.chr (length_in_bits land 0x000000ff ))

  (*
let () =
  let input = Sys.argv.(1) in
  let len = String.length input in
  let m_input =
    if input.[0] = '0' && input.[1] = 'x' then
      Hex.to_string (Hex.(`Hex (String.sub input 2 (len - 2))))
    else
      input
  in
  let input_len = ref (String.length m_input) in
  let hash = Array.make 4 zero in
  md5_hash (Byte.of_string m_input) input_len hash;
  display_hash hash
  *)
