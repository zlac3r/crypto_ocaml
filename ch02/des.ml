(* required because ocaml doesn't have a 8-bit type. i.e., 0xFF lsl 1 become 0x1FE instead of just 0xFE *)
let (<<) x y = (x lsl y) land 0xFF

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

let get_bit (byte_arr : bytes) (bit : int) : int = (Byte.g_ byte_arr (bit / 8)) land (0x80 lsr (bit mod 8))
let set_bit (byte_arr : bytes) (bit : int) : unit = Byte.s_ byte_arr (bit / 8) ((Byte.g_ byte_arr (bit / 8)) lor (0x80 lsr (bit mod 8))) "set_bit"
let clear_bit (byte_arr : bytes) (bit : int) : unit = Byte.s_ byte_arr (bit / 8) ((Byte.g_ byte_arr (bit / 8)) land (lnot (0x80 lsr (bit mod 8)))) "clear_bit"

let xor (target : bytes) (src : bytes) (len : int) : unit = 
  let rec iter i =
    if i < len then begin
      Byte.s_ target i ((Byte.g_ target i) lxor (Byte.g_ src i)) "xor iter";
      iter (i + 1)
    end
  in
  iter 0

let permute (target : bytes) (src : bytes) (permute_table : int list) (len : int) : unit =
  let rec iter i =
    match i < len * 8 with
    | false -> ()
    | true -> 
        match get_bit src ((List.nth permute_table i) - 1) != 0 with
        | true -> set_bit target i; iter (i + 1)
        | false -> clear_bit target i; iter (i + 1)
  in
  iter 0

let ip_table = [
  58; 50; 42; 34; 26; 18; 10; 2;
  60; 52; 44; 36; 28; 20; 12; 4;
  62; 54; 46; 38; 30; 22; 14; 6;
  64; 56; 48; 40; 32; 24; 16; 8;
  57; 49; 41; 33; 25; 17; 9;  1;
  59; 51; 43; 35; 27; 19; 11; 3;
  61; 53; 45; 37; 29; 21; 13; 5;
  63; 55; 47; 39; 31; 23; 15; 7
]

let fp_table = [
  40; 8; 48; 16; 56; 24; 64; 32;
  39; 7; 47; 15; 55; 23; 63; 31;
  38; 6; 46; 14; 54; 22; 62; 30;
  37; 5; 45; 13; 53; 21; 61; 29;
  36; 4; 44; 12; 52; 20; 60; 28;
  35; 3; 43; 11; 51; 19; 59; 27;
  34; 2; 42; 10; 50; 18; 58; 26;
  33; 1; 41;  9; 49; 17; 57; 25
]

let pc1_table = [
  57; 49; 41; 33; 25; 17;  9; 1;
  58; 50; 42; 34; 26; 18; 10; 2;
  59; 51; 43; 35; 27; 19; 11; 3;
  60; 52; 44; 36;
  63; 55; 47; 39; 31; 23; 15; 7;
  62; 54; 46; 38; 30; 22; 14; 6;
  61; 53; 45; 37; 29; 21; 13; 5;
  28; 20; 12;  4
]

let pc2_table = [
  14; 17; 11; 24;  1;  5;
  3; 28; 15;  6; 21; 10;
  23; 19; 12;  4; 26;  8;
  16;  7; 27; 20; 13;  2;
  41; 52; 31; 37; 47; 55;
  30; 40; 51; 45; 33; 48;
  44; 49; 39; 56; 34; 53;
  46; 42; 50; 36; 29; 32
]

let rol (target: bytes) : unit =
  let target_0 = Byte.g_ target 0 in
  let target_1 = Byte.g_ target 1 in
  let target_2 = Byte.g_ target 2 in
  let target_3 = Byte.g_ target 3 in
  let target_4 = Byte.g_ target 4 in
  let target_5 = Byte.g_ target 5 in
  let target_6 = Byte.g_ target 6 in
  let carry_left = (target_0 land 0x80) lsr 3 in
  Byte.s_ target 0 ((target_0 << 1) lor ((target_1 land 0x80) lsr 7)) "rol 0";
  Byte.s_ target 1 ((target_1 << 1) lor ((target_2 land 0x80) lsr 7)) "rol 1";
  Byte.s_ target 2 ((target_2 << 1) lor ((target_3 land 0x80) lsr 7)) "rol 2";
  let carry_right = (target_3 land 0x08) lsr 3 in
  Byte.s_ target 3 ((((target_3 << 1) lor ((target_4 land 0x80) lsr 7)) land (lnot 0x10)) lor carry_left) "rol 3";
  Byte.s_ target 4 ((target_4 << 1) lor ((target_5 land 0x80) lsr 7)) "rol 4";
  Byte.s_ target 5 ((target_5 << 1) lor ((target_6 land 0x80) lsr 7)) "rol 5";
  Byte.s_ target 6 ((target_6 << 1) lor carry_right) "rol 6"

let ror (target: bytes) : unit = 
  let target_0 = Byte.g_ target 0 in
  let target_1 = Byte.g_ target 1 in
  let target_2 = Byte.g_ target 2 in
  let target_3 = Byte.g_ target 3 in
  let target_4 = Byte.g_ target 4 in
  let target_5 = Byte.g_ target 5 in
  let target_6 = Byte.g_ target 6 in
  let carry_right = (target_6 land 0x01) << 3 in
  Byte.s_ target 6 ((target_6 lsr 1) lor ((target_5 land 0x01) << 7)) "ror";
  Byte.s_ target 5 ((target_5 lsr 1) lor ((target_4 land 0x01) << 7)) "ror";
  Byte.s_ target 4 ((target_4 lsr 1) lor ((target_3 land 0x01) << 7)) "ror";
  let carry_left = (target_3 land 0x10) << 3 in
  Byte.s_ target 3 ((((target_3 lsr 1) lor ((target_2 land 0x01) << 7)) land (lnot 0x08)) lor carry_right) "ror";
  Byte.s_ target 2 ((target_2 lsr 1) lor ((target_1 land 0x01) << 7)) "ror";
  Byte.s_ target 1 ((target_1 lsr 1) lor ((target_0 land 0x01) << 7)) "ror";
  Byte.s_ target 0 ((target_0 lsr 1) lor carry_left) "ror"

let expansion_table = [
  32;  1;  2;  3;  4;  5;
   4;  5;  6;  7;  8;  9;
   8;  9; 10; 11; 12; 13;
   12; 13; 14; 15; 16; 17;
   16; 17; 18; 19; 20; 21;
   20; 21; 22; 23; 24; 25;
   24; 25; 26; 27; 28; 29;
   28; 29; 30; 31; 32;  1
]

let sbox = [
  [
    14; 0; 4; 15; 13; 7; 1; 4; 2; 14; 15; 2; 11; 13; 8; 1;
     3; 10; 10; 6; 6; 12; 12; 11; 5; 9; 9; 5; 0; 3; 7; 8;
     4; 15; 1; 12; 14; 8; 8; 2; 13; 4; 6; 9; 2; 1; 11; 7;
    15; 5; 12; 11; 9; 3; 7; 14; 3; 10; 10; 0; 5; 6; 0; 13
  ];
  [
    15; 3; 1; 13; 8; 4; 14; 7; 6; 15; 11; 2; 3; 8; 4; 14;
     9; 12; 7; 0; 2; 1; 13; 10; 12; 6; 0; 9; 5; 11; 10; 5;
     0; 13; 14; 8; 7; 10; 11; 1; 10; 3; 4; 15; 13; 4; 1; 2;
     5; 11; 8; 6; 12; 7; 6; 12; 9; 0; 3; 5; 2; 14; 15; 9
  ];
  [
    10; 13; 0; 7; 9; 0; 14; 9; 6; 3; 3; 4; 15; 6; 5; 10;
     1; 2; 13; 8; 12; 5; 7; 14; 11; 12; 4; 11; 2; 15; 8; 1;
    13; 1; 6; 10; 4; 13; 9; 0; 8; 6; 15; 9; 3; 8; 0; 7;
    11; 4; 1; 15; 2; 14; 12; 3; 5; 11; 10; 5; 14; 2; 7; 12
  ];
  [
    7; 13; 13; 8; 14; 11; 3; 5; 0; 6; 6; 15; 9; 0; 10; 3;
    1; 4; 2; 7; 8; 2; 5; 12; 11; 1; 12; 10; 4; 14; 15; 9;
   10; 3; 6; 15; 9; 0; 0; 6; 12; 10; 11; 1; 7; 13; 13; 8;
   15; 9; 1; 4; 3; 5; 14; 11; 5; 12; 2; 7; 8; 2; 4; 14
  ];
  [
    2; 14; 12; 11; 4; 2; 1; 12; 7; 4; 10; 7; 11; 13; 6; 1;
    8; 5; 5; 0; 3; 15; 15; 10; 13; 3; 0; 9; 14; 8; 9; 6;
    4; 11; 2; 8; 1; 12; 11; 7; 10; 1; 13; 14; 7; 2; 8; 13;
   15; 6; 9; 15; 12; 0; 5; 9; 6; 10; 3; 4; 0; 5; 14; 3
  ];
  [
    12; 10; 1; 15; 10; 4; 15; 2; 9; 7; 2; 12; 6; 9; 8; 5;
     0; 6; 13; 1; 3; 13; 4; 14; 14; 0; 7; 11; 5; 3; 11; 8;
     9; 4; 14; 3; 15; 2; 5; 12; 2; 9; 8; 5; 12; 15; 3; 10;
     7; 11; 0; 14; 4; 1; 10; 7; 1; 6; 13; 0; 11; 8; 6; 13
  ];
  [
    4; 13; 11; 0; 2; 11; 14; 7; 15; 4; 0; 9; 8; 1; 13; 10;
    3; 14; 12; 3; 9; 5; 7; 12; 5; 2; 10; 15; 6; 8; 1; 6;
    1; 6; 4; 11; 11; 13; 13; 8; 12; 1; 3; 4; 7; 10; 14; 7;
    10; 9; 15; 5; 6; 0; 8; 15; 0; 14; 5; 2; 9; 3; 2; 12
  ];
  [
    13; 1; 2; 15; 8; 13; 4; 8; 6; 10; 15; 3; 11; 7; 1; 4;
    10; 12; 9; 5; 3; 6; 14; 11; 5; 0; 0; 14; 12; 9; 7; 2;
     7; 2; 11; 1; 4; 14; 1; 7; 9; 4; 12; 10; 14; 8; 2; 13;
     0; 15; 6; 12; 10; 9; 13; 0; 15; 3; 3; 5; 5; 6; 8; 11
  ]
]

let p_table = [
  16;  7; 20; 21;
  29; 12; 28; 17;
   1; 15; 23; 26;
   5; 18; 31; 10;
   2;  8; 24; 14;
  32; 27;  3;  9;
  19; 13; 30;  6;
  22; 11;  4; 25
]

let cDES_BLOCK_SIZE = 8 (* 64 bits, defined in the standard *)
let cDES_KEY_SIZE = 8 (* 56 bits, but must supply 64 (8 are ignored) *)
let cEXPANSION_BLOCK_SIZE = 6
let cPC1_KEY_SIZE = 7
let cSUBKEY_SIZE = 6

type en_or_de =
  | Encrypt
  | Decrypt

let des_block_operate (plaintext: bytes) (ciphertext: bytes) (key: bytes) (flag: en_or_de) =
  let ip_block = Bytes.create cDES_BLOCK_SIZE in
  let expansion_block = Bytes.create cEXPANSION_BLOCK_SIZE in
  let substitution_block = Bytes.create (cDES_BLOCK_SIZE / 2) in
  let pbox_target = Bytes.create (cDES_BLOCK_SIZE / 2) in
  let recomb_box = Bytes.create (cDES_BLOCK_SIZE / 2) in
  let pc1key = Bytes.create cPC1_KEY_SIZE in
  let subkey = Bytes.create cSUBKEY_SIZE in
  (* Initial Permutation *)
  permute ip_block plaintext ip_table cDES_BLOCK_SIZE;
  (* Key Schedule computation *)
  permute pc1key key pc1_table cPC1_KEY_SIZE;
  let rec round i =
    if i <> 16 then begin
      permute expansion_block (Bytes.sub ip_block 4 4) expansion_table 6;
      let () = match flag with
        | Encrypt ->
            rol pc1key;
            if (not ((i <= 1) || (i = 8) || (i = 15))) then rol pc1key (* rotate twice except in is 1, 2, 9 & 16 *)
        | _ -> ()
      in
      permute subkey pc1key pc2_table cSUBKEY_SIZE;
      let () = match flag with
        | Decrypt -> 
            ror pc1key;
            if (not ((i >= 14) || (i = 7) || (i = 0))) then ror pc1key
        | _ -> ()
      in
      xor expansion_block subkey 6;
      (* Substitution *)
      Bytes.fill substitution_block 0 (cDES_BLOCK_SIZE / 2) (Char.chr 0x00);
      let expan0 = Byte.g_ expansion_block 0 in
      let expan1 = Byte.g_ expansion_block 1 in
      let expan2 = Byte.g_ expansion_block 2 in
      let expan3 = Byte.g_ expansion_block 3 in
      let expan4 = Byte.g_ expansion_block 4 in
      let expan5 = Byte.g_ expansion_block 5 in
      Byte.s_ substitution_block 0 ((List.nth (List.nth sbox 0) ((expan0 land 0xFC) lsr 2)) << 4) "round";
      Byte.s_ substitution_block 0 ((Byte.g_ substitution_block 0) lor (List.nth (List.nth sbox 1) (((expan0 land 0x03) << 4) lor ((expan1 land 0xF0) lsr 4)))) "round";
      Byte.s_ substitution_block 1 ((List.nth (List.nth sbox 2) (((expan1 land 0x0F) << 2) lor ((expan2 land 0xC0) lsr 6))) << 4) "round";
      Byte.s_ substitution_block 1 ((Byte.g_ substitution_block 1) lor (List.nth (List.nth sbox 3) (expan2 land 0x3F))) "round";
      Byte.s_ substitution_block 2 ((List.nth (List.nth sbox 4) ((expan3 land 0xFC) lsr 2)) << 4) "round";
      Byte.s_ substitution_block 2 ((Byte.g_ substitution_block 2) lor (List.nth (List.nth sbox 5) (((expan3 land 0x03) << 4) lor ((expan4 land 0xF0) lsr 4)))) "round";
      Byte.s_ substitution_block 3 ((List.nth (List.nth sbox 6) (((expan4 land 0x0F) << 2) lor ((expan5 land 0xC0) lsr 6))) << 4) "round";
      Byte.s_ substitution_block 3 ((Byte.g_ substitution_block 3) lor (List.nth (List.nth sbox 7) (expan5 land 0x3F))) "round";
      permute pbox_target substitution_block p_table (cDES_BLOCK_SIZE / 2);
      Byte.blit ip_block 0 recomb_box 0 (cDES_BLOCK_SIZE / 2);
      Byte.blit ip_block 4 ip_block 0 (cDES_BLOCK_SIZE / 2);
      xor recomb_box pbox_target (cDES_BLOCK_SIZE / 2);
      Byte.blit recomb_box 0 ip_block 4 (cDES_BLOCK_SIZE / 2);
      round (i + 1)
    end
  in
  round 0;
  (* swap one last time *)
  Byte.blit ip_block 0 recomb_box 0 (cDES_BLOCK_SIZE / 2);
  Byte.blit ip_block 4 ip_block 0 (cDES_BLOCK_SIZE / 2);
  Byte.blit recomb_box 0 ip_block 4 (cDES_BLOCK_SIZE / 2);
  (* final permutation (undo initial permutation) *)
  permute ciphertext ip_block fp_table cDES_BLOCK_SIZE

(* note this function assumes that input_len is a multiple of DES_BLOCK_SIZE *)
let des_operate (input: bytes) (input_len: int) (output: bytes) (key: bytes) (op: en_or_de) (iv: bytes) : unit =
  let input_block = Bytes.create cDES_BLOCK_SIZE in
  let output_block = Bytes.create cDES_BLOCK_SIZE in
  let rec loop input_len off =
    if input_len > 0 then begin
      Bytes.blit input off input_block 0 cDES_BLOCK_SIZE;
      let () = match op with
        | Encrypt ->
            xor input_block iv cDES_BLOCK_SIZE;
            des_block_operate input_block output_block key op;
            Bytes.blit output_block 0 iv 0 cDES_BLOCK_SIZE
        | Decrypt ->
            des_block_operate input_block output_block key op;
            xor output_block iv cDES_BLOCK_SIZE;
            Bytes.blit input_block 0 iv 0 cDES_BLOCK_SIZE
      in
      Bytes.blit output_block 0 output off cDES_BLOCK_SIZE;
      loop (input_len - cDES_BLOCK_SIZE) (off + cDES_BLOCK_SIZE)
    end
  in
  loop input_len 0

let des_decrypt (ciphertext: bytes) (cipher_len: int) (plaintext: bytes) (iv: bytes) (key: bytes) : unit =
  des_operate ciphertext cipher_len plaintext key Decrypt iv

let des_encrypt (plaintext: bytes) (plain_len: int) (ciphertext: bytes) (iv: bytes) (key: bytes) : unit =
  des_operate plaintext plain_len ciphertext key Encrypt iv

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
      des_encrypt (Bytes.of_string m_input) input_len output (Bytes.of_string iv) (Bytes.of_string key)
    else
      des_decrypt (Bytes.of_string m_input) input_len output (Bytes.of_string iv) (Bytes.of_string key)
  in
  Byte.print_byte output;
  Printf.printf "%s\n" (Byte.to_string output)

