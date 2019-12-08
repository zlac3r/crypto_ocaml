open Digest

let hmac key key_length text text_length digest endian =
  let ipad = Byte.make digest_block_size (Char.chr 0x36) in
  let opad = Byte.make digest_block_size (Char.chr 0x5c) in
  let hash1 = ref {
    hash = Array.copy !digest.hash;
    hash_len = !digest.hash_len;
    input_len = ref !(!digest.input_len);
    block_operate = !digest.block_operate;
    block_finalize = !digest.block_finalize;
    block = Byte.copy !digest.block;
    block_len = ref !(!digest.block_len)
  }
  in
  for i = 0 to key_length - 1 do
    Byte.s_ ipad i ((Byte.g_ ipad i) lxor (Byte.g_ key i));
  done;
  hash1 := update_digest !hash1 ipad digest_block_size;
  hash1 := update_digest !hash1 text text_length;
  hash1 := finalize_digest !hash1;
  for i = 0 to key_length - 1 do
    Byte.s_ opad i ((Byte.g_ opad i) lxor (Byte.g_ key i))
  done;
  digest := update_digest !digest opad digest_block_size;
  digest := update_digest !digest (hash_to_bytes !hash1.hash !hash1.hash_len endian) (!hash1.hash_len * 4);
  digest := finalize_digest !digest

let hex_decode input = 
  let len = String.length input in
  if input.[0] = '0' && input.[1] = 'x' then
    Hex.to_string (Hex.(`Hex (String.sub input 2 (len - 2))))
  else 
    input

let () =
  let algo = Sys.argv.(1) in
  let key = Sys.argv.(2) in
  let input = Sys.argv.(3) in
  let m_input = hex_decode input in
  let m_key = hex_decode key in
  let key_len = ref (String.length m_key) in
  let input_len = ref (String.length m_input) in
  let digest = ref new_md5_digest in
  let endian = if algo = "-md5" then Little else Big in
  if algo = "-sha1" then digest := new_sha1_digest else if algo = "-sha256" then digest := new_sha256_digest;
  hmac (Byte.of_string m_key) !key_len (Byte.of_string m_input) !input_len digest endian;
  if algo = "-md5" then
    display_hash !digest.hash Md5.md5_result_size endian
  else if algo = "-sha1" then
    display_hash !digest.hash Sha.sha1_result_size endian
  else if algo = "-sha256" then
    display_hash !digest.hash Sha.sha256_result_size endian
