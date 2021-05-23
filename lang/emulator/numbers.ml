(* [as_8bit_unsigned] interprets an integer as an 8-bit,
  unsigned integer *)
let as_8bit_unsigned : int -> int =
  let bytes = Bytes.make 1 '0' in
  fun (n : int) ->
    Bytes.set_uint8 bytes 0 n;
    Bytes.get_uint8 bytes 0

(* [as_8bit_signed] interprets an integer as an 8-bit,
  signed (two's complement) integer *)
let as_8bit_signed : int -> int =
  let bytes = Bytes.make 1 '0' in
  fun (n : int) ->
    Bytes.set_int8 bytes 0 n;
    Bytes.get_int8 bytes 0
