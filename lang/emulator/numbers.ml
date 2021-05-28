(* How much to shift left in order to erase all bits 
  other than the rightmost eight *)
let shift_amount = Sys.int_size - 8

(* [as_8bit_unsigned] interprets an integer as an 8-bit,
  unsigned integer *)
let as_8bit_unsigned (n : int) : int = (n lsl shift_amount) lsr shift_amount

(* [as_8bit_signed] interprets an integer as an 8-bit,
  signed (two's complement) integer *)
let as_8bit_signed (n : int) : int = (n lsl shift_amount) asr shift_amount
