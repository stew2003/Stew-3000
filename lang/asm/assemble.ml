open Isa

(* [assemble] processes a list of asm instructions and 
  produces a byte sequence representing the program in binary form *)
let assemble (_instrs : instr list) : bytes = Bytes.create 0
