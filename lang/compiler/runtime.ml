open Ast
open Asm.Isa

(* [runtime] constructs the runtime code necessary for a given 
  program. Usually, this will be empty, but if the program requires
  special runtime functionality (multiplication, division, ...) this
  will contribute those subroutines *)
let runtime (program : prog) : instr list = []

(* Implementation of multiplication *)
let runtime_multiply = []

(* Implementation of division *)
let runtime_divide = []
