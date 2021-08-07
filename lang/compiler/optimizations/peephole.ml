open Asm.Isa

(* [eliminate_nops] eliminates instructions which have no effect. *)
let eliminate_nops (instrs : instr list) : instr list =
  (* addi 0, _
     subi 0, _
     ani -1, _
     ori 0, _
     nop

     sts R, I; lds I, R --> sts R, I
  *)
  failwith "not implemented!"

(* [replace_with_smaller] replaces single instructions with
   equivalent instructions with a smaller byte representation *)
let replace_with_smaller (instrs : instr list) : instr list =
  (*
     addi 1, _ --> inr _
     subi 1, _ --> dcr _

     This would be the place to take advantage of a 0 register
  *)
  failwith "not implemented!"

(* [peephole_optimize] applies all peephole optimizations to the given program *)
let peephole_optimize (instrs : instr list) : instr list =
  instrs |> eliminate_nops |> replace_with_smaller
