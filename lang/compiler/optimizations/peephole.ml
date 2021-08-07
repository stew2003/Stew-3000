open Asm.Isa

(* [eliminate_nops] eliminates instructions which have no effect. *)
let rec eliminate_nops (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | Addi (0, _, _) :: rest
  | Subi (0, _, _) :: rest
  | Ani (-1, _, _) :: rest
  | Ani (255, _, _) :: rest
  | Ori (0, _, _) :: rest
  | Nop _ :: rest ->
      eliminate_nops rest
  | (Sts (reg, imm, _) as store) :: Lds (same_imm, same_reg, _) :: rest
    when reg = same_reg && imm = same_imm ->
      store :: eliminate_nops rest
  | first :: rest -> first :: eliminate_nops rest

(* [replace_with_smaller] replaces single instructions with
   equivalent instructions with a smaller byte representation *)
let rec replace_with_smaller (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | Addi (1, reg, loc) :: rest
  | Subi (-1, reg, loc) :: rest
  | Subi (255, reg, loc) :: rest ->
      Inr (reg, loc) :: replace_with_smaller rest
  | Addi (-1, reg, loc) :: rest
  | Subi (1, reg, loc) :: rest
  | Addi (255, reg, loc) :: rest ->
      Dcr (reg, loc) :: replace_with_smaller rest
  | first :: rest -> first :: replace_with_smaller rest

(* [peephole_optimize] applies all peephole optimizations to the given program *)
let peephole_optimize (instrs : instr list) : instr list =
  instrs |> eliminate_nops |> replace_with_smaller
