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
  | Xri (0, _, _) :: rest
  | Nop _ :: rest ->
      eliminate_nops rest
  | (Sts (reg, imm, _) as store) :: Lds (same_imm, same_reg, _) :: rest
    when reg = same_reg && imm = same_imm ->
      store :: eliminate_nops rest
  | Inr (reg, _) :: Dcr (same_reg, _) :: rest when reg = same_reg ->
      eliminate_nops rest
  | Dcr (reg, _) :: Inr (same_reg, _) :: rest when reg = same_reg ->
      eliminate_nops rest
  | Xri (255, reg, loc) :: rest | Xri (-1, reg, loc) :: rest ->
      Not (reg, loc) :: eliminate_nops rest
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
  | Addi (2, reg, loc) :: rest
  | Subi (-2, reg, loc) :: rest
  | Subi (254, reg, loc) :: rest ->
      Inr2 (reg, loc) :: replace_with_smaller rest
  | Addi (3, reg, loc) :: rest
  | Subi (-3, reg, loc) :: rest
  | Subi (253, reg, loc) :: rest ->
      Inr3 (reg, loc) :: replace_with_smaller rest
  | Addi (-1, reg, loc) :: rest
  | Subi (1, reg, loc) :: rest
  | Addi (255, reg, loc) :: rest ->
      Dcr (reg, loc) :: replace_with_smaller rest
  | Addi (-2, reg, loc) :: rest
  | Subi (2, reg, loc) :: rest
  | Addi (254, reg, loc) :: rest ->
      Dcr2 (reg, loc) :: replace_with_smaller rest
  | Addi (-3, reg, loc) :: rest
  | Subi (3, reg, loc) :: rest
  | Addi (253, reg, loc) :: rest ->
      Dcr3 (reg, loc) :: replace_with_smaller rest
  | Not (reg1, loc) :: Inr (reg2, _) :: rest when reg1 = reg2 ->
      Neg (reg1, loc) :: replace_with_smaller rest
  | Mvi (0, reg, loc) :: rest -> Mov (Z, reg, loc) :: replace_with_smaller rest
  | Cmpi (Imm 0, Reg reg, loc) :: rest ->
      Cmp (Z, reg, loc) :: replace_with_smaller rest
  | Cmpi (Reg reg, Imm 0, loc) :: rest ->
      Cmp (reg, Z, loc) :: replace_with_smaller rest
  | first :: rest -> first :: replace_with_smaller rest

(* [peephole_optimize] applies all peephole optimizations to the given program *)
let peephole_optimize (instrs : instr list) : instr list =
  instrs |> eliminate_nops |> replace_with_smaller
