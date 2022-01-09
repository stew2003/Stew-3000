open Printf
open Util.Srcloc

(* Programmer-facing registers: A, B, C, Z, and stack pointer *)
type register = A | B | C | Z | SP

(* Immediate values (really, 8-bit integers) *)
type immediate = int

(* Either a register or an immediate.
   NOTE: this isn't used much, since most instructions are
   restricted in their operands, and we can express that *)
type operand = Reg of register | Imm of immediate

(* Assembly instructions *)
type instr =
  | Add of register * register * maybe_loc
  | Addi of immediate * register * maybe_loc
  | Addc of register * register * maybe_loc
  | Addci of immediate * register * maybe_loc
  | Sub of register * register * maybe_loc
  | Subi of immediate * register * maybe_loc
  | Subb of register * register * maybe_loc
  | Subbi of immediate * register * maybe_loc
  | And of register * register * maybe_loc
  | Ani of immediate * register * maybe_loc
  | Or of register * register * maybe_loc
  | Ori of immediate * register * maybe_loc
  | Xor of register * register * maybe_loc
  | Xri of immediate * register * maybe_loc
  | Not of register * maybe_loc
  | Neg of register * maybe_loc
  | Inr of register * maybe_loc
  | Inr2 of register * maybe_loc
  | Inr3 of register * maybe_loc
  | Dcr of register * maybe_loc
  | Dcr2 of register * maybe_loc
  | Dcr3 of register * maybe_loc
  | Mov of register * register * maybe_loc
  | Mvi of immediate * register * maybe_loc
  | Ld of register * register * maybe_loc
  | St of register * register * maybe_loc
  | Lds of immediate * register * maybe_loc
  | Sts of register * immediate * maybe_loc
  | Stsi of immediate * immediate * maybe_loc
  | Cmp of register * register * maybe_loc
  | Cmpi of operand * operand * maybe_loc
  | Label of string * maybe_loc
  | Jmp of string * maybe_loc
  | Je of string * maybe_loc
  | Jne of string * maybe_loc
  | Jg of string * maybe_loc
  | Jge of string * maybe_loc
  | Jl of string * maybe_loc
  | Jle of string * maybe_loc
  | Ja of string * maybe_loc
  | Jae of string * maybe_loc
  | Jb of string * maybe_loc
  | Jbe of string * maybe_loc
  | Call of string * maybe_loc
  | Ret of maybe_loc
  | Dic of immediate * maybe_loc
  | Did of immediate * maybe_loc
  | Dd of register * maybe_loc
  | Hlt of maybe_loc
  | Nop of maybe_loc
  | Out of register * maybe_loc
  | Outi of immediate * maybe_loc

(* [string_of_register] converts a register into a printable string *)
let string_of_register (reg : register) : string =
  match reg with A -> "a" | B -> "b" | C -> "c" | SP -> "sp" | Z -> "z"

(* [string_of_imm] converts an immediate value into a string *)
let string_of_imm (imm : immediate) : string = string_of_int imm

(* [string_of_operand] converts an operand (either register
   or immediate) into a string *)
let string_of_operand (op : operand) : string =
  match op with Reg r -> string_of_register r | Imm imm -> string_of_imm imm

(* [string_of_instr] converts an asm instruction into a string *)
let string_of_instr (ins : instr) : string =
  match ins with
  | Add (src, dst, _) ->
      sprintf "\tadd %s, %s" (string_of_register src) (string_of_register dst)
  | Addi (imm, dst, _) ->
      sprintf "\taddi %s, %s" (string_of_imm imm) (string_of_register dst)
  | Addc (src, dst, _) ->
      sprintf "\taddc %s, %s" (string_of_register src) (string_of_register dst)
  | Addci (imm, dst, _) ->
      sprintf "\taddci %s, %s" (string_of_imm imm) (string_of_register dst)
  | Sub (src, dst, _) ->
      sprintf "\tsub %s, %s" (string_of_register src) (string_of_register dst)
  | Subi (imm, dst, _) ->
      sprintf "\tsubi %s, %s" (string_of_imm imm) (string_of_register dst)
  | Subb (src, dst, _) ->
      sprintf "\tsubb %s, %s" (string_of_register src) (string_of_register dst)
  | Subbi (imm, dst, _) ->
      sprintf "\tsubbi %s, %s" (string_of_imm imm) (string_of_register dst)
  | And (src, dst, _) ->
      sprintf "\tand %s, %s" (string_of_register src) (string_of_register dst)
  | Ani (imm, dst, _) ->
      sprintf "\tani %s, %s" (string_of_imm imm) (string_of_register dst)
  | Or (src, dst, _) ->
      sprintf "\tor %s, %s" (string_of_register src) (string_of_register dst)
  | Ori (imm, dst, _) ->
      sprintf "\tori %s, %s" (string_of_imm imm) (string_of_register dst)
  | Xor (src, dst, _) ->
      sprintf "\txor %s, %s" (string_of_register src) (string_of_register dst)
  | Xri (imm, dst, _) ->
      sprintf "\txri %s, %s" (string_of_imm imm) (string_of_register dst)
  | Not (reg, _) -> sprintf "\tnot %s" (string_of_register reg)
  | Neg (reg, _) -> sprintf "\tneg %s" (string_of_register reg)
  | Inr (reg, _) -> sprintf "\tinr %s" (string_of_register reg)
  | Inr2 (reg, _) -> sprintf "\tinr2 %s" (string_of_register reg)
  | Inr3 (reg, _) -> sprintf "\tinr3 %s" (string_of_register reg)
  | Dcr (reg, _) -> sprintf "\tdcr %s" (string_of_register reg)
  | Dcr2 (reg, _) -> sprintf "\tdcr2 %s" (string_of_register reg)
  | Dcr3 (reg, _) -> sprintf "\tdcr3 %s" (string_of_register reg)
  | Mov (src, dst, _) ->
      sprintf "\tmov %s, %s" (string_of_register src) (string_of_register dst)
  | Mvi (imm, dst, _) ->
      sprintf "\tmvi %s, %s" (string_of_imm imm) (string_of_register dst)
  | Ld (src, dst, _) ->
      sprintf "\tld %s, %s" (string_of_register src) (string_of_register dst)
  | St (src, dst, _) ->
      sprintf "\tst %s, %s" (string_of_register src) (string_of_register dst)
  | Lds (imm, dst, _) ->
      sprintf "\tlds %s, %s" (string_of_imm imm) (string_of_register dst)
  | Sts (src, imm, _) ->
      sprintf "\tsts %s, %s" (string_of_register src) (string_of_imm imm)
  | Stsi (imm1, imm2, _) ->
      sprintf "\tstsi %s, %s" (string_of_imm imm1) (string_of_imm imm2)
  | Cmp (left, right, _) ->
      sprintf "\tcmp %s, %s" (string_of_register left)
        (string_of_register right)
  | Cmpi (left, right, _) ->
      sprintf "\tcmpi %s, %s" (string_of_operand left) (string_of_operand right)
  | Label (name, _) -> sprintf "%s:" name
  | Jmp (lbl, _) -> sprintf "\tjmp %s" lbl
  | Je (lbl, _) -> sprintf "\tje %s" lbl
  | Jne (lbl, _) -> sprintf "\tjne %s" lbl
  | Jg (lbl, _) -> sprintf "\tjg %s" lbl
  | Jge (lbl, _) -> sprintf "\tjge %s" lbl
  | Jl (lbl, _) -> sprintf "\tjl %s" lbl
  | Jle (lbl, _) -> sprintf "\tjle %s" lbl
  | Ja (lbl, _) -> sprintf "\tja %s" lbl
  | Jae (lbl, _) -> sprintf "\tjae %s" lbl
  | Jb (lbl, _) -> sprintf "\tjb %s" lbl
  | Jbe (lbl, _) -> sprintf "\tjbe %s" lbl
  | Call (lbl, _) -> sprintf "\tcall %s" lbl
  | Ret _ -> "\tret"
  | Dic (byte, _) -> sprintf "\tdic %d" byte
  | Did (byte, _) -> sprintf "\tdid %d" byte
  | Dd (reg, _) -> sprintf "\tdd %s" (string_of_register reg)
  | Hlt _ -> "\thlt"
  | Nop _ -> "\tnop"
  | Out (reg, _) -> sprintf "\tout %s" (string_of_register reg)
  | Outi (imm, _) -> sprintf "\touti %s" (string_of_imm imm)

(* [string_of_instr_list] converts a list of instructions
    into a single, newline-separated string *)
let string_of_instr_list (instrs : instr list) =
  (* convert instrs to strings *)
  (instrs |> List.map string_of_instr
 (* concat them all into one big string with newline separators *)
 |> String.concat "\n")
  ^ "\n"

(* [loc_from_instr] extracts the (optional) source
    location info from an instruction *)
let loc_from_instr (ins : instr) : maybe_loc =
  match ins with
  | Add (_, _, loc)
  | Addi (_, _, loc)
  | Addc (_, _, loc)
  | Addci (_, _, loc)
  | Sub (_, _, loc)
  | Subi (_, _, loc)
  | Subb (_, _, loc)
  | Subbi (_, _, loc)
  | And (_, _, loc)
  | Ani (_, _, loc)
  | Or (_, _, loc)
  | Ori (_, _, loc)
  | Xor (_, _, loc)
  | Xri (_, _, loc)
  | Not (_, loc)
  | Neg (_, loc)
  | Inr (_, loc)
  | Inr2 (_, loc)
  | Inr3 (_, loc)
  | Dcr (_, loc)
  | Dcr2 (_, loc)
  | Dcr3 (_, loc)
  | Mov (_, _, loc)
  | Mvi (_, _, loc)
  | Ld (_, _, loc)
  | St (_, _, loc)
  | Lds (_, _, loc)
  | Sts (_, _, loc)
  | Stsi (_, _, loc)
  | Cmp (_, _, loc)
  | Cmpi (_, _, loc)
  | Label (_, loc)
  | Jmp (_, loc)
  | Je (_, loc)
  | Jne (_, loc)
  | Jg (_, loc)
  | Jge (_, loc)
  | Jl (_, loc)
  | Jle (_, loc)
  | Ja (_, loc)
  | Jae (_, loc)
  | Jb (_, loc)
  | Jbe (_, loc)
  | Call (_, loc)
  | Ret loc
  | Dic (_, loc)
  | Did (_, loc)
  | Dd (_, loc)
  | Hlt loc
  | Nop loc
  | Out (_, loc)
  | Outi (_, loc) ->
      loc

(* [count_instrs] counts the number of actual instructions in a list of
    instructions (excluding labels). *)
let count_instrs (instrs : instr list) : int =
  instrs
  |> List.filter (fun ins -> match ins with Label _ -> false | _ -> true)
  |> List.length
