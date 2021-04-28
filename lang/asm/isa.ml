open Printf

(* Programmer-facing registers: A, B, C, and stack pointer *)
type register = A | B | C | SP

(* Immediate values (really, signed 8-bit integers) *)
type immediate = int

(* Either a register or an immediate.
  NOTE: this isn't used much, since most instructions are 
  restricted in their operands, and we can express that *)
type operand = Reg of register | Imm of immediate

(* Assembly instructions *)
type instr =
  | Add of register * register
  | Addi of immediate * register
  | Sub of register * register
  | Subi of immediate * register
  | And of register * register
  | Ani of immediate * register
  | Or of register * register
  | Ori of immediate * register
  | Xor of register * register
  | Xri of immediate * register
  | Not of register
  | Inr of register
  | Dcr of register
  | Mov of register * register
  | Mvi of immediate * register
  | Ld of register * register
  | St of register * register
  | Lds of immediate * register
  | Sts of register * immediate
  | Cmp of register * register
  | Cmpi of operand * operand
  | Label of string
  | Jmp of string
  | Je of string
  | Jne of string
  | Jg of string
  | Jge of string
  | Jl of string
  | Jle of string
  | Call of string
  | Ret
  | Dic of immediate
  | Did of immediate
  | Hlt
  | Nop
  | Out of register

(* [string_of_register] converts a register into a printable string *)
let string_of_register (reg : register) : string =
  match reg with A -> "a" | B -> "b" | C -> "c" | SP -> "sp"

(* [string_of_imm] converts an immediate value into a string *)
let string_of_imm (imm : immediate) : string = string_of_int imm

(* [string_of_operand] converts an operand (either register 
  or immediate) into a string *)
let string_of_operand (op : operand) : string =
  match op with Reg r -> string_of_register r | Imm imm -> string_of_imm imm

(* [string_of_instr] converts an asm instruction into a string *)
let string_of_instr (ins : instr) : string =
  match ins with
  | Add (src, dst) ->
      sprintf "\tadd %s, %s" (string_of_register src) (string_of_register dst)
  | Addi (imm, dst) ->
      sprintf "\taddi %s, %s" (string_of_imm imm) (string_of_register dst)
  | Sub (src, dst) ->
      sprintf "\tsub %s, %s" (string_of_register src) (string_of_register dst)
  | Subi (imm, dst) ->
      sprintf "\tsubi %s, %s" (string_of_imm imm) (string_of_register dst)
  | And (src, dst) ->
      sprintf "\tand %s, %s" (string_of_register src) (string_of_register dst)
  | Ani (imm, dst) ->
      sprintf "\tani %s, %s" (string_of_imm imm) (string_of_register dst)
  | Or (src, dst) ->
      sprintf "\tor %s, %s" (string_of_register src) (string_of_register dst)
  | Ori (imm, dst) ->
      sprintf "\tori %s, %s" (string_of_imm imm) (string_of_register dst)
  | Xor (src, dst) ->
      sprintf "\txor %s, %s" (string_of_register src) (string_of_register dst)
  | Xri (imm, dst) ->
      sprintf "\txri %s, %s" (string_of_imm imm) (string_of_register dst)
  | Not reg -> sprintf "\tnot %s" (string_of_register reg)
  | Inr reg -> sprintf "\tinr %s" (string_of_register reg)
  | Dcr reg -> sprintf "\tdcr %s" (string_of_register reg)
  | Mov (src, dst) ->
      sprintf "\tmov %s, %s" (string_of_register src) (string_of_register dst)
  | Mvi (imm, dst) ->
      sprintf "\tmvi %s, %s" (string_of_imm imm) (string_of_register dst)
  | Ld (src, dst) ->
      sprintf "\tld %s, %s" (string_of_register src) (string_of_register dst)
  | St (src, dst) ->
      sprintf "\tst %s, %s" (string_of_register src) (string_of_register dst)
  | Lds (imm, dst) ->
      sprintf "\tlds %s, %s" (string_of_imm imm) (string_of_register dst)
  | Sts (src, imm) ->
      sprintf "\tsts %s, %s" (string_of_register src) (string_of_imm imm)
  | Cmp (left, right) ->
      sprintf "\tcmp %s, %s" (string_of_register left)
        (string_of_register right)
  | Cmpi (left, right) ->
      sprintf "\tcmpi %s, %s" (string_of_operand left) (string_of_operand right)
  | Label name -> sprintf "%s:" name
  | Jmp lbl -> sprintf "\tjmp %s" lbl
  | Je lbl -> sprintf "\tje %s" lbl
  | Jne lbl -> sprintf "\tjne %s" lbl
  | Jg lbl -> sprintf "\tjg %s" lbl
  | Jge lbl -> sprintf "\tjge %s" lbl
  | Jl lbl -> sprintf "\tjl %s" lbl
  | Jle lbl -> sprintf "\tjle %s" lbl
  | Call lbl -> sprintf "\tcall %s" lbl
  | Ret -> "\tret"
  | Dic byte -> sprintf "\tdic %d" byte
  | Did byte -> sprintf "\tdid %d" byte
  | Hlt -> "\thlt"
  | Nop -> "\tnop"
  | Out reg -> sprintf "\tout %s" (string_of_register reg)

(* [string_of_instr_list] converts a list of instructions
    into a single, newline-separated string *)
let string_of_instr_list (instrs : instr list) =
  (* convert instrs to strings *)
  instrs |> List.map string_of_instr
  (* concat them all into one big string with newline separators *)
  |> String.concat "\n"
