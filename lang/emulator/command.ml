open Machine
open Asm.Isa

type register = A | B | C | SP | PC

type flag = ZF | SF | OF

type command =
  | PrintReg of register
  | PrintFlag of flag
  | PrintStackAtAddr of int
  | PrintRegs
  | PrintFlags
  | PrintDecHistory
  | PrintStack
  | PrintFullState
  | PrintCurrentIns
  | SetReg of register * int
  | SetFlag of flag * bool
  | SetStackAtAddr of int * int
  | SetHalted of bool
  | Next

let print_line (str : string) = Printf.printf "%s\n" str

(* [exec_command] carries out the command given, updating the given machine *)
let exec_command (cmd : command) (machine : stew_3000) (ins : instr) =
  match cmd with
  | PrintReg reg ->
      print_line
        (match reg with
        | A -> string_of_reg "a" machine.a
        | B -> string_of_reg "b" machine.b
        | C -> string_of_reg "c" machine.c
        | SP -> string_of_reg "sp" machine.sp
        | PC -> string_of_reg "pc" machine.pc)
  | PrintFlag flag ->
      print_line
        (match flag with
        | ZF -> string_of_flag "zf" machine.zflag
        | SF -> string_of_flag "sf" machine.sflag
        | OF -> string_of_flag "of" machine.oflag)
  | PrintStackAtAddr addr ->
      print_line (string_of_stack_at_addr machine.stack addr)
  | PrintRegs -> print_line (string_of_all_regs machine)
  | PrintFlags -> print_line (string_of_all_flags machine)
  | PrintDecHistory ->
      print_line (string_of_dec_display machine.dec_disp_history)
  | PrintStack -> print_line (string_of_stack machine.stack)
  | PrintFullState -> print_line (string_of_stew_3000 machine)
  | PrintCurrentIns -> print_line (string_of_instr ins)
  | SetReg (reg, value) -> (
      match reg with
      | A -> machine.a <- value
      | B -> machine.b <- value
      | C -> machine.c <- value
      | SP -> machine.sp <- value
      | PC -> machine.pc <- value)
  | SetFlag (flag, value) -> (
      match flag with
      | ZF -> machine.zflag <- value
      | SF -> machine.sflag <- value
      | OF -> machine.oflag <- value)
  | SetStackAtAddr (addr, value) ->
      let unsigned_addr = Numbers.as_8bit_unsigned addr in
      Array.set machine.stack unsigned_addr value
  | SetHalted halted -> machine.halted <- halted
  | Next -> ()
