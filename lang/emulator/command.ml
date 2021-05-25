open Machine
open Asm.Isa
open Ast
open Parser

(* 
  Command Examples:
  print a
  print zf
  print stack[17]
  print regs
  print flags
  print dec
  print stack
  print machine
  print ins
  set a 0xae
  set of 1
  set stack[10] 0xff
  set halted true
  next
 *)

(* [exec_command] carries out the command given, updating the given machine *)
let exec_command (cmd : command) (machine : stew_3000) (ins : instr) =
  match cmd with
  | PrintReg reg ->
      print_endline
        (match reg with
        | A -> string_of_reg "a" machine.a
        | B -> string_of_reg "b" machine.b
        | C -> string_of_reg "c" machine.c
        | SP -> string_of_reg "sp" machine.sp
        | PC -> string_of_reg "pc" machine.pc)
  | PrintFlag flag ->
      print_endline
        (match flag with
        | ZF -> string_of_flag "zf" machine.zflag
        | SF -> string_of_flag "sf" machine.sflag
        | OF -> string_of_flag "of" machine.oflag)
  | PrintStackAtAddr addr ->
      print_endline (string_of_stack_at_addr machine.stack addr)
  | PrintRegs -> print_endline (string_of_all_regs machine)
  | PrintFlags -> print_endline (string_of_all_flags machine)
  | PrintDecHistory ->
      print_endline (string_of_dec_display machine.dec_disp_history)
  | PrintStack -> print_endline (string_of_stack machine.stack)
  | PrintFullState -> print_endline (string_of_stew_3000 machine)
  | PrintCurrentIns -> print_endline (string_of_instr ins)
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
  | NoCommand | Next -> ()

let stop_for_commands (machine : stew_3000) (ins : instr) =
  let last_cmd = ref None in
  let rec loop _ =
    let cmd =
      print_string "(3db) ";
      try Some (parse (read_line ()))
      with CmdParseError msg ->
        print_endline (Printf.sprintf "command parse error: %s" msg);
        None
    in
    match cmd with
    (* next instruction, break out of loop & back to emulator *)
    | Some Next -> ()
    (* empty input, repeat last command *)
    | Some NoCommand -> (
        match !last_cmd with
        | Some cmd ->
            exec_command cmd machine ins;
            loop ()
        | None -> loop ())
    (* parsed command, run it *)
    | Some cmd ->
        last_cmd := Some cmd;
        exec_command cmd machine ins;
        loop ()
    | None -> loop ()
  in
  loop ()
