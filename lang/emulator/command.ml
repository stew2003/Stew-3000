open Machine
open Asm.Isa
open Ast
open Parser
open Util

(* [clear_screen] clears the screen and moves the cursor to home position *)
let clear_screen _ = Printf.printf "\027[2J\027[H"

let help_message =
  let help_line cmd descrip = Printf.sprintf "  %23s\t%s" cmd descrip in
  [
    "3db - The 3000 debugger";
    "";
    "Commands:";
    help_line "print <reg>" "print register contents";
    help_line "print <flag>" "print flag's state";
    help_line "print stack[<addr>]" "print stack contents at address";
    help_line "print regs" "print all register contents";
    help_line "print flags" "print all flag info";
    help_line "print dec" "print decimal display history";
    help_line "print stack" "print full stack";
    help_line "print machine" "print full machine state";
    help_line "print ins" "print instruction about to be executed";
    help_line "print halted" "print whether machine has halted";
    help_line "set <reg> <imm>" "set a register's contents";
    help_line "set <flag> <bool>" "set/unset a flag";
    help_line "set stack[<addr>] <imm>" "set stack at address";
    help_line "set halted <bool>" "set the machine's halted state";
    help_line "next" "execute current instruction and move to next";
    help_line "continue"
      "execute the rest of the program without stopping for commands";
    help_line "clear" "clears the screen";
    help_line "help" "print this message";
    "";
    "Note: enter repeats the last command";
  ]
  |> String.concat "\n"

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
  | PrintCurrentIns ->
      print_endline
        (Printf.sprintf "0x%02x %s" machine.pc (string_of_instr ins))
  | PrintHalted -> print_endline (string_of_bool machine.halted)
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
  | NoCommand | Next | Continue -> ()
  | Help -> print_endline help_message
  | Clear -> clear_screen ()

let prompt = Colors.log "(3db) "

(* [loop_for_commands] loops, receiving commands from user input,
  and executing them in the context of the current machine state
  and instruction. It breaks out of the loop when a "next" command
  is encountered. *)
let loop_for_commands =
  (* last_cmd tracks the last executed command, and is shared
     across all calls to loop_for_commands *)
  let last_cmd = ref None in
  let continuing = ref false in
  fun (machine : stew_3000) (ins : instr) ->
    let rec loop _ =
      let cmd =
        print_string prompt;
        try Some (parse (read_line ()))
        with CmdParseError msg ->
          print_endline (Printf.sprintf "command parse error: %s" msg);
          (* clear cached last command *)
          last_cmd := None;
          None
      in
      match cmd with
      | Some Continue -> continuing := true
      (* next instruction, break out of loop & back to emulator *)
      | Some Next -> last_cmd := Some Next
      (* empty input, repeat last command *)
      | Some NoCommand -> (
          match !last_cmd with
          | Some Next -> ()
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
    if not !continuing then loop ()
