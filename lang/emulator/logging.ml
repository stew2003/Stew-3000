open Util
open Printf
open Asm.Isa
open Machine

(* [log_output] logs the bytes sent to the decimal display  
  via out instructions as they are emulated, if verbosity is 
  at level 1 or greater. *)
let log_output (verbosity : int) (out_value : int) =
  if verbosity >= 1 then printf "%s %d\n" (Colors.log "[output]") out_value
  else ()

(* [log_current_ins] logs the current instruction as it is about
  to be executed, if verbosity is at level 2 or greater *)
let log_current_ins (verbosity : int) (machine : stew_3000) (ins : instr) =
  if verbosity >= 2 then
    printf "%s %s\n"
      (Colors.log (sprintf "[executing at 0x%02x]" machine.pc))
      (string_of_instr ins)
  else ()
