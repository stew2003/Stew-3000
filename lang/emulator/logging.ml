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
let log_current_ins (verbosity : int) (ins : instr) =
  if verbosity >= 2 then
    printf "%s %s\n" (Colors.log "[current instruction]") (string_of_instr ins)
  else ()

(* [log_full_state] logs the entire state of the machine after
  executing an instruction, if verbosity is at level 3 or greater. *)
let log_full_state (verbosity : int) (machine : stew_3000) =
  if verbosity >= 3 then
    printf "%s\n%s"
      (Colors.log "[state after executing]")
      (string_of_stew_3000 machine)
  else ()
