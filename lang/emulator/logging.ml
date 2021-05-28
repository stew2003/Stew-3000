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

(* [warn_arith_overflow] prints a warning (if enabled) indicating
  that signed overflow occurred during an arithmetic instruction *)
let warn_arith_overflow (warn : bool) (ins : instr) (result : int) (pc : int) =
  if warn then
    print_endline
      (Colors.warn
         (sprintf "[arithmetic overflow at pc 0x%02x %s: produced %d]" pc
            (string_of_instr ins) result))
  else ()
