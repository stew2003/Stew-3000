open Printf
open Util.Srcloc

(* Warnings that may be emitted by the assembler. *)
type asm_warn_inner = ProgramTooLarge of int

type asm_warn = asm_warn_inner with_loc_opt

(* Type for a function that is passed into the assembler
    to handle warnings. *)
type asm_warn_handler = asm_warn -> unit

(* [string_of_asm_warn_inner] converts an assembler warning into a printable string. *)
let string_of_asm_warn_inner (warning : asm_warn_inner) : string =
  match warning with
  | ProgramTooLarge size ->
      sprintf "assembled program exceeds maximum binary size (%d bytes)" size
