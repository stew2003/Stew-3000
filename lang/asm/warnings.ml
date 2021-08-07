open Util.Srcloc

(* Warnings that may be emitted by the assembler. *)
type asm_warn =
  | ProgramTooLarge of int
  | OutOfBoundsLabel of string * int * maybe_loc

(* Type for a function that is passed into the assembler to handle warnings. *)
type asm_warn_handler = asm_warn -> unit

(* [message_of_asm_warn] converts an assembler warning into a tuple of
    strings (message, extra, help). *)
let message_of_asm_warn (warning : asm_warn) (source_text : string)
    (source_filename : string) : string * string option * string option =
  match warning with
  | ProgramTooLarge size ->
      ( Printf.sprintf
          "assembled program exceeds maximum binary size (%d bytes)" size,
        None,
        Some "consider reducing program size" )
  | OutOfBoundsLabel (label, addr, loc) ->
      ( Printf.sprintf "out of bounds label `%s` (at byte 0x%x)" label addr,
        Some (string_of_maybe_loc loc source_text source_filename),
        Some "reduce program size" )
