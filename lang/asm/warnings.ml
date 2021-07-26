(* Warnings that may be emitted by the assembler. *)
type asm_warn = ProgramTooLarge of int

(* Type for a function that is passed into the assembler to handle warnings. *)
type asm_warn_handler = asm_warn -> unit

(* [message_of_asm_warn] converts an assembler warning into a tuple of 
    strings (message, extra, help). *)
let message_of_asm_warn (warning : asm_warn) (_source_text : string)
    (_source_filename : string) : string * string option * string option =
  match warning with
  | ProgramTooLarge size ->
      ( Printf.sprintf
          "assembled program exceeds maximum binary size (%d bytes)" size,
        None,
        Some "consider reducing program size" )
