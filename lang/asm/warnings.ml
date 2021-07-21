open Printf

(* Warnings that may be emitted by the assembler. *)
type asm_warn = ProgramTooLarge of int

(* Type for a function that is passed into the assembler
    to handle warnings. *)
type asm_warn_handler = asm_warn -> unit

(* [string_of_asm_warn] converts an assembler warning into a tuple of 
    strings (message, extra, help). The message is a short sentence 
    describing the nature of the warning. The extra and help are both optional
    and contain extra info to be printed (such as src locs) and a help 
    suggestion, respectively. *)
let string_of_asm_warn (warning : asm_warn) :
    string * string option * string option =
  match warning with
  | ProgramTooLarge size ->
      ( sprintf "assembled program exceeds maximum binary size (%d bytes)" size,
        None,
        Some "consider reducing program size" )
