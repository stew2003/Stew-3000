open Util.Srcloc
open Ast

(* Warnings that may be emitted by the compiler (generally speaking). *)
type compiler_warn = DivisionByZero of expr

(* Type for a function that is passed into the compiler to handle warnings. *)
type compiler_warn_handler = compiler_warn -> unit

(* [message_of_compiler_warn] converts a compiler warning into a tuple of 
    strings (message, extra, help). *)
let message_of_compiler_warn (warning : compiler_warn) (source_text : string)
    (source_filename : string) : string * string option * string option =
  match warning with
  | DivisionByZero div_expr ->
      ( "division by zero (undefined behavior)",
        Some
          (string_of_maybe_loc (loc_from_expr div_expr) source_text
             source_filename),
        None )
