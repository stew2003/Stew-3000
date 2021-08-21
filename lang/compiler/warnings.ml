open Util.Srcloc
open Ast
open Printf

(* Warnings that may be emitted by the compiler (generally speaking). *)
type compiler_warn =
  | DivisionByZero of expr
  | ZeroSizeArray of string * stmt
  | EmptyInitializer of string * stmt

(* Type for a function that is passed into the compiler to handle warnings. *)
type compiler_warn_handler = compiler_warn -> unit

(* [message_of_compiler_warn] converts a compiler warning into a tuple of
    strings (message, extra, help). *)
let message_of_compiler_warn (warning : compiler_warn) (source_text : string)
    (source_filename : string) : string * string option * string option =
  match warning with
  | DivisionByZero div_expr ->
      ( "division by zero (undefined behavior)",
        Option.map
          (fun loc -> string_of_src_loc loc source_text source_filename)
          (loc_from_expr div_expr),
        None )
  | ZeroSizeArray (arr, decl) ->
      ( sprintf "array `%s` has zero size" arr,
        Option.map
          (fun loc -> string_of_src_loc loc source_text source_filename)
          (loc_from_stmt decl),
        None )
  | EmptyInitializer (arr, decl) ->
      ( sprintf "array `%s` has an empty initializer" arr,
        Option.map
          (fun loc -> string_of_src_loc loc source_text source_filename)
          (loc_from_stmt decl),
        Some "consider omitting the initializer entirely" )
