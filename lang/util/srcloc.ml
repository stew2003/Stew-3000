open Lexing

(* src_loc describes a range of line numbers in a source file *)
type src_loc = { startl : int; endl : int }

(* with_loc describes a type that has a src location attached to it *)
type 'a with_loc = 'a * src_loc

(* with_loc_opt describes a type that might have a src location attached *)
type 'a with_loc_opt = 'a * src_loc option

(* [loc] generates a new src_loc record with 
  the given start and ending line *)
let loc (startl : int) (endl : int) = { startl; endl }

(* [loc_from_lexbuf] constructs a src loc from
  the current line position of the lexing buffer *)
let loc_from_lexbuf (buf : lexbuf) =
  loc buf.lex_curr_p.pos_lnum buf.lex_curr_p.pos_lnum

(* [string_of_src_loc] converts a source location and a source file 
  contents into a message indicating the position in the source file *)
let string_of_src_loc (loc : src_loc) (source : string) : string =
  "TODO: SOURCE LOC"

(* [string_of_maybe_loc] is a wrapper for string of src loc that handles 
  optional locations *)
let string_of_maybe_loc (loc : src_loc option) (source : string) : string =
  match loc with None -> "" | Some loc -> string_of_src_loc loc source
