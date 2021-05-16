open Lexing
open Printf

(* src_loc describes a range of line numbers in a source file *)
type src_loc = { startl : int; endl : int }

type maybe_loc = src_loc option

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
  let lno = (lexeme_start_p buf).pos_lnum in
  loc lno lno

(* the periphery is how many lines above and below the source 
  location are printed when the location is printed *)
let periphery = 2

(* [string_of_src_loc] converts a source location and a source file 
  contents into a message indicating the position in the source file *)
let string_of_src_loc (loc : src_loc) (source : string) : string =
  let lines_with_color lines colorize =
    List.map
      (fun (i, line) ->
        let lineno = Colors.br_black (sprintf "%3d | " i) in
        sprintf "%s%s" lineno (colorize line))
      lines
  in
  (* get lines of file, with their line numbers *)
  let lines =
    String.split_on_char '\n' source |> List.mapi (fun i line -> (i + 1, line))
  in
  let lines_by_idx (pred : int -> bool) =
    List.filter (fun (i, _) -> pred i) lines
  in
  let at_loc = lines_by_idx (fun i -> i >= loc.startl && i <= loc.endl) in
  let before_loc =
    lines_by_idx (fun i -> loc.startl - i <= periphery && loc.startl - i > 0)
  in
  let after_loc =
    lines_by_idx (fun i -> i - loc.endl <= periphery && i - loc.endl > 0)
  in
  (* get lines at & around src loc, colorized appropriately *)
  [ "At location:" ]
  @ lines_with_color before_loc (fun line -> Colors.br_black line)
  @ lines_with_color at_loc (fun line -> line)
  @ lines_with_color after_loc (fun line -> Colors.br_black line)
  |> String.concat "\n"

(* [string_of_maybe_loc] is a wrapper for string of src loc that handles 
  optional locations *)
let string_of_maybe_loc (loc : src_loc option) (source : string) : string =
  match loc with None -> "" | Some loc -> string_of_src_loc loc source
