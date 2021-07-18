open Stdlib
open Printf
open Isa
open Util.Srcloc

exception AsmParseError of string * src_loc

(* [parse] consumes a string representing an assembly program
  and parses it into a list of instructions *)
let parse (s : string) : instr list =
  let buf = Lexing.from_string s in
  match Parse.main Lex.token buf with
  | instrs -> instrs
  | exception Lex.Error (msg, loc) -> raise (AsmParseError (msg, loc))
  | exception Parse.Error ->
      let pos = Lexing.lexeme_start_p buf in
      let msg =
        sprintf "bad token '%s' on line %d"
          (String.escaped (Lexing.lexeme buf))
          pos.pos_lnum
      in
      raise (AsmParseError (msg, loc_from_lexbuf buf))
