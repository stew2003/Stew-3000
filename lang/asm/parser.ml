open Stdlib
open Printf
open Isa
open Util.Srcloc

exception AsmParseError of string

(* [parse] consumes a string representing an assembly program
  and parses it into a list of instructions with source location info *)
let parse (s : string) : instr with_loc list =
  let buf = Lexing.from_string s in
  match Parse.main Lex.token buf with
  | instrs -> instrs
  | exception Lex.Error msg -> raise (AsmParseError msg)
  | exception Parse.Error ->
      let pos = Lexing.lexeme_start_p buf in
      let msg =
        sprintf "bad token '%s' on line %d" (Lexing.lexeme buf) pos.pos_lnum
      in
      raise (AsmParseError msg)
