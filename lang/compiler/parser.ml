open Stdlib
open Printf
open Ast
open Util.Srcloc

exception CompilerParseError of string * src_loc

(* [parse] consumes a string representing a source program
  and parses it into a list of function definitions *)
let parse (s : string) : func_defn list =
  let buf = Lexing.from_string s in
  match Parse.program Lex.token buf with
  | funcs -> funcs
  | exception Lex.Error (msg, loc) -> raise (CompilerParseError (msg, loc))
  | exception Parse.Error ->
      let pos = Lexing.lexeme_start_p buf in
      let msg =
        sprintf "bad token '%s' on line %d"
          (String.escaped (Lexing.lexeme buf))
          pos.pos_lnum
      in
      raise (CompilerParseError (msg, loc_from_lexbuf buf))
