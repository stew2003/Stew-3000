open Stdlib
open Printf

exception AsmParseError of string

let parse (s : string) =
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
