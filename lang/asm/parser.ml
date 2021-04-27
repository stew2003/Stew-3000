open Stdlib
open Printf

let parse_and_handle_errs buf =
  match Parse.main Lex.token buf with
  | instrs -> instrs
  | exception Lex.Error msg ->
      eprintf "%s\n" msg;
      exit 1
  | exception Parse.Error ->
      let pos = Lexing.lexeme_start_p buf in
      eprintf "Bad token '%s' on line %d\n" (Lexing.lexeme buf) pos.pos_lnum;
      exit 1

let parse (s : string) =
  let buf = Lexing.from_string s in
  parse_and_handle_errs buf

let parse_file file =
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
  parse_and_handle_errs lexbuf
