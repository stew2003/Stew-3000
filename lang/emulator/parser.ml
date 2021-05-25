open Ast

exception CmdParseError of string

(* [parse] consumes a string command and parses it into command ast *)
let parse (s : string) : command =
  let buf = Lexing.from_string s in
  match Parse.main Lex.token buf with
  | cmd -> cmd
  | exception Lex.Error msg -> raise (CmdParseError msg)
  | exception Parse.Error ->
      let pos = Lexing.lexeme_start_p buf in
      let msg =
        Printf.sprintf "bad token '%s' at character %d"
          (String.escaped (Lexing.lexeme buf))
          pos.pos_bol
      in
      raise (CmdParseError msg)
