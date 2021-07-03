open Stdlib
open Printf
open Ast
open Util.Srcloc

exception CompilerParseError of string * maybe_loc

(* [prog_from_defns] converts a list of function definitions
  into a program, by extracting out the main function *)
let prog_from_defns (defines : pp_define list) (defns : func_defn list) : prog =
  match List.partition (fun defn -> defn.name = "main") defns with
  | [ main ], funcs -> { defines; main; funcs }
  | _ ->
      raise
        (CompilerParseError ("program must have exactly one main function", None))

(* [parse] consumes a string representing a source program
  and parses it into a list of function definitions *)
let parse (s : string) : prog =
  let buf = Lexing.from_string s in
  match Parse.program Lex.token buf with
  | defines, funcs -> prog_from_defns defines funcs
  | exception Lex.Error (msg, loc) -> raise (CompilerParseError (msg, Some loc))
  | exception Parse.Error ->
      let pos = Lexing.lexeme_start_p buf in
      let msg =
        sprintf "bad token '%s' on line %d"
          (String.escaped (Lexing.lexeme buf))
          pos.pos_lnum
      in
      raise (CompilerParseError (msg, Some (loc_from_lexbuf buf)))
