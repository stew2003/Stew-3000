open Stdlib
open Printf
open Ast
open Util.Srcloc

exception CompilerParseError of string * maybe_loc

(* [prog_from_defns] converts a list of function definitions
  into a program, by extracting out the main function *)
let prog_from_defns (defns : func_defn list) : prog =
  match lookup "main" defns with
  | Some main ->
      let funcs = List.filter (fun d -> d.name <> "main") defns in
      { main; funcs }
  | None -> raise (CompilerParseError ("program had no main function", None))

(* [parse] consumes a string representing a source program
  and parses it into a list of function definitions *)
let parse (s : string) : prog =
  let buf = Lexing.from_string s in
  match Parse.program Lex.token buf with
  | funcs -> prog_from_defns funcs
  | exception Lex.Error (msg, loc) -> raise (CompilerParseError (msg, Some loc))
  | exception Parse.Error ->
      let pos = Lexing.lexeme_start_p buf in
      let msg =
        sprintf "bad token '%s' on line %d"
          (String.escaped (Lexing.lexeme buf))
          pos.pos_lnum
      in
      raise (CompilerParseError (msg, Some (loc_from_lexbuf buf)))
