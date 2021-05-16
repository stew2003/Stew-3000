open Ast
open Asm.Isa
open Asm
open Core
open Util

(* [runtime] constructs the runtime code necessary for a given 
  program. Usually, this will be empty, but if the program requires
  special runtime functionality (multiplication, division, ...) this
  will contribute those subroutines *)
let runtime (program : prog) : instr list = []

(* [from_file] reads a file containing an asm implementation
  of runtime functionality and parses it into a list of instructions. *)
let from_file (filename : string) : instr list =
  let path = Printf.sprintf "compiler/runtime/%s" filename in
  try
    let source = In_channel.read_all path in
    try
      let asm = Parser.parse source in
      List.map asm ~f:(fun (ins, _) -> ins)
    with Parser.AsmParseError (msg, loc) ->
      Printf.eprintf "%s: %s\n%s\n"
        (Colors.error "Error parsing runtime functions from `" ^ filename ^ "`")
        msg
        (Srcloc.string_of_src_loc loc source);
      exit 1
  with err ->
    Printf.eprintf "Error with getting runtime functions from `%s`: %s\n"
      filename (Exn.to_string err);
    exit 1

(* Implementation of multiplication *)
let runtime_multiply = from_file "multiply.3000.s"

(* Implementation of division *)
let runtime_divide = from_file "divide.3000.s"

(* Helpful functionality for dealing with signed operands to mult/div *)
let runtime_sign_utils = from_file "sign_utils.3000.s"
