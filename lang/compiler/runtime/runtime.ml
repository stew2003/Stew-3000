open Ast
open Asm.Isa
open Asm.Parser

(* The below blobs include runtime implementations from the files
  in this (runtime/) directory. *)

let runtime_multiply = parse [%blob "multiply.3000.s"]

let runtime_divide = parse [%blob "divide.3000.s"]

let runtime_sign_utils = parse [%blob "sign_utils.3000.s"]

let runtime_assert = parse [%blob "assert.3000.s"]

(* [uses_mult] determines if a program uses multiplication *)
let uses_mult (pgrm : prog) =
  check_for_expr pgrm (function BinOp (Mult, _, _, _) -> true | _ -> false)

(* [uses_div] determines if a program uses division *)
let uses_div (pgrm : prog) =
  check_for_expr pgrm (function BinOp (Div, _, _, _) -> true | _ -> false)

(* [uses_mod] determines if a program uses modulus *)
let uses_mod (pgrm : prog) =
  check_for_expr pgrm (function BinOp (Mod, _, _, _) -> true | _ -> false)

(* [uses_assert] determines if a program uses assert statements *)
let uses_assert (pgrm : prog) =
  check_for_stmt pgrm (function Assert _ -> true | _ -> false)

(* [conditionally_include] returns either the given code or an empty program,
  depending on whether the given condition is true or not. *)
let conditionally_include (code : instr list) (condition : bool) : instr list =
  if condition then code else []

(* [runtime] constructs the runtime code necessary for a given 
  program. Usually, this will be empty, but if the program requires
  special runtime functionality (multiplication, division, ...) this
  will contribute those subroutines *)
let runtime (program : prog) : instr list =
  (* multiply has its own implementation, but modulus and division
     use the same division algorithm *)
  let needs_mult_code = uses_mult program in
  let needs_div_code = uses_div program || uses_mod program in

  (* only include what this program needs. ~~ zero cost abstraction ~~ *)
  conditionally_include runtime_sign_utils (needs_mult_code || needs_div_code)
  @ conditionally_include runtime_multiply needs_mult_code
  @ conditionally_include runtime_divide needs_div_code
  @ conditionally_include runtime_assert (uses_assert program)