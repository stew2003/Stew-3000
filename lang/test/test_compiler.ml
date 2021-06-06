open OUnit2
open Compiler
open Emulator
open Emulator__Machine

(* [run] parses a source program, compiles it, and runs
  the generated instructions in the emulator, returning
  the final machine state. *)
let run (source : string) : stew_3000 =
  let pgrm = Parser.parse source in
  let instrs = Compile.compile pgrm in
  emulate instrs

(* [main_from_body] constructs a source string for a main function
  given the contents of its body. *)
let main_from_body (body : string) : string =
  Printf.sprintf "void main() { %s }" body

(* [run_body] generates a program with main() containing the given 
  body and runs it in the emulator. *)
let run_body (body : string) : stew_3000 = run (main_from_body body)

let test_expr_stmt _ =
  let machine = run_body "7;" in
  assert_equal 7 machine.a

let test_let _ =
  let machine = run_body "int x = 17; x;" in
  assert_equal 17 machine.a

let test_if_else _ =
  let machine = run_body "if (1) { 50; } else { 100; }" in
  assert_equal 50 machine.a;
  let machine = run_body "if (5 == 17) { 4; } else { 6; }" in
  assert_equal 6 machine.a

let suite =
  "Compiler Tests"
  >::: [
         "test_expr_stmt" >:: test_expr_stmt;
         "test_let" >:: test_let;
         "test_if_else" >:: test_if_else;
       ]

let () = run_test_tt_main suite
