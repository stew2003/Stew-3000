open OUnit2
open Compiler
open Compiler.Ast
open Testing_utils

(* [preprocess] is a wrapper around parsing a source string, preprocessing it,
  and normalizing its source locations. *)
let preprocess (source : string) : prog =
  source |> Parser.parse |> Preprocess.preprocess |> norm_prog_locs

(* [assert_preprocesses_to] asserts that the program specified by the given 
  "before" source text will be equivalent to the program specified by the given 
  "after" source text once it has been run through the preprocessor.  *)
let assert_preprocesses_to (before : string) (after : string) =
  assert_prog_eq (parse_norm after) (preprocess before)

let test_no_pp_no_change _ =
  let empty = "void main() {}" in
  assert_preprocesses_to empty empty;
  let more_complex = "void main() { if (5 < 10) { print(11); } exit(0); }" in
  assert_preprocesses_to more_complex more_complex

let test_pp_constant _ =
  assert_preprocesses_to "#define A 40 void main() { A; }" "void main() { 40; }";
  assert_preprocesses_to "#define B 1 #define C 0 void main() { print(B + C); }"
    "void main() { print(1 + 0); }"

let test_pp_expression _ =
  assert_preprocesses_to
    "#define EXPR (1 + 2 + 3 + 4) void main() { exit(EXPR); }"
    "void main() { exit(1 + 2 + 3 + 4); }"

let test_pp_non_constant _ =
  assert_preprocesses_to
    "#define CALL f(1, 2) void main() { CALL; } void f(int a, int b) {}"
    "void main() { f(1, 2); } void f(int a, int b) {}"

let test_pp_var _ =
  assert_preprocesses_to "#define VAR x void main() { int x = 10; print(VAR); }"
    "void main() { int x = 10; print(x); }"

let test_defines_can_see_earlier_defines _ =
  assert_preprocesses_to
    "\n\
     #define A 4 \n\
     #define B (A + 1) \n\
     #define C (B + A + 1) \n\
     void main() { A; B; C; }" "void main() { 4; 4 + 1; 4 + 1 + 4 + 1; }"

let test_defines_operate_on_prev_expansions _ =
  (* X is expanded to Y first, then Y is expanded in both positions to 4 *)
  assert_preprocesses_to "#define X Y #define Y 4 void main() { X; Y; }"
    "void main() { 4; 4; }"

let test_expanded_in_func_defns _ =
  assert_preprocesses_to
    "#define CONST 101 void main() {} int my_func() { return CONST; }"
    "void main() {} int my_func() { return 101; }"

let suite =
  "Preprocessor Tests"
  >::: [
         "test_no_pp_no_change" >:: test_no_pp_no_change;
         "test_pp_constant" >:: test_pp_constant;
         "test_pp_expression" >:: test_pp_expression;
         "test_pp_non_constant" >:: test_pp_non_constant;
         "test_pp_var" >:: test_pp_var;
         "test_defines_can_see_earlier_defines"
         >:: test_defines_can_see_earlier_defines;
         "test_defines_operate_on_prev_expansions"
         >:: test_defines_operate_on_prev_expansions;
         "test_expanded_in_func_defns" >:: test_expanded_in_func_defns;
       ]

let () = run_test_tt_main suite
