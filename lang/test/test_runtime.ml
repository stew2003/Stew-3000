open OUnit2
open Compiler
open Compiler.Ast

let pgrm_from_body (body : string) : prog =
  Parser.parse (Printf.sprintf "void main() { %s }" body)

let test_expr_detection _ =
  assert_equal true (Runtime.uses_mult (pgrm_from_body "2 * 3;"));
  assert_equal false (Runtime.uses_mult (pgrm_from_body "1 + 2 / 7;"));
  assert_equal true (Runtime.uses_div (pgrm_from_body "4 / 10;"));
  assert_equal false (Runtime.uses_div (pgrm_from_body "f(4, 7 * 12);"));
  assert_equal true
    (Runtime.uses_mult
       (Parser.parse
          "int func(int n, int m) { if (n == 0) { return n * m; } return 1; } \
           void main() {}"));
  assert_equal true
    (Runtime.uses_div (Parser.parse "void main() { exit(100 / 25); }"));
  assert_equal true
    (Runtime.uses_assert (pgrm_from_body "int y = 10; assert(y != 5);"));
  assert_equal false (Runtime.uses_assert (pgrm_from_body "if (1) { 2 + 2; }"));
  assert_equal true
    (Runtime.uses_mod (Parser.parse "void main() { int x = 40 % 2; }"));
  assert_equal false (Runtime.uses_mod (Parser.parse "void main() { 10 / 3; }"))

let test_empty_runtime _ =
  assert_equal []
    (Runtime.runtime
       (Parser.parse "void main() { int x = 5; x++; exit(x + 3); }"))

let suite =
  "Runtime Tests"
  >::: [
         "test_expr_detection" >:: test_expr_detection;
         "test_empty_runtime" >:: test_empty_runtime;
       ]

let () = run_test_tt_main suite
