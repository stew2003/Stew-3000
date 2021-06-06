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
  assert_equal false (Runtime.uses_assert (pgrm_from_body "if (1) { 2 + 2; }"))

let suite =
  "Runtime Tests" >::: [ "test_expr_detection" >:: test_expr_detection ]

let () = run_test_tt_main suite
