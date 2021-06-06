open OUnit2
open Compiler
open Compiler.Ast
open Util.Srcloc

let uses_mult (pgrm : prog) : bool =
  Runtime.check_program pgrm (function
    | BinOp (Mult, _, _, _) -> true
    | _ -> false)

let uses_div (pgrm : prog) : bool =
  Runtime.check_program pgrm (function
    | BinOp (Div, _, _, _) -> true
    | _ -> false)

let pgrm_from_body (body : string) : prog =
  Parser.parse (Printf.sprintf "void main() { %s }" body)

let test_expr_detection _ =
  assert_equal true (uses_mult (pgrm_from_body "2 * 3;"));
  assert_equal false (uses_mult (pgrm_from_body "1 + 2 / 7;"));
  assert_equal true (uses_div (pgrm_from_body "4 / 10;"));
  assert_equal false (uses_div (pgrm_from_body "f(4, 7 * 12);"));
  assert_equal true
    (uses_mult
       (Parser.parse
          "int func(int n, int m) { if (n == 0) { return n * m; } return 1; } \
           void main() {}"))

let suite =
  "Runtime Tests" >::: [ "test_expr_detection" >:: test_expr_detection ]

let () = run_test_tt_main suite
