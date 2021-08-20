open OUnit2
open Compiler
open Compiler.Ast
open Asm.Isa
open Testing_utils

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

(* [runtime_from_src] gets the runtime code generated for a given source program. *)
let runtime_from_src (source : string) : instr list * instr list =
  Runtime.runtime (Parser.parse source)

let test_empty_runtime _ =
  assert_equal ([], [])
    (runtime_from_src "void main() { int x = 5; x++; exit(x + 3); }")

let test_runtime_multiply _ =
  let _, rt =
    runtime_from_src
      "void main() { if (10 * 2 > 5) { print(1); } else { print(0); } }"
  in
  assert_bool "multiply is included" (includes_label rt "runtime_multiply");
  let _, rt =
    runtime_from_src "void main() { while (5 < 10) { int x = 0; x++; } }"
  in
  assert_bool "multiply is not included"
    (not (includes_label rt "runtime_multiply"))

let test_runtime_divide _ =
  let _, rt = runtime_from_src "void main() { exit(5 / 10); }" in
  assert_bool "divide is included" (includes_label rt "runtime_divide");
  let _, rt =
    runtime_from_src "void main() { int x = 4; int y = x + 1; y--; }"
  in
  assert_bool "divide is not included"
    (not (includes_label rt "runtime_divide"))

let test_runtime_assert _ =
  let _, rt =
    runtime_from_src "void main() { if (5 > 10) { assert(1 + 2 == 3); } }"
  in
  assert_bool "assert is included" (includes_label rt "runtime_assert");
  let _, rt = runtime_from_src "void main() { int x = 4; exit(5 * 2 / x); }" in
  assert_bool "assert is not included"
    (not (includes_label rt "runtime_assert"))

let test_runtime_ignore_asserts _ =
  let _, rt =
    Runtime.runtime ~ignore_asserts:true
      (Parser.parse "void main() { assert(100 == 50); exit(0); assert(0); }")
  in
  assert_bool "runtime assert is not included"
    (not (includes_label rt "runtime_assert"))

let test_runtime_print_lcd _ = 
  let rt_init, rt_subroutines = runtime_from_src "
    void main() {
      char s[] = \"string literal\";
      print_lcd(s);
    }
  " in 
  assert_bool "lcd_init is included in init" (includes_label rt_init "runtime_lcd_init");
  assert_bool "print_lcd is included" (includes_label rt_subroutines "runtime_print_lcd")
  [@@ocamlformat "disable"]

let suite =
  "Runtime Tests"
  >::: [
         "test_expr_detection" >:: test_expr_detection;
         "test_empty_runtime" >:: test_empty_runtime;
         "test_runtime_multiply" >:: test_runtime_multiply;
         "test_runtime_divide" >:: test_runtime_divide;
         "test_runtime_assert" >:: test_runtime_assert;
         "test_runtime_ignore_asserts" >:: test_runtime_ignore_asserts;
         "test_runtime_print_lcd" >:: test_runtime_print_lcd;
       ]

let () = run_test_tt_main suite
