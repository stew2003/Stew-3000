open OUnit2
open Compiler.Optimizations.No_effect
open Testing_utils

(* [assert_eliminates_to] asserts that the given program equals an expected
    program after performing unused var/no effect stmt elimination *)
let assert_eliminates_to (before : string) (after : string) =
  assert_prog_eq (parse_norm after)
    (eliminate_unused_vars_and_no_effects (parse_norm before))

let main_from_body (body : string) : string =
  Printf.sprintf "void main() { %s }" body

let assert_body_eliminates_to (before_body : string) (after_body : string) =
  assert_eliminates_to (main_from_body before_body) (main_from_body after_body)

let test_simple _ =
  assert_body_eliminates_to "int x;" "";
  assert_body_eliminates_to "int y; int x; int z;" "";
  assert_body_eliminates_to "char s[] = { 'a', 'b', 'c' };" ""

let test_identifies_effect_stmts _ =
  assert_body_eliminates_to "print(10);" "print(10);";
  assert_body_eliminates_to "exit(-1);" "exit(-1);";
  assert_body_eliminates_to "while (1) {}" "while (1) {}";
  assert_body_eliminates_to "char *s; print_lcd(s);" "char *s; print_lcd(s);";
  assert_body_eliminates_to "assert(17);" "assert(17);";
  assert_body_eliminates_to "int x; x++; exit(x);" "int x; x++; exit(x);";
  assert_body_eliminates_to "int y; y--; print(y);" "int y; y--; print(y);"

let test_identifies_no_effect_stmts _ =
  assert_body_eliminates_to "4 * 10; 5 == 6;" "";
  assert_body_eliminates_to "int unused = 40 + 20; unused * 12; unused;" "";
  assert_body_eliminates_to "int used = 7; used + 14; assert(used == 7);"
    "int used = 7; assert(used == 7);"

let test_function_calls_have_effects _ =
  let pgrm = "void f() {} void main() { f(); }" in
  assert_eliminates_to pgrm pgrm

let test_does_not_loop_forever _ =
  let pgrm = "void recur() { recur(); } void main() { recur(); }" in
  assert_eliminates_to pgrm pgrm

let test_iteratively_eliminated _ = 
  (* Without iterative application of these optimizations, it's unlikely all
    of this would get eliminated. *)
  assert_body_eliminates_to "
    int v1;
    int v2 = v1;
    int v3 = v2 + v1;
    int v4 = v3;
    int v5 = v4;
    int v6 = v1 * v2 * v3 * v4 * v5;
  " "" [@@ocamlformat "disable"]

let test_preserves_initializers_with_effects _ =
  assert_eliminates_to "
    void f() {} 
    void main() { 
      int x = (int)f(); 
    }
  " 
  " void f() {}
    void main() {
      (int)f();
    }
  "; 
  assert_body_eliminates_to 
    "int x; int y = x++; assert(x);"
    "int x; x++; assert(x);";
  assert_eliminates_to "
    char f() { return 'c'; }
    void main() {
      char s[] = { 'a', 'b', f() };
    }
  " "
    char f() { return 'c'; }
    void main() {
      f();
    }
  "
  [@@ocamlformat "disable"]

let suite =
  "Unused variable and no-effect statement elimination Tests"
  >::: [
         "test_simple" >:: test_simple;
         "test_identifies_effect_stmts" >:: test_identifies_effect_stmts;
         "test_identifies_no_effect_stmts" >:: test_identifies_no_effect_stmts;
         "test_function_calls_have_effects" >:: test_function_calls_have_effects;
         "test_does_not_loop_forever" >:: test_does_not_loop_forever;
         "test_iteratively_eliminated" >:: test_iteratively_eliminated;
         "test_preserves_initializers_with_effects"
         >:: test_preserves_initializers_with_effects;
       ]

let () = run_test_tt_main suite
