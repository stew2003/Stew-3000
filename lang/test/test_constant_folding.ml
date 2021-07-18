open OUnit2
open Compiler.Optimizations.Constant_fold
open Testing_utils

let assert_const_folds_to (before : string) (after : string) =
  assert_prog_eq (parse_norm after) (constant_fold (parse_norm before))

let main_from_body (body : string) : string =
  Printf.sprintf "void main() { %s }" body

let assert_body_const_folds_to (before_body : string) (after_body : string) =
  assert_const_folds_to (main_from_body before_body) (main_from_body after_body)

let test_addition _ =
  assert_body_const_folds_to "10 + 20;" "30;";
  assert_body_const_folds_to "5 + 0 + -8 + 11;" "8;"

let test_subtraction _ =
  assert_body_const_folds_to "100 - 99;" "1;";
  assert_body_const_folds_to "10 - 5 - 3;" "2;"

let test_multiply _ =
  assert_body_const_folds_to "25 * 4;" "100;";
  assert_body_const_folds_to "-2 * 3 * 4;" "-24;"

let test_divide _ =
  assert_body_const_folds_to "120 / 60;" "2;";
  assert_body_const_folds_to "88 / 11 / 4 / 2;" "1;"

let test_mod _ =
  assert_body_const_folds_to "10 % 4;" "2;";
  assert_body_const_folds_to "100 % 67 % 11;" "0;"

let test_band _ =
  assert_body_const_folds_to "0b0101 & 0b1011;" "0b0001;";
  assert_body_const_folds_to "0b1110 & 0b0011;" "0b0010;"

let test_bor _ =
  assert_body_const_folds_to "0b0101 | 0b1011;" "0b1111;";
  assert_body_const_folds_to "0b0000 | 0b0011;" "0b0011;"

let test_bxor _ =
  assert_body_const_folds_to "0b0101 ^ 0b1011;" "0b1110;";
  assert_body_const_folds_to "0b1101 ^ 0b0001;" "0b1100;"

let test_gt _ =
  assert_body_const_folds_to "100 > 4;" "1;";
  assert_body_const_folds_to "-18 > 10;" "0;"

let test_gte _ =
  assert_body_const_folds_to "17 >= 17;" "1;";
  assert_body_const_folds_to "111 >= 24;" "1;";
  assert_body_const_folds_to "99 >= 110;" "0;"

let test_lt _ =
  assert_body_const_folds_to "78 < 100;" "1;";
  assert_body_const_folds_to "0 < -15;" "0;"

let test_lte _ =
  assert_body_const_folds_to "68 <= 141;" "1;";
  assert_body_const_folds_to "11 <= 11;" "1;";
  assert_body_const_folds_to "101 <= 17;" "0;"

let test_eq _ =
  assert_body_const_folds_to "4 == 4;" "1;";
  assert_body_const_folds_to "7 == 41;" "0;"

let test_neq _ =
  assert_body_const_folds_to "16 != 18;" "1;";
  assert_body_const_folds_to "100 != 100;" "0;"

let test_land _ =
  assert_body_const_folds_to "15 && 100;" "100;";
  assert_body_const_folds_to "0 && 41;" "0;"

let test_lor _ =
  assert_body_const_folds_to "99 || 2;" "99;";
  assert_body_const_folds_to "0 || 17;" "17;"

let test_lnot _ =
  assert_body_const_folds_to "!100;" "0;";
  assert_body_const_folds_to "!0;" "1;"

let test_if _ =
  assert_body_const_folds_to "if (1) { print(50); }" "{ print(50); }";
  assert_body_const_folds_to "if (0) { exit(1 + 2); }" "{}"

let test_if_else _ =
  assert_body_const_folds_to "if (1) { 5; } else { 10; }" "{ 5; }";
  assert_body_const_folds_to "if (0) { 5; } else { 10; }" "{ 10; }"

let test_while _ =
  assert_body_const_folds_to "while (0) { print(100); }" "{}";
  assert_body_const_folds_to "while (5 + 10) { print(100); }"
    "while (15) { print(100); }"

let test_does_not_fold_vars _ =
  assert_body_const_folds_to "int x = 15; 1 + 2 + x;" "int x = 15; 3 + x;"

let suite =
  "Constant folding Tests"
  >::: [
         "test_addition" >:: test_addition;
         "test_subtraction" >:: test_subtraction;
         "test_multiply" >:: test_multiply;
         "test_divide" >:: test_divide;
         "test_mod" >:: test_mod;
         "test_band" >:: test_band;
         "test_bor" >:: test_bor;
         "test_bxor" >:: test_bxor;
         "test_gt" >:: test_gt;
         "test_gte" >:: test_gte;
         "test_lt" >:: test_lt;
         "test_lte" >:: test_lte;
         "test_eq" >:: test_eq;
         "test_neq" >:: test_neq;
         "test_land" >:: test_land;
         "test_lor" >:: test_lor;
         "test_lnot" >:: test_lnot;
         "test_if" >:: test_if;
         "test_if_else" >:: test_if_else;
         "test_while" >:: test_while;
         "test_does_not_fold_vars" >:: test_does_not_fold_vars;
       ]

let () = run_test_tt_main suite
