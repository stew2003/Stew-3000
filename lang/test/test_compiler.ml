open OUnit2
open Compiler
open Emulator__Machine

(* [run] parses a source program, checks it, compiles it, and runs
  the generated instructions in the emulator, returning
  the final machine state. *)
let run (source : string) : stew_3000 =
  let pgrm = Parser.parse source in
  Check.check pgrm;
  let instrs = Compile.compile pgrm in
  Emulator.emulate instrs

(* [main_from_body] constructs a source string for a main function
  given the contents of its body. *)
let main_from_body (body : string) : string =
  Printf.sprintf "void main() { %s }" body

(* [run_body] generates a program with main() containing the given 
  body and runs it in the emulator. *)
let run_body (body : string) : stew_3000 = run (main_from_body body)

(* [assert_a] asserts that after running a program with 
  the given body, the machine's a register will equal a given value. *)
let assert_a (body : string) (a : int) =
  let machine = run_body body in
  assert_equal a machine.a ~printer:string_of_int

(* [assert_dec] asserts that after running a program with
  the given body, the machine's decimal display history
  will equal the given list. *)
let assert_dec (body : string) (dec_history : int list) =
  let machine = run_body body in
  assert_equal dec_history machine.dec_disp_history
    ~printer:string_of_dec_display

let test_expr_stmt _ =
  assert_a "7;" 7;
  assert_a "0;" 0;
  assert_a "-128;" (-128)

let test_let _ =
  assert_a "int x = 17; x;" 17;
  assert_a "int a = 1; int b = a; int c = b; c;" 1

let test_if _ =
  assert_a "100; if (1) { 17; }" 17;
  assert_a "int x = 100; if (0) { x = 17; } x;" 100

let test_if_else _ =
  assert_a "if (1) { 50; } else { 100; }" 50;
  assert_a "if (5 == 17) { 4; } else { 6; }" 6

let test_block _ =
  assert_a "{ { { 90; } 80; } 70; }" 70;
  assert_a "10; {} {}" 10

let test_inr _ =
  assert_a "int x = 50; x++; x++; x;" 52;
  assert_a "int y = -16; y++; y;" (-15)

let test_dcr _ =
  assert_a "int a = 71; a--; a;" 70;
  assert_a "int b = -100; b--; b--; b;" (-102)

let test_exit _ =
  assert_dec "exit(15); assert(0);" [ 15 ];
  assert_dec "exit(); assert(0);" []

let test_print_dec _ = assert_dec "print(1); print(3); print(5);" [ 1; 3; 5 ]

let test_assert _ =
  assert_dec "assert(5 == 5); assert(1); assert(-1); exit(0);" [ 0 ];
  assert_dec "assert(2 == 2); assert(3 == 1); assert(4 == 4); exit(0);" [ -1 ]

let test_while _ =
  assert_dec "int x = 0; while (x < 5) { print(x); x++; }" [ 0; 1; 2; 3; 4 ]

let test_nums _ =
  (* decimal literals *)
  assert_a "41;" 41;
  assert_a "-120;" (-120);
  (* hex literals *)
  assert_a "0xa;" 0xa;
  assert_a "-0x04;" (-0x04);
  (* binary literals *)
  assert_a "0b10110;" 0b10110;
  assert_a "-0b110101;" (-0b110101)

let test_unops _ =
  (* bitwise not *)
  assert_a "~0b11111111;" 0b00000000;
  assert_a "~0b10010111;" 0b01101000

let test_binops _ =
  (* addition *)
  assert_a "2 + 3;" 5;
  assert_a "5 + 10 + 15;" 30;

  (* subtraction *)
  assert_a "4 - 7;" (-3);
  assert_a "10 - 9 - 3;" (-2);

  (* multiplication *)
  assert_a "7 * 6;" 42;
  assert_a "-10 * 5 * 2;" (-100);

  (* division *)
  assert_a "100 / 25;" 4;
  assert_a "17 / -6;" (-2);

  (* modulus *)
  assert_a "40 % 10;" 0;
  assert_a "21 % 50;" 21;
  assert_a "10 % 4;" 2;

  (* bitwise and *)
  assert_a "0b1100 & 0b0101;" 0b0100;
  assert_a "0b11110010 & 0b11;" 0b10;

  (* bitwise or *)
  assert_a "0b1101 | 0b1010;" 0b1111;
  assert_a "0b11110000 | 0b00101101;" 0b11111101;

  (* bitwise xor *)
  assert_a "0b1011 ^ 0b1110;" 0b0101;
  assert_a "0b1111 ^ 0b0000;" 0b1111;

  (* greater than *)
  assert_a "50 > 32;" 1;
  assert_a "-9 > 9;" 0;
  assert_a "14 > 14;" 0;

  (* less than *)
  assert_a "100 < 120;" 1;
  assert_a "16 < -2;" 0;
  assert_a "88 < 88;" 0;

  (* greater than or eq *)
  assert_a "14 >= 12;" 1;
  assert_a "-4 >= 0;" 0;
  assert_a "79 >= 79;" 1;

  (* less than or eq *)
  assert_a "7 <= 21;" 1;
  assert_a "-44 <= -127;" 0;
  assert_a "-9 <= -9;" 1;

  (* equal *)
  assert_a "45 == 45;" 1;
  assert_a "-100 == 18;" 0;

  (* not equal *)
  assert_a "16 != 90;" 1;
  assert_a "47 != 47;" 0

let test_log_ops _ =
  (* logical and *)
  assert_a "10 && 2;" 2;
  assert_a "0 && 17;" 0;
  assert_a "1 && 2 && 0 && 3;" 0;
  assert_a "-1 && 18 && 44 && 11;" 11;

  (* logical or *)
  assert_a "18 || 15;" 18;
  assert_a "100 || 0;" 100;
  assert_a "0 || 0 || 3 || 0;" 3;
  assert_a "0 || 0 || 0;" 0;

  (* logical not *)
  assert_a "!17;" 0;
  assert_a "!0;" 1

let test_functions _ =
  let machine =
    run
      "\n\
      \    int five() { return 5; }\n\
      \    void main() {\n\
      \      five();\n\
      \    }\n\
      \  "
  in
  assert_equal 5 machine.a;
  let machine =
    run
      "\n\
      \    void print_double(int n) { print(2 * n); }\n\
      \    void main() {\n\
      \      print_double(7);\n\
      \      print_double(10);\n\
      \      print_double(-16);\n\
      \    }\n\
      \  "
  in
  assert_equal [ 14; 20; -32 ] machine.dec_disp_history;
  let machine =
    run
      "\n\
      \    int sum(int a, int b, int c) {\n\
      \      return a + b + c;\n\
      \    }\n\
      \    void main() {\n\
      \      sum(4, 5, 6) + sum(-3, -2, -1);\n\
      \    }\n\
      \  "
  in
  assert_equal 9 machine.a

let suite =
  "Compiler Tests"
  >::: [
         "test_expr_stmt" >:: test_expr_stmt;
         "test_let" >:: test_let;
         "test_if" >:: test_if;
         "test_if_else" >:: test_if_else;
         "test_block" >:: test_block;
         "test_inr" >:: test_inr;
         "test_dcr" >:: test_dcr;
         "test_exit" >:: test_exit;
         "test_print_dec" >:: test_print_dec;
         "test_assert" >:: test_assert;
         "test_while" >:: test_while;
         "test_nums" >:: test_nums;
         "test_unops" >:: test_unops;
         "test_binops" >:: test_binops;
         "test_log_ops" >:: test_log_ops;
         "test_functions" >:: test_functions;
       ]

let () = run_test_tt_main suite
