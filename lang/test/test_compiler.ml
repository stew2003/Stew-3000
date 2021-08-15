open OUnit2
open Compiler
open Emulator__Machine
open Asm.Isa

(* [run] parses a source program, checks it, compiles it, and runs
   the generated instructions in the emulator, returning
   the final machine state. *)
let run (source : string) : stew_3000 =
  let pgrm = Parser.parse source in
  let pgrm = Preprocess.preprocess pgrm in
  let pgrm = Desugar.desugar pgrm in
  let pgrm = Check.check pgrm in
  let instrs = Compile.compile pgrm in
  Emulator.emulate instrs

(* [run_body] generates a program with main() containing the given
   body and runs it in the emulator. *)
let run_body (body : string) : stew_3000 =
  run (Printf.sprintf "void main() { %s }" body)

(* [assert_expr] asserts that a given expression evaluates to
   a given value. *)
let assert_expr (expr : string) (value : int) =
  let machine = run_body (Printf.sprintf "print(%s);" expr) in
  assert_equal [ value ] machine.dec_disp_history ~printer:string_of_dec_display

(* [assert_dec] asserts that after running a program with
   the given body, the machine's decimal display history
   will equal the given list. *)
let assert_dec (body : string) (dec_history : int list) =
  let machine = run_body body in
  assert_equal dec_history machine.dec_disp_history
    ~printer:string_of_dec_display

let test_let _ =
  assert_dec "int x = 17; print(x);" [ 17 ];
  assert_dec "int a = 1; int b = a; int c = b; print(c);" [ 1 ]

let test_if _ =
  assert_dec "if (1) { print(17); }" [ 17 ];
  assert_dec "int x = 100; if (0) { x = 17; } print(x);" [ 100 ]

let test_if_else _ =
  assert_dec "if (1) { print(50); } else { print(100); }" [ 50 ];
  assert_dec "if (5 == 17) { print(4); } else { print(6); }" [ 6 ]

let test_block _ =
  assert_dec "{ { { print(90); } print(80); } print(70); }" [ 90; 80; 70 ];
  assert_dec "{} print(10); {} {}" [ 10 ]

let test_prefix_inr _ =
  assert_dec "int x = 14; print(++x);" [ 15 ];
  assert_dec "int x = 0; int *px = &x; print(++(*px)); print(x);" [ 1; 1 ]

let test_postfix_inr _ =
  assert_dec "int x = 50; x++; x++; print(x);" [ 52 ];
  assert_dec "int y = -16; y++; print(y);" [ -15 ];
  assert_dec "int x = 5; int *px = &x; *(px)++; print(x);" [ 6 ];
  assert_dec "int x = 14; print(x++);" [ 14 ]

let test_prefix_dcr _ =
  assert_dec "int x = -19; print(--x);" [ -20 ];
  assert_dec "int x = 120; int *px = &x; print(--(*px)); print(x);" [ 119; 119 ]

let test_postfix_dcr _ =
  assert_dec "int a = 71; a--; print(a);" [ 70 ];
  assert_dec "int b = -100; b--; b--; print(b);" [ -102 ];
  assert_dec "int x = 14; int *px = &x; *(px)--; print(x);" [ 13 ];
  assert_dec "int x = 71; print(x--);" [ 71 ]

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
  assert_expr "41" 41;
  assert_expr "-120" (-120);
  (* hex literals *)
  assert_expr "0xa" 0xa;
  assert_expr "-0x04" (-0x04);
  (* binary literals *)
  assert_expr "0b10110" 0b10110;
  assert_expr "-0b110101" (-0b110101)

let test_unops _ =
  (* bitwise not *)
  assert_expr "~0b00001011" 0b11110100;
  assert_expr "~0b00000110" 0b011111001

let test_binops _ =
  (* addition *)
  assert_expr "2 + 3" 5;
  assert_expr "5 + 10 + 15" 30;

  (* subtraction *)
  assert_expr "4 - 7" (-3);
  assert_expr "10 - 9 - 3" (-2);

  (* multiplication *)
  assert_expr "7 * 6" 42;
  assert_expr "-10 * 5 * 2" (-100);

  (* division *)
  assert_expr "100 / 25" 4;
  assert_expr "17 / -6" (-2);

  (* modulus *)
  assert_expr "40 % 10" 0;
  assert_expr "21 % 50" 21;
  assert_expr "10 % 4" 2;

  (* bitwise and *)
  assert_expr "0b1100 & 0b0101" 0b0100;
  assert_expr "0b0110 & 0b11" 0b10;

  (* bitwise or *)
  assert_expr "0b1101 | 0b1010" 0b1111;
  assert_expr "0b00001111 | 0b00101101" 0b00101111;

  (* bitwise xor *)
  assert_expr "0b1011 ^ 0b1110" 0b0101;
  assert_expr "0b1111 ^ 0b0000" 0b1111;

  (* greater than *)
  assert_expr "50 > 32" 1;
  assert_expr "-9 > 9" 0;
  assert_expr "14 > 14" 0;

  (* less than *)
  assert_expr "100 < 120" 1;
  assert_expr "16 < -2" 0;
  assert_expr "88 < 88" 0;

  (* greater than or eq *)
  assert_expr "14 >= 12" 1;
  assert_expr "-4 >= 0" 0;
  assert_expr "79 >= 79" 1;

  (* less than or eq *)
  assert_expr "7 <= 21" 1;
  assert_expr "-44 <= -127" 0;
  assert_expr "-9 <= -9" 1;

  (* equal *)
  assert_expr "45 == 45" 1;
  assert_expr "-100 == 18" 0;

  (* not equal *)
  assert_expr "16 != 90" 1;
  assert_expr "47 != 47" 0

let test_log_ops _ =
  (* logical and *)
  assert_expr "10 && 2" 2;
  assert_expr "0 && 17" 0;
  assert_expr "1 && 2 && 0 && 3" 0;
  assert_expr "-1 && 18 && 44 && 11" 11;

  (* logical or *)
  assert_expr "18 || 15" 18;
  assert_expr "100 || 0" 100;
  assert_expr "0 || 0 || 3 || 0" 3;
  assert_expr "0 || 0 || 0" 0;

  (* logical not *)
  assert_expr "!17" 0;
  assert_expr "!0" 1

let test_functions _ =
  let machine =
    run
      "
      int five() { return 5; }
      void main() {
        five();
      }"
  in
  assert_equal 5 machine.a; 
  let machine =
    run
      "
      void print_double(int n) { print(2 * n); }
      void main() {
        print_double(7);
        print_double(10);
        print_double(-16);
      }"
  in
  assert_equal [ 14; 20; -32 ] machine.dec_disp_history;
  let machine =
    run "
      int sum(int a, int b, int c) {
        return a + b + c;
      }
      void main() {
        sum(4, 5, 6) + sum(-3, -2, -1);
      }"
  in
  assert_equal 9 machine.a [@@ocamlformat "disable"]

let test_ignore_asserts _ =
  let compile_with_ignore_asserts (source : string) =
    let pgrm = Parser.parse source in
    let pgrm = Check.check pgrm in
    Compile.compile ~ignore_asserts:true pgrm
  in
  assert_equal [ Hlt None ]
    (compile_with_ignore_asserts "void main() { assert(0); }");
  assert_equal [ Hlt None ]
    (compile_with_ignore_asserts
       "void main() { assert(1 == 2); assert(1); assert(41 - 1 == 40); }")

let test_char_literals  _ = 
  assert_dec "
      char a = 'A';
      print((int)a);" [65];
  assert_dec "
      char c = '[';
      if (c + 2 == ']') {
        print(1);
      } else {
        print(0);
      }" [1]
  [@@ocamlformat "disable"]

let test_deref _ = 
  assert_dec "
    int x = 41;
    int *px = &x;
    print(*px);
  " [41];
  assert_dec "
    int x = -3;
    int *px = &x;
    int **ppx = &px;
    print(**ppx);
  " [-3]; 
  assert_dec "
    *(unsigned*)(1 + 2) = 255;
    print(*(int*)3);
  " [255]
  [@@ocamlformat "disable"]

let test_addr_of _ = 
  assert_dec "
    int x;
    print(&x);
  " [1];
  assert_dec "
    int x;
    int *px;
    print(&*px == &x);
    print(&*(px + 1) == &x + 1);
  " [1; 1]
  [@@ocamlformat "disable"]

let test_assign _ = 
  assert_dec "
    int a = 10;
    *(&a) = 41;
    print(a);
  " [41];
  assert_dec "
    char c = 'v';
    c = 'z';
    c = 'B';
    print((int)c);
  " [66]
  [@@ocamlformat "disable"]

let test_array_decl _ = 
  assert_dec "
    int arr[5] = { 2, 4, 6, 8, 10 };
    print(*(arr + 0));
    print(*(arr + 1));
    print(*(arr + 2));
    print(*(arr + 3));
    print(*(arr + 4));
  " [2; 4; 6; 8; 10];
  assert_dec "
    char str[] = \"hello\";
    print((int)*(arr + 0));
    print((int)*(arr + 1));
    print((int)*(arr + 2));
    print((int)*(arr + 3));
    print((int)*(arr + 4));
    print((int)*(arr + 5));
  " [104; 101; 108; 108; 111; 0;]
  [@@ocamlformat "disable"]

let suite =
  "Compiler Tests"
  >::: [
         "test_let" >:: test_let;
         "test_if" >:: test_if;
         "test_if_else" >:: test_if_else;
         "test_block" >:: test_block;
         "test_prefix_inr" >:: test_prefix_inr;
         "test_postfix_inr" >:: test_postfix_inr;
         "test_prefix_dcr" >:: test_prefix_dcr;
         "test_postfix_dcr" >:: test_postfix_dcr;
         "test_exit" >:: test_exit;
         "test_print_dec" >:: test_print_dec;
         "test_assert" >:: test_assert;
         "test_while" >:: test_while;
         "test_nums" >:: test_nums;
         "test_unops" >:: test_unops;
         "test_binops" >:: test_binops;
         "test_log_ops" >:: test_log_ops;
         "test_functions" >:: test_functions;
         "test_ignore_asserts" >:: test_ignore_asserts;
         "test_char_literals" >:: test_char_literals;
         "test_deref" >:: test_deref;
         "test_assign" >:: test_assign;
         "test_array_decl" >:: test_array_decl;
       ]

let () = run_test_tt_main suite
