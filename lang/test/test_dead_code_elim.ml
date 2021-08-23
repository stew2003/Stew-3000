open OUnit2
open Compiler.Optimizations.Dead_code_elimination
open Compiler
open Testing_utils

let assert_eliminates_to (before : string) (after : string) =
  (* NOTE: this runs everything through the checker to ensure
     the ctrl_reaches_end field is set. *)
  assert_prog_eq
    (Check.check (parse_norm after))
    (eliminate_dead_code (Check.check (parse_norm before)))

let main_from_body (body : string) : string =
  Printf.sprintf "void main() { %s }" body

let assert_body_eliminates_to (before_body : string) (after_body : string) =
  assert_eliminates_to (main_from_body before_body) (main_from_body after_body)

let test_if _ =
  assert_body_eliminates_to "if (1) { print(50); }" "{ print(50); }";
  assert_body_eliminates_to "if (0) { exit(1 + 2); }" ""

let test_if_else _ =
  assert_body_eliminates_to "if (1) { 5; } else { 10; }" "{ 5; }";
  assert_body_eliminates_to "if (0) { 5; } else { 10; }" "{ 10; }"

let test_while _ =
  assert_body_eliminates_to "while (0) { print(100); }" "";
  assert_prog_eq
    {
      defines = [];
      funcs = [];
      main =
        {
          name = "main";
          params = [];
          body = [ Loop ([ PrintDec (NumLiteral (100, None), None) ], None) ];
          return_ty = Void;
          ctrl_reaches_end = Some false;
          loc = None;
        };
    }
    (eliminate_dead_code
       (parse_norm (main_from_body "while (15) { print(100); }")))

let test_elim_unused_functions _ = 
  assert_eliminates_to "
    int f(int a, int b) {
      return a + b;
    }
    void g() {}
    void main() {
      int x;
      if (x > 10) {
        print(0);
      }
    }
  " "
    void main() {
      int x;
      if (x > 10) {
        print(0);
      }
    }
  ";
  assert_eliminates_to "
    void fun(int x, char c) {
      if (x) {
        print((int)c);
      }
    }
    void main() {
      if (0) {
        // dead code
        fun(2, 'z');
      } else {
        // no call to fun
      }
    }
  " "
    void main() {}
  " [@@ocamlformat "disable"]

let suite =
  "Dead code elimination Tests"
  >::: [
         "test_if" >:: test_if;
         "test_if_else" >:: test_if_else;
         "test_while" >:: test_while;
         "test_elim_unused_functions" >:: test_elim_unused_functions;
       ]

let () = run_test_tt_main suite
