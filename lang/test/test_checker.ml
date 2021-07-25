open OUnit2
open Compiler
open Compiler.Check
open Testing_utils

(* [check] parses a source program and passes it through the checker. *)
let check (source : string) =
  let pgrm = Parser.parse source in
  Check.check pgrm

(* [norm_check_err] normalizes the source locations present in any 
  check errors *)
let norm_check_err (err : Check.check_err) =
  match err with
  | TypeError (e, exp, act) -> TypeError (norm_expr_locs e, exp, act)
  | InvalidTypeError (e, act) -> InvalidTypeError (norm_expr_locs e, act)
  | TypeMismatch (name, op1, ty1, op2, ty2) ->
      TypeMismatch (name, norm_expr_locs op1, ty1, norm_expr_locs op2, ty2)
  | NonVoidMain | ReturnInMain | CtrlReachesEndOfNonVoid _ | MismatchedReturn _
  | UnboundVariable _ | UndefinedFunction _ | NonFunctionAnnotatedAsVoid _
  | ArityMismatch _ | MultipleDefinitions _ | UnrepresentableNumber _ ->
      err

(* [assert_raises_check_err] runs a thunk and checks if it
  raises the indicated CheckError *)
let assert_raises_check_err (err : Check.check_err) (source : string) =
  try check source with
  | Check.CheckError (actual_err, _) ->
      assert_equal (norm_check_err err)
        (norm_check_err actual_err)
        ~printer:Check.string_of_check_err
  | other_err ->
      (* assert something blatantly false so the test fails
         and we'll use this to print a useful message *)
      assert_equal 0 1
        ~msg:
          (Printf.sprintf "Raised non-CheckError exception: %s"
             (Core.Exn.to_string other_err))

let test_ctrl_reaches_end _ =
  assert_raises_check_err (CtrlReachesEndOfNonVoid "f")
    "void main() {} int f() {}";
  assert_raises_check_err (CtrlReachesEndOfNonVoid "g")
    "void main() {} \n\
    \    int g() {\n\
    \      int x = 5;\n\
    \      if (x > 17) {\n\
    \        return 3;\n\
    \      } else {\n\
    \        if (x == 8) {\n\
    \          return 0;\n\
    \        }\n\
    \      }\n\
    \    }"

let test_mismatched_return _ =
  assert_raises_check_err
    (MismatchedReturn ("f", Void))
    "void main() {} void f(int x) { return x; }";
  assert_raises_check_err
    (MismatchedReturn ("h", Int))
    "void main() {} int h() { return; }"

let test_unbound_var _ =
  assert_raises_check_err (UnboundVariable "x") "void main() { x; }";
  assert_raises_check_err (UnboundVariable "n")
    "void main() { if (0) { print(n + 5); } }"

let test_undefined_func _ =
  assert_raises_check_err (UndefinedFunction "f")
    "void main() { f(5, 2); } int g(int n) { return n; }";
  assert_raises_check_err (UndefinedFunction "not_here")
    "void main() { exit(not_here()); }"

let test_type_err _ =
  assert_raises_check_err
    (TypeError (Call ("g", [], None), Int, Void))
    "void main() { f(g()); } void f(int n) {} void g() {}";
  assert_raises_check_err
    (TypeError (Call ("g", [], None), Int, Void))
    "void main() { int x = g(); } void g() {}";
  assert_raises_check_err
    (TypeError (Call ("g", [], None), Int, Void))
    "void main() { int x = 1; x = g(); } void g() {}";
  assert_raises_check_err
    (TypeError (Call ("g", [], None), Int, Void))
    "void main() {} int f() { return g(); } void g() {}";
  assert_raises_check_err
    (TypeError (Call ("g", [], None), Int, Void))
    "void main() { exit(g()); } void g() {}";
  assert_raises_check_err
    (TypeError (Call ("g", [], None), Int, Void))
    "void main() { print(g()); } void g() {}"

let test_invalid_type_err _ =
  assert_raises_check_err
    (InvalidTypeError (Call ("g", [], None), Void))
    "void main() { ~g(); } void g() {}";
  assert_raises_check_err
    (InvalidTypeError (Call ("g", [], None), Void))
    "void main() { g() * h(); } void g(){} void h(){}"

let test_type_mismatch _ =
  assert_raises_check_err
    (TypeMismatch ("addition", Num (2, None), Int, Call ("g", [], None), Void))
    "void main() { 2 + g(); } void g(){}";
  assert_raises_check_err
    (TypeMismatch
       ("bitwise xor", Call ("f", [], None), Void, Num (0x4, None), Int))
    "void main() { f() ^ 0x4; } void f(){}"

let test_non_void_main _ =
  assert_raises_check_err NonVoidMain "int main() { return 1; }"

let test_non_func_void _ =
  assert_raises_check_err (NonFunctionAnnotatedAsVoid "x")
    "void main() { void x = 5; }";
  assert_raises_check_err (NonFunctionAnnotatedAsVoid "n")
    "void main() {} void f(void n) {}"

let test_arity_mismatch _ =
  assert_raises_check_err
    (ArityMismatch ("f", 2, 1))
    "void main() { f(1); } void f(int a, int b) {}";
  assert_raises_check_err
    (ArityMismatch ("f", 0, 3))
    "void main() { f(6, 5, 2); } void f() {}"

let test_mult_defns _ =
  assert_raises_check_err (MultipleDefinitions "f")
    "void main() {} void f(int n) {} int g() { return 1; } void f() {}"

let test_ret_in_main _ =
  assert_raises_check_err ReturnInMain
    "void main() { if (10 > 11) { return 5; } else {} }";
  assert_raises_check_err ReturnInMain "void main() { while (1) { return; } }"

let test_unrepresentable_number _ =
  assert_raises_check_err (UnrepresentableNumber 256)
    "void main() { print(256); }";
  assert_raises_check_err (UnrepresentableNumber (-129))
    "void main() { if (0 < -129) { exit(-1); } }"

let suite =
  "Checker Tests"
  >::: [
         "test_ctrl_reaches_end" >:: test_ctrl_reaches_end;
         "test_mismatched_return" >:: test_mismatched_return;
         "test_unbound_var" >:: test_unbound_var;
         "test_undefined_func" >:: test_undefined_func;
         "test_type_err" >:: test_type_err;
         "test_invalid_type_err" >:: test_invalid_type_err;
         "test_type_mismatch" >:: test_type_mismatch;
         "test_non_void_main" >:: test_non_void_main;
         "test_non_func_void" >:: test_non_func_void;
         "test_arity_mismatch" >:: test_arity_mismatch;
         "test_mult_defns" >:: test_mult_defns;
         "test_ret_in_main" >:: test_ret_in_main;
         "test_unrepresentable_number" >:: test_unrepresentable_number;
       ]

let () = run_test_tt_main suite
