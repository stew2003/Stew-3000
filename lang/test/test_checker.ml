open OUnit2
open Compiler
open Compiler.Check
open Testing_utils

(* [check] parses a source program and passes it through the checker. *)
let check (source : string) =
  let pgrm = Parser.parse source in
  let pgrm = Preprocess.preprocess pgrm in
  let pgrm = Desugar.desugar pgrm in
  Check.check pgrm

(* [norm_check_err] normalizes the source locations present in any
   check errors *)
let norm_check_err (err : Check.check_err) =
  match err with
  | TypeError (e, exp, act) -> TypeError (norm_expr_locs e, exp, act)
  | InvalidTypeError (e, act) -> InvalidTypeError (norm_expr_locs e, act)
  | TypeMismatch (name, op1, ty1, op2, ty2) ->
      TypeMismatch (name, norm_expr_locs op1, ty1, norm_expr_locs op2, ty2)
  | InvalidLValue e -> InvalidLValue (norm_expr_locs e)
  | NonVoidMain | ReturnInMain | CtrlReachesEndOfNonVoid _ | MismatchedReturn _
  | UnboundVariable _ | UndefinedFunction _ | NonFunctionAnnotatedAsVoid _
  | ArityMismatch _ | MultipleDefinitions _ | UnrepresentableConstant _
  | DerefVoidPointer | CastToVoid | DerefNonPointer _ | NonConstantArraySize _
  | UnderspecifiedArray _ | TooLargeInitializer _ ->
      err

(* [assert_raises_check_err] runs a thunk and checks if it
   raises the indicated CheckError *)
let assert_raises_check_err (err : Check.check_err) (source : string) =
  let raised =
    try
      check source |> ignore;
      false
    with
    | Check.CheckError (actual_err, _) ->
        assert_equal (norm_check_err err)
          (norm_check_err actual_err)
          ~printer:Check.string_of_check_err;
        true
    | other_err ->
        (* assert something blatantly false so the test fails
           and we'll use this to print a useful message *)
        assert_equal 0 1
          ~msg:
            (Printf.sprintf "Raised non-CheckError exception: %s"
               (Core.Exn.to_string other_err));
        true
  in
  if not raised then assert_equal 0 1 ~msg:"No exception was raised"

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
    "void main() { exit(g()); } void g() {}"

let test_invalid_type_err _ =
  assert_raises_check_err
    (InvalidTypeError (Call ("g", [], None), Void))
    "void main() { ~g(); } void g() {}";
  assert_raises_check_err
    (InvalidTypeError (Call ("g", [], None), Void))
    "void main() { g() * h(); } void g(){} void h(){}";
  assert_raises_check_err
    (InvalidTypeError (Call ("g", [], None), Void))
    "void main() { 2 + g(); } void g(){}";
  assert_raises_check_err
    (InvalidTypeError (Call ("f", [], None), Void))
    "void main() { f() ^ 0x4; } void f(){}";
  assert_raises_check_err
    (InvalidTypeError (Call ("g", [], None), Void))
    "void main() { print(g()); } void g() {}"

let test_type_mismatch _ =
  assert_raises_check_err
    (TypeMismatch ("addition", Var ("c", None), Char, Var ("x", None), Int))
    "void main() { char c; int x; c + x; }";
  assert_raises_check_err
    (TypeMismatch
       ( "modulus",
         Var ("x", None),
         Pointer Unsigned,
         Var ("y", None),
         Pointer (Pointer Char) ))
    "void main() { unsigned *x; char **y; x % y; }"

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

let test_unrepresentable_const _ =
  assert_raises_check_err (UnrepresentableConstant 256)
    "void main() { print(256); }";
  assert_raises_check_err (UnrepresentableConstant (-129))
    "void main() { if (0 < -129) { exit(-1); } }"

let test_deref_non_pointer _ =
  assert_raises_check_err (DerefNonPointer (ConstrainedTo Int))
    "void main() { int nonptr; *nonptr = 10; }";
  assert_raises_check_err (DerefNonPointer Unconstrained)
    "void main() { *5 = 10; }";
  assert_raises_check_err (DerefNonPointer (ConstrainedTo Char))
    "void main() { char **doublep; ***doublep; }";
  assert_raises_check_err (DerefNonPointer (ConstrainedTo Unsigned))
    "void main() { unsigned x; *(x + 10 + 15); }"

let test_deref_void_pointer _ =
  assert_raises_check_err DerefVoidPointer "void main() { void *v; *v = 10; }";
  assert_raises_check_err DerefVoidPointer
    "void main() { void *ptr = 0x0; *ptr; }"

let test_invalid_lvalue _ =
  assert_raises_check_err
    (InvalidLValue (NumLiteral (5, None)))
    "void main() { 5 = 10; }";
  assert_raises_check_err
    (InvalidLValue
       (BinOp (Plus, NumLiteral (2, None), NumLiteral (2, None), None)))
    "void main() { int x = &(2 + 2); }";
  assert_raises_check_err
    (InvalidLValue (NumLiteral (9, None)))
    "void main() { 9++; }"

let test_cast_void _ =
  assert_raises_check_err CastToVoid "void main() { (void)0; }"

let test_non_const_array_size _ =
  assert_raises_check_err (NonConstantArraySize "arr")
    "void main() { int x; int arr[40 + x]; }";
  assert_raises_check_err (NonConstantArraySize "s")
    "void main() { int y; char s[2 * 3 + y] = \"string\"; }"

let test_underspec_array _ =
  assert_raises_check_err (UnderspecifiedArray "bad")
    "void main() { unsigned *bad[]; }"

let test_too_large_init _ =
  assert_raises_check_err
    (TooLargeInitializer ("a", 3, 4))
    "void main() { int a[3] = { 1, 3, 5, 7 }; }";
  assert_raises_check_err
    (TooLargeInitializer ("array", 0, 2))
    "void main() { char **array[0] = { 10, 5 }; }"

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
         "test_unrepresentable_const" >:: test_unrepresentable_const;
         "test_deref_non_pointer" >:: test_deref_non_pointer;
         "test_deref_void_pointer" >:: test_deref_void_pointer;
         "test_invalid_lvalue" >:: test_invalid_lvalue;
         "test_cast_void" >:: test_cast_void;
         "test_non_const_array_size" >:: test_non_const_array_size;
         "test_underspec_array" >:: test_underspec_array;
         "test_too_large_init" >:: test_too_large_init;
       ]

let () = run_test_tt_main suite
