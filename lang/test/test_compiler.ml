open OUnit2
open Compiler.Compile
open Compiler.Ast
open Emulator
open Emulator__Machine
open Util.Srcloc

let dummy_src_loc = loc 0 0

let main_from_body (body : stmt list) : func_defn =
  { name = "main"; params = []; body; return_ty = Void; loc = dummy_src_loc }

let compile_and_run (pgrm : prog) : stew_3000 =
  let instrs = compile pgrm in
  emulate instrs 0 false

let test_simple_pgrm _ =
  let machine =
    compile_and_run
      {
        funcs = [];
        main =
          main_from_body [ ExprStmt (Num (7, dummy_src_loc), dummy_src_loc) ];
      }
  in
  assert_equal 7 machine.a

let suite = "Compiler Tests" >::: [ "test_simple_pgrm" >:: test_simple_pgrm ]

let () = run_test_tt_main suite
