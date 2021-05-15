open OUnit2
open Compiler.Compile
open Compiler.Ast
open Asm.Isa
open Emulator
open Util.Srcloc

let dummy_src_loc = loc 0 0

let compile_and_run (pgrm : prog) : stew_3000 =
  let instrs = compile pgrm in
  emulate (List.map (fun ins -> (ins, dummy_src_loc)) instrs) 0

let test_simple_pgrm _ =
  let machine = compile_and_run { funcs = []; main = [ ExprStmt (Num 7) ] } in
  assert_equal 7 machine.a

let suite = "Compiler Tests" >::: [ "test_simple_pgrm" >:: test_simple_pgrm ]

let () = run_test_tt_main suite
