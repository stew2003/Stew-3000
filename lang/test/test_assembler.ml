open OUnit2
open Asm.Assemble
open Asm.Isa

let test_simple_pgrm _ =
  let instrs = [ Mvi (5, A); Dcr A; Hlt ] in
  let bin = bytes_from_list [ 0x49; 0x05; 0x3f; 0x7e ] in
  assert_equal (assemble instrs) bin

let suite = "Suite1" >::: [ "test_simple_pgrm" >:: test_simple_pgrm ]

let () = run_test_tt_main suite
