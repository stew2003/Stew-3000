open OUnit2
open Compiler.Optimizations.Peephole
open Asm.Isa

let test_empty_no_change _ = assert_equal [] (peephole_optimize [])

let test_eliminates_nops _ =
  assert_equal
    [ Mvi (10, A, None); Addi (20, A, None); Hlt None ]
    (peephole_optimize
       [
         Mvi (10, A, None);
         Addi (0, A, None);
         Nop None;
         Addi (20, A, None);
         Hlt None;
       ]);
  assert_equal
    [ St (A, B, None); And (B, C, None) ]
    (peephole_optimize
       [
         St (A, B, None); Subi (0, B, None); And (B, C, None); Ani (-1, C, None);
       ]);
  assert_equal [] (peephole_optimize [ Nop None; Nop None; Nop None ]);
  assert_equal [] (peephole_optimize [ Inr (A, None); Dcr (A, None) ]);
  assert_equal
    [ Sts (C, 17, None) ]
    (peephole_optimize [ Sts (C, 17, None); Lds (17, C, None) ]);
  let cannot_improve = [ Sts (A, 10, None); Lds (10, B, None) ] in
  assert_equal cannot_improve (peephole_optimize cannot_improve)

let test_reduces_instr_size _ =
  assert_equal [ Dcr (B, None) ] (peephole_optimize [ Subi (1, B, None) ]);
  assert_equal [ Inr (C, None) ] (peephole_optimize [ Addi (1, C, None) ])

let suite =
  "Peephole optimization Tests"
  >::: [
         "test_empty_no_change" >:: test_empty_no_change;
         "test_eliminates_nops" >:: test_eliminates_nops;
         "test_reduces_instr_size" >:: test_reduces_instr_size;
       ]

let () = run_test_tt_main suite
