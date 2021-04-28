open OUnit2
open Asm.Assemble
open Asm.Isa

(* [assert_assembles_to] asserts that the given program assembles
  to the given list of bytes *)
let assert_assembles_to (instrs : instr list) (bin : int list) =
  let bin_as_bytes = bytes_from_list bin in
  assert_equal (assemble instrs) bin_as_bytes

let test_simple_pgrm _ =
  assert_assembles_to [ Mvi (5, A); Dcr A; Hlt ] [ 0x49; 0x05; 0x3f; 0x7e ]

let test_longer_pgrm _ =
  assert_assembles_to
    [
      Nop;
      Nop;
      Label "loop";
      Mvi (10, A);
      Sts (A, 1);
      Lds (1, B);
      Out B;
      Jmp "loop";
    ]
    [ 0x7f; 0x7f; 0x49; 10; 0x61; 1; 0x5f; 1; 0x7a; 0x70; 2 ]

let test_invalid_instr _ =
  assert_raises
    (AssembleError (InvalidInstr (Mvi (5, SP))))
    (fun _ -> assemble [ Add (A, B); Subi (1, B); Mvi (5, SP) ]);
  assert_raises
    (AssembleError (InvalidInstr (Cmpi (Reg B, Reg A))))
    (fun _ -> assemble [ Cmpi (Reg B, Reg A) ])

let test_dup_label _ =
  assert_raises (AssembleError (DuplicateLabel "dup")) (fun _ ->
      assemble [ Label "dup"; Label "dup" ])

let test_invalid_imm _ =
  assert_raises (AssembleError (InvalidImm 500)) (fun _ ->
      assemble [ Mvi (500, A) ]);
  assert_raises (AssembleError (InvalidImm (-129))) (fun _ ->
      assemble [ Addi (-129, C) ]);
  assert_raises (AssembleError (InvalidImm 128)) (fun _ ->
      assemble [ Subi (128, B) ])

let test_invalid_stack_off _ =
  assert_raises (AssembleError (InvalidStackOffset (-1))) (fun _ ->
      assemble [ Lds (-1, C) ]);
  assert_raises (AssembleError (InvalidStackOffset 256)) (fun _ ->
      assemble [ Sts (A, 256) ])

let test_invalid_target _ =
  assert_raises (AssembleError (InvalidTarget "nonexistent")) (fun _ ->
      assemble [ Jle "nonexistent" ])

let test_pgrm_too_large _ =
  let big = 257 in
  (* [pgrm] builds a program of size nops *)
  let rec pgrm (size : int) = if size = 0 then [] else Nop :: pgrm (size - 1) in
  assert_raises (AssembleError (ProgramTooLarge big)) (fun _ ->
      assemble (pgrm big))

let suite =
  "Assembler Tests"
  >::: [
         "test_simple_pgrm" >:: test_simple_pgrm;
         "test_longer_pgrm" >:: test_longer_pgrm;
         "test_invalid_instr" >:: test_invalid_instr;
         "test_dup_label" >:: test_dup_label;
         "test_invalid_imm" >:: test_invalid_imm;
         "test_invalid_target" >:: test_invalid_target;
         "test_pgrm_too_large" >:: test_pgrm_too_large;
       ]

let () = run_test_tt_main suite
