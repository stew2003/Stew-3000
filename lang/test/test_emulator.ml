open OUnit2
open Asm.Isa
open Emulator
open Emulator__Machine
open Asm.Assemble

(* wrapper around emulate, with no logging *)
let run_emulator (pgrm : instr list) = emulate pgrm 0

let assert_int_eq (exp : int) (act : int) =
  assert_equal exp act ~printer:string_of_int

let string_of_int_list (lst : int list) =
  "[" ^ (List.map string_of_int lst |> String.concat "; ") ^ "]"

let assert_int_list_eq (exp : int list) (act : int list) =
  assert_equal exp act ~printer:string_of_int_list

let test_simple_pgrm _ =
  let machine =
    run_emulator [ Mvi (10, A, None); Addi (-6, A, None); Hlt None ]
  in
  assert_int_eq machine.a 4

let test_emulates_outs _ =
  let machine =
    run_emulator
      [
        Mvi (2, A, None);
        Mov (A, B, None);
        Inr (B, None);
        Mvi (-17, C, None);
        Out (C, None);
        Out (B, None);
        Out (B, None);
        Out (A, None);
        Hlt None;
      ]
  in
  assert_int_eq 2 machine.a;
  assert_int_eq 3 machine.b;
  assert_int_eq (-17) machine.c;
  assert_int_list_eq [ -17; 3; 3; 2 ] machine.dec_disp_history

let test_loads_stores _ =
  let machine =
    run_emulator
      [
        Mvi (5, A, None);
        Mvi (11, B, None);
        (* RAM[A] = B *)
        St (B, A, None);
        (* C = RAM[A] *)
        Ld (A, C, None);
        Hlt None;
      ]
  in
  assert_int_eq 11 machine.c

let test_stack_offset_access _ =
  let machine =
    run_emulator
      [ Mvi (25, B, None); Sts (B, 15, None); Lds (15, A, None); Hlt None ]
  in
  assert_int_eq 25 machine.a

let test_jmps _ =
  let machine =
    run_emulator
      [
        Label ("entry", None);
        Mvi (2, A, None);
        Jmp ("after", None);
        Label ("skip", None);
        Mvi (100, A, None);
        Label ("after", None);
        Hlt None;
      ]
  in
  assert_int_eq 2 machine.a;
  let machine =
    run_emulator
      [
        Mvi (0, A, None);
        Label ("loop", None);
        Out (A, None);
        Inr (A, None);
        Cmpi (Reg A, Imm 5, None);
        Jl ("loop", None);
        Hlt None;
      ]
  in
  assert_int_list_eq [ 0; 1; 2; 3; 4 ] machine.dec_disp_history;
  assert_int_eq 5 machine.a;
  let machine =
    run_emulator
      [
        Mvi (10, A, None);
        Mvi (9, B, None);
        Cmp (A, B, None);
        Jg ("greater", None);
        Mvi (0, C, None);
        Hlt None;
        Label ("greater", None);
        Mvi (1, C, None);
        Hlt None;
      ]
  in
  assert_int_eq 1 machine.c;
  let machine =
    run_emulator
      [
        Mvi (87, B, None);
        Mov (B, A, None);
        Cmp (A, B, None);
        Je ("equal", None);
        Mvi (0, C, None);
        Hlt None;
        Label ("equal", None);
        Mvi (1, C, None);
        Hlt None;
      ]
  in
  assert_int_eq 1 machine.c

let test_hlt _ =
  let machine =
    run_emulator
      [
        Hlt None;
        Mvi (1, C, None);
        Mvi (2, A, None);
        Add (C, A, None);
        Out (A, None);
      ]
  in
  assert_int_eq 0 machine.a;
  assert_int_eq 0 machine.c

let test_zero_flag _ =
  let machine =
    run_emulator
      [ Mvi (5, A, None); Mov (A, B, None); Sub (A, B, None); Hlt None ]
  in
  assert_bool "zero flag should be set" machine.zflag;
  let machine =
    run_emulator
      [ Mvi (1, A, None); Mvi (-1, B, None); Cmp (B, A, None); Hlt None ]
  in
  assert_bool "zero flag should not be set" (not machine.zflag);
  let machine =
    run_emulator [ Mvi (127, A, None); Cmpi (Reg A, Imm 127, None); Hlt None ]
  in
  assert_bool "zero flag set after comparing 127 with itself" machine.zflag

let test_sign_flag _ =
  let machine =
    run_emulator
      [ Mvi (35, A, None); Mvi (50, B, None); Sub (B, A, None); Hlt None ]
  in
  assert_bool "sign flag should be set" machine.sflag;
  let machine =
    run_emulator
      [ Mvi (20, A, None); Mvi (10, B, None); Sub (B, A, None); Hlt None ]
  in
  assert_bool "sign flag should not be set" (not machine.sflag);
  let machine = run_emulator [ Mvi (0, A, None); Dcr (A, None); Hlt None ] in
  assert_bool "sign flag set after 0--" machine.sflag

let test_overflow_flag _ =
  let machine = run_emulator [ Mvi (127, A, None); Inr (A, None); Hlt None ] in
  assert_bool "overflow flag set after increment 127" machine.oflag;
  let machine =
    run_emulator [ Mvi (-128, B, None); Subi (1, B, None); Hlt None ]
  in
  assert_bool "overflow flag set after cmp -128 - 1" machine.oflag;
  let machine =
    run_emulator
      [ Mvi (-120, A, None); Mvi (100, B, None); Cmp (A, B, None); Hlt None ]
  in
  assert_bool "overflow flag set after -120 - 100" machine.oflag;
  let machine =
    run_emulator [ Mvi (-1, A, None); Subi (127, A, None); Hlt None ]
  in
  assert_bool "overflow flag not set after -1-127" (not machine.oflag)

let test_call_ret _ =
  let machine =
    run_emulator
      [
        Mvi (4, C, None);
        Sts (C, 2, None);
        Mvi (7, C, None);
        Sts (C, 3, None);
        Call ("add", None);
        Out (A, None);
        Hlt None;
        Label ("add", None);
        Lds (1, A, None);
        Lds (2, B, None);
        Add (B, A, None);
        Ret None;
      ]
  in
  assert_int_list_eq [ 11 ] machine.dec_disp_history

let test_dup_label _ =
  assert_raises
    (AssembleError (DuplicateLabel "dup", None))
    (fun _ ->
      run_emulator
        [ Label ("dup", None); Label ("dup", None); Label ("dup", None) ])

let test_invalid_pc_increment _ =
  let machine = new_stew_3000 () in
  (* pc runs right off the end, can't increment from 0x2 to 0x3 *)
  machine.pc <- 2;
  assert_raises
    (EmulatorError (InvalidPCIncrement machine, None))
    (fun _ -> run_emulator [ Nop None; Nop None; Nop None ])

let test_invalid_target _ =
  assert_raises
    (AssembleError (InvalidTarget "not_here", None))
    (fun _ -> run_emulator [ Jmp ("not_here", None) ]);
  assert_raises
    (AssembleError (InvalidTarget "not_a_fun", None))
    (fun _ -> run_emulator [ Call ("not_a_fun", None) ])

let test_invalid_imm _ =
  assert_raises
    (AssembleError (InvalidImm (-129), None))
    (fun _ -> run_emulator [ Addi (-129, B, None) ]);
  assert_raises
    (AssembleError (InvalidImm (-129), None))
    (fun _ -> run_emulator [ Lds (-129, A, None) ]);
  assert_raises
    (AssembleError (InvalidImm 256, None))
    (fun _ -> run_emulator [ Sts (C, 256, None) ])

let test_invalid_instr _ =
  assert_raises
    (AssembleError (InvalidInstr (Sub (B, B, None)), None))
    (fun _ -> run_emulator [ Sub (B, B, None) ]);
  assert_raises
    (AssembleError (InvalidInstr (Cmpi (Imm 1, Imm 2, None)), None))
    (fun _ -> run_emulator [ Cmpi (Imm 1, Imm 2, None) ])

let test_overflow_immediates _ =
  let machine =
    run_emulator
      [
        Mvi (-1, A, None);
        Mvi (101, B, None);
        St (B, A, None);
        Ld (A, C, None);
        Hlt None;
      ]
  in
  assert_equal 101 machine.c;
  assert_equal 101 machine.stack.(255);
  let machine =
    run_emulator
      [
        Mvi (255, A, None);
        Mvi (101, B, None);
        St (B, A, None);
        Ld (A, C, None);
        Hlt None;
      ]
  in
  assert_equal 101 machine.c;
  assert_equal 101 machine.stack.(255)

let suite =
  "Emulator Tests"
  >::: [
         "test_simple_pgrm" >:: test_simple_pgrm;
         "test_emulates_outs" >:: test_emulates_outs;
         "test_loads_stores" >:: test_loads_stores;
         "test_stack_offset_access" >:: test_stack_offset_access;
         "test_jmps" >:: test_jmps;
         "test_hlt" >:: test_hlt;
         "test_zero_flag" >:: test_zero_flag;
         "test_sign_flag" >:: test_sign_flag;
         "test_overflow_flag" >:: test_overflow_flag;
         "test_call_ret" >:: test_call_ret;
         "test_invalid_pc_increment" >:: test_invalid_pc_increment;
         "test_invalid_target" >:: test_invalid_target;
         "test_invalid_imm" >:: test_invalid_imm;
         "test_invalid_instr" >:: test_invalid_instr;
         "test_overflow_immediates" >:: test_overflow_immediates;
       ]

let () = run_test_tt_main suite
