open OUnit2
open Asm.Isa
open Emulator
open Util.Srcloc

let dummy_src_loc = loc 0 0

(* wrapper around emulate, with no logging and adding dummy src locations *)
let run_emulator (pgrm : instr list) =
  emulate (List.map (fun ins -> (ins, dummy_src_loc)) pgrm) 0

let assert_int_eq (exp : int) (act : int) =
  assert_equal exp act ~printer:string_of_int

let string_of_int_list (lst : int list) =
  "[" ^ (List.map string_of_int lst |> String.concat "; ") ^ "]"

let assert_int_list_eq (exp : int list) (act : int list) =
  assert_equal exp act ~printer:string_of_int_list

let test_simple_pgrm _ =
  let machine = run_emulator [ Mvi (10, A); Addi (-6, A); Hlt ] in
  assert_int_eq machine.a 4

let test_emulates_outs _ =
  let machine =
    run_emulator
      [
        Mvi (2, A);
        Mov (A, B);
        Inr B;
        Mvi (-17, C);
        Out C;
        Out B;
        Out B;
        Out A;
        Hlt;
      ]
  in
  assert_int_eq 2 machine.a;
  assert_int_eq 3 machine.b;
  assert_int_eq (-17) machine.c;
  assert_int_list_eq [ 2; 3; 3; -17 ] machine.dec_disp_history

let test_loads_stores _ =
  let machine =
    run_emulator
      [
        Mvi (5, A);
        Mvi (11, B);
        (* RAM[A] = B *)
        St (B, A);
        (* C = RAM[A] *)
        Ld (A, C);
        Hlt;
      ]
  in
  assert_int_eq 11 machine.c

let test_stack_offset_access _ =
  let machine = run_emulator [ Mvi (25, B); Sts (B, 15); Lds (15, A); Hlt ] in
  assert_int_eq 25 machine.a

let test_jmps _ =
  let machine =
    run_emulator
      [
        Label "entry";
        Mvi (2, A);
        Jmp "after";
        Label "skip";
        Mvi (100, A);
        Label "after";
        Hlt;
      ]
  in
  assert_int_eq 2 machine.a;
  let machine =
    run_emulator
      [
        Mvi (0, A);
        Label "loop";
        Out A;
        Inr A;
        Cmpi (Reg A, Imm 5);
        Jl "loop";
        Hlt;
      ]
  in
  assert_int_list_eq [ 4; 3; 2; 1; 0 ] machine.dec_disp_history;
  assert_int_eq 5 machine.a;
  let machine =
    run_emulator
      [
        Mvi (10, A);
        Mvi (9, B);
        Cmp (A, B);
        Jg "greater";
        Mvi (0, C);
        Hlt;
        Label "greater";
        Mvi (1, C);
        Hlt;
      ]
  in
  assert_int_eq 1 machine.c;
  let machine =
    run_emulator
      [
        Mvi (87, B);
        Mov (B, A);
        Cmp (A, B);
        Je "equal";
        Mvi (0, C);
        Hlt;
        Label "equal";
        Mvi (1, C);
        Hlt;
      ]
  in
  assert_int_eq 1 machine.c

let test_hlt _ =
  let machine =
    run_emulator [ Hlt; Mvi (1, C); Mvi (2, A); Add (C, A); Out A ]
  in
  assert_int_eq 0 machine.a;
  assert_int_eq 0 machine.c

let test_zero_flag _ =
  let machine = run_emulator [ Mvi (5, A); Mov (A, B); Sub (A, B); Hlt ] in
  assert_bool "zero flag should be set" machine.zflag;
  let machine = run_emulator [ Mvi (1, A); Mvi (-1, B); Cmp (B, A); Hlt ] in
  assert_bool "zero flag should not be set" (not machine.zflag);
  let machine = run_emulator [ Mvi (127, A); Cmpi (Reg A, Imm 127); Hlt ] in
  assert_bool "zero flag set after comparing 127 with itself" machine.zflag

let test_sign_flag _ =
  let machine = run_emulator [ Mvi (35, A); Mvi (50, B); Sub (B, A); Hlt ] in
  assert_bool "sign flag should be set" machine.sflag;
  let machine = run_emulator [ Mvi (20, A); Mvi (10, B); Sub (B, A); Hlt ] in
  assert_bool "sign flag should not be set" (not machine.sflag);
  let machine = run_emulator [ Mvi (0, A); Dcr A; Hlt ] in
  assert_bool "sign flag set after 0--" machine.sflag

let test_overflow_flag _ =
  let machine = run_emulator [ Mvi (127, A); Inr A; Hlt ] in
  assert_bool "overflow flag set after increment 127" machine.oflag;
  let machine = run_emulator [ Mvi (-128, B); Subi (1, B); Hlt ] in
  assert_bool "overflow flag set after -128 - 1" machine.oflag;
  let machine = run_emulator [ Mvi (-120, A); Mvi (100, B); Cmp (A, B); Hlt ] in
  assert_bool "overflow flag set after -120 - 100" machine.oflag;
  let machine = run_emulator [ Mvi (-1, A); Subi (127, A); Hlt ] in
  assert_bool "overflow flag not set after -1-127" (not machine.oflag)

let test_call_ret _ =
  let machine =
    run_emulator
      [
        Mvi (4, C);
        Sts (C, 2);
        Mvi (7, C);
        Sts (C, 3);
        Call "add";
        Out A;
        Hlt;
        Label "add";
        Lds (1, A);
        Lds (2, B);
        Add (B, A);
        Ret;
      ]
  in
  assert_int_list_eq [ 11 ] machine.dec_disp_history

let test_dup_label _ =
  assert_raises
    (EmulatorError (DuplicateLabel "dup", Some dummy_src_loc))
    (fun _ -> run_emulator [ Label "dup"; Label "dup"; Label "dup" ])

let test_invalid_pc _ =
  let machine = new_stew_3000 () in
  (* pc runs right off the end *)
  machine.pc <- 3;
  assert_raises
    (EmulatorError (InvalidProgramCounter machine, None))
    (fun _ -> run_emulator [ Nop; Nop; Nop ])

let test_invalid_target _ =
  assert_raises
    (EmulatorError (InvalidTarget "not_here", Some dummy_src_loc))
    (fun _ -> run_emulator [ Jmp "not_here" ]);
  assert_raises
    (EmulatorError (InvalidTarget "not_a_fun", Some dummy_src_loc))
    (fun _ -> run_emulator [ Call "not_a_fun" ])

let test_invalid_imm _ =
  assert_raises
    (EmulatorError (InvalidImm (-129), Some dummy_src_loc))
    (fun _ -> run_emulator [ Addi (-129, B) ]);
  assert_raises
    (EmulatorError (InvalidImm (-129), Some dummy_src_loc))
    (fun _ -> run_emulator [ Lds (-129, A) ]);
  assert_raises
    (EmulatorError (InvalidImm 256, Some dummy_src_loc))
    (fun _ -> run_emulator [ Sts (C, 256) ])

let test_invalid_instr _ =
  assert_raises
    (EmulatorError (InvalidInstr (Sub (B, B)), Some dummy_src_loc))
    (fun _ -> run_emulator [ Sub (B, B) ]);
  assert_raises
    (EmulatorError (InvalidInstr (Cmpi (Imm 1, Imm 2)), Some dummy_src_loc))
    (fun _ -> run_emulator [ Cmpi (Imm 1, Imm 2) ])

let test_overflow_immediates _ =
  let machine =
    run_emulator [ Mvi (-1, A); Mvi (101, B); St (B, A); Ld (A, C); Hlt ]
  in
  assert_equal 101 machine.c;
  assert_equal 101 machine.stack.(255);
  let machine =
    run_emulator [ Mvi (255, A); Mvi (101, B); St (B, A); Ld (A, C); Hlt ]
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
         "test_invalid_pc" >:: test_invalid_pc;
         "test_invalid_target" >:: test_invalid_target;
         "test_invalid_imm" >:: test_invalid_imm;
         "test_invalid_instr" >:: test_invalid_instr;
         "test_overflow_immediates" >:: test_overflow_immediates;
       ]

let () = run_test_tt_main suite
