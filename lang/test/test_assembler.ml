open OUnit2
open Asm.Assemble
open Asm.Isa
open Asm.Warnings

(* [assert_assembles_to] asserts that the given program assembles
   to the given list of bytes *)
let assert_assembles_to (instrs : instr list) (bin : int list) =
  let bin_as_bytes = bytes_from_list bin in
  assert_equal (assemble instrs) bin_as_bytes

let test_simple_pgrm _ =
  assert_assembles_to
    [ Mvi (5, A, None); Dcr (A, None); Hlt None ]
    [ 0x7f; 0x05; 0x67; 0xc7 ]

let test_longer_pgrm _ =
  assert_assembles_to
    [
      Nop None;
      Nop None;
      Label ("loop", None);
      Mvi (10, A, None);
      Sts (A, 1, None);
      Lds (1, B, None);
      Out (B, None);
      Jmp ("loop", None);
    ]
    [ 0xc8; 0xc8; 0x7f; 10; 0x9a; 1; 0x98; 1; 0xbf; 0xb1; 2 ]

let test_invalid_instr _ =
  assert_raises
    (AssembleError (InvalidInstr (Mvi (5, SP, None)), None))
    (fun _ ->
      assemble [ Add (A, B, None); Subi (1, B, None); Mvi (5, SP, None) ]);
  assert_raises
    (AssembleError (InvalidInstr (Cmpi (Reg B, Reg A, None)), None))
    (fun _ -> assemble [ Cmpi (Reg B, Reg A, None) ])

let test_dup_label _ =
  assert_raises
    (AssembleError (DuplicateLabel "dup", None))
    (fun _ -> assemble [ Label ("dup", None); Label ("dup", None) ])

let test_invalid_imm _ =
  assert_raises
    (AssembleError (InvalidImm 500, None))
    (fun _ -> assemble [ Mvi (500, A, None) ]);
  assert_raises
    (AssembleError (InvalidImm (-129), None))
    (fun _ -> assemble [ Addi (-129, C, None) ]);
  assert_raises
    (AssembleError (InvalidImm 256, None))
    (fun _ -> assemble [ Sts (A, 256, None) ]);
  assert_raises
    (AssembleError (InvalidImm (-129), None))
    (fun _ -> assemble [ Lds (-129, C, None) ])

let test_invalid_target _ =
  assert_raises
    (AssembleError (InvalidTarget "nonexistent", None))
    (fun _ -> assemble [ Jle ("nonexistent", None) ])

(* [make_program] builds a program of size nops *)
let rec make_program (size : int) =
  if size = 0 then [] else Nop None :: make_program (size - 1)

(* [get_warnings] invokes the assembler on a given program and
   accumulates warnings into a list, which it returns. Earlier
   warnings occur earlier in the list. *)
let get_warnings (instrs : instr list) : asm_warn list =
  let warning_list = ref [] in
  let warn_handler (warning : asm_warn) =
    warning_list := warning :: !warning_list
  in
  assemble instrs ~emit_warning:warn_handler |> ignore;
  List.rev !warning_list

let test_pgrm_too_large _ =
  let big = 257 in
  let warnings = get_warnings (make_program big) in
  assert_equal [ ProgramTooLarge big ] warnings

let test_out_of_bounds_label _ =
  let warnings =
    get_warnings
      ([ Jmp ("out_of_bounds", None) ]
      @ make_program 254
      @ [ Label ("out_of_bounds", None) ])
  in
  assert_equal [ OutOfBoundsLabel ("out_of_bounds", 256, None) ] warnings

let test_overflow_immediates _ =
  assert_equal
    (assemble [ Lds (255, A, None) ])
    (assemble [ Lds (-1, A, None) ]);
  assert_equal
    (assemble [ Mvi (128, C, None) ])
    (assemble [ Mvi (-128, C, None) ])

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
         "test_overflow_immediates" >:: test_overflow_immediates;
         "test_out_of_bounds_label" >:: test_out_of_bounds_label;
       ]

let () = run_test_tt_main suite
