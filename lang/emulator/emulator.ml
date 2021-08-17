open Asm.Isa
open Printf
open Util.Env
open Util.Num_env
open Util.Srcloc
open Util.Err
open Util
open Asm
open Machine

type emu_err =
  | InvalidProgramCounter of stew_3000
  | InvalidPCIncrement of stew_3000

exception EmulatorError of emu_err with_loc_opt

let string_of_emu_err (err : emu_err) =
  match err with
  | InvalidProgramCounter machine ->
      sprintf "invalid program counter: %d\n%s" machine.pc
        (string_of_stew_3000 machine)
  | InvalidPCIncrement machine ->
      sprintf "invalid program counter increment from %d\n%s" machine.pc
        (string_of_stew_3000 machine)

(* [emulate_instr] emulates the effect of the given instruction
   on the machine, by mutating the machine in-place *)
let emulate_instr (ins : instr) (machine : stew_3000) (label_to_addr : int env)
    (addr_to_index : int num_env) (index_to_addr : int num_env)
    (verbosity : int) (warn : bool) =
  (* [next_pc] computes the address of the next consecutive instruction
     after the current PC value. *)
  let next_pc _ =
    (* lookup current PC's index in instruction list *)
    match Num_env.find_opt machine.pc addr_to_index with
    | Some index -> (
        (* lookup the address of the next instruction (index + 1) *)
        match Num_env.find_opt (index + 1) index_to_addr with
        | Some addr -> addr
        | None -> raise (EmulatorError (InvalidPCIncrement machine, None)))
    | None -> raise (EmulatorError (InvalidProgramCounter machine, None))
  in
  (* [inc_pc] increments the program counter by 1 *)
  let inc_pc _ = machine.pc <- next_pc () in
  (* [load_reg] retrieves the value currently stored in a register *)
  let load_reg (reg : register) : int =
    match reg with
    | A -> machine.a
    | B -> machine.b
    | C -> machine.c
    | Z -> 0
    | SP -> machine.sp
  in
  (* [store_reg] stores a value in a given register *)
  let store_reg (reg : register) (value : int) =
    match reg with
    | A -> machine.a <- value
    | B -> machine.b <- value
    | C -> machine.c <- value
    (* zero register doesn't actually exist, can't write to it *)
    | Z -> ()
    | SP -> machine.sp <- value
  in
  (* [load_stack] retrieves the value on the machine's stack at
     the given location, or errors out as appropriate *)
  let load_stack (addr : int) : int =
    try Array.get machine.stack (Numbers.as_8bit_unsigned addr)
    with Invalid_argument _ ->
      raise
        (InternalError
           "emulator: load stack: index error with 8-bit unsigned address")
  in
  (* [store_stack] writes a given value to the stack at a given
     location, or errors if the access is bad *)
  let store_stack (addr : int) (value : int) =
    try Array.set machine.stack (Numbers.as_8bit_unsigned addr) value
    with Invalid_argument _ ->
      raise
        (InternalError
           "emulator: store stack: index error with 8-bit unsigned address")
  in
  (* [get_label_addr] converts a string label to its corresponding address,
     erroring if there is no address for the label *)
  let get_label_addr (label : string) =
    match Env.find_opt label label_to_addr with
    | Some addr -> addr
    | None -> raise (InternalError "emulator: get label addr: invalid label")
  in
  (* [set_flags] sets zero, signed, and overflow flags based on the
     result of an operation and the two values that were operated on.
     The sub boolean indicates whether subtraction was used to obtain
     the result. *)
  let set_flags (unchecked_result : int) (left : int) (right : int) (sub : bool)
      =
    let i8_result = Numbers.as_8bit_signed unchecked_result in
    let i8_left = Numbers.as_8bit_signed left in
    let i8_right = Numbers.as_8bit_signed right in
    (* zero flag: result is 0 *)
    machine.zflag <- i8_result = 0;
    (* sign flag: result is negative *)
    machine.sflag <- i8_result < 0;
    (* signed overflow flag: signs of left/right are same,
       but different from sign of result *)
    machine.oflag <-
      i8_left < 0 = (i8_right < 0) && i8_result < 0 <> (i8_left < 0);
    (* carry flag: If the 9th bit is set, the addition CF will be set.
       If subtraction was performed, invert this CF to get the machine's CF. *)
    machine.cflag <-
      (let add_cf = unchecked_result land 0b100000000 <> 0 in
       if sub then not add_cf else add_cf)
  in
  (* [negate_value] takes an integer and negates it by flipping its bits
     and adding 1. *)
  let negate_value (value : int) : int =
    Numbers.as_8bit_unsigned (lnot (Numbers.as_8bit_unsigned value)) + 1
  in
  (* [emulate_arithmetic] emulates binary arithmetic operators that
     may overflow, and store in a destination register. If sub is true,
     then the src value will be subtracted from the dest value, otherwise
     added to. *)
  let emulate_arithmetic (src_value : int) (dest : register) (sub : bool)
      (include_carry : bool) =
    (* perform arithmetic with signed 8-bit integers
       NOTE: src value (right operand) is negated if subtraction
       is to be performed. This allows the overflow flag to
       correctly determine its value assuming the result is a sum. *)
    let i8_src_value = if sub then negate_value src_value else src_value in
    let i8_dest_value = Numbers.as_8bit_signed (load_reg dest) in
    let carry_in =
      if include_carry then
        let cf = bool_to_int machine.cflag in
        if sub then negate_value cf else cf
      else 0
    in
    let unchecked_result = i8_dest_value + i8_src_value + carry_in in
    store_reg dest (Numbers.as_8bit_signed unchecked_result);
    set_flags unchecked_result i8_dest_value i8_src_value sub;
    (* emit a warning if overflow occurred *)
    if machine.oflag then
      Logging.warn_arith_overflow warn ins unchecked_result machine.pc
    else ();
    inc_pc ()
  in
  (* [emulate_logic] emulates binary logical operators that
     store their result in a destination register *)
  let emulate_logic (src_value : int) (dest : register)
      (operator : int -> int -> int) =
    (* treat operands as unsigned for logic operation *)
    let u8_src_value = Numbers.as_8bit_unsigned src_value in
    let u8_dest_value = Numbers.as_8bit_unsigned (load_reg dest) in
    (* treat result as unsigned *)
    let unchecked_result = operator u8_dest_value u8_src_value in
    store_reg dest (Numbers.as_8bit_unsigned unchecked_result);
    set_flags unchecked_result u8_dest_value u8_src_value false;
    inc_pc ()
  in
  (* [emulate_cmp] emulates performing a comparison between a left
     and right value by subtracting right from left and setting flags *)
  let emulate_cmp (left_value : int) (right_value : int) =
    (* treat left and right operands as signed 8-bit integers
       NOTE: to subtract right from left, we negate right and add it.
       This allows the overflow flag to correctly determine
       whether or not signed overflow has occurred by assuming the result
       is always from adding two numbers. *)
    let u8_left_value = Numbers.as_8bit_unsigned left_value in
    (* NOTE: the negated right value is interpreted as UNsigned here, because
       then if the addition overflows, the overflow will be present in the 9th
       bit, and won't be propagated further. This allows us to check the 9th
       bit for carry from addition. *)
    let negated_u8_right_value = negate_value right_value in
    (* allow this result value to overflow *)
    let unchecked_result = u8_left_value + negated_u8_right_value in
    set_flags unchecked_result u8_left_value negated_u8_right_value true;
    inc_pc ()
  in
  (* [emulate_load] emulates a load from the stack into a destination register *)
  let emulate_load (addr : int) (dest : register) =
    let mem_at_addr = load_stack addr in
    store_reg dest mem_at_addr;
    inc_pc ()
  in
  (* [emulate_store] emulates storing from a src register onto the stack *)
  let emulate_store (src : register) (addr : int) =
    let src_value = load_reg src in
    store_stack addr src_value;
    inc_pc ()
  in
  (* [emulate_jmp] emulates a conditional jump, by setting the pc to the
     index of a given label if a condition is true and incrementing otherwise *)
  let emulate_jmp (condition : bool) (target : string) =
    machine.pc <- (if condition then get_label_addr target else next_pc ())
  in
  (* [insert_at_end] adds an element to the end of a list *)
  let rec insert_at_end lst elt =
    match lst with [] -> [ elt ] | f :: r -> f :: insert_at_end r elt
  in

  (* simulate the effects of the instruction*)
  match ins with
  (* arithmetic/logical operators *)
  | Add (src, dest, _) -> emulate_arithmetic (load_reg src) dest false false
  | Addc (src, dest, _) -> emulate_arithmetic (load_reg src) dest false true
  | Addi (imm, dest, _) -> emulate_arithmetic imm dest false false
  | Addci (imm, dest, _) -> emulate_arithmetic imm dest false true
  | Sub (src, dest, _) -> emulate_arithmetic (load_reg src) dest true false
  | Subb (src, dest, _) -> emulate_arithmetic (load_reg src) dest true true
  | Subi (imm, dest, _) -> emulate_arithmetic imm dest true false
  | Subbi (imm, dest, _) -> emulate_arithmetic imm dest true true
  | Inr (dest, _) -> emulate_arithmetic 1 dest false false
  | Inr2 (dest, _) -> emulate_arithmetic 2 dest false false
  | Inr3 (dest, _) -> emulate_arithmetic 3 dest false false
  | Dcr (dest, _) -> emulate_arithmetic 1 dest true false
  | Dcr2 (dest, _) -> emulate_arithmetic 2 dest true false
  | Dcr3 (dest, _) -> emulate_arithmetic 3 dest true false
  | And (src, dest, _) -> emulate_logic (load_reg src) dest ( land )
  | Ani (imm, dest, _) -> emulate_logic imm dest ( land )
  | Or (src, dest, _) -> emulate_logic (load_reg src) dest ( lor )
  | Ori (imm, dest, _) -> emulate_logic imm dest ( lor )
  | Xor (src, dest, _) -> emulate_logic (load_reg src) dest ( lxor )
  | Xri (imm, dest, _) -> emulate_logic imm dest ( lxor )
  | Not (dest, _) -> emulate_logic 0 dest (fun v _ -> lnot v)
  (* moves *)
  | Mov (src, dest, _) ->
      store_reg dest (load_reg src);
      inc_pc ()
  | Mvi (imm, dest, _) ->
      store_reg dest imm;
      inc_pc ()
  (* memory operations *)
  | Ld (src, dest, _) -> emulate_load (load_reg src) dest
  | St (src, dest, _) -> emulate_store src (load_reg dest)
  | Lds (imm, dest, _) -> emulate_load (machine.sp + imm) dest
  | Sts (src, imm, _) -> emulate_store src (machine.sp + imm)
  | Stsi (value, addr, _) ->
      store_stack addr value;
      inc_pc ()
  (* comparisons *)
  | Cmp (left, right, _) -> emulate_cmp (load_reg left) (load_reg right)
  | Cmpi (Imm imm, Reg right, _) -> emulate_cmp imm (load_reg right)
  | Cmpi (Reg left, Imm imm, _) -> emulate_cmp (load_reg left) imm
  (* jumps *)
  | Jmp (target, _) -> emulate_jmp true target
  | Je (target, _) -> emulate_jmp machine.zflag target
  | Jne (target, _) -> emulate_jmp (not machine.zflag) target
  | Jg (target, _) ->
      (* ~(SF ^ OF) & ~ZF *)
      emulate_jmp
        ((not (machine.sflag <> machine.oflag)) && not machine.zflag)
        target
  | Jge (target, _) ->
      (* ~(SF ^ OF) *)
      emulate_jmp (not (machine.sflag <> machine.oflag)) target
  | Jl (target, _) ->
      (* SF ^ OF *)
      emulate_jmp (machine.sflag <> machine.oflag) target
  | Jle (target, _) ->
      (* (SF ^ OF) | ZF *)
      emulate_jmp (machine.sflag <> machine.oflag || machine.zflag) target
  | Ja (target, _) ->
      (* ~CF & ~ZF *)
      emulate_jmp ((not machine.cflag) && not machine.zflag) target
  | Jae (target, _) ->
      (* ~CF *)
      emulate_jmp (not machine.cflag) target
  | Jb (target, _) ->
      (* CF *)
      emulate_jmp machine.cflag target
  | Jbe (target, _) ->
      (* CF | ZF *)
      emulate_jmp (machine.cflag || machine.zflag) target
  | Call (target, _) ->
      let fun_pc = get_label_addr target in
      let ret_addr = next_pc () in
      machine.sp <- machine.sp + 1;
      store_stack machine.sp ret_addr;
      machine.pc <- fun_pc
  | Ret _ ->
      let ret_addr = load_stack machine.sp in
      machine.sp <- machine.sp - 1;
      machine.pc <- ret_addr
  | Hlt _ -> machine.halted <- true
  | Out (src, _) ->
      let src_value = load_reg src in
      machine.dec_disp_history <-
        insert_at_end machine.dec_disp_history src_value;
      Logging.log_output verbosity src_value;
      inc_pc ()
  | Outi (imm, _) ->
      machine.dec_disp_history <- insert_at_end machine.dec_disp_history imm;
      Logging.log_output verbosity imm;
      inc_pc ()
  | Dic (imm, _) ->
      machine.lcd_disp_history <-
        insert_at_end machine.lcd_disp_history (Command imm);
      inc_pc ()
  | Did (imm, _) ->
      machine.lcd_disp_history <-
        insert_at_end machine.lcd_disp_history (Data imm);
      inc_pc ()
  | Dd (reg, _) ->
      let reg_value = load_reg reg in
      machine.lcd_disp_history <-
        insert_at_end machine.lcd_disp_history (Data reg_value);
      inc_pc ()
  | Label _ | Nop _ -> inc_pc ()
  | Cmpi (Imm _, Imm _, _) | Cmpi (Reg _, Reg _, _) ->
      raise
        (InternalError
           (sprintf "emulator: invalid instruction: %s" (string_of_instr ins)))

(* [map_addr_to_index] constructs an environment mapping physical
   addresses of the beginnings of instructions in a binary to the index
   in pgrm at which that instruction appears, and an env implementing
   the inverse mapping. *)
let map_addrs_and_indices (pgrm : instr list) : int num_env * int num_env =
  let addr_to_index, index_to_addr, _ =
    List.mapi (fun i ins -> (i, ins)) pgrm
    |> List.fold_left
         (fun (a_to_i, i_to_a, addr) (i, ins) ->
           let instr_size = Assemble.size_of ins in
           ( Num_env.add addr i a_to_i,
             Num_env.add i addr i_to_a,
             addr + instr_size ))
         (Num_env.empty, Num_env.empty, 0)
  in
  (addr_to_index, index_to_addr)

(* [get_current_ins] retrieves the current instruction to execute
   by indexing into the program using the machine's program counter *)
let get_current_ins (pgrm : instr list) (machine : stew_3000)
    (addr_to_index : int num_env) : instr =
  (* lookup machine's PC to find index in instructions *)
  match Num_env.find_opt machine.pc addr_to_index with
  | Some index -> (
      try List.nth pgrm index
      with Failure _ | Invalid_argument _ ->
        raise
          (InternalError
             "emulator: get current ins: addr to index yielded bad index"))
  | None -> raise (EmulatorError (InvalidProgramCounter machine, None))

(* [emulate] emulates running the given assembly program
   on the Stew 3000, and returns the final machine state after the run.
   verbosity indicates how much logging should happen during the run. *)
let emulate ?(verbosity = 0) ?(db_mode = false) ?(warn = false)
    (pgrm : instr list) : stew_3000 =
  (* get byte-level info on the program from the assembler *)
  let label_to_addr, _, _ = Assemble.assemble_with_rich_info pgrm in
  let addr_to_index, index_to_addr = map_addrs_and_indices pgrm in
  let machine = new_stew_3000 () in
  let rec run _ =
    if machine.halted then ()
    else
      (* fetch the instruction at the program counter *)
      let ins = get_current_ins pgrm machine addr_to_index in
      (* in debug mode, stop to allow user to run commands
         before the instruction is executed *)
      if db_mode then Command.loop_for_commands machine ins else ();
      (* log the current instruction and execute it *)
      Logging.log_current_ins verbosity machine ins;
      emulate_instr ins machine label_to_addr addr_to_index index_to_addr
        verbosity warn;
      run ()
  in
  run ();
  machine
