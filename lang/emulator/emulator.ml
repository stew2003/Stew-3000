open Asm.Isa
open Asm.Validate
open Printf
open Util.Env
open Util.Srcloc
open Util

(*  stew_3000 models the programmer-visible state of the machine
    NOTE: dec_disp_history is a record of every byte that has 
    been sent to the decimal display so far *)
type stew_3000 = {
  mutable a : int;
  mutable b : int;
  mutable c : int;
  mutable sp : int;
  mutable zflag : bool;
  mutable sflag : bool;
  mutable oflag : bool;
  mutable stack : int array;
  mutable dec_disp_history : int list;
  mutable pc : int;
  mutable halted : bool;
}

let string_of_stew_3000 (machine : stew_3000) : string =
  let string_of_dec_display (history : int list) =
    match history with
    | [] -> "(no output)"
    | _ -> List.map string_of_int history |> String.concat ", "
  in
  let string_of_stack (stack : int list) : string =
    List.mapi
      (fun i elt ->
        if i mod 8 = 0 then sprintf "\n0x%02x:\t|%3d" i elt
        else sprintf "%3d" elt)
      stack
    |> String.concat "|"
  in

  let bool_to_int (b : bool) = if b then 1 else 0 in
  sprintf
    "Emulated Stew 3000 State:\n\
     == Registers ==\n\
     a: %d\n\
     b: %d\n\
     c: %d\n\
     sp: %d\n\
     pc: %d\n\n\
     == Flags ==\n\
     zf: %d\n\
     sf: %d\n\
     of: %d\n\n\
     halted? %s\n\n\
     == Decimal Display History == (most recent last)\n\
     %s\n\n\
     == Stack ==%s\n"
    machine.a machine.b machine.c machine.sp machine.pc
    (bool_to_int machine.zflag)
    (bool_to_int machine.sflag)
    (bool_to_int machine.oflag)
    (if machine.halted then "yes" else "no")
    (string_of_dec_display machine.dec_disp_history)
    (string_of_stack (Array.to_list machine.stack))

let stack_size = 256

(* [new_stew_3000] constructs a new machine state in initial state *)
let new_stew_3000 _ : stew_3000 =
  {
    a = 0;
    b = 0;
    c = 0;
    sp = 0;
    zflag = false;
    sflag = false;
    oflag = false;
    stack = Array.make stack_size 0;
    dec_disp_history = [];
    pc = 0;
    halted = false;
  }

type emu_err =
  | DuplicateLabel of string
  | InvalidProgramCounter of stew_3000
  | InvalidTarget of string
  | InvalidImm of immediate
  | InvalidInstr of instr

exception EmulatorError of emu_err with_loc_opt

let string_of_emu_err (err : emu_err) =
  match err with
  | DuplicateLabel label -> sprintf "label `%s` appears more than once" label
  | InvalidProgramCounter machine ->
      sprintf "invalid program counter: %d\n%s" machine.pc
        (string_of_stew_3000 machine)
  | InvalidTarget label -> sprintf "invalid target: `%s`" label
  | InvalidImm imm -> sprintf "invalid immediate value: %s" (string_of_imm imm)
  | InvalidInstr ins -> sprintf "invalid instruction: %s" (string_of_instr ins)

(* [emulate_instr] emulates the effect of the given instruction
  on the machine, by mutating the machine in-place *)
let emulate_instr (ins : instr) (machine : stew_3000) (label_map : int env)
    (verbosity : int) =
  (* [inc_pc] increments the program counter by 1 *)
  let inc_pc _ = machine.pc <- machine.pc + 1 in
  (* [load_reg] retrieves the value currently stored in a register *)
  let load_reg (reg : register) : int =
    match reg with
    | A -> machine.a
    | B -> machine.b
    | C -> machine.c
    | SP -> machine.sp
  in
  (* [store_reg] stores a value in a given register *)
  let store_reg (reg : register) (value : int) =
    match reg with
    | A -> machine.a <- value
    | B -> machine.b <- value
    | C -> machine.c <- value
    | SP -> machine.sp <- value
  in
  (* [load_stack] retrieves the value on the machine's stack at
     the given location, or errors out as appropriate *)
  let load_stack (addr : int) : int =
    try Array.get machine.stack (Numbers.as_8bit_unsigned addr)
    with Invalid_argument _ ->
      failwith "load stack: index error with 8-bit unsigned address"
  in
  (* [store_stack] writes a given value to the stack at a given
     location, or errors if the access is bad *)
  let store_stack (addr : int) (value : int) =
    try Array.set machine.stack (Numbers.as_8bit_unsigned addr) value
    with Invalid_argument _ ->
      failwith "store stack: index error with 8-bit unsigned address"
  in
  (* [label_to_index] converts a string label to its corresponding index,
     erroring if there is no index for the label *)
  let label_to_index (label : string) (srcloc : maybe_loc) =
    match Env.find_opt label label_map with
    | Some index -> index
    | None -> raise (EmulatorError (InvalidTarget label, srcloc))
  in
  (* [set_flags] sets zero, signed, and overflow flags based on the
     result of an operation and the two values that were operated on. *)
  let set_flags (result : int) (left : int) (right : int) =
    let i8_result = Numbers.as_8bit_signed result in
    let i8_left = Numbers.as_8bit_signed left in
    let i8_right = Numbers.as_8bit_signed right in
    (* zero flag: result is 0 *)
    machine.zflag <- i8_result = 0;
    (* sign flag: result is negative *)
    machine.sflag <- i8_result < 0;
    (* signed overflow flag: signs of left/right are same,
       but different from sign of result *)
    machine.oflag <-
      i8_left < 0 = (i8_right < 0) && i8_result < 0 <> (i8_left < 0)
  in
  (* [emulate_arithmetic] emulates binary arithmetic operators that
     may overflow, and store in a destination register. If sub is true,
     then the src value will be subtracted from the dest value, otherwise
     added to. *)
  let emulate_arithmetic (src_value : int) (dest : register) (sub : bool) =
    (* perform arithmetic with signed 8-bit integers
       NOTE: src value (right operand) is negated if subtraction
       is to be performed. This allows the overflow flag to
       correctly determine its value assuming the result is a sum. *)
    let i8_src_value =
      (if sub then -1 else 1) * Numbers.as_8bit_signed src_value
    in
    let i8_dest_value = Numbers.as_8bit_signed (load_reg dest) in
    (* interpret result as signed 8-bit *)
    let result = Numbers.as_8bit_signed (i8_dest_value + i8_src_value) in
    store_reg dest result;
    set_flags result i8_dest_value i8_src_value;
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
    let result =
      Numbers.as_8bit_unsigned (operator u8_dest_value u8_src_value)
    in
    store_reg dest result;
    set_flags result u8_dest_value u8_src_value;
    inc_pc ()
  in
  (* [emulate_cmp] emulates performing a comparison between a left
     and right value by subtracting right from left and setting flags *)
  let emulate_cmp (left_value : int) (right_value : int) =
    (* treat left and right operands as signed 8-bit integers
       NOTE: to subtract right from left, we negate right and add it.
       This allows the overflow flag to correctly compute determine
       whether or not signed overflow has occurred by assuming the result
       is always from adding two numbers. *)
    let i8_left_value = Numbers.as_8bit_signed left_value in
    let negated_i8_right_value = -1 * Numbers.as_8bit_signed right_value in
    (* interpret result as signed 8-bit integer *)
    let diff =
      Numbers.as_8bit_signed (i8_left_value + negated_i8_right_value)
    in
    set_flags diff i8_left_value negated_i8_right_value;
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
  let emulate_jmp (condition : bool) (target : string) (srcloc : maybe_loc) =
    machine.pc <-
      (if condition then label_to_index target srcloc else machine.pc + 1)
  in
  (* [insert_at_end] adds an element to the end of a list *)
  let rec insert_at_end lst elt =
    match lst with [] -> [ elt ] | f :: r -> f :: insert_at_end r elt
  in

  (* first, ensure that the instruction is valid *)
  (try validate_instr ins with
  | ValidityError (InvalidImm imm, maybe_loc) ->
      raise (EmulatorError (InvalidImm imm, maybe_loc))
  | ValidityError (InvalidInstr ins, maybe_loc) ->
      raise (EmulatorError (InvalidInstr ins, maybe_loc)));

  (* simulate the effects of the instruction*)
  match ins with
  (* arithmetic/logical operators *)
  | Add (src, dest, _) -> emulate_arithmetic (load_reg src) dest false
  | Addi (imm, dest, _) -> emulate_arithmetic imm dest false
  | Sub (src, dest, _) -> emulate_arithmetic (load_reg src) dest true
  | Subi (imm, dest, _) -> emulate_arithmetic imm dest true
  | Inr (dest, _) -> emulate_arithmetic 1 dest false
  | Dcr (dest, _) -> emulate_arithmetic 1 dest true
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
  (* comparisons *)
  | Cmp (left, right, _) -> emulate_cmp (load_reg left) (load_reg right)
  | Cmpi (Imm imm, Reg right, _) -> emulate_cmp imm (load_reg right)
  | Cmpi (Reg left, Imm imm, _) -> emulate_cmp (load_reg left) imm
  (* jumps *)
  | Jmp (target, loc) -> emulate_jmp true target loc
  | Je (target, loc) -> emulate_jmp machine.zflag target loc
  | Jne (target, loc) -> emulate_jmp (not machine.zflag) target loc
  | Jg (target, loc) ->
      (* ~(SF ^ OF) & ~ZF *)
      emulate_jmp
        ((not (machine.sflag <> machine.oflag)) && not machine.zflag)
        target loc
  | Jge (target, loc) ->
      (* ~(SF ^ OF) *)
      emulate_jmp (not (machine.sflag <> machine.oflag)) target loc
  | Jl (target, loc) ->
      (* SF ^ OF *)
      emulate_jmp (machine.sflag <> machine.oflag) target loc
  | Jle (target, loc) ->
      (* (SF ^ OF) | ZF *)
      emulate_jmp (machine.sflag <> machine.oflag || machine.zflag) target loc
  | Call (target, loc) ->
      let fun_pc = label_to_index target loc in
      let ret_addr = machine.pc + 1 in
      machine.sp <- machine.sp + 1;
      store_stack machine.sp ret_addr;
      machine.pc <- fun_pc
  | Ret _ ->
      let ret_addr = load_stack machine.sp in
      machine.sp <- machine.sp - 1;
      machine.pc <- ret_addr
  (* miscellaneous *)
  | Hlt _ -> machine.halted <- true
  | Out (src, _) ->
      (* add value to decimal display history *)
      let src_value = load_reg src in
      machine.dec_disp_history <-
        insert_at_end machine.dec_disp_history src_value;
      (* at verbosity level 1, out instrs print their output *)
      if verbosity >= 1 then printf "%s %d\n" (Colors.log "[output]") src_value
      else ();
      inc_pc ()
  | Label _ | Nop _ -> inc_pc ()
  (* XXX: Dic and Did not currently supported *)
  | Dic _ | Did _ -> inc_pc ()
  | _ -> raise (EmulatorError (InvalidInstr ins, loc_from_instr ins))

(* [map_labels] constructs an environment that maps label names to indices in
  the program's list of instructions *)
let map_labels (instrs : instr list) =
  (* pair instructions with their indices in the program *)
  List.mapi (fun i ins -> (i, ins)) instrs
  (* accumulate an environment of labels->indices *)
  |> List.fold_left
       (fun env (i, ins) ->
         match ins with
         | Label (name, loc) ->
             (* map each label to the index following it *)
             if Env.mem name env then
               raise (EmulatorError (DuplicateLabel name, loc))
             else Env.add name (i + 1) env
         | _ -> env)
       Env.empty

(* [get_current_ins] retrieves the current instruction to execute 
  by indexing into the program using the machine's program counter *)
let get_current_ins (pgrm : instr list) (machine : stew_3000) : instr =
  try List.nth pgrm machine.pc
  with Failure _ | Invalid_argument _ ->
    raise (EmulatorError (InvalidProgramCounter machine, None))

(* [emulate] emulates running the given assembly program 
  on the Stew 3000, and returns the final machine state after the run.
  verbosity indicates how much logging should happen during the run. *)
let emulate (pgrm : instr list) (verbosity : int) : stew_3000 =
  let machine = new_stew_3000 () in
  let label_map = map_labels pgrm in
  let rec run _ =
    if machine.halted then ()
    else
      let ins = get_current_ins pgrm machine in
      (* verbosity level 2, current instruction is logged *)
      if verbosity >= 2 then
        printf "%s %s\n"
          (Colors.log "[current instruction]")
          (string_of_instr ins)
      else ();
      emulate_instr ins machine label_map verbosity;
      (* verbosity level 3, entire machine state is logged *)
      if verbosity >= 3 then
        printf "%s\n%s"
          (Colors.log "[state after executing]")
          (string_of_stew_3000 machine)
      else ();
      run ()
  in
  run ();
  machine
