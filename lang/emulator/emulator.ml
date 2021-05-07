open Asm.Isa
open Asm.Validate
open Printf
open Util.Env
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
     == Decimal Display History == (most recent first)\n\
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
  | InvalidStackAccess of int * stew_3000

exception EmulatorError of emu_err

let string_of_emu_err (err : emu_err) =
  match err with
  | DuplicateLabel label -> sprintf "duplicate label: %s" label
  | InvalidProgramCounter machine ->
      sprintf "invalid program counter: %d\n%s" machine.pc
        (string_of_stew_3000 machine)
  | InvalidTarget label -> sprintf "invalid target: %s" label
  | InvalidImm imm -> sprintf "invalid immediate value: %s" (string_of_imm imm)
  | InvalidInstr ins -> sprintf "invalid instruction: %s" (string_of_instr ins)
  | InvalidStackAccess (loc, machine) ->
      sprintf "invalid stack access: location %d\n%s\n" loc
        (string_of_stew_3000 machine)

(* [emulate_instr] emulates the effect of the given instruction
  on the machine, by mutating the machine in-place *)
let emulate_instr (ins : instr) (machine : stew_3000) (label_map : int env)
    (verbosity : int) =
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
  let load_stack (loc : int) : int =
    try Array.get machine.stack loc
    with Invalid_argument _ ->
      raise (EmulatorError (InvalidStackAccess (loc, machine)))
  in
  (* [store_stack] writes a given value to the stack at a given
     location, or errors if the access is bad *)
  let store_stack (loc : int) (value : int) =
    try Array.set machine.stack loc value
    with Invalid_argument _ ->
      raise (EmulatorError (InvalidStackAccess (loc, machine)))
  in
  (* [label_to_index] converts a string label to its corresponding index,
     erroring if there is no index for the label *)
  let label_to_index (label : string) =
    match Env.find_opt label label_map with
    | Some index -> index
    | None -> raise (EmulatorError (InvalidTarget label))
  in
  (* [as_8bit_signed] checks if a value has 8-bit signed overflow,
     and keeps it within the representable range [-128, 128),
     also setting the overflow flag if an overflow has occurred *)
  let as_8bit_signed (value : int) : int =
    if value > 127 then (
      machine.oflag <- true;
      -128 + (value mod 128))
    else if value < -128 then (
      machine.oflag <- true;
      value mod 128)
    else (
      machine.oflag <- false;
      value)
  in
  (* [as_unsigned] interprets the given value as an unsigned 8-bit integer *)
  let as_unsigned (value : int) : int =
    if value < 0 then 256 + (value mod 256) else value mod 256
  in
  (* [set_zf_sf] sets the zero and sign flags based on the given
     result of an operation. NOTE: overflow flag is set as
     potentially overflowing arithmetic is performed, and
     so does not need to be set here *)
  let set_zf_sf (result : int) =
    machine.zflag <- result = 0;
    machine.sflag <- result < 0
  in
  (* [inc_pc] increments the program counter by 1 *)
  let inc_pc _ = machine.pc <- machine.pc + 1 in
  (* [emulate_arithmetic] emulates binary arithmetic operators that
     may overflow, and store in a destination register *)
  let emulate_arithmetic (src_value : int) (dest : register) op =
    let dest_value = load_reg dest in
    let result = as_8bit_signed (op dest_value src_value) in
    store_reg dest result;
    set_zf_sf result;
    inc_pc ()
  in
  (* [emulate_logic] emulates binary logical operators that
     store their result in a destination register *)
  let emulate_logic src_value dest op =
    let dest_value = load_reg dest in
    let result = op dest_value src_value in
    store_reg dest result;
    set_zf_sf result;
    inc_pc ()
  in
  (* [emulate_cmp] emulates performing a comparison between a left
     and right value by subtracting right from left and setting flags *)
  let emulate_cmp (left_value : int) (right_value : int) =
    let diff = as_8bit_signed (left_value - right_value) in
    set_zf_sf diff;
    inc_pc ()
  in
  (* [emulate_load] emulates a load from the stack into a destination register *)
  let emulate_load (mem_loc : int) (dest : register) =
    let from_mem = load_stack (as_unsigned mem_loc) in
    store_reg dest from_mem;
    inc_pc ()
  in
  (* [emulate_store] emulates storing from a src register onto the stack *)
  let emulate_store (src : register) (mem_loc : int) =
    let to_mem = load_reg src in
    store_stack (as_unsigned mem_loc) to_mem;
    inc_pc ()
  in
  (* [emulate_jmp] emulates a conditional jump, by setting the pc to the
     index of a given label if a condition is true and incrementing otherwise *)
  let emulate_jmp (condition : bool) (target : string) =
    machine.pc <- (if condition then label_to_index target else machine.pc + 1)
  in

  (* first, ensure that the instruction is valid *)
  (try validate_instr ins with
  | ValidityError (InvalidImm imm) -> raise (EmulatorError (InvalidImm imm))
  | ValidityError (InvalidInstr ins) -> raise (EmulatorError (InvalidInstr ins)));

  (* simulate the effects of the instruction*)
  match ins with
  (* arithmetic/logical operators *)
  | Add (src, dest) -> emulate_arithmetic (load_reg src) dest ( + )
  | Addi (imm, dest) -> emulate_arithmetic imm dest ( + )
  | Sub (src, dest) -> emulate_arithmetic (load_reg src) dest ( - )
  | Subi (imm, dest) -> emulate_arithmetic imm dest ( - )
  | Inr dest -> emulate_arithmetic 1 dest ( + )
  | Dcr dest -> emulate_arithmetic 1 dest ( - )
  | And (src, dest) -> emulate_logic (load_reg src) dest ( land )
  | Ani (imm, dest) -> emulate_logic imm dest ( land )
  | Or (src, dest) -> emulate_logic (load_reg src) dest ( lor )
  | Ori (imm, dest) -> emulate_logic imm dest ( lor )
  | Xor (src, dest) -> emulate_logic (load_reg src) dest ( lxor )
  | Xri (imm, dest) -> emulate_logic imm dest ( lxor )
  | Not dest -> emulate_logic () dest (fun v _ -> lnot v)
  (* moves *)
  | Mov (src, dest) ->
      store_reg dest (load_reg src);
      inc_pc ()
  | Mvi (imm, dest) ->
      store_reg dest imm;
      inc_pc ()
  (* memory operations *)
  | Ld (src, dest) -> emulate_load (load_reg src) dest
  | St (src, dest) -> emulate_store src (load_reg dest)
  | Lds (imm, dest) -> emulate_load (machine.sp + imm) dest
  | Sts (src, imm) -> emulate_store src (machine.sp + imm)
  (* comparisons *)
  | Cmp (left, right) -> emulate_cmp (load_reg left) (load_reg right)
  | Cmpi (Imm imm, Reg right) -> emulate_cmp imm (load_reg right)
  | Cmpi (Reg left, Imm imm) -> emulate_cmp (load_reg left) imm
  (* jumps *)
  | Jmp target -> emulate_jmp true target
  | Je target -> emulate_jmp machine.zflag target
  | Jne target -> emulate_jmp (not machine.zflag) target
  | Jg target ->
      (* ~(SF ^ OF) & ~ZF *)
      emulate_jmp
        ((not (machine.sflag <> machine.oflag)) && not machine.zflag)
        target
  | Jge target ->
      (* ~(SF ^ OF) *)
      emulate_jmp (not (machine.sflag <> machine.oflag)) target
  | Jl target ->
      (* SF ^ OF *)
      emulate_jmp (machine.sflag <> machine.oflag) target
  | Jle target ->
      (* (SF ^ OF) | ZF *)
      emulate_jmp (machine.sflag <> machine.oflag || machine.zflag) target
  | Call target ->
      let fun_pc = label_to_index target in
      let ret_addr = machine.pc + 1 in
      machine.sp <- machine.sp + 1;
      store_stack machine.sp ret_addr;
      machine.pc <- fun_pc
  | Ret ->
      let ret_addr = load_stack machine.sp in
      machine.sp <- machine.sp - 1;
      machine.pc <- ret_addr
  (* miscellaneous *)
  | Hlt -> machine.halted <- true
  | Out src ->
      (* add value to decimal display history *)
      let src_value = load_reg src in
      machine.dec_disp_history <- src_value :: machine.dec_disp_history;
      (* at verbosity level 1, out instrs print their output *)
      if verbosity >= 1 then printf "%s %d\n" (Colors.log "[output]") src_value
      else ();
      inc_pc ()
  | Label _ | Nop -> inc_pc ()
  (* XXX: Dic and Did not currently supported *)
  | Dic _ | Did _ -> inc_pc ()
  | _ -> raise (EmulatorError (InvalidInstr ins))

(* [map_labels] constructs an environment that maps label names to indices in
  the program's list of instructions *)
let map_labels (instrs : instr list) =
  (* pair instructions with their indices in the program *)
  List.mapi (fun i ins -> (i, ins)) instrs
  (* accumulate an environment of labels->indices *)
  |> List.fold_left
       (fun env (i, ins) ->
         match ins with
         | Label name ->
             (* map each label to the index following it *)
             if Env.mem name env then
               raise (EmulatorError (DuplicateLabel name))
             else Env.add name (i + 1) env
         | _ -> env)
       Env.empty

(* [get_current_ins] retrieves the current instruction to execute 
  by indexing into the program using the machine's program counter *)
let get_current_ins (pgrm : instr list) (machine : stew_3000) : instr =
  try List.nth pgrm machine.pc
  with Failure _ | Invalid_argument _ ->
    raise (EmulatorError (InvalidProgramCounter machine))

(* [emulate_program] emulates running the given assembly program 
  on the Stew 3000, and returns the final machine state after the run.
  verbosity indicates how much logging should happen during the run. *)
let emulate_program (pgrm : instr list) (verbosity : int) : stew_3000 =
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
