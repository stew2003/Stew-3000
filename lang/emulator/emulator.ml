open Asm.Isa
open Printf

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
  let string_of_int_list lst =
    "[" ^ (List.map string_of_int lst |> String.concat ", ") ^ "]"
  in
  let bool_to_int (b : bool) = if b then 1 else 0 in
  sprintf
    "Machine State:\n\
     a: %d\n\
     b: %d\n\
     c: %d\n\
     sp: %d\n\
     zf: %d\n\
     sf: %d\n\
     of: %d\n\
     pc: %d\n\
     halted? %s\n\
     dec. display history: %s\n\
     stack:\n\
     %s"
    machine.a machine.b machine.c machine.sp
    (bool_to_int machine.zflag)
    (bool_to_int machine.sflag)
    (bool_to_int machine.oflag)
    machine.pc
    (if machine.halted then "yes" else "no")
    (string_of_int_list machine.dec_disp_history)
    (string_of_int_list (Array.to_list machine.stack))

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
  | InvalidStackOffset of immediate
  | InvalidStackAccess of int * stew_3000
  | InvalidInstr of instr

exception EmulatorError of emu_err

let string_of_emu_err (err : emu_err) =
  match err with
  | DuplicateLabel label -> sprintf "duplicate label: %s" label
  | InvalidProgramCounter machine ->
      sprintf "invalid program counter: %s" (string_of_stew_3000 machine)
  | InvalidTarget label -> sprintf "invalid target: %s" label
  | InvalidImm imm -> sprintf "invalid immediate value: %s" (string_of_imm imm)
  | InvalidStackOffset off ->
      sprintf "invalid stack offset: %s" (string_of_imm off)
  | InvalidStackAccess (addr, machine) ->
      sprintf "invalid stack access at address %s: %s" (string_of_int addr)
        (string_of_stew_3000 machine)
  | InvalidInstr ins -> sprintf "invalid instruction: %s" (string_of_instr ins)

(* [validate_instr] checks if a given instruction is a valid
  instruction for which we have an opcode, and errors otherwise.
  NOTE: this does not check immediates *)
let validate_instr (ins : instr) =
  match ins with
  (* Add src, dest *)
  | Add (A, A)
  | Add (A, B)
  | Add (A, C)
  | Add (A, SP)
  | Add (B, A)
  | Add (B, B)
  | Add (B, C)
  | Add (B, SP)
  | Add (C, A)
  | Add (C, B)
  | Add (C, C)
  | Add (C, SP)
  (* Addi byte, dest *)
  | Addi (_, A)
  | Addi (_, B)
  | Addi (_, C)
  | Addi (_, SP)
  (* Sub src, dest *)
  | Sub (B, A)
  | Sub (C, A)
  | Sub (A, B)
  | Sub (C, B)
  | Sub (A, C)
  | Sub (B, C)
  | Sub (A, SP)
  | Sub (B, SP)
  | Sub (C, SP)
  (* Subi byte, dest *)
  | Subi (_, A)
  | Subi (_, B)
  | Subi (_, C)
  | Subi (_, SP)
  (* And src, dest *)
  | And (B, A)
  | And (C, A)
  | And (A, B)
  | And (C, B)
  | And (A, C)
  | And (B, C)
  (* Ani byte, dest *)
  | Ani (_, A)
  | Ani (_, B)
  | Ani (_, C)
  (* Or src, dest *)
  | Or (B, A)
  | Or (C, A)
  | Or (A, B)
  | Or (C, B)
  | Or (A, C)
  | Or (B, C)
  (* Ori byte, dest *)
  | Ori (_, A)
  | Ori (_, B)
  | Ori (_, C)
  (* Xor src, dest *)
  | Xor (B, A)
  | Xor (C, A)
  | Xor (A, B)
  | Xor (C, B)
  | Xor (A, C)
  | Xor (B, C)
  (* Xri byte, dest *)
  | Xri (_, A)
  | Xri (_, B)
  | Xri (_, C)
  (* Not dest *)
  | Not A
  | Not B
  | Not C
  (* Inr dest *)
  | Inr A
  | Inr B
  | Inr C
  | Inr SP
  (* Dcr dest *)
  | Dcr A
  | Dcr B
  | Dcr C
  | Dcr SP
  (* Mov src, dest *)
  | Mov (A, B)
  | Mov (A, C)
  | Mov (B, A)
  | Mov (B, C)
  | Mov (C, A)
  | Mov (C, B)
  (* Mvi byte, dest *)
  | Mvi (_, A)
  | Mvi (_, B)
  | Mvi (_, C)
  (* Ld src, dest *)
  | Ld (A, A)
  | Ld (B, A)
  | Ld (C, A)
  | Ld (A, B)
  | Ld (B, B)
  | Ld (C, B)
  | Ld (A, C)
  | Ld (B, C)
  | Ld (C, C)
  (* St src, dest *)
  | St (A, A)
  | St (A, B)
  | St (A, C)
  | St (B, A)
  | St (B, B)
  | St (B, C)
  | St (C, A)
  | St (C, B)
  | St (C, C)
  (* Lds byte, dest *)
  | Lds (_, A)
  | Lds (_, B)
  | Lds (_, C)
  (* Sts src, byte *)
  | Sts (A, _)
  | Sts (B, _)
  | Sts (C, _)
  (* Cmp left, right *)
  | Cmp (A, B)
  | Cmp (A, C)
  | Cmp (B, A)
  | Cmp (B, C)
  | Cmp (C, A)
  | Cmp (C, B)
  (* Cmpi byte, reg or Cmpi reg, byte *)
  | Cmpi (Reg A, Imm _)
  | Cmpi (Imm _, Reg A)
  | Cmpi (Reg B, Imm _)
  | Cmpi (Imm _, Reg B)
  | Cmpi (Reg C, Imm _)
  | Cmpi (Imm _, Reg C)
  (* Jumps *)
  | Jmp _ | Je _ | Jne _ | Jg _ | Jge _ | Jl _ | Jle _
  (* Call and return *)
  | Call _ | Ret
  (* Out src *)
  | Out A
  | Out B
  | Out C
  (* Misc. *)
  | Dic _ | Did _ | Hlt | Nop | Label _ ->
      ()
  (* unrecognized instruction *)
  | _ -> raise (EmulatorError (InvalidInstr ins))

(* [emulate_instr] emulates the effect of the given instruction
  on the machine, by mutating the machine in-place *)
let emulate_instr (ins : instr) (machine : stew_3000)
    (label_map : (string, int) Hashtbl.t) =
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
  (* [as_unsigned] interprets the given value as an unsigned 8-bit integer *)
  let as_unsigned (value : int) : int =
    if value < 0 then 256 - (value mod 256) else value mod 256
  in
  (* [label_to_index] converts a string label to its corresponding index,
     erroring if there is no index for the label *)
  let label_to_index (label : string) =
    match Hashtbl.find_opt label_map label with
    | Some index -> index
    | None -> raise (EmulatorError (InvalidTarget label))
  in
  (* [validate_imm] checks that an immediate value is within the representable
     range [-128, 128), and raises an emulator error if not *)
  let validate_imm (imm : immediate) : immediate =
    if imm >= -128 && imm < 128 then imm
    else raise (EmulatorError (InvalidImm imm))
  in
  (* [validate_stack_offset] checks that an offset from the stack pointer
     is within the valid range [0, 256), and raises an error if not *)
  let validate_stack_offset (off : immediate) : immediate =
    if off >= 0 && off < 256 then off
    else raise (EmulatorError (InvalidStackOffset off))
  in
  (* [simulate_overflow] checks if a value has 8-bit signed overflow,
     and keeps it within the representable range [-128, 128),
     also setting the overflow flag if an overflow has occurred *)
  let simulate_overflow (value : int) : int =
    if value >= 128 then (
      machine.oflag <- true;
      -128 + (value - 128))
    else if value <= -129 then (
      machine.oflag <- true;
      127 - (-129 - value))
    else (
      machine.oflag <- false;
      value)
  in
  (* [set_flags] updates the machine's flags after performing
     an arithmetic operation between left/right that produced result *)
  let set_flags (left : int) (right : int) (result : int) =
    machine.zflag <- result = 0;
    machine.sflag <- result < 0;

    (* https://stackoverflow.com/questions/1633561/how-to-detect-overflow
       -when-subtracting-two-signed-32-bit-numbers-in-c *)
    machine.oflag <- result < left <> (right > 0)
  in
  (* [set_flags_unary] updates machine flags after a unary operation (inr, dcr, not)
     NOTE: oflag is set in simulate_overflow, which covers the inr/dcr cases *)
  let set_flags_unary (result : int) =
    machine.zflag <- result = 0;
    machine.sflag <- result < 0
  in
  (* [inc_pc] increments the program counter by 1 *)
  let inc_pc _ = machine.pc <- machine.pc + 1 in
  (* first, ensure that the instruction is
     representable in our instruction set *)
  validate_instr ins;
  (* simulate the effects of the instruction*)
  match ins with
  | Add (src, dest) ->
      let src_value = load_reg src in
      let dest_value = load_reg dest in
      let sum = simulate_overflow (src_value + dest_value) in
      store_reg dest sum;
      set_flags src_value dest_value sum;
      inc_pc ()
  | Addi (imm, dest) ->
      let dest_value = load_reg dest in
      let sum = simulate_overflow (validate_imm imm + dest_value) in
      store_reg dest sum;
      set_flags imm dest_value sum;
      inc_pc ()
  | Sub (src, dest) ->
      let src_value = load_reg src in
      let dest_value = load_reg dest in
      let diff = simulate_overflow (dest_value - src_value) in
      store_reg dest diff;
      set_flags dest_value src_value diff;
      inc_pc ()
  | Subi (imm, dest) ->
      let dest_value = load_reg dest in
      let diff = simulate_overflow (dest_value - validate_imm imm) in
      store_reg dest diff;
      set_flags dest_value imm diff;
      inc_pc ()
  | And (src, dest) ->
      let dest_value = load_reg dest in
      let src_value = load_reg src in
      let anded = src_value land dest_value in
      store_reg dest anded;
      set_flags src_value dest_value anded;
      inc_pc ()
  | Ani (imm, dest) ->
      let dest_value = load_reg dest in
      let anded = validate_imm imm land dest_value in
      store_reg dest anded;
      set_flags imm dest_value anded;
      inc_pc ()
  | Or (src, dest) ->
      let src_value = load_reg src in
      let dest_value = load_reg dest in
      let ored = src_value lor dest_value in
      store_reg dest ored;
      set_flags src_value dest_value ored;
      inc_pc ()
  | Ori (imm, dest) ->
      let dest_value = load_reg dest in
      let ored = validate_imm imm lor dest_value in
      store_reg dest ored;
      set_flags imm dest_value ored;
      inc_pc ()
  | Xor (src, dest) ->
      let src_value = load_reg src in
      let dest_value = load_reg dest in
      let xored = src_value lxor dest_value in
      store_reg dest xored;
      set_flags src_value dest_value xored;
      inc_pc ()
  | Xri (imm, dest) ->
      let dest_value = load_reg dest in
      let xored = validate_imm imm lxor dest_value in
      store_reg dest xored;
      set_flags imm dest_value xored;
      inc_pc ()
  | Not dest ->
      let dest_value = load_reg dest in
      let notted = lnot dest_value in
      store_reg dest notted;
      set_flags_unary notted;
      inc_pc ()
  | Inr dest ->
      let dest_value = load_reg dest in
      let inc = simulate_overflow (dest_value + 1) in
      store_reg dest inc;
      set_flags_unary inc;
      inc_pc ()
  | Dcr dest ->
      let dest_value = load_reg dest in
      let dec = simulate_overflow (dest_value - 1) in
      store_reg dest dec;
      set_flags_unary dec;
      inc_pc ()
  | Mov (src, dest) ->
      store_reg dest (load_reg src);
      inc_pc ()
  | Mvi (imm, dest) ->
      store_reg dest (validate_imm imm);
      inc_pc ()
  | Ld (src, dest) ->
      let src_value = as_unsigned (load_reg src) in
      let mem = load_stack src_value in
      store_reg dest mem;
      inc_pc ()
  | St (src, dest) ->
      let src_value = load_reg src in
      let dest_value = as_unsigned (load_reg dest) in
      store_stack dest_value src_value;
      inc_pc ()
  | Lds (imm, dest) ->
      let mem =
        load_stack (as_unsigned (machine.sp + validate_stack_offset imm))
      in
      store_reg dest mem;
      inc_pc ()
  | Sts (src, imm) ->
      let src_value = load_reg src in
      store_stack
        (as_unsigned (machine.sp + validate_stack_offset imm))
        src_value;
      inc_pc ()
  | Cmp (left, right) ->
      let left_value = load_reg left in
      let right_value = load_reg right in
      let diff = simulate_overflow (left_value - right_value) in
      set_flags left_value right_value diff;
      inc_pc ()
  | Cmpi (Imm imm, Reg right) ->
      let right_value = load_reg right in
      let diff = simulate_overflow (validate_imm imm - right_value) in
      set_flags imm right_value diff;
      inc_pc ()
  | Cmpi (Reg left, Imm imm) ->
      let left_value = load_reg left in
      let diff = simulate_overflow (left_value - validate_imm imm) in
      set_flags left_value imm diff;
      inc_pc ()
  | Label _ | Nop -> inc_pc ()
  | Jmp target -> machine.pc <- label_to_index target
  | Je target ->
      machine.pc <-
        (if machine.zflag then label_to_index target else machine.pc + 1)
  | Jne target ->
      machine.pc <-
        (if not machine.zflag then label_to_index target else machine.pc + 1)
  | Jg target ->
      machine.pc <-
        (* ~(SF ^ OF) & ~ZF *)
        (if (not (machine.sflag <> machine.oflag)) && not machine.zflag then
         label_to_index target
        else machine.pc + 1)
  | Jge target ->
      machine.pc <-
        (* ~(SF ^ OF) *)
        (if not (machine.sflag <> machine.oflag) then label_to_index target
        else machine.pc + 1)
  | Jl target ->
      machine.pc <-
        (* SF ^ OF *)
        (if machine.sflag <> machine.oflag then label_to_index target
        else machine.pc + 1)
  | Jle target ->
      machine.pc <-
        (* (SF ^ OF) | ZF *)
        (if machine.sflag <> machine.oflag || machine.zflag then
         label_to_index target
        else machine.pc + 1)
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
  | Hlt -> machine.halted <- true
  | Out src ->
      (* add value to decimal display history *)
      let src_value = load_reg src in
      machine.dec_disp_history <- src_value :: machine.dec_disp_history;
      inc_pc ()
  (* XXX: not currently supported *)
  | Dic _ | Did _ -> inc_pc ()
  | _ -> raise (EmulatorError (InvalidInstr ins))

(* [map_labels] constructs a Hashtbl that maps label names to indices in
  the program's list of instructions *)
let map_labels (instrs : instr list) =
  let map = Hashtbl.create (List.length instrs) in
  let _ =
    List.mapi
      (fun i ins ->
        match ins with
        | Label name ->
            (* map each label to the index following it *)
            if Hashtbl.mem map name then
              raise (EmulatorError (DuplicateLabel name))
            else Hashtbl.add map name (i + 1)
        | _ -> ())
      instrs
  in
  map

(* [get_current_ins] retrieves the current instruction to execute 
  by indexing into the program using the machine's program counter *)
let get_current_ins (pgrm : instr list) (machine : stew_3000) : instr =
  try List.nth pgrm machine.pc
  with Failure _ | Invalid_argument _ ->
    raise (EmulatorError (InvalidProgramCounter machine))

(* [emulate_program] emulates running the given assembly program 
  on the Stew 3000, and returns the final machine state after the run *)
let emulate_program (pgrm : instr list) : stew_3000 =
  let machine = new_stew_3000 () in
  let label_map = map_labels pgrm in
  let rec run _ =
    if machine.halted then ()
    else
      let ins = get_current_ins pgrm machine in
      emulate_instr ins machine label_map;
      run ()
  in
  run ();
  machine
