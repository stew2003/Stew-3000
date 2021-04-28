open Isa

type asm_err =
  | ProgramTooLarge of int
  | DuplicateLabel of string
  | InvalidInstr of instr
  | InvalidImm of immediate
  | InvalidStackOffset of immediate
  | InvalidTarget of string

exception AssembleError of asm_err

(* 256 bytes is the size of our program memory *)
let max_pgrm_size = 256

(* [validate_imm] checks that an immediate value is within the representable 
  range [-128, 128), and throws AssembleError InvalidImm if not *)
let validate_imm (imm : immediate) : immediate =
  if imm >= -128 && imm < 128 then imm
  else raise (AssembleError (InvalidImm imm))

(* [validate_stack_offset] checks that an offset from the stack pointer
  is within the valid range [0, 256), and throws an AssembleError otherwise *)
let validate_stack_offset (off : immediate) : immediate =
  if off >= 0 && off < 256 then off
  else raise (AssembleError (InvalidStackOffset off))

(* [assemble_instr] generates bytes for a given instruction. Raises 
  AssembleError InvalidInstr if given an un-assemblable instruction *)
let assemble_instr (ins : instr) (label_map : (string, int) Hashtbl.t) :
    int list =
  (* [to_addr] converts a string label to its corresponding address,
     raising an assembler error if the label_map has no mapping *)
  let to_addr (label : string) =
    match Hashtbl.find_opt label_map label with
    | Some addr -> addr
    | None -> raise (AssembleError (InvalidTarget label))
  in
  match ins with
  (* Add src, dest *)
  | Add (A, A) -> [ 0x00 ]
  | Add (A, B) -> [ 0x01 ]
  | Add (A, C) -> [ 0x02 ]
  | Add (A, SP) -> [ 0x03 ]
  | Add (B, A) -> [ 0x04 ]
  | Add (B, B) -> [ 0x05 ]
  | Add (B, C) -> [ 0x06 ]
  | Add (B, SP) -> [ 0x07 ]
  | Add (C, A) -> [ 0x08 ]
  | Add (C, B) -> [ 0x09 ]
  | Add (C, C) -> [ 0x0a ]
  | Add (C, SP) -> [ 0x0b ]
  (* Addi byte, dest *)
  | Addi (imm, A) -> [ 0x0c; validate_imm imm ]
  | Addi (imm, B) -> [ 0x0d; validate_imm imm ]
  | Addi (imm, C) -> [ 0x0e; validate_imm imm ]
  | Addi (imm, SP) -> [ 0x0f; validate_imm imm ]
  (* Sub src, dest *)
  | Sub (B, A) -> [ 0x10 ]
  | Sub (C, A) -> [ 0x11 ]
  | Sub (A, B) -> [ 0x12 ]
  | Sub (C, B) -> [ 0x13 ]
  | Sub (A, C) -> [ 0x14 ]
  | Sub (B, C) -> [ 0x15 ]
  | Sub (A, SP) -> [ 0x16 ]
  | Sub (B, SP) -> [ 0x17 ]
  | Sub (C, SP) -> [ 0x18 ]
  (* Subi byte, dest *)
  | Subi (imm, A) -> [ 0x19; validate_imm imm ]
  | Subi (imm, B) -> [ 0x1a; validate_imm imm ]
  | Subi (imm, C) -> [ 0x1b; validate_imm imm ]
  | Subi (imm, SP) -> [ 0x1c; validate_imm imm ]
  (* And src, dest *)
  | And (B, A) -> [ 0x1d ]
  | And (C, A) -> [ 0x1e ]
  | And (A, B) -> [ 0x1f ]
  | And (C, B) -> [ 0x20 ]
  | And (A, C) -> [ 0x21 ]
  | And (B, C) -> [ 0x22 ]
  (* Ani byte, dest *)
  | Ani (imm, A) -> [ 0x23; validate_imm imm ]
  | Ani (imm, B) -> [ 0x24; validate_imm imm ]
  | Ani (imm, C) -> [ 0x25; validate_imm imm ]
  (* Or src, dest *)
  | Or (B, A) -> [ 0x26 ]
  | Or (C, A) -> [ 0x27 ]
  | Or (A, B) -> [ 0x28 ]
  | Or (C, B) -> [ 0x29 ]
  | Or (A, C) -> [ 0x2a ]
  | Or (B, C) -> [ 0x2b ]
  (* Ori byte, dest *)
  | Ori (imm, A) -> [ 0x2c; validate_imm imm ]
  | Ori (imm, B) -> [ 0x2d; validate_imm imm ]
  | Ori (imm, C) -> [ 0x2e; validate_imm imm ]
  (* Xor src, dest *)
  | Xor (B, A) -> [ 0x2f ]
  | Xor (C, A) -> [ 0x30 ]
  | Xor (A, B) -> [ 0x31 ]
  | Xor (C, B) -> [ 0x32 ]
  | Xor (A, C) -> [ 0x33 ]
  | Xor (B, C) -> [ 0x34 ]
  (* Xri byte, dest *)
  | Xri (imm, A) -> [ 0x35; validate_imm imm ]
  | Xri (imm, B) -> [ 0x36; validate_imm imm ]
  | Xri (imm, C) -> [ 0x37; validate_imm imm ]
  (* Not dest *)
  | Not A -> [ 0x38 ]
  | Not B -> [ 0x39 ]
  | Not C -> [ 0x3a ]
  (* Inr dest *)
  | Inr A -> [ 0x3b ]
  | Inr B -> [ 0x3c ]
  | Inr C -> [ 0x3d ]
  | Inr SP -> [ 0x3e ]
  (* Dcr dest *)
  | Dcr A -> [ 0x3f ]
  | Dcr B -> [ 0x40 ]
  | Dcr C -> [ 0x41 ]
  | Dcr SP -> [ 0x42 ]
  (* Mov src, dest *)
  | Mov (A, B) -> [ 0x43 ]
  | Mov (A, C) -> [ 0x44 ]
  | Mov (B, A) -> [ 0x45 ]
  | Mov (B, C) -> [ 0x46 ]
  | Mov (C, A) -> [ 0x47 ]
  | Mov (C, B) -> [ 0x48 ]
  (* Mvi byte, dest *)
  | Mvi (imm, A) -> [ 0x49; validate_imm imm ]
  | Mvi (imm, B) -> [ 0x4a; validate_imm imm ]
  | Mvi (imm, C) -> [ 0x4b; validate_imm imm ]
  (* Ld src, dest *)
  | Ld (A, A) -> [ 0x4c ]
  | Ld (B, A) -> [ 0x4d ]
  | Ld (C, A) -> [ 0x4e ]
  | Ld (A, B) -> [ 0x4f ]
  | Ld (B, B) -> [ 0x50 ]
  | Ld (C, B) -> [ 0x51 ]
  | Ld (A, C) -> [ 0x52 ]
  | Ld (B, C) -> [ 0x53 ]
  | Ld (C, C) -> [ 0x54 ]
  (* St src, dest *)
  | St (A, A) -> [ 0x55 ]
  | St (A, B) -> [ 0x56 ]
  | St (A, C) -> [ 0x57 ]
  | St (B, A) -> [ 0x58 ]
  | St (B, B) -> [ 0x59 ]
  | St (B, C) -> [ 0x5a ]
  | St (C, A) -> [ 0x5b ]
  | St (C, B) -> [ 0x5c ]
  | St (C, C) -> [ 0x5d ]
  (* Lds byte, dest *)
  | Lds (imm, A) -> [ 0x5e; validate_stack_offset imm ]
  | Lds (imm, B) -> [ 0x5f; validate_stack_offset imm ]
  | Lds (imm, C) -> [ 0x60; validate_stack_offset imm ]
  (* Sts src, byte *)
  | Sts (A, imm) -> [ 0x61; validate_stack_offset imm ]
  | Sts (B, imm) -> [ 0x62; validate_stack_offset imm ]
  | Sts (C, imm) -> [ 0x63; validate_stack_offset imm ]
  (* Cmp left, right *)
  | Cmp (A, B) -> [ 0x64 ]
  | Cmp (A, C) -> [ 0x65 ]
  | Cmp (B, A) -> [ 0x66 ]
  | Cmp (B, C) -> [ 0x67 ]
  | Cmp (C, A) -> [ 0x68 ]
  | Cmp (C, B) -> [ 0x69 ]
  (* Cmpi byte, reg or Cmpi reg, byte *)
  | Cmpi (Reg A, Imm imm) -> [ 0x6a; validate_imm imm ]
  | Cmpi (Imm imm, Reg A) -> [ 0x6b; validate_imm imm ]
  | Cmpi (Reg B, Imm imm) -> [ 0x6c; validate_imm imm ]
  | Cmpi (Imm imm, Reg B) -> [ 0x6d; validate_imm imm ]
  | Cmpi (Reg C, Imm imm) -> [ 0x6e; validate_imm imm ]
  | Cmpi (Imm imm, Reg C) -> [ 0x6f; validate_imm imm ]
  (* Jumps *)
  | Jmp label -> [ 0x70; to_addr label ]
  | Je label -> [ 0x71; to_addr label ]
  | Jne label -> [ 0x72; to_addr label ]
  | Jg label -> [ 0x73; to_addr label ]
  | Jge label -> [ 0x74; to_addr label ]
  | Jl label -> [ 0x75; to_addr label ]
  | Jle label -> [ 0x76; to_addr label ]
  (* Call and return *)
  | Call label -> [ 0x77; to_addr label ]
  | Ret -> [ 0x78 ]
  (* Out src *)
  | Out A -> [ 0x79 ]
  | Out B -> [ 0x7a ]
  | Out C -> [ 0x7b ]
  (* Misc. *)
  | Dic imm -> [ 0x7c; imm ]
  | Did imm -> [ 0x7d; imm ]
  | Hlt -> [ 0x7e ]
  | Nop -> [ 0x7f ]
  (* Labels don't appear in the assembled program *)
  | Label _ -> []
  (* unrecognized instruction *)
  | _ -> raise (AssembleError (InvalidInstr ins))

(* [assemble_to_list] converts the given instructions to a list of 
  integers representing the assembled bytes of the program. *)
let assemble_to_list (instrs : instr list) (label_map : (string, int) Hashtbl.t)
    : int list =
  instrs |> List.map (fun ins -> assemble_instr ins label_map) |> List.concat

(* [size_of] determines the size (in bytes) of the given instruction *)
let size_of (ins : instr) : int =
  match ins with
  (* labels don't appear in assembled program *)
  | Label _ -> 0
  (* one-byte instructions *)
  | Add _ | Sub _ | And _ | Or _ | Xor _ | Mov _ | Ld _ | St _ | Cmp _ | Not _
  | Inr _ | Dcr _ | Ret | Hlt | Nop | Out _ ->
      1
  (* two-byte instructions *)
  | Addi _ | Subi _ | Ani _ | Ori _ | Xri _ | Mvi _ | Lds _ | Sts _ | Jmp _
  | Je _ | Jne _ | Jg _ | Jge _ | Jl _ | Jle _ | Call _ | Dic _ | Did _ | Cmpi _
    ->
      2

(* [map_labels] constructs a Hashtbl that maps label names to memory addresses *)
let map_labels (instrs : instr list) =
  let map = Hashtbl.create (List.length instrs) in
  (* [populate_map] iterates over the list of instructions,
     add mappings as it encounters labels and otherwise keeping
     a running byte_pos, which is the current address in the binary *)
  let rec populate_map (byte_pos : int) (instrs : instr list) =
    match instrs with
    | [] -> ()
    | ins :: rest -> (
        match ins with
        | Label name ->
            (* if label already mapped, this is an error *)
            if Hashtbl.mem map name then
              raise (AssembleError (DuplicateLabel name))
            else Hashtbl.add map name byte_pos;
            populate_map byte_pos rest
        | _ -> populate_map (byte_pos + size_of ins) rest)
  in
  (* populate map starting from byte position 0 *)
  populate_map 0 instrs;
  map

(* [bytes_from_list] constructs a buffer of raw bytes from
  the given list of integers *)
let bytes_from_list (l : int list) : bytes =
  let buf = Bytes.create (List.length l) in
  List.mapi (fun i b -> Bytes.set_int8 buf i b) l |> ignore;
  buf

(* [assemble] processes a list of asm instructions and 
  produces a byte sequence representing the program in binary form *)
let assemble (instrs : instr list) : bytes =
  let label_map = map_labels instrs in
  let bytes_as_list = assemble_to_list instrs label_map in
  (* check assembled program size to ensure it can fit *)
  let size = List.length bytes_as_list in
  if size > max_pgrm_size then raise (AssembleError (ProgramTooLarge size))
  else bytes_from_list bytes_as_list
