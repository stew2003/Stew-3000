open Isa
open Printf
open Validate
open Util.Env

type asm_err =
  | ProgramTooLarge of int
  | DuplicateLabel of string
  | InvalidInstr of instr
  | InvalidImm of immediate
  | InvalidStackOffset of immediate
  | InvalidTarget of string

exception AssembleError of asm_err

(* [string_of_asm_err] converts an assemble error internal into a printable message *)
let string_of_asm_err = function
  | ProgramTooLarge size ->
      sprintf "assembled program was too large (%d bytes)" size
  | DuplicateLabel label -> sprintf "label `%s` appears more than once" label
  | InvalidInstr ins -> sprintf "invalid instruction: %s" (string_of_instr ins)
  | InvalidImm imm -> sprintf "invalid immediate value: %s" (string_of_imm imm)
  | InvalidStackOffset off ->
      sprintf "invalid stack offset: %s" (string_of_imm off)
  | InvalidTarget label -> sprintf "invalid target: %s" label

(* [assemble_instr] generates bytes for a given instruction. Raises 
  AssembleError InvalidInstr if given an un-assemblable instruction *)
let assemble_instr (ins : instr) (label_map : int env) : int list =
  (* [to_addr] converts a string label to its corresponding address,
     raising an assembler error if the label_map has no mapping *)
  let to_addr (label : string) =
    match Env.find_opt label label_map with
    | Some addr -> addr
    | None -> raise (AssembleError (InvalidTarget label))
  in

  (* first, ensure that the instruction is valid *)
  (try validate_instr ins with
  | ValidityError (InvalidImm imm) -> raise (AssembleError (InvalidImm imm))
  | ValidityError (InvalidStackOffset off) ->
      raise (AssembleError (InvalidStackOffset off))
  | ValidityError (InvalidInstr ins) -> raise (AssembleError (InvalidInstr ins)));

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
  | Addi (imm, A) -> [ 0x0c; imm ]
  | Addi (imm, B) -> [ 0x0d; imm ]
  | Addi (imm, C) -> [ 0x0e; imm ]
  | Addi (imm, SP) -> [ 0x0f; imm ]
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
  | Subi (imm, A) -> [ 0x19; imm ]
  | Subi (imm, B) -> [ 0x1a; imm ]
  | Subi (imm, C) -> [ 0x1b; imm ]
  | Subi (imm, SP) -> [ 0x1c; imm ]
  (* And src, dest *)
  | And (B, A) -> [ 0x1d ]
  | And (C, A) -> [ 0x1e ]
  | And (A, B) -> [ 0x1f ]
  | And (C, B) -> [ 0x20 ]
  | And (A, C) -> [ 0x21 ]
  | And (B, C) -> [ 0x22 ]
  (* Ani byte, dest *)
  | Ani (imm, A) -> [ 0x23; imm ]
  | Ani (imm, B) -> [ 0x24; imm ]
  | Ani (imm, C) -> [ 0x25; imm ]
  (* Or src, dest *)
  | Or (B, A) -> [ 0x26 ]
  | Or (C, A) -> [ 0x27 ]
  | Or (A, B) -> [ 0x28 ]
  | Or (C, B) -> [ 0x29 ]
  | Or (A, C) -> [ 0x2a ]
  | Or (B, C) -> [ 0x2b ]
  (* Ori byte, dest *)
  | Ori (imm, A) -> [ 0x2c; imm ]
  | Ori (imm, B) -> [ 0x2d; imm ]
  | Ori (imm, C) -> [ 0x2e; imm ]
  (* Xor src, dest *)
  | Xor (B, A) -> [ 0x2f ]
  | Xor (C, A) -> [ 0x30 ]
  | Xor (A, B) -> [ 0x31 ]
  | Xor (C, B) -> [ 0x32 ]
  | Xor (A, C) -> [ 0x33 ]
  | Xor (B, C) -> [ 0x34 ]
  (* Xri byte, dest *)
  | Xri (imm, A) -> [ 0x35; imm ]
  | Xri (imm, B) -> [ 0x36; imm ]
  | Xri (imm, C) -> [ 0x37; imm ]
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
  | Mvi (imm, A) -> [ 0x49; imm ]
  | Mvi (imm, B) -> [ 0x4a; imm ]
  | Mvi (imm, C) -> [ 0x4b; imm ]
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
  | Lds (imm, A) -> [ 0x5e; imm ]
  | Lds (imm, B) -> [ 0x5f; imm ]
  | Lds (imm, C) -> [ 0x60; imm ]
  (* Sts src, byte *)
  | Sts (A, imm) -> [ 0x61; imm ]
  | Sts (B, imm) -> [ 0x62; imm ]
  | Sts (C, imm) -> [ 0x63; imm ]
  (* Cmp left, right *)
  | Cmp (A, B) -> [ 0x64 ]
  | Cmp (A, C) -> [ 0x65 ]
  | Cmp (B, A) -> [ 0x66 ]
  | Cmp (B, C) -> [ 0x67 ]
  | Cmp (C, A) -> [ 0x68 ]
  | Cmp (C, B) -> [ 0x69 ]
  (* Cmpi byte, reg or Cmpi reg, byte *)
  | Cmpi (Reg A, Imm imm) -> [ 0x6a; imm ]
  | Cmpi (Imm imm, Reg A) -> [ 0x6b; imm ]
  | Cmpi (Reg B, Imm imm) -> [ 0x6c; imm ]
  | Cmpi (Imm imm, Reg B) -> [ 0x6d; imm ]
  | Cmpi (Reg C, Imm imm) -> [ 0x6e; imm ]
  | Cmpi (Imm imm, Reg C) -> [ 0x6f; imm ]
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
let assemble_to_list (instrs : instr list) (label_map : int env) : int list =
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

(* [map_labels] constructs an environment mapping label names to memory addresses *)
let map_labels (instrs : instr list) =
  (* accumulate an environment of labels->addresses *)
  let env, _ =
    List.fold_left
      (fun (env, addr) ins ->
        match ins with
        | Label name ->
            (* if label already mapped, this is an error *)
            if Env.mem name env then raise (AssembleError (DuplicateLabel name))
            else (Env.add name addr env, addr)
        | _ -> (env, addr + size_of ins))
      (Env.empty, 0) instrs
  in
  env

(* [bytes_from_list] constructs a buffer of raw bytes from
  the given list of integers *)
let bytes_from_list (l : int list) : bytes =
  let buf = Bytes.create (List.length l) in
  List.mapi (fun i b -> Bytes.set_int8 buf i b) l |> ignore;
  buf

(* 256 bytes is the size of our program memory *)
let max_pgrm_size = 256

(* [assemble] processes a list of asm instructions and 
  produces a byte sequence representing the program in binary form *)
let assemble (instrs : instr list) : bytes =
  let label_map = map_labels instrs in
  let bytes_as_list = assemble_to_list instrs label_map in
  (* check assembled program size to ensure it can fit *)
  let size = List.length bytes_as_list in
  if size > max_pgrm_size then raise (AssembleError (ProgramTooLarge size))
  else bytes_from_list bytes_as_list
