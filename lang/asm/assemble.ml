open Isa
open Printf
open Warnings
open Validate
open Util.Env
open Util.Srcloc

type asm_err =
  | DuplicateLabel of string
  | InvalidInstr of instr
  | InvalidImm of immediate
  | InvalidTarget of string

exception AssembleError of asm_err with_loc_opt

(* [string_of_asm_err] converts an assemble error internal into a printable message *)
let string_of_asm_err = function
  | DuplicateLabel label -> sprintf "label `%s` appears more than once" label
  | InvalidInstr ins -> sprintf "invalid instruction: %s" (string_of_instr ins)
  | InvalidImm imm -> sprintf "invalid immediate value: %s" (string_of_imm imm)
  | InvalidTarget label -> sprintf "invalid target: %s" label

(* [assemble_instr] generates bytes for a given instruction. Raises
   AssembleError InvalidInstr if given an un-assemblable instruction *)
let assemble_instr (ins : instr) (label_map : int env) : int list =
  (* [to_addr] converts a string label to its corresponding address,
     raising an assembler error if the label_map has no mapping *)
  let to_addr (label : string) (loc : maybe_loc) =
    match Env.find_opt label label_map with
    | Some addr -> addr
    | None -> raise (AssembleError (InvalidTarget label, loc))
  in

  (* first, ensure that the instruction is valid *)
  (try validate_instr ins with
  | ValidityError (InvalidImm imm, maybe_loc) ->
      raise (AssembleError (InvalidImm imm, maybe_loc))
  | ValidityError (InvalidInstr ins, maybe_loc) ->
      raise (AssembleError (InvalidInstr ins, maybe_loc)));

  match ins with
  (* Add src, dest *)
  | Add (A, A, _) -> [ 0x00 ]
  | Add (A, B, _) -> [ 0x01 ]
  | Add (A, C, _) -> [ 0x02 ]
  | Add (A, SP, _) -> [ 0x03 ]
  | Add (B, A, _) -> [ 0x04 ]
  | Add (B, B, _) -> [ 0x05 ]
  | Add (B, C, _) -> [ 0x06 ]
  | Add (B, SP, _) -> [ 0x07 ]
  | Add (C, A, _) -> [ 0x08 ]
  | Add (C, B, _) -> [ 0x09 ]
  | Add (C, C, _) -> [ 0x0a ]
  | Add (C, SP, _) -> [ 0x0b ]
  (* Addi byte, dest *)
  | Addi (imm, A, _) -> [ 0x0c; imm ]
  | Addi (imm, B, _) -> [ 0x0d; imm ]
  | Addi (imm, C, _) -> [ 0x0e; imm ]
  | Addi (imm, SP, _) -> [ 0x0f; imm ]
  (* Addc src, dest *)
  | Addc (A, A, _) -> [ 0x10 ]
  | Addc (A, B, _) -> [ 0x11 ]
  | Addc (A, C, _) -> [ 0x12 ]
  | Addc (A, SP, _) -> [ 0x13 ]
  | Addc (B, A, _) -> [ 0x14 ]
  | Addc (B, B, _) -> [ 0x15 ]
  | Addc (B, C, _) -> [ 0x16 ]
  | Addc (B, SP, _) -> [ 0x17 ]
  | Addc (C, A, _) -> [ 0x18 ]
  | Addc (C, B, _) -> [ 0x19 ]
  | Addc (C, C, _) -> [ 0x1a ]
  | Addc (C, SP, _) -> [ 0x1b ]
  (* Addci byte, dest *)
  | Addci (imm, A, _) -> [ 0x1c; imm ]
  | Addci (imm, B, _) -> [ 0x1d; imm ]
  | Addci (imm, C, _) -> [ 0x1e; imm ]
  | Addci (imm, SP, _) -> [ 0x1f; imm ]
  (* Sub src, dest *)
  | Sub (B, A, _) -> [ 0x20 ]
  | Sub (C, A, _) -> [ 0x21 ]
  | Sub (A, B, _) -> [ 0x22 ]
  | Sub (C, B, _) -> [ 0x23 ]
  | Sub (A, C, _) -> [ 0x24 ]
  | Sub (B, C, _) -> [ 0x25 ]
  | Sub (A, SP, _) -> [ 0x26 ]
  | Sub (B, SP, _) -> [ 0x27 ]
  | Sub (C, SP, _) -> [ 0x28 ]
  (* Subi byte, dest *)
  | Subi (imm, A, _) -> [ 0x29; imm ]
  | Subi (imm, B, _) -> [ 0x2a; imm ]
  | Subi (imm, C, _) -> [ 0x2b; imm ]
  | Subi (imm, SP, _) -> [ 0x2c; imm ]
  (* Subb src, dest *)
  | Subb (B, A, _) -> [ 0x2d ]
  | Subb (C, A, _) -> [ 0x2e ]
  | Subb (A, B, _) -> [ 0x2f ]
  | Subb (C, B, _) -> [ 0x30 ]
  | Subb (A, C, _) -> [ 0x31 ]
  | Subb (B, C, _) -> [ 0x32 ]
  | Subb (A, SP, _) -> [ 0x33 ]
  | Subb (B, SP, _) -> [ 0x34 ]
  | Subb (C, SP, _) -> [ 0x35 ]
  (* Subbi byte, dest *)
  | Subbi (imm, A, _) -> [ 0x36; imm ]
  | Subbi (imm, B, _) -> [ 0x37; imm ]
  | Subbi (imm, C, _) -> [ 0x38; imm ]
  | Subbi (imm, SP, _) -> [ 0x39; imm ]
  (* And src, dest *)
  | And (B, A, _) -> [ 0x3a ]
  | And (C, A, _) -> [ 0x3b ]
  | And (A, B, _) -> [ 0x3c ]
  | And (C, B, _) -> [ 0x3d ]
  | And (A, C, _) -> [ 0x3e ]
  | And (B, C, _) -> [ 0x3f ]
  (* Ani byte, dest *)
  | Ani (imm, A, _) -> [ 0x40; imm ]
  | Ani (imm, B, _) -> [ 0x41; imm ]
  | Ani (imm, C, _) -> [ 0x42; imm ]
  (* Or src, dest *)
  | Or (B, A, _) -> [ 0x43 ]
  | Or (C, A, _) -> [ 0x44 ]
  | Or (A, B, _) -> [ 0x45 ]
  | Or (C, B, _) -> [ 0x46 ]
  | Or (A, C, _) -> [ 0x47 ]
  | Or (B, C, _) -> [ 0x48 ]
  (* Ori byte, dest *)
  | Ori (imm, A, _) -> [ 0x49; imm ]
  | Ori (imm, B, _) -> [ 0x4a; imm ]
  | Ori (imm, C, _) -> [ 0x4b; imm ]
  (* Xor src, dest *)
  | Xor (B, A, _) -> [ 0x4c ]
  | Xor (C, A, _) -> [ 0x4d ]
  | Xor (A, B, _) -> [ 0x4e ]
  | Xor (C, B, _) -> [ 0x4f ]
  | Xor (A, C, _) -> [ 0x50 ]
  | Xor (B, C, _) -> [ 0x51 ]
  (* Xri byte, dest *)
  | Xri (imm, A, _) -> [ 0x52; imm ]
  | Xri (imm, B, _) -> [ 0x53; imm ]
  | Xri (imm, C, _) -> [ 0x54; imm ]
  (* Not dest *)
  | Not (A, _) -> [ 0x55 ]
  | Not (B, _) -> [ 0x56 ]
  | Not (C, _) -> [ 0x57 ]
  (* Neg dest *)
  | Neg (A, _) -> [ 0x58 ]
  | Neg (B, _) -> [ 0x59 ]
  | Neg (C, _) -> [ 0x5a ]
  (* Inr dest *)
  | Inr (A, _) -> [ 0x5b ]
  | Inr (B, _) -> [ 0x5c ]
  | Inr (C, _) -> [ 0x5d ]
  | Inr (SP, _) -> [ 0x5e ]
  (* Inr2 dest *)
  | Inr2 (A, _) -> [ 0x5f ]
  | Inr2 (B, _) -> [ 0x60 ]
  | Inr2 (C, _) -> [ 0x61 ]
  | Inr2 (SP, _) -> [ 0x62 ]
  (* Inr3 dest *)
  | Inr3 (A, _) -> [ 0x63 ]
  | Inr3 (B, _) -> [ 0x64 ]
  | Inr3 (C, _) -> [ 0x65 ]
  | Inr3 (SP, _) -> [ 0x66 ]
  (* Dcr dest *)
  | Dcr (A, _) -> [ 0x67 ]
  | Dcr (B, _) -> [ 0x68 ]
  | Dcr (C, _) -> [ 0x69 ]
  | Dcr (SP, _) -> [ 0x6a ]
  (* Dcr2 dest *)
  | Dcr2 (A, _) -> [ 0x6b ]
  | Dcr2 (B, _) -> [ 0x6c ]
  | Dcr2 (C, _) -> [ 0x6d ]
  | Dcr2 (SP, _) -> [ 0x6e ]
  (* Dcr3 dest *)
  | Dcr3 (A, _) -> [ 0x6f ]
  | Dcr3 (B, _) -> [ 0x70 ]
  | Dcr3 (C, _) -> [ 0x71 ]
  | Dcr3 (SP, _) -> [ 0x72 ]
  (* Mov src, dest *)
  | Mov (A, B, _) -> [ 0x73 ]
  | Mov (A, C, _) -> [ 0x74 ]
  | Mov (B, A, _) -> [ 0x75 ]
  | Mov (B, C, _) -> [ 0x76 ]
  | Mov (C, A, _) -> [ 0x77 ]
  | Mov (C, B, _) -> [ 0x78 ]
  | Mov (Z, A, _) -> [ 0x79 ]
  | Mov (Z, B, _) -> [ 0x7a ]
  | Mov (Z, C, _) -> [ 0x7b ]
  | Mov (SP, A, _) -> [ 0x7c ]
  | Mov (SP, B, _) -> [ 0x7d ]
  | Mov (SP, C, _) -> [ 0x7e ]
  (* Mvi byte, dest *)
  | Mvi (imm, A, _) -> [ 0x7f; imm ]
  | Mvi (imm, B, _) -> [ 0x80; imm ]
  | Mvi (imm, C, _) -> [ 0x81; imm ]
  (* Ld src, dest *)
  | Ld (A, A, _) -> [ 0x82 ]
  | Ld (B, A, _) -> [ 0x83 ]
  | Ld (C, A, _) -> [ 0x84 ]
  | Ld (A, B, _) -> [ 0x85 ]
  | Ld (B, B, _) -> [ 0x86 ]
  | Ld (C, B, _) -> [ 0x87 ]
  | Ld (A, C, _) -> [ 0x88 ]
  | Ld (B, C, _) -> [ 0x89 ]
  | Ld (C, C, _) -> [ 0x8a ]
  (* St src, dest *)
  | St (A, A, _) -> [ 0x8b ]
  | St (A, B, _) -> [ 0x8c ]
  | St (A, C, _) -> [ 0x8d ]
  | St (B, A, _) -> [ 0x8e ]
  | St (B, B, _) -> [ 0x8f ]
  | St (B, C, _) -> [ 0x90 ]
  | St (C, A, _) -> [ 0x91 ]
  | St (C, B, _) -> [ 0x92 ]
  | St (C, C, _) -> [ 0x93 ]
  | St (Z, A, _) -> [ 0x94 ]
  | St (Z, B, _) -> [ 0x95 ]
  | St (Z, C, _) -> [ 0x96 ]
  (* Lds byte, dest *)
  | Lds (imm, A, _) -> [ 0x97; imm ]
  | Lds (imm, B, _) -> [ 0x98; imm ]
  | Lds (imm, C, _) -> [ 0x99; imm ]
  (* Sts src, byte *)
  | Sts (A, imm, _) -> [ 0x9a; imm ]
  | Sts (B, imm, _) -> [ 0x9b; imm ]
  | Sts (C, imm, _) -> [ 0x9c; imm ]
  | Sts (Z, imm, _) -> [ 0x9d; imm ]
  (* Stsi byte, byte *)
  | Stsi (value, offset, _) -> [ 0x9e; value; offset ]
  (* Cmp left, right *)
  | Cmp (A, B, _) -> [ 0x9f ]
  | Cmp (A, C, _) -> [ 0xa0 ]
  | Cmp (A, Z, _) -> [ 0xa1 ]
  | Cmp (B, A, _) -> [ 0xa2 ]
  | Cmp (B, C, _) -> [ 0xa3 ]
  | Cmp (B, Z, _) -> [ 0xa4 ]
  | Cmp (C, A, _) -> [ 0xa5 ]
  | Cmp (C, B, _) -> [ 0xa6 ]
  | Cmp (C, Z, _) -> [ 0xa7 ]
  | Cmp (Z, A, _) -> [ 0xa8 ]
  | Cmp (Z, B, _) -> [ 0xa9 ]
  | Cmp (Z, C, _) -> [ 0xaa ]
  (* Cmpi byte, reg or Cmpi reg, byte *)
  | Cmpi (Reg A, Imm imm, _) -> [ 0xab; imm ]
  | Cmpi (Imm imm, Reg A, _) -> [ 0xac; imm ]
  | Cmpi (Reg B, Imm imm, _) -> [ 0xad; imm ]
  | Cmpi (Imm imm, Reg B, _) -> [ 0xae; imm ]
  | Cmpi (Reg C, Imm imm, _) -> [ 0xaf; imm ]
  | Cmpi (Imm imm, Reg C, _) -> [ 0xb0; imm ]
  (* Jumps *)
  | Jmp (label, loc) -> [ 0xb1; to_addr label loc ]
  | Je (label, loc) -> [ 0xb2; to_addr label loc ]
  | Jne (label, loc) -> [ 0xb3; to_addr label loc ]
  | Jg (label, loc) -> [ 0xb4; to_addr label loc ]
  | Jge (label, loc) -> [ 0xb5; to_addr label loc ]
  | Jl (label, loc) -> [ 0xb6; to_addr label loc ]
  | Jle (label, loc) -> [ 0xb7; to_addr label loc ]
  | Ja (label, loc) -> [ 0xb8; to_addr label loc ]
  | Jae (label, loc) -> [ 0xb9; to_addr label loc ]
  | Jb (label, loc) -> [ 0xba; to_addr label loc ]
  | Jbe (label, loc) -> [ 0xbb; to_addr label loc ]
  (* Call and return *)
  | Call (label, loc) -> [ 0xbc; to_addr label loc ]
  | Ret _ -> [ 0xbd ]
  (* Out src *)
  | Out (A, _) -> [ 0xbe ]
  | Out (B, _) -> [ 0xbf ]
  | Out (C, _) -> [ 0xc0 ]
  (* Outi byte *)
  | Outi (imm, _) -> [ 0xc1; imm ]
  (* LCD display *)
  | Dic (imm, _) -> [ 0xc2; imm ]
  | Did (imm, _) -> [ 0xc3; imm ]
  | Dd (A, _) -> [ 0xc4 ]
  | Dd (B, _) -> [ 0xc5 ]
  | Dd (C, _) -> [ 0xc6 ]
  | Hlt _ -> [ 0xc7 ]
  | Nop _ -> [ 0xc8 ]
  (* Labels don't appear in the assembled program *)
  | Label _ -> []
  (* unrecognized instruction *)
  | _ -> raise (AssembleError (InvalidInstr ins, loc_from_instr ins))

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
  | Add _ | Addc _ | Sub _ | Subb _ | And _ | Or _ | Xor _ | Mov _ | Ld _ | St _
  | Cmp _ | Not _ | Neg _ | Inr _ | Inr2 _ | Inr3 _ | Dcr _ | Dcr2 _ | Dcr3 _
  | Ret _ | Hlt _ | Nop _ | Out _ | Dd _ ->
      1
  (* two-byte instructions *)
  | Addi _ | Addci _ | Subi _ | Subbi _ | Ani _ | Ori _ | Xri _ | Mvi _ | Lds _
  | Sts _ | Jmp _ | Je _ | Jne _ | Jg _ | Jge _ | Jl _ | Jle _ | Ja _ | Jae _
  | Jb _ | Jbe _ | Call _ | Dic _ | Did _ | Cmpi _ | Outi _ ->
      2
  (* three-byte instruction *)
  | Stsi _ -> 3

(* 256 bytes is the size of our program memory *)
let max_pgrm_size = 256

(* [map_labels] constructs an environment mapping label names to memory addresses *)
let map_labels (emit_warning : asm_warn_handler) (instrs : instr list) =
  (* accumulate an environment of labels->addresses *)
  let env, _ =
    List.fold_left
      (fun (env, addr) ins ->
        match ins with
        | Label (name, loc) ->
            (* if label already mapped, this is an error *)
            if Env.mem name env then
              raise (AssembleError (DuplicateLabel name, loc));
            (* label is out of bounds (due to pgrm size), warning *)
            if addr >= max_pgrm_size then
              emit_warning (OutOfBoundsLabel (name, addr, loc));
            (Env.add name addr env, addr)
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

(* [assemble_with_rich_info] assembles the given list of instructions
   and produces several pieces of information about the program:
     - a map from label names to physical addresses in the
       generated binary
     - a list of list of bytes, one inner list per instruction, which
       encodes which instructions assembled to which bytes
     - a byte array containing the bytes of the assembled program *)
let assemble_with_rich_info ?(emit_warning : asm_warn_handler = fun _ -> ())
    (instrs : instr list) : int env * int list list * bytes =
  let label_map = map_labels emit_warning instrs in
  (* assemble instructions to list of list of bytes (preserving
     which bytes constitute which instructions) *)
  let unflattened_bytes =
    instrs |> List.map (fun ins -> assemble_instr ins label_map)
  in
  (* convert list of list of bytes into a flat array of bytes *)
  let bytes = bytes_from_list (unflattened_bytes |> List.concat) in
  (* check assembled program size (in bytes) to ensure it can fit *)
  let size = Bytes.length bytes in
  if size > max_pgrm_size then emit_warning (ProgramTooLarge size);
  (label_map, unflattened_bytes, bytes)

(* [assemble] processes a list of asm instructions and
   produces a byte sequence representing the program in binary form *)
let assemble ?(emit_warning : asm_warn_handler = fun _ -> ())
    (instrs : instr list) : bytes =
  let _, _, bytes = assemble_with_rich_info instrs ~emit_warning in
  bytes
