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

  (* TODO: update to incorporate new instructions and new opcodes. *)
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
  (* Sub src, dest *)
  | Sub (B, A, _) -> [ 0x10 ]
  | Sub (C, A, _) -> [ 0x11 ]
  | Sub (A, B, _) -> [ 0x12 ]
  | Sub (C, B, _) -> [ 0x13 ]
  | Sub (A, C, _) -> [ 0x14 ]
  | Sub (B, C, _) -> [ 0x15 ]
  | Sub (A, SP, _) -> [ 0x16 ]
  | Sub (B, SP, _) -> [ 0x17 ]
  | Sub (C, SP, _) -> [ 0x18 ]
  (* Subi byte, dest *)
  | Subi (imm, A, _) -> [ 0x19; imm ]
  | Subi (imm, B, _) -> [ 0x1a; imm ]
  | Subi (imm, C, _) -> [ 0x1b; imm ]
  | Subi (imm, SP, _) -> [ 0x1c; imm ]
  (* And src, dest *)
  | And (B, A, _) -> [ 0x1d ]
  | And (C, A, _) -> [ 0x1e ]
  | And (A, B, _) -> [ 0x1f ]
  | And (C, B, _) -> [ 0x20 ]
  | And (A, C, _) -> [ 0x21 ]
  | And (B, C, _) -> [ 0x22 ]
  (* Ani byte, dest *)
  | Ani (imm, A, _) -> [ 0x23; imm ]
  | Ani (imm, B, _) -> [ 0x24; imm ]
  | Ani (imm, C, _) -> [ 0x25; imm ]
  (* Or src, dest *)
  | Or (B, A, _) -> [ 0x26 ]
  | Or (C, A, _) -> [ 0x27 ]
  | Or (A, B, _) -> [ 0x28 ]
  | Or (C, B, _) -> [ 0x29 ]
  | Or (A, C, _) -> [ 0x2a ]
  | Or (B, C, _) -> [ 0x2b ]
  (* Ori byte, dest *)
  | Ori (imm, A, _) -> [ 0x2c; imm ]
  | Ori (imm, B, _) -> [ 0x2d; imm ]
  | Ori (imm, C, _) -> [ 0x2e; imm ]
  (* Xor src, dest *)
  | Xor (B, A, _) -> [ 0x2f ]
  | Xor (C, A, _) -> [ 0x30 ]
  | Xor (A, B, _) -> [ 0x31 ]
  | Xor (C, B, _) -> [ 0x32 ]
  | Xor (A, C, _) -> [ 0x33 ]
  | Xor (B, C, _) -> [ 0x34 ]
  (* Xri byte, dest *)
  | Xri (imm, A, _) -> [ 0x35; imm ]
  | Xri (imm, B, _) -> [ 0x36; imm ]
  | Xri (imm, C, _) -> [ 0x37; imm ]
  (* Not dest *)
  | Not (A, _) -> [ 0x38 ]
  | Not (B, _) -> [ 0x39 ]
  | Not (C, _) -> [ 0x3a ]
  (* Inr dest *)
  | Inr (A, _) -> [ 0x3b ]
  | Inr (B, _) -> [ 0x3c ]
  | Inr (C, _) -> [ 0x3d ]
  | Inr (SP, _) -> [ 0x3e ]
  (* Dcr dest *)
  | Dcr (A, _) -> [ 0x3f ]
  | Dcr (B, _) -> [ 0x40 ]
  | Dcr (C, _) -> [ 0x41 ]
  | Dcr (SP, _) -> [ 0x42 ]
  (* Mov src, dest *)
  | Mov (A, B, _) -> [ 0x43 ]
  | Mov (A, C, _) -> [ 0x44 ]
  | Mov (B, A, _) -> [ 0x45 ]
  | Mov (B, C, _) -> [ 0x46 ]
  | Mov (C, A, _) -> [ 0x47 ]
  | Mov (C, B, _) -> [ 0x48 ]
  | Mov (SP, A, _) -> [ 0xff ]
  (* Mvi byte, dest *)
  | Mvi (imm, A, _) -> [ 0x49; imm ]
  | Mvi (imm, B, _) -> [ 0x4a; imm ]
  | Mvi (imm, C, _) -> [ 0x4b; imm ]
  (* Ld src, dest *)
  | Ld (A, A, _) -> [ 0x4c ]
  | Ld (B, A, _) -> [ 0x4d ]
  | Ld (C, A, _) -> [ 0x4e ]
  | Ld (A, B, _) -> [ 0x4f ]
  | Ld (B, B, _) -> [ 0x50 ]
  | Ld (C, B, _) -> [ 0x51 ]
  | Ld (A, C, _) -> [ 0x52 ]
  | Ld (B, C, _) -> [ 0x53 ]
  | Ld (C, C, _) -> [ 0x54 ]
  (* St src, dest *)
  | St (A, A, _) -> [ 0x55 ]
  | St (A, B, _) -> [ 0x56 ]
  | St (A, C, _) -> [ 0x57 ]
  | St (B, A, _) -> [ 0x58 ]
  | St (B, B, _) -> [ 0x59 ]
  | St (B, C, _) -> [ 0x5a ]
  | St (C, A, _) -> [ 0x5b ]
  | St (C, B, _) -> [ 0x5c ]
  | St (C, C, _) -> [ 0x5d ]
  (* Lds byte, dest *)
  | Lds (imm, A, _) -> [ 0x5e; imm ]
  | Lds (imm, B, _) -> [ 0x5f; imm ]
  | Lds (imm, C, _) -> [ 0x60; imm ]
  (* Sts src, byte *)
  | Sts (A, imm, _) -> [ 0x61; imm ]
  | Sts (B, imm, _) -> [ 0x62; imm ]
  | Sts (C, imm, _) -> [ 0x63; imm ]
  (* Cmp left, right *)
  | Cmp (A, B, _) -> [ 0x64 ]
  | Cmp (A, C, _) -> [ 0x65 ]
  | Cmp (B, A, _) -> [ 0x66 ]
  | Cmp (B, C, _) -> [ 0x67 ]
  | Cmp (C, A, _) -> [ 0x68 ]
  | Cmp (C, B, _) -> [ 0x69 ]
  (* Cmpi byte, reg or Cmpi reg, byte *)
  | Cmpi (Reg A, Imm imm, _) -> [ 0x6a; imm ]
  | Cmpi (Imm imm, Reg A, _) -> [ 0x6b; imm ]
  | Cmpi (Reg B, Imm imm, _) -> [ 0x6c; imm ]
  | Cmpi (Imm imm, Reg B, _) -> [ 0x6d; imm ]
  | Cmpi (Reg C, Imm imm, _) -> [ 0x6e; imm ]
  | Cmpi (Imm imm, Reg C, _) -> [ 0x6f; imm ]
  (* Jumps *)
  | Jmp (label, loc) -> [ 0x70; to_addr label loc ]
  | Je (label, loc) -> [ 0x71; to_addr label loc ]
  | Jne (label, loc) -> [ 0x72; to_addr label loc ]
  | Jg (label, loc) -> [ 0x73; to_addr label loc ]
  | Jge (label, loc) -> [ 0x74; to_addr label loc ]
  | Jl (label, loc) -> [ 0x75; to_addr label loc ]
  | Jle (label, loc) -> [ 0x76; to_addr label loc ]
  | Ja (label, loc) -> [ 0xff; to_addr label loc ]
  | Jae (label, loc) -> [ 0xff; to_addr label loc ]
  | Jb (label, loc) -> [ 0xff; to_addr label loc ]
  | Jbe (label, loc) -> [ 0xff; to_addr label loc ]
  (* Call and return *)
  | Call (label, loc) -> [ 0x77; to_addr label loc ]
  | Ret _ -> [ 0x78 ]
  (* Out src *)
  | Out (A, _) -> [ 0x79 ]
  | Out (B, _) -> [ 0x7a ]
  | Out (C, _) -> [ 0x7b ]
  (* Misc. *)
  | Dic (imm, _) -> [ 0x7c; imm ]
  | Did (imm, _) -> [ 0x7d; imm ]
  | Hlt _ -> [ 0x7e ]
  | Nop _ -> [ 0x7f ]
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
