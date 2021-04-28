open Isa

type asm_err =
  | ProgramTooLarge
  | DuplicateLabel
  | InvalidInstr of instr
  | InvalidImm of immediate

exception AssembleError of asm_err

(* 256 bytes is the size of our program memory *)
let max_pgrm_size = 256

(* [bytes_from_list] constructs a buffer of raw bytes from
  the given list of integers *)
let bytes_from_list (l : int list) : bytes =
  let buf = Bytes.create (List.length l) in
  List.mapi (fun i b -> Bytes.set_int8 buf i b) l |> ignore;
  buf

(* [validate_imm] checks that an immediate value is within the representable 
  range [-128, 128), and throws AssembleError InvalidImm if not *)
let validate_imm (imm : immediate) : immediate =
  if imm >= -128 && imm < 128 then imm
  else raise (AssembleError (InvalidImm imm))

(* [assemble_to_list] converts the given instructions to a list of 
  integers representing the assembled bytes of the program. Raises 
  AssembleError InvalidInstr if an un-assemblable instruction is encountered *)
let assemble_to_list (instrs : instr list) (label_map : (string, int) Hashtbl.t)
    : int list =
  []

(* [map_labels] constructs a mapping from label names to memory addresses *)
let map_labels (instrs : instr list) = Hashtbl.create 0
(* 
label_to_addr = {}
byte_pos = 0
pass over instrs:
  if Label _:
    label_to_addr[label] = byte_pos
  otherwise:
    byte_pos += sizeof(ins)

^^ this function can raise AssembleError DuplicateLabel
 *)

(* [size_of] determines the size (in bytes) of the given instruction *)
let size_of (ins : instr) : int =
  match ins with
  (* labels don't appear in assembled program *)
  | Label _ -> 0
  (* one-byte instructions *)
  | Add _ | Sub _ | And _ | Or _ | Xor _ | Mov _ | Ld _ | St _ | Cmp _ | Not _
  | Inr _ | Dcr _ | Ret | Hlt | Nop ->
      1
  (* two-byte instructions *)
  | Addi _ | Subi _ | Ani _ | Ori _ | Xri _ | Mvi _ | Lds _ | Sts _ | Jmp _
  | Je _ | Jne _ | Jg _ | Jge _ | Jl _ | Jle _ | Call _ | Dic _ | Did _ | Out _
  | Cmpi _ ->
      2

(* [assemble] processes a list of asm instructions and 
  produces a byte sequence representing the program in binary form *)
let assemble (instrs : instr list) : bytes =
  let label_map = map_labels instrs in
  let bytes_as_list = assemble_to_list instrs label_map in
  (* check program size to ensure it can fit *)
  if List.length bytes_as_list > max_pgrm_size then
    raise (AssembleError ProgramTooLarge)
  else bytes_from_list bytes_as_list
