open Isa

(* [bytes_from_list] constructs a buffer of raw bytes from
  the given list of integers *)
let bytes_from_list (l : int list) : bytes =
  let buf = Bytes.create (List.length l) in
  List.mapi (fun i b -> Bytes.set_int8 buf i b) l |> ignore;
  buf

(* [assemble_to_list] converts the given instructions to a list of 
  integers representing the assembled bytes of the program *)
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
  bytes_from_list bytes_as_list
