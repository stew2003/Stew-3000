open Isa
open Util.Srcloc

type valid_err = InvalidImm of immediate | InvalidInstr of instr

exception ValidityError of valid_err with_loc_opt

let i8_min = -128

let u8_max = 255

(* [validate_imm] checks that an immediate value is within the representable
   range for both signed/unsigned 8bit ints, and raises an error if not *)
let validate_imm (imm : immediate) (loc : src_loc) =
  (* is it in [-128, 255] *)
  if imm >= i8_min && imm <= u8_max then ()
  else raise (ValidityError (InvalidImm imm, Some loc))

(* [validate_instr] checks if a given instruction is a valid
  instruction for which we have an opcode, and that its 
  immediates/offsets on the stack are valid. Errors if not *)
let validate_instr (ins : instr with_loc) =
  let ins, loc = ins in
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
  | Add (C, SP) ->
      ()
  (* Addi byte, dest *)
  | Addi (imm, A) | Addi (imm, B) | Addi (imm, C) | Addi (imm, SP) ->
      validate_imm imm loc
  (* Sub src, dest *)
  | Sub (B, A)
  | Sub (C, A)
  | Sub (A, B)
  | Sub (C, B)
  | Sub (A, C)
  | Sub (B, C)
  | Sub (A, SP)
  | Sub (B, SP)
  | Sub (C, SP) ->
      ()
  (* Subi byte, dest *)
  | Subi (imm, A) | Subi (imm, B) | Subi (imm, C) | Subi (imm, SP) ->
      validate_imm imm loc
  (* And src, dest *)
  | And (B, A) | And (C, A) | And (A, B) | And (C, B) | And (A, C) | And (B, C)
    ->
      ()
  (* Ani byte, dest *)
  | Ani (imm, A) | Ani (imm, B) | Ani (imm, C) -> validate_imm imm loc
  (* Or src, dest *)
  | Or (B, A) | Or (C, A) | Or (A, B) | Or (C, B) | Or (A, C) | Or (B, C) -> ()
  (* Ori byte, dest *)
  | Ori (imm, A) | Ori (imm, B) | Ori (imm, C) -> validate_imm imm loc
  (* Xor src, dest *)
  | Xor (B, A) | Xor (C, A) | Xor (A, B) | Xor (C, B) | Xor (A, C) | Xor (B, C)
    ->
      ()
  (* Xri byte, dest *)
  | Xri (imm, A) | Xri (imm, B) | Xri (imm, C) -> validate_imm imm loc
  (* Not dest *)
  | Not A | Not B | Not C -> ()
  (* Inr dest *)
  | Inr A | Inr B | Inr C | Inr SP -> ()
  (* Dcr dest *)
  | Dcr A | Dcr B | Dcr C | Dcr SP -> ()
  (* Mov src, dest *)
  | Mov (A, B) | Mov (A, C) | Mov (B, A) | Mov (B, C) | Mov (C, A) | Mov (C, B)
    ->
      ()
  (* Mvi byte, dest *)
  | Mvi (imm, A) | Mvi (imm, B) | Mvi (imm, C) -> validate_imm imm loc
  (* Ld src, dest *)
  | Ld (A, A)
  | Ld (B, A)
  | Ld (C, A)
  | Ld (A, B)
  | Ld (B, B)
  | Ld (C, B)
  | Ld (A, C)
  | Ld (B, C)
  | Ld (C, C) ->
      ()
  (* St src, dest *)
  | St (A, A)
  | St (A, B)
  | St (A, C)
  | St (B, A)
  | St (B, B)
  | St (B, C)
  | St (C, A)
  | St (C, B)
  | St (C, C) ->
      ()
  (* Lds byte, dest *)
  | Lds (off, A) | Lds (off, B) | Lds (off, C) -> validate_imm off loc
  (* Sts src, byte *)
  | Sts (A, off) | Sts (B, off) | Sts (C, off) -> validate_imm off loc
  (* Cmp left, right *)
  | Cmp (A, B) | Cmp (A, C) | Cmp (B, A) | Cmp (B, C) | Cmp (C, A) | Cmp (C, B)
    ->
      ()
  (* Cmpi byte, reg or Cmpi reg, byte *)
  | Cmpi (Reg A, Imm imm)
  | Cmpi (Imm imm, Reg A)
  | Cmpi (Reg B, Imm imm)
  | Cmpi (Imm imm, Reg B)
  | Cmpi (Reg C, Imm imm)
  | Cmpi (Imm imm, Reg C) ->
      validate_imm imm loc
  (* Jumps *)
  | Jmp _ | Je _ | Jne _ | Jg _ | Jge _ | Jl _ | Jle _ -> ()
  (* Call and return *)
  | Call _ | Ret -> ()
  (* Out src *)
  | Out A | Out B | Out C -> ()
  (* Misc. *)
  | Dic _ | Did _ | Hlt | Nop | Label _ -> ()
  (* unrecognized instruction *)
  | _ -> raise (ValidityError (InvalidInstr ins, Some loc))
