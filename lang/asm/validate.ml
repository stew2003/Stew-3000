open Isa
open Util.Srcloc

type valid_err = InvalidImm of immediate | InvalidInstr of instr

exception ValidityError of valid_err with_loc_opt

let i8_min = -128

let u8_max = 255

(* [validate_imm] checks that an immediate value is within the representable
   range for both signed/unsigned 8bit ints, and raises an error if not *)
let validate_imm (imm : immediate) (loc : maybe_loc) =
  (* is it in [-128, 255] *)
  if imm >= i8_min && imm <= u8_max then ()
  else raise (ValidityError (InvalidImm imm, loc))

(* [validate_instr] checks if a given instruction is a valid
  instruction for which we have an opcode, and that its 
  immediates/offsets on the stack are valid. Errors if not *)
let validate_instr (ins : instr) =
  match ins with
  (* Add src, dest *)
  | Add (A, A, _)
  | Add (A, B, _)
  | Add (A, C, _)
  | Add (A, SP, _)
  | Add (B, A, _)
  | Add (B, B, _)
  | Add (B, C, _)
  | Add (B, SP, _)
  | Add (C, A, _)
  | Add (C, B, _)
  | Add (C, C, _)
  | Add (C, SP, _) ->
      ()
  (* Addi byte, dest *)
  | Addi (imm, A, loc)
  | Addi (imm, B, loc)
  | Addi (imm, C, loc)
  | Addi (imm, SP, loc) ->
      validate_imm imm loc
  (* Sub src, dest *)
  | Sub (B, A, _)
  | Sub (C, A, _)
  | Sub (A, B, _)
  | Sub (C, B, _)
  | Sub (A, C, _)
  | Sub (B, C, _)
  | Sub (A, SP, _)
  | Sub (B, SP, _)
  | Sub (C, SP, _) ->
      ()
  (* Subi byte, dest *)
  | Subi (imm, A, loc)
  | Subi (imm, B, loc)
  | Subi (imm, C, loc)
  | Subi (imm, SP, loc) ->
      validate_imm imm loc
  (* And src, dest *)
  | And (B, A, _)
  | And (C, A, _)
  | And (A, B, _)
  | And (C, B, _)
  | And (A, C, _)
  | And (B, C, _) ->
      ()
  (* Ani byte, dest *)
  | Ani (imm, A, loc) | Ani (imm, B, loc) | Ani (imm, C, loc) ->
      validate_imm imm loc
  (* Or src, dest *)
  | Or (B, A, _)
  | Or (C, A, _)
  | Or (A, B, _)
  | Or (C, B, _)
  | Or (A, C, _)
  | Or (B, C, _) ->
      ()
  (* Ori byte, dest *)
  | Ori (imm, A, loc) | Ori (imm, B, loc) | Ori (imm, C, loc) ->
      validate_imm imm loc
  (* Xor src, dest *)
  | Xor (B, A, _)
  | Xor (C, A, _)
  | Xor (A, B, _)
  | Xor (C, B, _)
  | Xor (A, C, _)
  | Xor (B, C, _) ->
      ()
  (* Xri byte, dest *)
  | Xri (imm, A, loc) | Xri (imm, B, loc) | Xri (imm, C, loc) ->
      validate_imm imm loc
  (* Not dest *)
  | Not (A, _) | Not (B, _) | Not (C, _) -> ()
  (* Inr dest *)
  | Inr (A, _) | Inr (B, _) | Inr (C, _) | Inr (SP, _) -> ()
  (* Dcr dest *)
  | Dcr (A, _) | Dcr (B, _) | Dcr (C, _) | Dcr (SP, _) -> ()
  (* Mov src, dest *)
  | Mov (A, B, _)
  | Mov (A, C, _)
  | Mov (B, A, _)
  | Mov (B, C, _)
  | Mov (C, A, _)
  | Mov (C, B, _) ->
      ()
  (* Mvi byte, dest *)
  | Mvi (imm, A, loc) | Mvi (imm, B, loc) | Mvi (imm, C, loc) ->
      validate_imm imm loc
  (* Ld src, dest *)
  | Ld (A, A, _)
  | Ld (B, A, _)
  | Ld (C, A, _)
  | Ld (A, B, _)
  | Ld (B, B, _)
  | Ld (C, B, _)
  | Ld (A, C, _)
  | Ld (B, C, _)
  | Ld (C, C, _) ->
      ()
  (* St src, dest *)
  | St (A, A, _)
  | St (A, B, _)
  | St (A, C, _)
  | St (B, A, _)
  | St (B, B, _)
  | St (B, C, _)
  | St (C, A, _)
  | St (C, B, _)
  | St (C, C, _) ->
      ()
  (* Lds byte, dest *)
  | Lds (off, A, loc) | Lds (off, B, loc) | Lds (off, C, loc) ->
      validate_imm off loc
  (* Sts src, byte *)
  | Sts (A, off, loc) | Sts (B, off, loc) | Sts (C, off, loc) ->
      validate_imm off loc
  (* Cmp left, right *)
  | Cmp (A, B, _)
  | Cmp (A, C, _)
  | Cmp (B, A, _)
  | Cmp (B, C, _)
  | Cmp (C, A, _)
  | Cmp (C, B, _) ->
      ()
  (* Cmpi byte, reg or Cmpi reg, byte *)
  | Cmpi (Reg A, Imm imm, loc)
  | Cmpi (Imm imm, Reg A, loc)
  | Cmpi (Reg B, Imm imm, loc)
  | Cmpi (Imm imm, Reg B, loc)
  | Cmpi (Reg C, Imm imm, loc)
  | Cmpi (Imm imm, Reg C, loc) ->
      validate_imm imm loc
  (* Jumps *)
  | Jmp _ | Je _ | Jne _ | Jg _ | Jge _ | Jl _ | Jle _ -> ()
  (* Call and return *)
  | Call _ | Ret _ -> ()
  (* Out src *)
  | Out (A, _) | Out (B, _) | Out (C, _) -> ()
  (* Misc. *)
  | Dic _ | Did _ | Hlt _ | Nop _ | Label _ -> ()
  (* unrecognized instruction *)
  | _ -> raise (ValidityError (InvalidInstr ins, loc_from_instr ins))
