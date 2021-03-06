{
  open Parse
  open Util.Srcloc
  exception Error of string * src_loc
}

rule token = parse
| [' ' '\t']+
  { token lexbuf }
| ';'[^'\n']*?
  { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| ':' { COLON }
| ',' { COMMA }
| '-'?['0'-'9']+ as i
  { IMM (int_of_string i) }
| '-'?"0x"['0'-'9''a'-'f''A'-'F']+ as hex
  { IMM (int_of_string hex) }
| '-'?"0b"['0' '1']+ as binary
  { IMM (int_of_string binary) }
| "a" 
  { REG_A }
| "b"
  { REG_B }
| "c"
  { REG_C }
| "z"
  { REG_Z }
| "sp"
  { REG_SP }
| "add"
  { ADD (loc_from_lexbuf lexbuf) }
| "addi"
  { ADDI (loc_from_lexbuf lexbuf) }
| "addc"
  { ADDC (loc_from_lexbuf lexbuf) }
| "addci"
  { ADDCI (loc_from_lexbuf lexbuf) }
| "sub"
  { SUB (loc_from_lexbuf lexbuf) }
| "subi"
  { SUBI (loc_from_lexbuf lexbuf) }
| "subb"
  { SUBB (loc_from_lexbuf lexbuf) }
| "subbi"
  { SUBBI (loc_from_lexbuf lexbuf) }
| "and"
  { AND (loc_from_lexbuf lexbuf) }
| "ani"
  { ANI (loc_from_lexbuf lexbuf) }
| "or"
  { OR (loc_from_lexbuf lexbuf) }
| "ori"
  { ORI (loc_from_lexbuf lexbuf) }
| "xor"
  { XOR (loc_from_lexbuf lexbuf) }
| "xri"
  { XRI (loc_from_lexbuf lexbuf) }
| "not"
  { NOT (loc_from_lexbuf lexbuf) }
| "neg"
  { NEG (loc_from_lexbuf lexbuf) }
| "inr"
  { INR (loc_from_lexbuf lexbuf) }
| "inr2"
  { INR2 (loc_from_lexbuf lexbuf) }
| "inr3"
  { INR3 (loc_from_lexbuf lexbuf) }
| "dcr"
  { DCR (loc_from_lexbuf lexbuf) }
| "dcr2"
  { DCR2 (loc_from_lexbuf lexbuf) }
| "dcr3"
  { DCR3 (loc_from_lexbuf lexbuf) }
| "mov"
  { MOV (loc_from_lexbuf lexbuf) }
| "mvi"
  { MVI (loc_from_lexbuf lexbuf) }
| "ld"
  { LD (loc_from_lexbuf lexbuf) }
| "st"
  { ST (loc_from_lexbuf lexbuf) }
| "lds"
  { LDS (loc_from_lexbuf lexbuf) }
| "sts"
  { STS (loc_from_lexbuf lexbuf) }
| "stsi"
  { STSI (loc_from_lexbuf lexbuf) }
| "cmp"
  { CMP (loc_from_lexbuf lexbuf) }
| "cmpi"
  { CMPI (loc_from_lexbuf lexbuf) }
| "jmp"
  { JMP (loc_from_lexbuf lexbuf) }
| "je"
  { JE (loc_from_lexbuf lexbuf) }
| "jne"
  { JNE (loc_from_lexbuf lexbuf) }
| "jg"
  { JG (loc_from_lexbuf lexbuf) }
| "jge"
  { JGE (loc_from_lexbuf lexbuf) }
| "jl"
  { JL (loc_from_lexbuf lexbuf) }
| "jle"
  { JLE (loc_from_lexbuf lexbuf) }
| "ja"
  { JA (loc_from_lexbuf lexbuf) }
| "jae"
  { JAE (loc_from_lexbuf lexbuf) }
| "jb"
  { JB (loc_from_lexbuf lexbuf) }
| "jbe"
  { JBE (loc_from_lexbuf lexbuf) }
| "call"
  { CALL (loc_from_lexbuf lexbuf) }
| "ret"
  { RET (loc_from_lexbuf lexbuf) }
| "dic"
  { DIC (loc_from_lexbuf lexbuf) }
| "did"
  { DID (loc_from_lexbuf lexbuf) }
| "dd"
  { DD (loc_from_lexbuf lexbuf) }
| "hlt"
  { HLT (loc_from_lexbuf lexbuf) }
| "nop"
  { NOP (loc_from_lexbuf lexbuf) }
| "out"
  { OUT (loc_from_lexbuf lexbuf) }
| "outi"
  { OUTI (loc_from_lexbuf lexbuf) }
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as name
    { LABEL (name, (loc_from_lexbuf lexbuf)) }
| eof
    { EOF }
| _
    { raise (Error 
      ((Printf.sprintf 
        "unexpected character '%s' at position %d:%d" 
        (String.escaped (Lexing.lexeme lexbuf))
        (Lexing.lexeme_start_p lexbuf).pos_lnum
        (Lexing.lexeme_start lexbuf)), 
      (loc_from_lexbuf lexbuf))) }
