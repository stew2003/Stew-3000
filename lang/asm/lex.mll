{
  open Parse
  exception Error of string
}

rule token = parse
| [' ' '\t']+
    { token lexbuf }
| '\n'+ { 
  let pos = lexbuf.lex_curr_p in 
  lexbuf.lex_curr_p <- 
  {
    pos with pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  };
  NEWLINE }
| ':' { COLON }
| ',' { COMMA }
| '-'?['0'-'9']+ as i
    { IMM (int_of_string i) }
| "a" 
  { REG_A }
| "b"
  { REG_B }
| "c"
  { REG_C }
| "sp"
  { REG_SP }
| "add"
  { ADD }
| "addi"
  { ADDI }
| "sub"
  { SUB }
| "subi"
  { SUBI }
| "and"
  { AND }
| "ani"
  { ANI }
| "or"
  { OR }
| "ori"
  { ORI }
| "xor"
  { XOR }
| "xri"
  { XRI }
| "not"
  { NOT }
| "inr"
  { INR }
| "dcr"
  { DCR }
| "mov"
  { MOV }
| "mvi"
  { MVI }
| "ld"
  { LD }
| "st"
  { ST }
| "lds"
  { LDS }
| "sts"
  { STS }
| "cmp"
  { CMP }
| "cmpi"
  { CMPI }
| "jmp"
  { JMP }
| "je"
  { JE }
| "jne"
  { JNE }
| "jg"
  { JG }
| "jge"
  { JGE }
| "jl"
  { JL }
| "jle"
  { JLE }
| "call"
  { CALL }
| "ret"
  { RET }
| "dic"
  { DIC }
| "did"
  { DID }
| "hlt"
  { HLT }
| "nop"
  { NOP }
| "out"
  { OUT }
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as name
    { LABEL name }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf 
      "Unexpected character '%s' at position %d:%d" 
      (Lexing.lexeme lexbuf)
      (Lexing.lexeme_start_p lexbuf).pos_lnum
      (Lexing.lexeme_start lexbuf))) }
