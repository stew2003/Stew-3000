{
  open Parse
  exception Error of string
}

rule token = parse
| [' ' '\t']+
  { token lexbuf }
| '-'?['0'-'9']+ as i
  { NUM (int_of_string i) }
| '-'?"0x"['0'-'9''a'-'f''A'-'F']+ as hex
  { NUM (int_of_string hex) }
| '-'?"0b"['0' '1']+ as binary
  { NUM (int_of_string binary) }
| "true"
  { BOOL true }
| "false"
  { BOOL false }
| "a" 
  { REG A }
| "b"
  { REG B }
| "c"
  { REG C }
| "sp"
  { REG SP }
| "pc"
  { REG PC }
| "zf"
  { FLAG ZF }
| "sf"
  { FLAG SF }
| "of"
  { FLAG OF }
| "stack" 
  { STACK }
| '['
  { LBRACKET }
| ']'
  { RBRACKET }
| "regs"
  { REGS }
| "flags"
  { FLAGS }
| "machine"
  { MACHINE }
| "dec"
  { DEC }
| "ins"
  { INS }
| "halted"
  { HALTED }
| "print"
  { PRINT }
| "p"
  { PRINT }
| "set"
  { SET }
| "next"
  { NEXT }
| "n"
  { NEXT }
| "help"
  { HELP }
| eof
    { EOF }
| _
    { raise (Error 
      (Printf.sprintf 
        "unexpected character '%s' at position %d" 
        (String.escaped (Lexing.lexeme lexbuf))
        (Lexing.lexeme_start lexbuf))) }
