%{ 
  open Isa
%}

%token <int> IMM
%token REG_A
%token REG_B
%token REG_C
%token REG_SP
%token ADD
%token ADDI
%token SUB
%token SUBI
%token AND
%token ANI
%token OR
%token ORI
%token XOR
%token XRI
%token NOT
%token INR
%token DCR
%token MOV
%token MVI
%token LD
%token ST
%token LDS
%token STS
%token CMP
%token CMPI
%token JMP
%token JE
%token JNE
%token JG
%token JGE
%token JL
%token JLE
%token CALL
%token RET
%token DIC
%token DID
%token HLT
%token NOP
%token OUT
%token <string> LABEL

%token COLON
%token COMMA
%token NEWLINE
%token EOF

%start <instr list> main

%%

main:
| maybe_newlines i = instr NEWLINE maybe_newlines rest = main
  { i :: rest }
| i = instr maybe_newlines EOF
  { [i] }
| maybe_newlines EOF
  { [] }

maybe_newlines:
| {}
| NEWLINE maybe_newlines {}

reg:
| REG_A
  { A }
| REG_B
  { B }
| REG_C
  { C }
| REG_SP
  { SP }

instr:
| ADD src = reg COMMA dest = reg
  { Add(src, dest) }
| ADDI src = IMM COMMA dest = reg
  { Addi(src, dest) }
| SUB src = reg COMMA dest = reg
  { Sub(src, dest) }
| SUBI src = IMM COMMA dest = reg
  { Subi(src, dest) }
| AND src = reg COMMA dest = reg
  { And(src, dest) }
| ANI src = IMM COMMA dest = reg
  { Ani(src, dest) }
| OR src = reg COMMA dest = reg
  { Or(src, dest) }
| ORI src = IMM COMMA dest = reg
  { Ori(src, dest) }
| XOR src = reg COMMA dest = reg
  { Xor(src, dest) }
| XRI src = IMM COMMA dest = reg
  { Xri(src, dest) }
| NOT r = reg
  { Not r }
| INR r = reg
  { Inr r }
| DCR r = reg
  { Dcr r }
| MOV src = reg COMMA dest = reg
  { Mov(src, dest) }
| MVI src = IMM COMMA dest = reg
  { Mvi(src, dest) }
| LD src = reg COMMA dest = reg
  { Ld(src, dest) }
| ST src = reg COMMA dest = reg
  { St(src, dest) }
| LDS src = IMM COMMA dest = reg
  { Lds(src, dest) }
| STS src = reg COMMA dest = IMM
  { Sts(src, dest) }
| CMP left = reg COMMA right = reg
  { Cmp(left, right) }
| CMPI left = IMM COMMA right = reg
  { Cmpi(Imm left, Reg right) }
| CMPI left = reg COMMA right = IMM
  { Cmpi(Reg left, Imm right) }
| lb = LABEL COLON
  { Label lb }
| JMP target = LABEL
  { Jmp target }
| JE target = LABEL
  { Je target }
| JNE target = LABEL
  { Jne target }
| JG target = LABEL
  { Jg target }
| JGE target = LABEL
  { Jge target }
| JL target = LABEL
  { Jl target }
| JLE target = LABEL
  { Jle target }
| CALL target = LABEL
  { Call target }
| RET
  { Ret }
| DIC imm = IMM
  { Dic imm }
| DID imm = IMM
  { Did imm }
| HLT
  { Hlt }
| NOP
  { Nop }
| OUT r = reg
  { Out r }