%{ 
  open Isa
  open Util.Srcloc
%}

%token <int> IMM
%token REG_A
%token REG_B
%token REG_C
%token REG_SP
%token <Util.Srcloc.src_loc> ADD
%token <Util.Srcloc.src_loc> ADDI
%token <Util.Srcloc.src_loc> SUB
%token <Util.Srcloc.src_loc> SUBI
%token <Util.Srcloc.src_loc> AND
%token <Util.Srcloc.src_loc> ANI
%token <Util.Srcloc.src_loc> OR
%token <Util.Srcloc.src_loc> ORI
%token <Util.Srcloc.src_loc> XOR
%token <Util.Srcloc.src_loc> XRI
%token <Util.Srcloc.src_loc> NOT
%token <Util.Srcloc.src_loc> INR
%token <Util.Srcloc.src_loc> DCR
%token <Util.Srcloc.src_loc> MOV
%token <Util.Srcloc.src_loc> MVI
%token <Util.Srcloc.src_loc> LD
%token <Util.Srcloc.src_loc> ST
%token <Util.Srcloc.src_loc> LDS
%token <Util.Srcloc.src_loc> STS
%token <Util.Srcloc.src_loc> CMP
%token <Util.Srcloc.src_loc> CMPI
%token <Util.Srcloc.src_loc> JMP
%token <Util.Srcloc.src_loc> JE
%token <Util.Srcloc.src_loc> JNE
%token <Util.Srcloc.src_loc> JG
%token <Util.Srcloc.src_loc> JGE
%token <Util.Srcloc.src_loc> JL
%token <Util.Srcloc.src_loc> JLE
%token <Util.Srcloc.src_loc> CALL
%token <Util.Srcloc.src_loc> RET
%token <Util.Srcloc.src_loc> DIC
%token <Util.Srcloc.src_loc> DID
%token <Util.Srcloc.src_loc> HLT
%token <Util.Srcloc.src_loc> NOP
%token <Util.Srcloc.src_loc> OUT
%token <string * Util.Srcloc.src_loc> LABEL

%token COLON
%token COMMA
%token NEWLINE
%token EOF

%start <instr with_loc list> main

%%

main:
| maybe_newlines i = instr NEWLINE maybe_newlines rest = main
  { i :: rest }
| maybe_newlines i = instr maybe_newlines EOF
  { [i] }
| EOF
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
| loc = ADD src = reg COMMA dest = reg
  { (Add(src, dest), loc) }
| loc = ADDI src = IMM COMMA dest = reg
  { (Addi(src, dest), loc) }
| loc = SUB src = reg COMMA dest = reg
  { (Sub(src, dest), loc) }
| loc = SUBI src = IMM COMMA dest = reg
  { (Subi(src, dest), loc) }
| loc = AND src = reg COMMA dest = reg
  { (And(src, dest), loc) }
| loc = ANI src = IMM COMMA dest = reg
  { (Ani(src, dest), loc) }
| loc = OR src = reg COMMA dest = reg
  { (Or(src, dest), loc) }
| loc = ORI src = IMM COMMA dest = reg
  { (Ori(src, dest), loc) }
| loc = XOR src = reg COMMA dest = reg
  { (Xor(src, dest), loc) }
| loc = XRI src = IMM COMMA dest = reg
  { (Xri(src, dest), loc) }
| loc = NOT r = reg
  { (Not r, loc) }
| loc = INR r = reg
  { (Inr r, loc) }
| loc = DCR r = reg
  { (Dcr r, loc) }
| loc = MOV src = reg COMMA dest = reg
  { (Mov(src, dest), loc) }
| loc = MVI src = IMM COMMA dest = reg
  { (Mvi(src, dest), loc) }
| loc = LD src = reg COMMA dest = reg
  { (Ld(src, dest), loc) }
| loc = ST src = reg COMMA dest = reg
  { (St(src, dest), loc) }
| loc = LDS src = IMM COMMA dest = reg
  { (Lds(src, dest), loc) }
| loc = STS src = reg COMMA dest = IMM
  { (Sts(src, dest), loc) }
| loc = CMP left = reg COMMA right = reg
  { (Cmp(left, right), loc) }
| loc = CMPI left = IMM COMMA right = reg
  { (Cmpi(Imm left, Reg right), loc) }
| loc = CMPI left = reg COMMA right = IMM
  { (Cmpi(Reg left, Imm right), loc) }
| label = LABEL COLON
  { let (label, loc) = label in 
    (Label label, loc) }
| loc = JMP target = LABEL
  { let (target, _) = target in 
    (Jmp target, loc) }
| loc = JE target = LABEL
  { let (target, _) = target in 
    (Je target, loc) }
| loc = JNE target = LABEL
  { let (target, _) = target in 
    (Jne target, loc) }
| loc = JG target = LABEL
  { let (target, _) = target in 
    (Jg target, loc) }
| loc = JGE target = LABEL
  { let (target, _) = target in 
    (Jge target, loc) }
| loc = JL target = LABEL
  { let (target, _) = target in 
    (Jl target, loc) }
| loc = JLE target = LABEL
  { let (target, _) = target in 
    (Jle target, loc) }
| loc = CALL target = LABEL
  { let (target, _) = target in 
    (Call target, loc) }
| loc = RET
  { (Ret, loc) }
| loc = DIC imm = IMM
  { (Dic imm, loc) }
| loc = DID imm = IMM
  { (Did imm, loc) }
| loc = HLT
  { (Hlt, loc) }
| loc = NOP
  { (Nop, loc) }
| loc = OUT r = reg
  { (Out r, loc) }