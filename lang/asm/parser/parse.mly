%{ 
  open Isa
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
%token <Util.Srcloc.src_loc> JA
%token <Util.Srcloc.src_loc> JAE
%token <Util.Srcloc.src_loc> JB
%token <Util.Srcloc.src_loc> JBE
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

%start <instr list> main

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
  { Add(src, dest, Some loc) }
| loc = ADDI src = IMM COMMA dest = reg
  { Addi(src, dest, Some loc) }
| loc = SUB src = reg COMMA dest = reg
  { Sub(src, dest, Some loc) }
| loc = SUBI src = IMM COMMA dest = reg
  { Subi(src, dest, Some loc) }
| loc = AND src = reg COMMA dest = reg
  { And(src, dest, Some loc) }
| loc = ANI src = IMM COMMA dest = reg
  { Ani(src, dest, Some loc) }
| loc = OR src = reg COMMA dest = reg
  { Or(src, dest, Some loc) }
| loc = ORI src = IMM COMMA dest = reg
  { Ori(src, dest, Some loc) }
| loc = XOR src = reg COMMA dest = reg
  { Xor(src, dest, Some loc) }
| loc = XRI src = IMM COMMA dest = reg
  { Xri(src, dest, Some loc) }
| loc = NOT r = reg
  { Not(r, Some loc) }
| loc = INR r = reg
  { Inr(r, Some loc) }
| loc = DCR r = reg
  { Dcr(r, Some loc) }
| loc = MOV src = reg COMMA dest = reg
  { Mov(src, dest, Some loc) }
| loc = MVI src = IMM COMMA dest = reg
  { Mvi(src, dest, Some loc) }
| loc = LD src = reg COMMA dest = reg
  { Ld(src, dest, Some loc) }
| loc = ST src = reg COMMA dest = reg
  { St(src, dest, Some loc) }
| loc = LDS src = IMM COMMA dest = reg
  { Lds(src, dest, Some loc) }
| loc = STS src = reg COMMA dest = IMM
  { Sts(src, dest, Some loc) }
| loc = CMP left = reg COMMA right = reg
  { Cmp(left, right, Some loc) }
| loc = CMPI left = IMM COMMA right = reg
  { Cmpi(Imm left, Reg right, Some loc) }
| loc = CMPI left = reg COMMA right = IMM
  { Cmpi(Reg left, Imm right, Some loc) }
| label = LABEL COLON
  { let (label, loc) = label in 
    Label (label, Some loc) }
| loc = JMP target = LABEL
  { let (target, _) = target in 
    Jmp(target, Some loc) }
| loc = JE target = LABEL
  { let (target, _) = target in 
    Je(target, Some loc) }
| loc = JNE target = LABEL
  { let (target, _) = target in 
    Jne(target, Some loc) }
| loc = JG target = LABEL
  { let (target, _) = target in 
    Jg(target, Some loc) }
| loc = JGE target = LABEL
  { let (target, _) = target in 
    Jge(target, Some loc) }
| loc = JL target = LABEL
  { let (target, _) = target in 
    Jl(target, Some loc) }
| loc = JLE target = LABEL
  { let (target, _) = target in 
    Jle(target, Some loc) }
| loc = JA target = LABEL
  { let (target, _) = target in 
    Ja(target, Some loc) }
| loc = JAE target = LABEL
  { let (target, _) = target in 
    Jae(target, Some loc) }
| loc = JB target = LABEL
  { let (target, _) = target in 
    Jb(target, Some loc) }
| loc = JBE target = LABEL
  { let (target, _) = target in 
    Jbe(target, Some loc) }
| loc = CALL target = LABEL
  { let (target, _) = target in 
    Call(target, Some loc) }
| loc = RET
  { Ret (Some loc) }
| loc = DIC imm = IMM
  { Dic(imm, Some loc) }
| loc = DID imm = IMM
  { Did(imm, Some loc) }
| loc = HLT
  { Hlt (Some loc) }
| loc = NOP
  { Nop (Some loc) }
| loc = OUT r = reg
  { Out(r, Some loc) }
