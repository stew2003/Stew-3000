%{ 
  open Ast
%}

%token <int> NUM
%token <bool> BOOL
%token <Ast.register> REG
%token <Ast.flag> FLAG
%token LBRACKET
%token RBRACKET
%token STACK
%token REGS
%token FLAGS
%token MACHINE
%token DEC
%token INS
%token HALTED
%token PRINT
%token SET
%token NEXT

%token EOF

%start <command> main

%%

main:
| p = print_cmd EOF
  { p }
| s = set_cmd EOF
  { s }
| n = next EOF
  { n }
| EOF
  { NoCommand }

print_cmd:
| PRINT r = REG
  { PrintReg r }
| PRINT f = FLAG
  { PrintFlag f }
| PRINT STACK LBRACKET addr = NUM RBRACKET
  { PrintStackAtAddr addr }
| PRINT REGS
  { PrintRegs }
| PRINT FLAGS
  { PrintFlags }
| PRINT DEC
  { PrintDecHistory }
| PRINT STACK
  { PrintStack }
| PRINT MACHINE
  { PrintFullState }
| PRINT INS
  { PrintCurrentIns }
| PRINT HALTED
  { PrintHalted }

set_cmd:
| SET r = REG v = NUM
  { SetReg (r, v) }
| SET f = FLAG b = BOOL
  { SetFlag (f, b) }
| SET STACK LBRACKET addr = NUM RBRACKET v = NUM
  { SetStackAtAddr (addr, v) }
| SET HALTED b = BOOL
  { SetHalted b }

next:
  | NEXT { Next }
