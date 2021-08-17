type register = A | B | C | SP | PC

type flag = ZF | SF | OF | CF

type command =
  | PrintReg of register
  | PrintFlag of flag
  | PrintStackAtAddr of int
  | PrintRegs
  | PrintFlags
  | PrintDecHistory
  | PrintStack
  | PrintFullState
  | PrintCurrentIns
  | PrintHalted
  | SetReg of register * int
  | SetFlag of flag * bool
  | SetStackAtAddr of int * int
  | SetHalted of bool
  | Next
  | NoCommand
  | Help
  | Clear
  | Continue
