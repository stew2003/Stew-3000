entry:
  ; greater 
  mvi 105, a
  cmpi 106, a
  jge greater_or_eq1
  call assert_not_reached
greater_or_eq1:

  ; equal
  mvi 47, a
  cmpi 47, a
  jge greater_or_eq2
  call assert_not_reached
greater_or_eq2:

  ; less
  mvi -16, a
  cmpi -48, a
  jge greater_or_eq3
  jmp continue1
greater_or_eq3:
  call assert_not_reached
continue1:

  ; halt successfully
  mvi 0, c
  out c
  hlt

; assert_not_reached asserts that this block
; of code is never run. If it is, it halts
; with -1 on the decimal display
assert_not_reached:
  mvi -1, c
  out c
  hlt