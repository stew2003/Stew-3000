entry:
  ; equal
  mvi 100, a
  cmpi 100, a
  je equal1
  call assert_not_reached
equal1:

  ; not equal
  mvi 41, a
  cmpi 42, a
  je equal2
  jmp continue1
equal2:
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