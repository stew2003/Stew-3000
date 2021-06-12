entry:
  ; not equal
  mvi 100, a
  cmpi 14, a
  jne not_equal1
  call assert_not_reached
not_equal1:

  ; equal
  mvi -16, a
  cmpi -16, a
  jne not_equal2
  jmp continue1
not_equal2:
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