entry:
  ; greater
  mvi 40, a
  cmpi 45, a
  jg greater1
  call assert_not_reached
greater1:

  ; equal
  mvi 17, a
  cmpi 17, a
  jg greater2
  jmp continue1
greater2:
  call assert_not_reached
continue1:

  ; less
  mvi 57, a
  cmpi 14, a
  jg greater3
  jmp continue2
greater3:
  call assert_not_reached
continue2:

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