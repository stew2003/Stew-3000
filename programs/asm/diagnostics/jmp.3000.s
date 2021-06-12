entry:
  jmp l1
  call assert_not_reached
l1:
  jmp l2
  call assert_not_reached
  call assert_not_reached
  call assert_not_reached
  call assert_not_reached
l2:
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