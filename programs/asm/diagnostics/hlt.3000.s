entry:
  ; halt successfully
  mvi 0, c
  out c
  hlt
  call assert_not_reached

; assert_not_reached asserts that this block
; of code is never run. If it is, it halts
; with -1 on the decimal display
assert_not_reached:
  mvi -1, c
  out c
  hlt
