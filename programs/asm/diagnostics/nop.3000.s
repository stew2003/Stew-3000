entry:
  ; setup register state
  mvi 5, a
  mvi 6, b
  mvi 7, c

  nop
  nop
  nop
  nop

  ; no change in register state
  cmpi 5, a
  je good1
  call assert_not_reached
good1:
  cmpi 6, b
  je good2
  call assert_not_reached
good2:
  cmpi 7, c
  je good3
  call assert_not_reached
good3:

  nop
  nop
  nop

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
