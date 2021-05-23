;; !!! NOTE: this doesn't necessarily work on the emulator
;; indices into a list of instructions as "addresses",
;; but should succeed on actual hardware.

entry:
  ; fake a return address at the l1 label
  mvi 0x07, a
  sts a, 0

  ; return to it
  ret
  jmp assert_not_reached
l1:
  ; NOTE: manually observe: sp = -1

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