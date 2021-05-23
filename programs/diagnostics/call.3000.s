;; !!! NOTE: this fails on the emulator which uses
;; indices into a list of instructions as "addresses",
;; but should succeed on actual hardware. 

entry:
  call l1
  jmp assert_not_reached
l1:
  ; NOTE: manually observe: sp = 1

  lds 0, a ; ret addr = 0x02
  mvi 0x02, b
  cmp a, b
  je assert_succeeded1
  mvi -1, c
  out c
  hlt
assert_succeeded1:

  call l2
  jmp assert_not_reached
l2:
  ; NOTE: manually observe: sp = 2

  lds -1, a ; ret addr for first call is still 0x02
  mvi 0x02, b
  cmp a, b
  je assert_succeeded2
  mvi -1, c
  out c
  hlt
assert_succeeded2:

  lds 0, a ; ret addr for second call is 0x11 (17)
  mvi 0x11, b
  cmp a, b
  je assert_succeeded3
  mvi -1, c
  out c
  hlt
assert_succeeded3:

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