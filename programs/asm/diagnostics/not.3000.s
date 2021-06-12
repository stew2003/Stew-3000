entry:
  ; not a
  mvi 0b11101010, a
  not a
  mvi 0b00010101, b
  call assert_a_eq_b

  ; not b
  mvi 0b00100111, b
  not b
  mvi 0b11011000, a
  call assert_a_eq_b

  ; not c
  mvi 0b10110000, c
  not c
  mvi 0b01001111, a
  mov c, b
  call assert_a_eq_b

  ; halt successfully
  mvi 0, c
  out c
  hlt

; assert_a_eq_b compares registers a and b, and
; halts with -1 on the decimal display if they 
; are not equal.
assert_a_eq_b:
  cmp a, b
  je assert_succeeded
  mvi -1, c
  out c
  hlt
assert_succeeded:
  ret
