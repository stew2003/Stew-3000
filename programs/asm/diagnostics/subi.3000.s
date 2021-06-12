entry:
  ; subi byte, a
  mvi 16, a
  subi 20, a
  mvi -4, b
  call assert_a_eq_b

  ; subi byte, b
  mvi 47, b
  subi -13, b
  mvi 60, a
  call assert_a_eq_b

  ; subi byte, c
  mvi 100, c
  subi 14, c
  mvi 86, a
  mov c, b
  call assert_a_eq_b

  ; subi byte, sp
  subi 16, sp
  ; NOTE: manually observe: sp = -16

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
